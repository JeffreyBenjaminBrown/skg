use crate::types::misc::{ID, SourceName};
use crate::types::nodes::typedb::NodeTypedb;

use super::util::ConceptRowStream;

use futures::stream::{self, StreamExt};
use std::collections::{ HashSet, BTreeSet };
use std::error::Error;
use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use typedb_driver::answer::QueryAnswer;

/// Maps `create_one_node_in_own_tx` over `nodes`,
/// bounded by TYPEDB_CONCURRENT_TRANSACTIONS
/// to avoid overwhelming TypeDB with concurrent transactions.
/// Creates all `node` entities, all `extra_id` entities,
/// and all `has_extra_id` relationships.
///
/// PITFALL: In the TypeDB sense (but not the skg sense)
/// this creates relationships -- 'has_extra_id' ones, specifically.
pub async fn create_all_nodes (
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &[NodeTypedb]
)-> Result < (), Box<dyn Error> > {
  tracing::info!("Creating nodes ...");
  let results : Vec < Result < (), Box < dyn Error > > > =
    stream::iter ( nodes . iter ()
      . map ( |node| create_one_node_in_own_tx (
                db_name, driver, node )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  for result in results {
    result ?; }
  Ok (()) }

/// Creates (in TypeDB) nodes whose primary IDs are not in the DB.
/// Returns the number of nodes created.
pub async fn create_only_nodes_with_no_ids_present (
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &Vec <NodeTypedb>
) -> Result < usize, Box<dyn Error> > {
  let mut all_pids : BTreeSet < String > =
    BTreeSet::new ();
  for node in nodes {
    all_pids . insert (
      node . pid . to_string() ); }
  let known_ids : HashSet < String > =
    which_ids_exist (
      db_name,
      driver,
      &all_pids
    ) . await ?;
  let to_create : Vec < &NodeTypedb > =
    nodes . iter ()
    . filter ( |node| {
      let pid : &ID = &node . pid;
      ! known_ids . contains (pid . as_str ()) } )
    . collect ();
  tracing::info! ( "Creating {} new nodes ...",
              to_create . len () );
  let results : Vec < Result < (), Box < dyn Error > > > =
    stream::iter ( to_create . iter ()
      . map ( |node| create_one_node_in_own_tx (
                db_name, driver, node )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  for result in &results {
    if let Err (e) = result {
      return Err (format! ("Failed to create node: {}", e) . into()); }}
  Ok ( results . len () ) }

async fn create_one_node_in_own_tx (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &NodeTypedb,
) -> Result < (), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  create_node ( node, &tx ) . await ?;
  tx . commit () . await ?;
  Ok (()) }

/// Parallel existence check:
/// Given a set of candidate ID strings,
/// return the subset that already exist in the DB.
/// Sends one query per ID, bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
pub async fn which_ids_exist (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &BTreeSet < String >
) -> Result < HashSet < String >, Box<dyn Error> > {
  if ids . is_empty () {
    return Ok ( HashSet::new () ); }
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read ) . await ?;
  let result : Result < HashSet < String >, Box<dyn Error> > =
    which_ids_exist_in_tx ( &tx, ids ) . await;
  let close_result : typedb_driver::Result < () > =
    tx . close () . await;
  let found : HashSet < String > =
    result ?;
  close_result ?;
  Ok (found) }

async fn which_ids_exist_in_tx (
  tx  : &Transaction,
  ids : &BTreeSet < String >,
) -> Result < HashSet < String >, Box<dyn Error> > {
  let mut found : HashSet < String > =
    HashSet::new ();
  for id in ids {
    if check_one_id_exists_in_tx (tx, id) . await ? {
      found . insert (id . clone ()); }}
  Ok (found) }

/// Check whether a single ID exists in the DB.
/// Returns Some(id) if it exists, None otherwise.
async fn check_one_id_exists_in_tx (
  tx : &Transaction,
  id : &str,
) -> Result < bool, Box<dyn Error> > {
  let answer : QueryAnswer =
    tx . query ( format! (
      "match $n isa node, has id \"{}\"; select $n;",
      id ) ) . await ?;
  let result : bool = {
    let mut rows : ConceptRowStream = answer . into_rows ();
    match rows . next () . await {
      Some (Ok (_)) => true,
      Some (Err (e)) => return Err (e . into ()),
      None => false, } };
  Ok (result) }

pub async fn create_node (
  // Creates: the `node`,
  //          any `extra_id` entities it needs.
  //          any `has_extra_id` relationships it needs.
  // Does *not* commit.
  node: &NodeTypedb,
  tx: &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  let primary_id : &ID = &node . pid;
  tx . query ( {
    let insert_node_query : String = format! (
      r#"insert $n isa node,
                   has id "{}",
                   has source "{}";"#,
      primary_id . as_str (),
      node . source );
    insert_node_query } ) . await ?;
  insert_extra_ids ( &node, tx ) . await ?; // PITFALL: This creates has_extra_id relationships, so you might expect it to belong in `create_relationships_from_node`. But it's important that these relationships be created before any others, because the others might refer to nodes via their `extra_id`s. They are basically optional attributes of a node; they have no meaning beyond being another way to refer to a node.
  Ok (()) }

async fn insert_extra_ids (
  node : &NodeTypedb,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  if ! node . extra_ids . is_empty () {
    let primary_id : &ID = &node . pid;
    for extra_id in &node . extra_ids {
      tx . query (
        format! ( r#"
                    match
                        $n isa node, has id "{}";
                    insert
                        $e isa extra_id, has id "{}";
                        $r isa has_extra_id
                           ( node: $n,
                             extra_id: $e ); "#,
                    primary_id . as_str (),
                    extra_id . as_str () ))
        . await ?; }}
  Ok (()) }

/// Replace all has_extra_id relations (and their extra_id entities) for
/// this node with a fresh set derived from 'node.extra_ids'. Runs its
/// own transaction.
///
/// Needed by the save pipeline for /existing/ nodes being re-saved:
/// 'create_only_nodes_with_no_ids_present' skips existing nodes, so
/// their extra_ids never get updated through the normal create path.
///
/// PITFALL: This must complete for every to_write_pid /before/ any
/// 'create_all_relationships' call runs, because those calls resolve
/// target IDs through 'has_extra_id'. A neighbor save whose target is
/// the acquirer needs the freshly-added has_extra_id (acquiree_pid →
/// acquirer) to exist at lookup time.
pub async fn overwrite_extra_ids_of_node (
  db_name : &str,
  driver  : &TypeDBDriver,
  node    : &NodeTypedb,
) -> Result < (), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  // Delete old has_extra_id relations and their extra_id entities.
  tx . query ( format! (
    r#"match
         $node isa node, has id "{}";
         $e isa extra_id;
         $rel isa has_extra_id ( node: $node, extra_id: $e );
       delete $rel;
       delete $e;"#,
    node . pid . as_str () ) ) . await . ok (); // ok(): no-op if none exist
  insert_extra_ids ( node, &tx ) . await ?;
  tx . commit () . await ?;
  Ok (()) }

/// ASSUMES: All input IDs are PIDs.
/// PURPOSE: Delete the node corresponding to every ID it receives,
/// including its extra IDs.
/// Sends one transaction per node,
/// bounded by TYPEDB_CONCURRENT_TRANSACTIONS.
pub async fn delete_nodes_from_pids (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &[ID]
) -> Result < (), Box<dyn Error> > {
  if ids . is_empty() { return Ok ( () ); }
  let results : Vec < Result < (), Box < dyn Error > > > =
    stream::iter ( ids . iter ()
      . map ( |id| delete_one_node_from_pid (
                db_name, driver, id )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  for result in results {
    result ?; }
  Ok ( () ) }

/// Delete a single node and its extra IDs in one transaction.
async fn delete_one_node_from_pid (
  db_name : &str,
  driver  : &TypeDBDriver,
  id      : &ID,
) -> Result < (), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  // Find and delete extra_ids first (they reference the node).
  tx . query ( format! (
    r#"match
         $node isa node, has id "{}";
         $e isa extra_id;
         $rel isa has_extra_id ( node: $node, extra_id: $e );
       delete $rel;
       delete $e;"#,
    id . as_str () ) ) . await . ok (); // ok(): no-op if none exist
  // Delete the node.
  tx . query ( format! (
    r#"match $node isa node, has id "{}";
       delete $node;"#,
    id . as_str () ) ) . await ?;
  tx . commit () . await ?;
  Ok (()) }

/// Updates the source attribute of a node in TypeDB.
/// Deletes the old source and inserts the new one in a single transaction.
pub async fn update_node_source (
  db_name    : &str,
  driver     : &TypeDBDriver,
  pid        : &ID,
  new_source : &SourceName,
) -> Result<(), Box<dyn Error>> {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Write ) . await ?;
  tx . query ( format! (
    r#"match
         $n isa node, has id "{}";
         $n has source $old_src;
       delete
         has $old_src of $n;
       insert
         $n has source "{}";"#,
    pid . as_str(),
    new_source ) ) . await ?;
  tx . commit () . await ?;
  Ok (()) }
