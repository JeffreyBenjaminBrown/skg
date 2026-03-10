use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;

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
  nodes   : &[SkgNode]
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
  nodes   : &Vec <SkgNode>
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
  let to_create : Vec < &SkgNode > =
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
  node    : &SkgNode,
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
  let results : Vec < Option < String > > =
    stream::iter ( ids . iter ()
      . map ( |id| check_one_id_exists (
                db_name, driver, id . clone () )) )
    . buffer_unordered (
        crate::consts::TYPEDB_CONCURRENT_TRANSACTIONS )
    . collect () . await;
  let found : HashSet < String > =
    results . into_iter ()
    . flatten ()
    . collect ();
  Ok (found) }

/// Check whether a single ID exists in the DB.
/// Returns Some(id) if it exists, None otherwise.
async fn check_one_id_exists (
  db_name : &str,
  driver  : &TypeDBDriver,
  id      : String,
) -> Option < String > {
  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read ) . await . ok () ?;
  let answer : QueryAnswer =
    tx . query ( format! (
      "match $n isa node, has id \"{}\"; select $n;",
      id ) ) . await . ok () ?;
  let mut rows : ConceptRowStream = answer . into_rows ();
  if rows . next () . await . is_some () {
    Some (id) }
  else { None } }

pub async fn create_node (
  // Creates: the `node`,
  //          any `extra_id` entities it needs.
  //          any `has_extra_id` relationships it needs.
  // Does *not* commit.
  node: &SkgNode,
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
  node : &SkgNode,
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
       delete $rel, $e;"#,
    id . as_str () ) ) . await . ok (); // ok(): no-op if none exist
  // Delete the node.
  tx . query ( format! (
    r#"match $node isa node, has id "{}";
       delete $node;"#,
    id . as_str () ) ) . await ?;
  tx . commit () . await ?;
  Ok (()) }
