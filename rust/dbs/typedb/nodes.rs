use crate::types::{ID, SkgNode};
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;

use futures::StreamExt;
use std::collections::{ HashSet, BTreeSet };
use std::error::Error;
use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use typedb_driver::answer::QueryAnswer;

pub async fn create_all_nodes (
  // Maps `create_node` over `nodes`, creating:
  //   all `node`         entities
  //   all `extra_id`     entities
  //   all `has_extra_id` relationships
  // Then commits.
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &[SkgNode]
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Write )
    . await ?;
  println!("Creating nodes ...");
  for node in nodes {
    create_node ( node, &tx )
      . await ?; }
  tx . commit () . await ?;
  Ok (()) }

/// Creates (in TypeDB) nodes whose primary IDs are not in the DB.
/// Returns the number of nodes created.
pub async fn create_only_nodes_with_no_ids_present (
  db_name : &str,
  driver  : &TypeDBDriver,
  nodes   : &Vec <SkgNode>
) -> Result < usize, Box<dyn Error> > {

  let mut all_pids // All the primary IDs.
    : BTreeSet < String >
    = BTreeSet::new ();
  for node in nodes {
    // Don't check that the list is nonempty, because that should bork.
    let pid: &ID = &node.ids[0];
    all_pids.insert ( pid.to_string () ); }
  let known_ids : HashSet < String > =
    which_ids_exist (
      db_name,
      driver,
      &all_pids
    ) . await ?;
  let mut to_create : Vec < &SkgNode > =
    Vec::new ();
  for node in nodes {
    let pid: &ID = &node.ids[0];
    if ! known_ids.contains (
      pid.as_str () )
    { to_create.push ( node ); }}
  { // Create them.
    let tx : Transaction =
      driver . transaction (
        db_name,
        TransactionType::Write
      ) . await ?;
    println! ( "Creating {} new nodes ...",
                to_create.len () );
    for node in &to_create {
      create_node ( node, &tx ) . await ?; }
    tx . commit () . await ?; }
  Ok ( to_create.len () ) }

/// Batch existence check:
/// Given a set of candidate ID *strings*,
/// return the subset that already exist in the DB.
pub async fn which_ids_exist (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &BTreeSet < String >
) -> Result < HashSet < String >, Box<dyn Error> > {

  if ids.is_empty () {
    return Ok ( HashSet::new () ); }
  let or_block : String =
    ids . iter ()
    . map ( |v| {
      format! (
        r#"{{ $found isa id;
                $found == "{}"; }}"#,
        v ) } )
    . collect::<Vec<String>> ()
    . join ( " or\n" );
  let query : String = format! (
    "match\n{}\n;\nselect $found;",
    or_block );
  let tx : Transaction =
    driver . transaction (
      db_name,
      TransactionType::Read
    ) . await ?;
  let answer : QueryAnswer =
    tx . query ( query ) . await ?;
  let mut found : HashSet < String > =
    HashSet::new ();
  let mut rows = // TODO ? Is this hard to assign a type signature?
    answer . into_rows ();
  while let Some ( row_res ) = rows . next () . await {
    let row = row_res ?;
    if let Some ( concept ) =
      row . get ( "found" ) ? {
        let payload : String =
          extract_payload_from_typedb_string_rep (
            & concept . to_string () );
        found.insert ( payload ); }}
  Ok ( found ) }

pub async fn create_node (
  // Creates: the `node`,
  //          any `extra_id` entities it needs.
  //          any `has_extra_id` relationships it needs.
  // Does *not* commit.
  node: &SkgNode,
  tx: &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  if node . ids . is_empty () {
    return Err ( "SkgNode with no IDs.".into() ); }
  let primary_id : &str =
    node . ids [0] . as_str ();
  let insert_node_query : String = format! (
    r#"insert $n isa node,
                 has id "{}",
                 has source "{}";"#,
    primary_id,
    node . source );
  tx . query ( insert_node_query ) . await ?;
  insert_extra_ids ( &node, tx ) . await ?; // PITFALL: This creates has_extra_id relationships, so you might expect it to belong in `create_relationships_from_node`. But it's important that these relationships be created before any others, because the others might refer to nodes via their `extra_id`s. They are basically optional attributes of a node; they have no meaning beyond being another way to refer to a node.
  Ok (()) }

pub async fn insert_extra_ids (
  node : &SkgNode,
  tx   : &typedb_driver::Transaction
) -> Result < (), Box<dyn Error> > {

  if node.ids.len () > 1 {
    let primary_id : &str =
      node . ids [0] . as_str ();
    let extra_ids: Vec < &ID > =
      node . ids . iter() . skip(1) . collect();
    for extra_id in extra_ids {
      tx.query (
        format! ( r#"
                    match
                        $n isa node, has id "{}";
                    insert
                        $e isa extra_id, has id "{}";
                        $r isa has_extra_id
                           ( node: $n,
                             extra_id: $e ); "#,
                    primary_id,
                    extra_id.as_str () ))
        . await ?; }}
  Ok (()) }

/// ASSUMES: All input IDs are PIDs.
/// PURPOSE: Delete the node corresponding to every ID it receives,
/// including its extra IDs.
pub async fn delete_nodes_from_pids (
  db_name : &str,
  driver  : &TypeDBDriver,
  ids     : &[ID]
) -> Result < (), Box<dyn Error> > {
  if ids.is_empty() { return Ok ( () ); }
  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Write
    ). await ?;
  let pid_or_clause : String =
    ids . iter()
    . map ( |id| format! (
      r#"{{ $node has id "{}"; }}"#,
      id.0 ) )
    . collect::< Vec<_> > ()
    . join ( " or\n" );
  let extra_ids_query : String = format! (
    // To find every associated extra_id.
    r#"match $node isa node;
      {};
      $e isa extra_id;
      $rel isa has_extra_id ( node: $node, extra_id: $e );
      $e has id $extra_id_value;
      select $extra_id_value;"#,
    pid_or_clause );
  let answer : QueryAnswer =
    tx.query ( extra_ids_query ). await ?;
  let mut extra_id_values : Vec<String> =
    Vec::new();
  let mut rows = answer.into_rows();
  while let Some(row_res) = rows.next().await {
    let row = row_res?;
    if let Some(concept) = row.get("extra_id_value")? {
      let extra_id_value : String =
        extract_payload_from_typedb_string_rep(
          &concept.to_string());
      extra_id_values.push (extra_id_value); }}
  { // delete nodes
    let delete_nodes_from_pids_query : String = format! (
      r#"match $node isa node;
      {};
      delete $node;"#,
      pid_or_clause );
    let _answer : QueryAnswer = tx.query (
      delete_nodes_from_pids_query ). await ?; }
  { // Delete extra_ids
    if !extra_id_values.is_empty() {
      let extra_id_or_clause : String =
        extra_id_values . iter()
        . map ( |id| format! (
          r#"{{ $e has id "{}"; }}"#,
          id ) )
        . collect::< Vec<_> > ()
        . join ( " or\n" );
      let delete_extra_ids_query : String = format! (
        r#"match $e isa extra_id;
        {};
        delete $e;"#,
        extra_id_or_clause );
      let _answer : QueryAnswer = tx.query (
        delete_extra_ids_query ). await ?; }}
  tx . commit (). await ?;
  Ok ( () ) }
