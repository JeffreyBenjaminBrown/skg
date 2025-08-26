use crate::typedb::create::nodes::create_node;
use crate::typedb::search::extract_payload_from_typedb_string_rep;
use crate::types::FileNode;

use futures::StreamExt;
use std::collections::{ HashSet, BTreeSet };
use std::error::Error;
use typedb_driver::{
  Transaction,
  TransactionType,
  TypeDBDriver,
};


/* Creates FileNodes whose IDs are not present in the DB.
Returns the number of nodes created.
.
PITFALL: A node is only added if *all* of its IDs are absent from the DB. See the TODO comment in the code. */
pub async fn create_unknown_nodes_only (
  db_name   : &str,
  driver    : &TypeDBDriver,
  filenodes : &Vec <FileNode>
) -> Result < usize, Box<dyn Error> > {

  let mut all_ids // all the input IDs
    : BTreeSet < String >
    = BTreeSet::new ();
  for fnode in filenodes {
    for id in &fnode.ids {
      /* TODO ? Should I only include primary IDs here? It would be strange if a node's primary ID was unknown to the database but one of its extra IDs was known. */
      all_ids.insert ( id.to_string () ); }}
  let known_ids : HashSet < String > =
    fetch_existing_ids_batch (
      db_name,
      driver,
      &all_ids
    ) . await ?;
  let mut to_create : Vec < &FileNode > =
    Vec::new ();
  for fnode in filenodes {
    let any_known : bool =
      fnode . ids . iter ()
      . any ( |id| known_ids.contains ( id.as_str () ) );
    if ! any_known {
      to_create.push ( fnode ); }}
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
async fn fetch_existing_ids_batch (
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
  let answer = tx . query ( query ) . await ?;
  let mut found : HashSet < String > =
    HashSet::new ();
  let mut rows = answer . into_rows ();
  while let Some ( row_res ) = rows . next () . await {
    let row = row_res ?;
    if let Some ( concept ) =
      row . get ( "found" ) ? {
        let payload : String =
          extract_payload_from_typedb_string_rep (
            & concept . to_string () );
        found.insert ( payload ); }}
  Ok ( found ) }
