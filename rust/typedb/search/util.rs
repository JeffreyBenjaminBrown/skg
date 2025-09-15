use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::StreamExt;

use crate::types::ID;


/// Runs a single TypeDB query.
/// Returns the node's filepath.
pub async fn pid_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result < ID, Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let query = format! (
    r#"match
      $node isa node,
            has id $primary_id;
      {{ $node has id "{}"; }} or
      {{ $e   isa     extra_id, has id "{}";
         $rel isa has_extra_id ( node: $node,
                                 extra_id: $e ); }} ;
      select $primary_id;"#,
    node_id,
    node_id );
  let answer : QueryAnswer = tx.query ( query ). await ?;
  let mut stream = answer.into_rows ();
  if let Some (row_result) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some (concept) = row.get ("primary_id") ? {
      return Ok (
        ID (
          extract_payload_from_typedb_string_rep (
            &concept . to_string () )) ); }}
  Err ( format! ( "No primary id found for ID '{}'",
                   node_id )
        . into () ) }

pub fn extract_payload_from_typedb_string_rep (
  // Returns the string it finds
  // between the first and the second quotation marks.
  attribute_str : &str )
  -> String {

  if let Some (first_quote_pos) =
    attribute_str                          . find ('"')
  { if let Some (second_quote_pos) =
    attribute_str [first_quote_pos + 1 ..] . find ('"')
    { return attribute_str [ first_quote_pos + 1 ..
                             first_quote_pos + 1 + second_quote_pos ]
        . to_string (); }}
  panic! ( "Failed to extract payload from TypeDB output: {}",
            attribute_str ) }