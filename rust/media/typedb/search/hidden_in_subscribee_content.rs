/// Find which hidden IDs are in any subscribee's content.
///
/// Given a list of hidden IDs and a list of subscribee IDs,
/// returns the subset of hidden IDs that appear as direct content
/// of any subscribee.

use std::collections::HashSet;
use std::error::Error;
use futures::StreamExt;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

use crate::types::ID;
use crate::media::typedb::util::{
  build_id_disjunction,
  extract_payload_from_typedb_string_rep};

/// Returns the hidden IDs that ARE in any subscribee's direct content.
/// The caller can compute the complement (hidden outside content)
/// by subtracting from the original hidden set.
pub async fn hidden_ids_in_subscribee_content (
  db_name        : &str,
  driver         : &TypeDBDriver,
  hidden_ids     : &[ID],
  subscribee_ids : &[ID],
) -> Result < HashSet < ID >, Box < dyn Error > > {
  if hidden_ids . is_empty () || subscribee_ids . is_empty () {
    return Ok ( HashSet::new () ); }

  let tx : Transaction =
    driver . transaction (
      db_name, TransactionType::Read
    ) . await ?;

  let hidden_disjunction : String =
    build_id_disjunction ( hidden_ids, "hidden_id" );
  let subscribee_disjunction : String =
    build_id_disjunction ( subscribee_ids, "subscribee_id" );

  // Query: find hidden nodes that are direct content of any subscribee
  let query : String = format! ( r#"match
      $hidden isa node, has id $hidden_id;
      $subscribee isa node, has id $subscribee_id;
      $rel isa contains ( container: $subscribee,
                          contained: $hidden );
      {};
      {};
      select $hidden_id;"#,
    hidden_disjunction,
    subscribee_disjunction );

  let answer : QueryAnswer = tx . query ( query ) . await ?;
  let mut stream = answer . into_rows ();
  let mut result : HashSet < ID > = HashSet::new ();

  while let Some ( row_result ) = stream . next () . await {
    let row : ConceptRow = row_result ?;
    if let Some ( concept ) = row . get ( "hidden_id" ) ? {
      result . insert ( ID (
        extract_payload_from_typedb_string_rep (
          & concept . to_string () ) ) ); } }

  Ok ( result ) }
