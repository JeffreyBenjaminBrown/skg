pub mod pids_from_ids;
pub use pids_from_ids::{
  pids_from_ids,
  collect_ids_for_pid_lookup,
  assign_pids_from_map};

pub mod concept_document;
pub use concept_document::{
  extract_id_from_node,
  extract_id_from_map,
  build_id_disjunction};

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
/// Returns the PID associated with that ID, or None if not found.
pub async fn pid_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result < Option<ID>, Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let query : String = format! (
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
        Some ( ID (
          extract_payload_from_typedb_string_rep (
            &concept . to_string () )) ) ); }}
  Ok ( None ) }

/// Returns the string it finds
/// between the first and the second quotation marks.
pub fn extract_payload_from_typedb_string_rep (
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

/// Conjugates a binary role within a specified relation.
/// Given a relation name and one role, returns the other role.
/// Returns an error if the role doesn't belong to the relation.
pub fn conjugate_binary_role (
  relation : &str,
  role     : &str
) -> Result < &'static str,
                Box < dyn Error > > {
  match ( relation, role ) {
    ( "contains", "container" )            => Ok ( "contained" ),
    ( "contains", "contained" )            => Ok ( "container" ),
    ( "textlinks_to", "source" )          => Ok ( "dest" ),
    ( "textlinks_to", "dest" )            => Ok ( "source" ),
    ( "subscribes", "subscriber" )         => Ok ( "subscribee" ),
    ( "subscribes", "subscribee" )         => Ok ( "subscriber" ),
    ( "overrides_view_of", "replacement" ) => Ok ( "replaced" ),
    ( "overrides_view_of", "replaced" )    => Ok ( "replacement" ),
    ( "hides_from_its_subscriptions", "hider" )  => Ok ( "hidden" ),
    ( "hides_from_its_subscriptions", "hidden" ) => Ok ( "hider" ),
    _ => Err (
      format! (
        "Role '{}' does not belong to relation '{}'",
        role, relation
      ) . into () )
  }
}
