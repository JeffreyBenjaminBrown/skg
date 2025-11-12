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
  answer::{QueryAnswer,
           concept_document::{Node, Leaf}},
  Transaction,
  TransactionType,
  TypeDBDriver,
};
use futures::StreamExt;

use crate::types::ID;

pub async fn delete_database (
  driver  : &TypeDBDriver,
  db_name : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let databases = driver . databases ();
  if databases . contains ( db_name ) . await ? {
    databases . get ( db_name ) . await ? . delete () . await ?;
    println! ( "Database '{}' deleted successfully", db_name ); }
  Ok (( )) }

/// Runs a single TypeDB query to get both PID and source.
/// Returns None if not found.
pub async fn pid_and_source_from_id (
  db_name : &str,
  driver  : &TypeDBDriver,
  node_id : &ID
) -> Result < Option<(ID, String)>, Box<dyn Error> > {
  use Node;

  let tx : Transaction =
    driver.transaction (
      db_name, TransactionType::Read
    ). await ?;
  let query : String = format! (
    r#"match
      $node isa node,
            has id $primary_id,
            has source $source;
      {{ $node has id "{}"; }} or
      {{ $e   isa     extra_id, has id "{}";
         $rel isa has_extra_id ( node: $node,
                                 extra_id: $e ); }} ;
      fetch {{
        "primary_id": $primary_id,
        "source": $source
      }};"#,
    node_id,
    node_id );
  let answer : QueryAnswer = tx.query ( query ). await ?;

  if let QueryAnswer::ConceptDocumentStream ( _, mut stream ) =
    answer {
      if let Some (doc_result) = stream . next () . await {
        let doc = doc_result ?;
        if let Some ( Node::Map ( ref map ) ) = doc . root {
          let primary_id_opt : Option < ID > =
            map . get ( "primary_id" )
            . and_then ( extract_id_from_node );
          let source_opt : Option < String > =
            map . get ( "source" )
            . and_then ( | node : & Node | {
              if let Node::Leaf ( Some ( leaf ) ) = node {
                if let Leaf::Concept ( concept ) = leaf {
                  return Some (
                    extract_payload_from_typedb_string_rep (
                      & concept . to_string () ) ); }}
              None } );
          if let ( Some ( pid ), Some ( source ) )
            = ( primary_id_opt, source_opt )
          { return Ok ( Some ( ( pid, source ) ) ); }} }}
  Ok (None) }

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
