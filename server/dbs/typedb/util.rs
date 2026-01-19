pub mod pids_from_ids;
pub mod concept_document;

use std::error::Error;
use typedb_driver::{TypeDBDriver, DatabaseManager};

pub async fn delete_database (
  driver  : &TypeDBDriver,
  db_name : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let databases : &DatabaseManager = driver . databases ();
  if databases . contains ( db_name ) . await ? {
    databases . get ( db_name ) . await ? . delete () . await ?;
    println! ( "Database '{}' deleted successfully", db_name ); }
  Ok (( )) }

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
pub(super) fn conjugate_binary_role (
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
