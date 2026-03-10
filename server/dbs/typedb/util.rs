pub mod pids_from_ids;
pub mod concept_document;

use futures::executor::block_on;
use typedb_driver::answer::ConceptRow;
use typedb_driver::{Credentials, DatabaseManager, DriverOptions, TypeDBDriver};

pub type ConceptRowStream =
  futures::stream::BoxStream<'static,
                             typedb_driver::Result<ConceptRow>>;

pub fn connect_to_typedb () -> TypeDBDriver {
  tracing::info! ("Connecting to TypeDB...");
  block_on ( async {
    TypeDBDriver::new (
      crate::consts::TYPEDB_ADDRESS,
      Credentials::new ("admin", "password"),
      DriverOptions::new (false, None) . unwrap() )
      . await
      . unwrap_or_else ( |e| {
        tracing::error! ("Error connecting to TypeDB: {}", e);
        std::process::exit (1); } ) } ) }

pub async fn delete_database (
  driver  : &TypeDBDriver,
  db_name : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let databases : &DatabaseManager = driver . databases ();
  if databases . contains (db_name) . await ? {
    databases . get (db_name) . await ? . delete () . await ?;
    tracing::info! ( "Database '{}' deleted successfully", db_name ); }
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

