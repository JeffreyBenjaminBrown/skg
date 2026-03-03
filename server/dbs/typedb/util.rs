pub mod pids_from_ids;
pub mod concept_document;

use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::answer::ConceptRow;
use typedb_driver::{Credentials, DatabaseManager, DriverOptions, TypeDBDriver};

pub type ConceptRowStream =
  futures::stream::BoxStream<'static,
                             typedb_driver::Result<ConceptRow>>;

pub(crate) fn connect_to_typedb () -> TypeDBDriver {
  println! ("Connecting to TypeDB...");
  block_on ( async {
    TypeDBDriver::new (
      "127.0.0.1:1729",
      Credentials::new ("admin", "password"),
      DriverOptions::new (false, None) . unwrap() )
      . await
      . unwrap_or_else ( |e| {
        eprintln! ("Error connecting to TypeDB: {}", e);
        std::process::exit (1); } ) } ) }

pub async fn delete_database (
  driver  : &TypeDBDriver,
  db_name : &str,
) -> Result<(), Box<dyn std::error::Error>> {
  let databases : &DatabaseManager = driver . databases ();
  if databases . contains (db_name) . await ? {
    databases . get (db_name) . await ? . delete () . await ?;
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
  for (rel, left, right) in OUTBOUND_RELATIONSHIP_TYPES {
    if *rel == relation {
      if role == *left { return Ok (right); }
      if role == *right { return Ok (left); }}}
  Err ( format! (
    "Role '{}' does not belong to relation '{}'",
    role, relation
  ) . into () ) }
