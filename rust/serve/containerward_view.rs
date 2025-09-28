use crate::render::containerward_org_view;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::types::{ID, SkgConfig};
use crate::text_to_orgnodes::cursor::count_headline_level;
use crate::text_to_orgnodes::interpreted::parse_separating_metadata_and_title;
use crate::text_to_orgnodes::types::OrgNodeMetadata;

use futures::executor::block_on;
use sexp::{Atom, Sexp};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Extracts parameters from a containerward view request,
/// generates the containerward org view,
/// and sends the response with length prefix.
pub fn handle_containerward_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match extract_containerward_view_params ( request ) {
    Ok ( (node_id, level) ) => {
      send_response_with_length_prefix (
        stream,
        & containerward_view_wrapped (
          &node_id,
          level,
          typedb_driver,
          & config,
        )); },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting containerward view parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg );
    }
  }
}

/// Wrapper for containerward_org_view with async and error handling.
fn containerward_view_wrapped (
  node_id       : &ID,
  level         : usize,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  block_on (
    async {
      match containerward_org_view (
        typedb_driver,
        config,
        node_id,
        level ) . await
      { Ok  (s) => s,
        Err (e) => format!(
          "Error generating containerward view: {}", e), }} ) }

/* Parses a request like
'((request . "containerward view") (headline . "HEADLINE"))'
where HEADLINE includes asterisks and metadata but not the newline,
and extracts the node ID and level from the headline. */
pub fn extract_containerward_view_params (
  request : &str
) -> Result<(ID, usize), String> {

  // Parse the S-expression
  let sexp = sexp::parse ( request )
    . map_err ( |e| format! ( "Failed to parse S-expression: {}", e ) ) ?;

  // Extract headline from the parsed S-expression
  let headline_text = extract_headline_from_sexp ( &sexp ) ?;

  // Parse level from asterisks
  let level : usize =
    count_headline_level ( &headline_text )
    .ok_or ( "Invalid headline format - could not count asterisks" ) ?;

  // Extract the text after asterisks and whitespace for metadata parsing
  let mut i : usize = 0;
  let bytes : &[u8] = headline_text.as_bytes();
  while i < bytes.len() && bytes[i] == b'*' {
    i += 1; } // skip asterisks
  while i < bytes.len() && (bytes[i] == b' ' || bytes[i] == b'\t') {
    i += 1; } // skip whitespace
  let line_after_bullet : &str = &headline_text[i..];

  // Parse metadata to get ID
  let (metadata, _title) = parse_separating_metadata_and_title ( line_after_bullet );
  let node_id : ID = metadata.id
    .ok_or ( "No ID found in headline metadata" ) ?;

  Ok (( node_id, level ))
}

/// Extract the headline value from a parsed S-expression
/// Expected format: (.. (headline . "HEADLINE") ..)
fn extract_headline_from_sexp (
  sexp : &Sexp
) -> Result<String, String> {
  match sexp {
    Sexp::List ( items ) => {
      for item in items {
        if let Sexp::List ( pair ) = item {
          if pair.len() == 3 {
            if let ( Sexp::Atom ( Atom::S ( key ) ),
                     Sexp::Atom ( Atom::S ( dot ) ),
                     Sexp::Atom ( Atom::S ( value ) ) ) =
              ( &pair[0], &pair[1], &pair[2] )
            { if key == "headline" && dot == "." {
              return Ok ( value.clone() ); }} }} }
      Err ( "No headline field found in S-expression"
             . to_string() ) },
    _ => Err ( "Expected list as top-level S-expression"
                . to_string() ) }}
