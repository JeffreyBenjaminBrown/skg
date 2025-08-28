use crate::types::{ID};

use std::io::Write;
use std::net::TcpStream; // handles two-way communication

pub fn send_response (
  stream   : &mut TcpStream,
  response : &str) {
  writeln! ( // appends a newline
    stream, "{}", response )
    . unwrap ();
  stream . flush () . unwrap () ; }

pub fn send_response_with_length_prefix (
  // Responds "Content-Length: <bytes>\r\n\r\n" + payload
  stream   : &mut TcpStream,
  response : &str)
{ let payload = response.as_bytes ();
  let header  = format! ( "Content-Length: {}\r\n\r\n",
                           payload.len () );
  use std::io::Write as _;
  stream . write_all ( header.as_bytes () ) . unwrap ();
  stream . write_all ( payload )            . unwrap ();
  stream . flush ()                         . unwrap ();
}

pub fn request_type_from_request (
  request : &str
) -> Result<String, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(request . \"",
                                   "request type" ) }

pub fn node_id_from_single_root_view_request (
  request : &str
) -> Result<ID, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(id . \"",
                                   "ID" )
    . map(ID) }

pub fn search_terms_from_request (
  request : &str
) -> Result<String, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(terms . \"",
                                   "search terms" ) }

/// Returns the string between the first appearance of `pattern`
/// and the next quotation mark.
fn extract_quoted_value_from_sexp (
  request    : &str,
  pattern    : &str,
  field_name : &str
) -> Result < String, String > {
  // TODO: this is brittle to API changes
  // (athough seems safe as the API stands today).
  // It seems better to do proper s-exp parsing.

  if let Some ( start_pos ) = request.find ( pattern ) {
    let value_start = start_pos + pattern.len ();
    if let Some ( end_pos ) =
      request [ value_start.. ].find("\"")
    { Ok ( request [ value_start ..
                     (value_start + end_pos) ]
           . to_string () )
    } else { Err ( format! (
      "Could not find end quote for {} in request", field_name ) ) }
  } else { Err ( format! (
    "Could not find {} in request", field_name ) ) } }
