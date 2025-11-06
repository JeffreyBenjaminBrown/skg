use crate::types::misc::ID;

use sexp::{Atom, Sexp};
use std::io::Write;
use std::net::TcpStream; // handles two-way communication

/// Extract a string value from an S-expression key-value pair.
/// Expected format: (.. (key . "value") ..)
pub fn extract_v_from_kv_pair_in_sexp (
  sexp : &Sexp,
  key  : &str,
) -> Result<String, String> {
  match sexp {
    Sexp::List ( items ) => {
      for item in items {
        if let Sexp::List ( pair ) = item {
          if pair.len() == 3 {
            if let ( Sexp::Atom ( Atom::S ( k ) ),
                     Sexp::Atom ( Atom::S ( dot ) ),
                     Sexp::Atom ( Atom::S ( value ) ) ) =
              ( &pair[0], &pair[1], &pair[2] )
            { if k == key && dot == "." {
              return Ok ( value.clone() ); }} }} }
      Err ( format! (
        "No {} field found in S-expression", key ) ) },
    _ => Err ( "Expected list as top-level S-expression"
                . to_string() ) }}

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
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, "request" ) }

pub fn node_id_from_single_root_view_request (
  request : &str
) -> Result<ID, String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, "id" )
    . map(ID) }

pub fn search_terms_from_request (
  request : &str
) -> Result<String, String> {
  let sexp : Sexp =
    sexp::parse ( request )
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, "terms" ) }
