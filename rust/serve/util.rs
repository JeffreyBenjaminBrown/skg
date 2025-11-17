use crate::media::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::misc::ID;

use sexp::{Sexp, Atom};
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

/// Format buffer content and errors as an s-expression.
/// Format: ((content "...") (errors ("error1" "error2" ...)))
/// This is shared by save_buffer and single_root_view handlers.
pub fn format_buffer_response_sexp (
  buffer_content : &str,
  errors         : &[String]
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List (
        errors
          . iter ()
          . map ( |e| Sexp::Atom (
            Atom::S ( e . clone () )) )
          . collect () ) ] ) ] )
    . to_string () }
