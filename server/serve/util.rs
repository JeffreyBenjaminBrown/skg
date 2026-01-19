use crate::types::sexp::extract_v_from_kv_pair_in_sexp;

use sexp::{Sexp, Atom};
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
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
{ let payload : &[u8] = response.as_bytes ();
  let header : String = format! ( "Content-Length: {}\r\n\r\n",
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

/// Format buffer content and errors as an s-expression.
/// Format: ((content "...") (errors ("error1" "error2" ...)))
/// This is shared by save_buffer and single_root_view handlers.
pub(super) fn format_buffer_response_sexp (
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

/// Reads length-prefixed content from the stream.
/// Expected format:
///   "Content-Length: N\r\n\r\n" followed by N bytes of content.
pub(super) fn read_length_prefixed_content (
  reader : &mut BufReader <TcpStream>
) -> Result<String, Box<dyn Error>> {

  // Consume header lines already in this reader's buffer,
  // then read exactly Content-Length bytes from the same reader.
  let mut header_lines : Vec <String> =
    Vec::new ();
  loop { // Read header lines until reaching the empty line.
    let mut line : String = String::new();
    reader.read_line ( &mut line )?;
    if line == "\r\n" { break; }
    header_lines.push (line); }
  let content_length : usize =
    header_lines
    .iter()
    .find_map ( |line| {
      if line.starts_with("Content-Length: ")
      { line.strip_prefix("Content-Length: ")
        . and_then ( |s|
                      s.trim() . parse::<usize> () . ok( ))
      } else { None }} )
    . ok_or ("Content-Length header not found") ?;
  let mut buffer : Vec<u8> = // Read content_length bytes.
    vec! [0u8; content_length] ;
  reader.read_exact (&mut buffer) ?;
  let content : String =
    String::from_utf8 (buffer) ?;
  Ok (content) }
