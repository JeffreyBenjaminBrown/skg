use crate::serve::protocol::{RequestType, ResponseType};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::memory::ViewUri;

use sexp::{Sexp, Atom};
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;

/// Prepend a (response-type "TYPE") entry to an existing s-exp string.
/// Input:  "((content "...") (errors (...)))"
/// Output: "(("response-type" "TYPE") (content "...") (errors (...)))"
pub fn tag_sexp_response (
  response_type : ResponseType,
  sexp_payload  : &str,
) -> String {
  let tag : String =
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( response_type . repr_in_client ()
                             . to_string () )),
    ] ) . to_string ();
  format! ( "({} {}", tag, &sexp_payload [ 1.. ] ) }

/// Wrap plain text in a tagged s-exp for LP delivery.
/// Output: (("response-type" "TYPE") ("content" "TEXT"))
pub fn tag_text_response (
  response_type : ResponseType,
  text          : &str,
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( response_type . repr_in_client ()
                             . to_string () )), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content"       . to_string () )),
      Sexp::Atom ( Atom::S ( text            . to_string () )), ] ),
  ] ) . to_string () }

pub fn send_response (
  stream   : &mut TcpStream,
  response : &str,
) { if let Err (e) = writeln! (stream, "{}", response) {
      tracing::error!("Failed to send response: {}", e);
      return; }
    if let Err (e) = stream . flush () {
      tracing::error!("Failed to flush response: {}", e); } }

pub fn send_response_with_length_prefix (
  // Responds "Content-Length: <bytes>\r\n\r\n" + payload
  stream   : &mut TcpStream,
  response : &str,
) { let payload : &[u8] = response . as_bytes ();
    let preview_len : usize = payload . len () . min (200);
    let preview : &str = &response [..preview_len];
    tracing::debug!("Sending response ({} bytes): {}{}",
             payload . len (), preview,
             if payload . len () > 200 { "..." } else { "" });
    let header : String = format! ( "Content-Length: {}\r\n\r\n",
                                     payload . len () );
    if let Err (e) = stream . write_all ( header . as_bytes () ) {
      tracing::error!("Failed to send length-prefixed response: {}", e);
      return; }
    if let Err (e) = stream . write_all (payload) {
      tracing::error!("Failed to send length-prefixed response: {}", e);
      return; }
    if let Err (e) = stream . flush () {
      tracing::error!("Failed to flush length-prefixed response: {}", e); } }

pub fn request_type_from_request (
  request : &str
) -> Result<RequestType, String> {
  let sexp : Sexp =
    sexp::parse (request)
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  let request_value : String =
    extract_v_from_kv_pair_in_sexp ( &sexp, "request" ) ?;
  RequestType::from_client_string (&request_value) }

pub fn view_uri_from_request (
  request : &str,
) -> Result<ViewUri, String> {
  value_from_request_sexp ( "view-uri", request )
    . map (ViewUri::from_client_string) }

/// Extract a value from a request like
/// ((request . "type") (key . 'xyz))
pub fn value_from_request_sexp (
  key     : &str,
  request : &str,
) -> Result<String, String> {
  let sexp : Sexp =
    sexp::parse (request)
    . map_err ( |e| format! (
      "Failed to parse S-expression: {}", e ) ) ?;
  extract_v_from_kv_pair_in_sexp ( &sexp, key ) }

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

/// Format buffer content, errors, and other-views-to-update as an s-expression.
/// Format: ((content "...") (errors ("e1" ...)) (other-views-to-update (("URI1" "c1") ...)))
pub(super) fn format_buffer_response_sexp_with_updates (
  buffer_content   : &str,
  errors           : &[String],
  collateral_views : &[(ViewUri, String)],
) -> String {
  let collateral_views_sexp : Sexp =
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "other-views-to-update" . to_string () )),
      Sexp::List (
        collateral_views
          . iter ()
          . map ( |(uri, content)| Sexp::List ( vec! [
            Sexp::Atom ( Atom::S ( uri . repr_in_client () )),
            Sexp::Atom ( Atom::S ( content . clone () )) ] ) )
          . collect () ) ] );
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
          . collect () ) ] ),
    collateral_views_sexp ] )
    . to_string () }

/// Format: ((lock-collateral-views ("URI1" "URI2" ...)))
pub(super) fn format_collateral_uris_sexp (
  uris : &[ViewUri],
) -> String {
  let uri_sexps : Vec<Sexp> =
    uris . iter ()
    . map ( |u| Sexp::Atom ( Atom::S ( u . repr_in_client () )) )
    . collect ();
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "lock-collateral-views" . to_string () )),
      Sexp::List ( uri_sexps ) ] ) ] )
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
    reader . read_line (&mut line)?;
    if line == "\r\n" { break; }
    header_lines . push (line); }
  let content_length : usize =
    header_lines
    . iter()
    . find_map ( |line| {
      if line . starts_with ("Content-Length: ")
      { line . strip_prefix ("Content-Length: ")
        . and_then ( |s|
                      s . trim() . parse::<usize> () . ok( ))
      } else { None }} )
    . ok_or ("Content-Length header not found") ?;
  let mut buffer : Vec<u8> = // Read content_length bytes.
    vec! [0u8; content_length] ;
  reader . read_exact (&mut buffer) ?;
  let content : String =
    String::from_utf8 (buffer) ?;
  Ok (content) }
