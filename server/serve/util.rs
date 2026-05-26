use crate::serve::protocol::{RequestType, TcpToClient};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::views_state::ViewUri;

use sexp::{Sexp, Atom};
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;

/// Prepend a (response-type "TYPE") entry to an existing s-exp string.
/// Input:  "((content "...") (errors (...)) (warnings (...)))"
/// Output: "(("response-type" "TYPE") (content "...") (errors (...)) (warnings (...)))"
pub fn tag_sexp_response (
  response_type : TcpToClient,
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
  response_type : TcpToClient,
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
    let preview_len : usize = // PITFALL: floor_char_boundary is needed
      // because UTF-8 uses multiple bytes for some characters,
      // and slicing mid-character panics in Rust.
      response . floor_char_boundary ( payload . len () . min (200) );
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

/// Format buffer content, errors and warnings as an s-expression.
/// Format:
///   ((content "...") (errors ("error1" ...)) (warnings ("warning1" ...)))
/// This is shared by save_buffer and single_root_view handlers.
pub(crate) fn format_buffer_response_sexp (
  buffer_content : &str,
  errors         : &[String],
  warnings       : &[String],
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    format_string_list_sexp ("errors", errors),
    format_string_list_sexp ("warnings", warnings) ] )
    . to_string () }

/// Format a single view update as an s-expression.
/// Format: ((view-uri "URI") (content "CONTENT"))
/// Used for any streamed per-view message (collateral-view,
/// rerender-view, etc.). The caller tags it with the appropriate
/// TcpToClient variant.
pub(crate) fn format_single_view_sexp (
  uri     : &ViewUri,
  content : &str,
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "view-uri" . to_string () )),
      Sexp::Atom ( Atom::S ( uri . repr_in_client () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( content . to_string () )) ] ) ] )
    . to_string () }

/// Format: ((lock-views ("URI1" "URI2" ...)))
pub(super) fn format_lock_views_sexp (
  uris : &[ViewUri],
) -> String {
  let uri_sexps : Vec<Sexp> =
    uris . iter ()
    . map ( |u| Sexp::Atom ( Atom::S ( u . repr_in_client () )) )
    . collect ();
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "lock-views" . to_string () )),
      Sexp::List ( uri_sexps ) ] ) ] )
    . to_string () }

/// Format: ((errors ("e1" ...)) (warnings ("w1" ...)))
pub(super) fn format_errors_warnings_sexp (
  errors   : &[String],
  warnings : &[String],
) -> String {
  Sexp::List ( vec! [
    format_string_list_sexp ("errors", errors),
    format_string_list_sexp ("warnings", warnings) ] )
    . to_string () }

fn format_string_list_sexp (
  key    : &str,
  values : &[String],
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( key . to_string () )),
    Sexp::List (
      values . iter ()
        . map ( |value| Sexp::Atom ( Atom::S ( value . clone () )) )
        . collect () ) ] ) }

#[cfg(test)]
mod tests {
  use super::{
    format_buffer_response_sexp,
    format_errors_warnings_sexp,
    tag_sexp_response,
  };
  use crate::serve::protocol::TcpToClient;

  #[test]
  fn format_buffer_response_includes_empty_errors_and_warnings () {
    let response : String =
      format_buffer_response_sexp ("* root\n", &[], &[]);

    assert_eq!(
      response,
      "((content \"* root\n\") (errors ()) (warnings ()))" );
  }

  #[test]
  fn format_buffer_response_separates_errors_and_warnings () {
    let errors : Vec<String> =
      vec! [ "fatal problem" . to_string () ];
    let warnings : Vec<String> =
      vec! [ "nonfatal problem" . to_string () ];
    let response : String =
      format_buffer_response_sexp ("* root\n", &errors, &warnings);

    assert_eq!(
      response,
      "((content \"* root\n\") (errors (\"fatal problem\")) (warnings (\"nonfatal problem\")))" );
  }

  #[test]
  fn format_errors_warnings_response_supports_warnings_only () {
    let warnings : Vec<String> =
      vec! [ "rerender note" . to_string () ];
    let response : String =
      format_errors_warnings_sexp (&[], &warnings);

    assert_eq!(
      response,
      "((errors ()) (warnings (\"rerender note\")))" );
  }

  #[test]
  fn tagged_structured_response_keeps_warning_channel () {
    let warnings : Vec<String> =
      vec! [ "non fatal" . to_string () ];
    let payload : String =
      format_buffer_response_sexp ("", &[], &warnings);
    let response : String =
      tag_sexp_response (TcpToClient::SaveResult, &payload);

    assert_eq!(
      response,
      "((response-type save-result) (content ) (errors ()) (warnings (\"non fatal\")))" );
  }
}

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
