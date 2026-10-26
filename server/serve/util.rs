use crate::refresh::buffer_corrections::{ViewBufferCorrections, corrections_entry_sexp};
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::views_state::ViewforestId;

use sexp::{Sexp, Atom};
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;

/// Print a Sexp bound for the client. Every response the server sends
/// is printed by this, never by the 'sexp' crate's 'to_string', which
/// quotes a string atom only when it holds a quote, whitespace or a
/// number, and so prints an empty one as nothing ('(content )'). This
/// prints an empty string atom as '""', and every other atom as the
/// crate does: the server relies on a non-empty 'Atom::S' printing as
/// a bare symbol.
pub fn sexp_to_client_string (
  sexp : &Sexp,
) -> String {
  match sexp {
    Sexp::Atom ( Atom::S (text) ) if text . is_empty () =>
      "\"\"" . to_string (),
    Sexp::Atom (atom) => atom . to_string (),
    Sexp::List (items) => format! (
      "({})",
      items . iter ()
        . map (sexp_to_client_string)
        . collect::<Vec<String>> ()
        . join (" ") ), } }

/// Prepend a (response-type "TYPE") entry to an existing s-exp string.
/// Input:  "((content "...") (errors (...)) (warnings (...)))"
/// Output: "(("response-type" "TYPE") (content "...") (errors (...)) (warnings (...)))"
pub fn tag_sexp_response (
  response_type : TcpToClient,
  sexp_payload  : &str,
) -> String {
  let tag : String =
    sexp_to_client_string ( &Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( response_type . repr_in_client ()
                             . to_string () )),
    ] ));
  format! ( "({} {}", tag, &sexp_payload [ 1.. ] ) }

/// Wrap plain text in a tagged s-exp for LP delivery.
/// Output: (("response-type" "TYPE") ("content" "TEXT"))
pub fn tag_text_response (
  response_type : TcpToClient,
  text          : &str,
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "response-type" . to_string () )),
      Sexp::Atom ( Atom::S ( response_type . repr_in_client ()
                             . to_string () )), ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content"       . to_string () )),
      Sexp::Atom ( Atom::S ( text            . to_string () )), ] ),
  ] )) }


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

pub fn viewforest_id_from_request (
  request : &str,
) -> Result<ViewforestId, String> {
  value_from_request_sexp ( "view-id", request )
    . map (ViewforestId::from_client_string) }

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
  sexp_to_client_string ( &Sexp::List ( buffer_response_entries (
    buffer_content, errors, warnings )) ) }

/// The entries of 'format_buffer_response_sexp', for a caller that
/// adds more.
pub(crate) fn buffer_response_entries (
  buffer_content : &str,
  errors         : &[String],
  warnings       : &[String],
) -> Vec<Sexp> {
  vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    format_string_list_sexp ("errors", errors),
    format_string_list_sexp ("warnings", warnings) ] }

/// Format the fork-confirmation response: a write-protected buffer listing
/// the foreign nodes about to be forked, plus the one-line minibuffer
/// prompt. It has no view-id: the client adopts the buffer under its
/// own generated view ID because the confirmation is transient, not a
/// registered server view.
/// Format: ((content "...") (to-minibuffer "...") (errors ()) (warnings ()))
pub(crate) fn format_fork_confirmation_response_sexp (
  buffer_content : &str,
  to_minibuffer  : &str,
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "to-minibuffer" . to_string () )),
      Sexp::Atom ( Atom::S ( to_minibuffer . to_string () )) ] ),
    format_string_list_sexp ("errors", &[]),
    format_string_list_sexp ("warnings", &[]) ] )) }

/// Format a single view update as an s-expression.
/// Format: ((view-id "VIEW_ID") (content "CONTENT"))
/// Used for any streamed per-view message (collateral-view,
/// rerender-view, etc.). The caller tags it with the appropriate
/// TcpToClient variant.
pub(crate) fn format_single_view_sexp (
  viewforest_id : &ViewforestId,
  content : &str,
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "view-id" . to_string () )),
      Sexp::Atom ( Atom::S ( viewforest_id . repr_in_client () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( content . to_string () )) ] ) ] )) }

/// Format: ((lock-views ("VIEW_ID1" "VIEW_ID2" ...)))
pub(crate) fn format_lock_views_sexp (
  viewforest_ids : &[ViewforestId],
) -> String {
  let viewforest_id_sexps : Vec<Sexp> =
    viewforest_ids . iter ()
    . map ( |u| Sexp::Atom ( Atom::S ( u . repr_in_client () )) )
    . collect ();
  sexp_to_client_string ( &Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "lock-views" . to_string () )),
      Sexp::List ( viewforest_id_sexps ) ] ) ] )) }

/// The "rerender-done" message: ((errors ("e1" ...)) (warnings ("w1"
/// ...)) (corrections (("VIEW_ID" "c1" ...) ...))).
pub(crate) fn format_rerender_done_sexp (
  errors      : &[String],
  warnings    : &[String],
  buffer_corrections : &[ViewBufferCorrections],
) -> String {
  sexp_to_client_string ( &Sexp::List ( vec! [
    format_string_list_sexp ("errors", errors),
    format_string_list_sexp ("warnings", warnings),
    corrections_entry_sexp (buffer_corrections) ] )) }

/// Format: (KEY . t)
pub(crate) fn true_entry_sexp (
  key : &str,
) -> Sexp {
  Sexp::List ( vec! [ Sexp::Atom ( Atom::S ( key . to_string () )),
                      Sexp::Atom ( Atom::S ( "." . to_string () )),
                      Sexp::Atom ( Atom::S ( "t" . to_string () )) ] ) }

/// Format: (KEY ("v1" ...))
pub(crate) fn format_string_list_sexp (
  key    : &str,
  values : &[String],
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::Atom ( Atom::S ( key . to_string () )),
    Sexp::List (
      values . iter ()
        . map ( |value| Sexp::Atom ( Atom::S ( value . clone () )) )
        . collect () ) ] ) }


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

#[cfg(test)]
#[path = "../../tests/unit/serve_util.rs"]
mod tests;
