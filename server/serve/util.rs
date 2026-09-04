use crate::serve::protocol::{RequestType, TcpToClient};
use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
use crate::types::views_state::ViewUri;

use sexp::{Sexp, Atom};
use std::cell::RefCell;
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;

thread_local! {
  /// The one foreground operation owned by this serial connection thread.
  /// Search keeps it across idle-loop snapshot/enrichment continuations.
  static CURRENT_REQUEST_CONTEXT : RefCell<Option<RequestContext>> =
    const { RefCell::new (None) };
  static LAST_SEND_FAILURE : RefCell<Option<String>> =
    const { RefCell::new (None) };
}

#[derive(Clone, Debug, Eq, PartialEq)]
struct RequestContext {
  request_id  : String,
  incident_id : Option<String>, }

pub fn begin_request_context (request : &str) -> Result<String, String> {
  let sexp = sexp::parse (request)
    . map_err ( |error| format! ("Malformed request envelope: {}", error)) ?;
  let request_id = extract_v_from_kv_pair_in_sexp (&sexp, "request-id")
    .map_err ( |_| "Request envelope has no request-id" . to_string ())?;
  let incident_id = extract_v_from_kv_pair_in_sexp (&sexp, "incident-id")
    . ok ();
  CURRENT_REQUEST_CONTEXT . with ( |slot| {
    let mut current = slot . borrow_mut ();
    match current . as_ref () {
      Some (active) if active . request_id != request_id => Err (format! (
        "Request {} arrived while request {} is still active",
        request_id, active . request_id)),
      Some (active) if active . incident_id != incident_id => Err (format! (
        "Continuation {} changed incident identity", request_id)),
      _ => {
        *current = Some (RequestContext {
          request_id: request_id . clone (), incident_id });
        Ok (request_id) }}})
}

fn clear_request_context () {
  CURRENT_REQUEST_CONTEXT . with (
    |slot| *slot . borrow_mut () = None); }

pub fn request_context_active () -> bool {
  CURRENT_REQUEST_CONTEXT . with (|slot| slot . borrow () . is_some ())
}

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


pub fn send_response_with_length_prefix (
  // Responds "Content-Length: <bytes>\r\n\r\n" + payload
  stream   : &mut TcpStream,
  response : &str,
) -> std::io::Result<()> {
    let enveloped = envelope_response (response);
    let payload : &[u8] = enveloped . as_bytes ();
    let preview_len : usize = // PITFALL: floor_char_boundary is needed
      // because UTF-8 uses multiple bytes for some characters,
      // and slicing mid-character panics in Rust.
      enveloped . floor_char_boundary ( payload . len () . min (200) );
    let preview : &str = &enveloped [..preview_len];
    tracing::debug!("Sending response ({} bytes): {}{}",
             payload . len (), preview,
             if payload . len () > 200 { "..." } else { "" });
    let header : String = format! ( "Content-Length: {}\r\n\r\n",
                                     payload . len () );
    if let Err (error) = stream . write_all (header . as_bytes ())
      . and_then (|_| stream . write_all (payload))
      . and_then (|_| stream . flush ())
    {
      tracing::error! ("Failed to send length-prefixed response: {}", error);
      LAST_SEND_FAILURE . with (|slot|
        *slot . borrow_mut () = Some (error . to_string ()));
      return Err (error); }
    if response_terminal_status (&enveloped) . is_some () {
      clear_request_context (); }
    Ok (( ))
}

pub fn take_send_failure () -> Option<String> {
  LAST_SEND_FAILURE . with (|slot| slot . borrow_mut () . take ())
}

pub fn ensure_request_has_terminal_response (
  stream       : &mut TcpStream,
  request_type : RequestType,
) -> std::io::Result<()> {
  if !request_context_active () { return Ok (( )); }
  let message = format! (
    "request handler for {:?} returned without a terminal response",
    request_type);
  tracing::error! ("{}", message);
  send_response_with_length_prefix (
    stream, &tag_terminal_text_response (
      TcpToClient::Error, "failed", &message))
}

fn envelope_response (response : &str) -> String {
  let Some (context) = CURRENT_REQUEST_CONTEXT . with (
    |slot| slot . borrow () . clone ())
  else { return response . to_string (); };
  let Ok (Sexp::List (mut fields)) = sexp::parse (response)
  else { return response . to_string (); };
  if field_atom (&fields, "server-push") . as_deref () == Some ("true") {
    return response . to_string (); }
  let response_type = field_atom (&fields, "response-type")
    . unwrap_or_else ( || "unknown" . into ());
  if field_atom (&fields, "request-id") . is_none () {
    fields . push (sexp_field ("request-id", &context . request_id)); }
  if field_atom (&fields, "incident-id") . is_none () {
    if let Some (incident_id) = &context . incident_id {
      fields . push (sexp_field ("incident-id", incident_id)); }}
  if field_atom (&fields, "frame-kind") . is_none () {
    fields . push (sexp_field ("frame-kind", &response_type)); }
  if field_atom (&fields, "terminal-status") . is_none () {
    if let Some (status) = inferred_terminal_status (&response_type) {
      fields . push (sexp_field ("terminal-status", status)); }}
  Sexp::List (fields) . to_string ()
}

fn sexp_field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec! [
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

fn field_atom (fields : &[Sexp], key : &str) -> Option<String> {
  fields . iter () . find_map ( |field| match field {
    Sexp::List (parts) if parts . len () >= 2 => match (&parts[0], &parts[1]) {
      (Sexp::Atom (Atom::S (candidate)), Sexp::Atom (Atom::S (value)))
        if candidate == key => Some (value . clone ()),
      _ => None, },
    _ => None, })
}

fn response_terminal_status (response : &str) -> Option<String> {
  let Ok (Sexp::List (fields)) = sexp::parse (response) else { return None; };
  field_atom (&fields, "terminal-status")
}

fn inferred_terminal_status (response_type : &str) -> Option<&'static str> {
  match response_type {
    "save-lock" | "save-relax-lock" | "collateral-view"
    | "search-results" | "request-snapshot"
    | "rerender-lock" | "rerender-view"
    | "git-diff-mode" | "active-source-set" => None,
    "fork-confirmation" | "telescope-hoist-confirmation"
    | "ugly-telescope-confirmation" => Some ("needs-authorization"),
    "error" => Some ("failed"),
    _ => Some ("complete"), }}

pub fn tag_terminal_text_response (
  response_type : TcpToClient,
  status        : &str,
  text          : &str,
) -> String {
  let Ok (Sexp::List (mut fields)) = sexp::parse (
    &tag_text_response (response_type, text))
  else { unreachable! () };
  fields . push (sexp_field ("terminal-status", status));
  Sexp::List (fields) . to_string ()
}

pub fn tag_terminal_sexp_response (
  response_type : TcpToClient,
  status        : &str,
  sexp_payload  : &str,
) -> String {
  let Ok (Sexp::List (mut fields)) = sexp::parse (
    &tag_sexp_response (response_type, sexp_payload))
  else { unreachable! () };
  fields . push (sexp_field ("terminal-status", status));
  Sexp::List (fields) . to_string ()
}

/// Mark an unsolicited, server-owned operation frame. Server pushes do not
/// borrow the currently active client request: they carry their own
/// operation-id and are dispatched through the client's push registry.
pub fn tag_server_push_sexp_response (
  response_type : TcpToClient,
  operation_id  : &str,
  sexp_payload  : &str,
) -> String {
  let Ok (Sexp::List (mut fields)) = sexp::parse (
    &tag_sexp_response (response_type, sexp_payload))
  else { unreachable! () };
  fields . push (sexp_field (
    "frame-kind", response_type . repr_in_client ()));
  fields . push (sexp_field ("server-push", "true"));
  fields . push (sexp_field ("operation-id", operation_id));
  Sexp::List (fields) . to_string ()
}

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

/// Add the exact server/client application identity for a newly rendered or
/// foreground-updated view.  Clients must retain these fields with the text;
/// later saves and background ACKs name the same authority explicitly.
pub(crate) fn add_view_authority_to_response (
  response : &str,
  state    : &crate::types::views_state::ViewState,
) -> String {
  let Ok (Sexp::List (mut fields)) = sexp::parse (response) else {
    unreachable! ("buffer response formatter produced invalid sexp"); };
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("graph-generation" . into ())),
    Sexp::Atom (Atom::I (state . graph_generation as i64)),
  ]));
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("presentation-generation" . into ())),
    Sexp::Atom (Atom::I (state . presentation_generation as i64)),
  ]));
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("server-revision" . into ())),
    Sexp::Atom (Atom::I (state . revision as i64)),
  ]));
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("client-application-token" . into ())),
    Sexp::Atom (Atom::I (state . client_application_token as i64)),
  ]));
  Sexp::List (fields) . to_string ()
}

/// Format the override-choice buffer response: a content-view
/// response that additionally tells the client which URI the server
/// registered the menu under (the client would otherwise assume its
/// own generated UUID) and what to echo in the minibuffer.
/// Format:
///   ((content "...") (view-uri "override-menu:PID")
///    (to-minibuffer "...") (errors ()) (warnings (...)))
pub(crate) fn format_override_menu_response_sexp (
  buffer_content : &str,
  uri            : &ViewUri,
  to_minibuffer  : &str,
  warnings       : &[String],
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "view-uri" . to_string () )),
      Sexp::Atom ( Atom::S ( uri . repr_in_client () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "to-minibuffer" . to_string () )),
      Sexp::Atom ( Atom::S ( to_minibuffer . to_string () )) ] ),
    format_string_list_sexp ("errors", &[]),
    format_string_list_sexp ("warnings", warnings) ] )
    . to_string () }

/// Format the fork-confirmation response: a read-only buffer listing
/// the foreign nodes about to be forked, plus the one-line minibuffer
/// prompt. Like the override-menu response but with NO view-uri -- the
/// client adopts the buffer under its own generated URI (the
/// confirmation is transient, not a registered server view).
/// Format: ((content "...") (to-minibuffer "...") (errors ()) (warnings ()))
pub(crate) fn format_fork_confirmation_response_sexp (
  buffer_content : &str,
  to_minibuffer  : &str,
) -> String {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( buffer_content . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "to-minibuffer" . to_string () )),
      Sexp::Atom ( Atom::S ( to_minibuffer . to_string () )) ] ),
    format_string_list_sexp ("errors", &[]),
    format_string_list_sexp ("warnings", &[]) ] )
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
pub(crate) fn format_lock_views_sexp (
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
