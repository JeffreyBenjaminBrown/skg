use crate::serve::ViewsState;
use crate::serve::handlers::text_search::SearchEnrichmentPayload;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  value_from_request_sexp};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{SkgConfig, SourceSetName};

use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

pub fn handle_source_set_request (
  stream           : &mut TcpStream,
  request          : &str,
  config           : &SkgConfig,
  views_state      : &mut ViewsState,
  active_source_set : &mut ActiveSourceSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  match request_type_from_request (request) {
    Ok (RequestType::ListSourceSets) =>
      send_source_sets_response (stream, config, active_source_set),
    Ok (RequestType::ActiveSourceSet) =>
      send_active_source_set_response (stream, active_source_set),
    Ok (RequestType::SetActiveSourceSet) =>
      set_active_source_set (
        stream, request, config, views_state,
        active_source_set, enrichment_slot, search_cancelled ),
    Ok (_) =>
      send_error (stream, "not a source-set request"),
    Err (e) =>
      send_error (stream, &e), }}

fn set_active_source_set (
  stream           : &mut TcpStream,
  request          : &str,
  config           : &SkgConfig,
  views_state      : &mut ViewsState,
  active_source_set : &mut ActiveSourceSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let name : SourceSetName =
    match value_from_request_sexp ("name", request) {
      Ok (name) => SourceSetName::from (name),
      Err (e) => {
        send_error (stream, &e);
        return; }};
  let active : ActiveSourceSet =
    match ActiveSourceSet::named (config, name) {
      Ok (active) => active,
      Err (e) => {
        send_error (stream, &e . to_string ());
        return; }};
  search_cancelled . store (true, Ordering::SeqCst);
  if let Ok (mut slot) = enrichment_slot . lock () {
    *slot = None; }
  views_state . open_views . clear ();
  *active_source_set = active;
  send_active_source_set_response (stream, active_source_set); }

fn send_source_sets_response (
  stream      : &mut TcpStream,
  config      : &SkgConfig,
  active      : &ActiveSourceSet,
) {
  let mut names : Vec<String> =
    config . source_sets . keys ()
    . map ( |name| name . 0 . clone () )
    . collect ();
  names . push ("all" . to_string ());
  names . sort ();
  names . dedup ();
  let names_sexp : String =
    names . iter ()
    . map ( |name| format! ("\"{}\"", escape_string (name)) )
    . collect::<Vec<String>> ()
    . join (" ");
  let response : String =
    format! (
      "((response-type {}) (active \"{}\") (sets ({})))",
      TcpToClient::SourceSets . repr_in_client (),
      escape_string (&active . name . 0),
      names_sexp );
  send_response_with_length_prefix (stream, &response); }

fn send_active_source_set_response (
  stream : &mut TcpStream,
  active : &ActiveSourceSet,
) {
  let name : &str =
    &active . name . 0;
  let response : String =
    format! (
      "((response-type {}) (active \"{}\") (content \"Active source-set: {}\"))",
      TcpToClient::ActiveSourceSet . repr_in_client (),
      escape_string (name),
      escape_string (name));
  send_response_with_length_prefix (stream, &response); }

fn send_error (
  stream : &mut TcpStream,
  msg    : &str,
) {
  let response : String =
    format! (
      "((response-type {}) (content \"{}\"))",
      TcpToClient::Error . repr_in_client (),
      escape_string (msg));
  send_response_with_length_prefix (stream, &response); }

fn escape_string (
  s : &str,
) -> String {
  s . replace ('\\', "\\\\")
    . replace ('"', "\\\"")
    . replace ('\n', "\\n") }
