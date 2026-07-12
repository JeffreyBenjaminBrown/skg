use crate::serve::ViewsState;
use crate::serve::handlers::rerender_all_views::{ stream_empty_rerender, stream_rerender_views};
use crate::serve::handlers::text_search::SearchEnrichmentPayload;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  value_from_request_sexp};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::SourceSetName;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::update_buffer::source_switch::convert_and_prune_for_source_switch;

use std::net::TcpStream;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

pub fn handle_source_set_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  active_source_set : &mut ActiveSourceSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  match request_type_from_request (request) {
    Ok (RequestType::ListSourceSets) =>
      send_source_sets_response (stream, &env . config, active_source_set),
    Ok (RequestType::ActiveSourceSet) =>
      send_active_source_set_response (stream, active_source_set),
    Ok (RequestType::SetActiveSourceSet) =>
      set_active_source_set (
        stream, request, env, views_state,
        active_source_set, enrichment_slot, search_cancelled ),
    Ok (_) =>
      // Reachable only from malformed requests no current client
      // sends, but Emacs may have locked buffers and set its stream
      // guard before any source-set request, so even these paths
      // answer in the unwinding shape.
      refuse_unwinding (
        stream, active_source_set, "not a source-set request"),
    Err (e) =>
      refuse_unwinding (stream, active_source_set, &e), }}

/// TODO/full-schema/9-2_source-set-safety.org: a source-set switch
/// RE-RENDERS open views in place rather than closing them.  Each
/// view gets the convert-and-prune prepass (now-inactive Actives
/// become InactiveNodes; childless inactive branches, quals of
/// inactive owners, indefinitive partners, emptied cols and dead
/// scaffolds are pruned), then completion with PartnerCol creation
/// enabled, because a switch can also ACTIVATE sources, revealing
/// members and cols.  Results stream via the rerender-all message
/// flow (lock, per-view, done).
fn set_active_source_set (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  active_source_set : &mut ActiveSourceSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let name : SourceSetName =
    match value_from_request_sexp ("name", request) {
      Ok (name) => SourceSetName::from (name),
      Err (e) => {
        refuse_unwinding (stream, active_source_set, &e);
        return; }};
  let active : ActiveSourceSet =
    match ActiveSourceSet::named (&env . config, name) {
      Ok (active) => active,
      Err (e) => {
        refuse_unwinding (
          stream, active_source_set, &e . to_string ());
        return; }};
  if views_state . diff_mode_enabled
     && ! active . is_all () {
    { // Refuse to switch to a restricted set while diff mode is
      // on.  (Switching TO 'all' is always allowed.)  This check
      // precedes every side effect: search-enrichment
      // cancellation, the set assignment, and the rerenders.
      let msg : String = format! (
        "Cannot switch to source-set {}: git diff mode is on, and it requires active source-set all. Disable diff mode first.",
        active . name . 0 );
      tracing::info! ( msg = %msg, "Source-set switch refused" );
      refuse_unwinding (stream, active_source_set, &msg);
      return; }}
  search_cancelled . store (true, Ordering::SeqCst);
  if let Ok (mut slot) = enrichment_slot . lock () {
    *slot = None; }
  *active_source_set = active;
  send_active_source_set_response (stream, active_source_set);
  { let active : ActiveSourceSet = active_source_set . clone ();
    let prepass = |viewforest : &mut ViewForest|
      -> Result<(), Box<dyn std::error::Error>> {
      convert_and_prune_for_source_switch (
        viewforest . as_internal_tree_mut (), &active ) };
    stream_rerender_views (
      stream, env, views_state, Some (active_source_set),
      Some (&prepass),
      true ); }}

fn send_source_sets_response (
  stream      : &mut TcpStream,
  config      : &SkgConfig,
  active      : &ActiveSourceSet,
) {
  // Source-sets are the prefixes of the privacy order, so the
  // choices are the sources themselves, in that order (each meaning
  // "this source and everything more public"), plus "all" last (the
  // longest prefix). Order is meaningful; do not sort.
  let mut names : Vec<String> =
    config . ordered_sources () . iter ()
    . map ( |name| name . 0 . clone () )
    . collect ();
  names . push ("all" . to_string ());
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

/// The unwinding refusal shape (the quiet shape): the endpoint's
/// normal active-source-set response-type carrying explanatory text
/// and the UNCHANGED active set, followed by an empty rerender
/// stream.  Emacs locks all Skg buffers and sets its stream guard
/// before sending a switch request; a response-type it has no
/// handler for would leave it wedged, so refusals and errors alike
/// must answer in this shape.
fn refuse_unwinding (
  stream : &mut TcpStream,
  active : &ActiveSourceSet,
  msg    : &str,
) {
  let response : String =
    format! (
      "((response-type {}) (active \"{}\") (content \"{}\"))",
      TcpToClient::ActiveSourceSet . repr_in_client (),
      escape_string (&active . name . 0),
      escape_string (msg));
  send_response_with_length_prefix (stream, &response);
  stream_empty_rerender (stream); }

fn escape_string (
  s : &str,
) -> String {
  s . replace ('\\', "\\\\")
    . replace ('"', "\\\"")
    . replace ('\n', "\\n") }
