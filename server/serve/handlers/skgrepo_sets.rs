use crate::serve::ViewsState;
use crate::serve::handlers::rerender_all_views::{
  authorize_prepared_rerenders,
  prepare_rerender_views_with_runtime,
  stream_empty_rerender,
  stream_prepared_rerenders};
use crate::serve::handlers::text_release::approved_pids_from_request;
use crate::serve::handlers::text_search::{
  SearchEnrichmentPayload, cancel_search_enrichment};
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  value_from_request_sexp};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoSetName;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::update_buffer::skgrepo_switch::convert_and_prune_for_skgrepo_switch;

use std::net::TcpStream;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

pub fn handle_skgrepo_set_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  skgrepo_restriction : &mut SkgrepoRestriction,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let runtime = env . runtime_snapshot ();
  match request_type_from_request (request) {
    Ok (RequestType::ListSkgrepoSets) =>
      send_skgrepo_sets_response (stream, &runtime . config, skgrepo_restriction),
    Ok (RequestType::SkgrepoRestriction) =>
      send_skgrepo_restriction_response (stream, skgrepo_restriction),
    Ok (RequestType::SetSkgrepoRestriction) =>
      set_skgrepo_restriction (
        stream, request, env, runtime, views_state,
        skgrepo_restriction, enrichment_slot, search_cancelled ),
    Ok (_) =>
      // Reachable only from malformed requests no current client
      // sends, but Emacs may have locked buffers and set its stream
      // guard before any repo-set request, so even these paths
      // answer in the unwinding shape.
      refuse_unwinding (
        stream, skgrepo_restriction, "not a repo-set request"),
    Err (e) =>
      refuse_unwinding (stream, skgrepo_restriction, &e), }}

/// TODO/DONE/full-schema/DONE/9-2_source-set-safety.org: a repo-set switch
/// RE-RENDERS open views in place rather than closing them.  Each
/// view gets the convert-and-prune prepass (now-restricted unrestricted vognodes
/// become RestrictedVognodes; childless restricted branches, properties of
/// restricted recorders, write-protected partners, emptied folders and dead
/// non-vognodes are pruned), then completion with PartnerFolder creation
/// enabled, because a switch can also ACTIVATE skgrepos, revealing
/// members and folders.  Results stream via the rerender-all message
/// flow (lock, per-view, done).
fn set_skgrepo_restriction (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  runtime          : Arc<RuntimeGeneration>,
  views_state      : &mut ViewsState,
  skgrepo_restriction : &mut SkgrepoRestriction,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let name : SkgrepoSetName =
    match value_from_request_sexp ("name", request) {
      Ok (name) => SkgrepoSetName::from (name),
      Err (e) => {
        refuse_unwinding (stream, skgrepo_restriction, &e);
        return; }};
  let restriction : SkgrepoRestriction =
    match SkgrepoRestriction::named (&runtime . config, name) {
      Ok (restriction) => restriction,
      Err (e) => {
        refuse_unwinding (
          stream, skgrepo_restriction, &e . to_string ());
        return; }};
  if views_state . diff_mode_enabled
     && ! restriction . is_all () {
    { // Refuse to switch to a restricted set while diff mode is
      // on.  (Switching TO 'all' is always allowed.)  This check
      // precedes every side effect: search-enrichment
      // cancellation, the set assignment, and the rerenders.
      let msg : String = format! (
        "Cannot switch to repo-set {}: git diff mode is on, and it requires no skgrepo restriction (the skgrepo-set all). Disable diff mode first.",
        restriction . name . 0 );
      tracing::info! ( msg = %msg, "Repo-set switch refused" );
      refuse_unwinding (stream, skgrepo_restriction, &msg);
      return; }}
  let mut prepared =
  { let target : SkgrepoRestriction = restriction . clone ();
    let prepass = |viewforest : &mut ViewForest|
      -> Result<(), Box<dyn std::error::Error>> {
      convert_and_prune_for_skgrepo_switch (
        viewforest . as_internal_tree_mut (), &target ) };
    prepare_rerender_views_with_runtime (
      env, runtime, views_state, views_state . diff_mode_enabled,
      Some (&target), Some (&prepass), true ) };
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, Some (&restriction),
    "repo-set-switch-rerender",
    &approved_pids_from_request (request) ) {
    return; }
  cancel_search_enrichment (enrichment_slot, search_cancelled);
  *skgrepo_restriction = restriction;
  send_skgrepo_restriction_response (stream, skgrepo_restriction);
  stream_prepared_rerenders (stream, views_state, prepared); }

fn send_skgrepo_sets_response (
  stream      : &mut TcpStream,
  config      : &SkgConfig,
  restriction : &SkgrepoRestriction,
) {
  // Repo-sets are the prefixes of the privacy order, so the
  // choices are the skgrepos themselves, in that order (each meaning
  // "this repo and everything more public"), plus "all" last (the
  // longest prefix). Order is meaningful; do not sort.
  let mut names : Vec<String> =
    config . ordered_skgrepos () . iter ()
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
      "((response-type {}) (restriction \"{}\") (sets ({})))",
      TcpToClient::SkgrepoSets . repr_in_client (),
      escape_string (&restriction . name . 0),
      names_sexp );
  send_response_with_length_prefix (stream, &response); }

fn send_skgrepo_restriction_response (
  stream : &mut TcpStream,
  restriction : &SkgrepoRestriction,
) {
  let name : &str =
    &restriction . name . 0;
  let response : String =
    format! (
      "((response-type {}) (restriction \"{}\") (content \"Skgrepo restriction: {}\"))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      escape_string (name),
      escape_string (name));
  send_response_with_length_prefix (stream, &response); }

/// The unwinding refusal shape (the quiet shape): the endpoint's
/// normal skgrepo-restriction response-type carrying explanatory text
/// and the UNCHANGED skgrepo restriction, followed by an empty rerender
/// stream.  Emacs locks all Skg buffers and sets its stream guard
/// before sending a switch request; a response-type it has no
/// handler for would leave it wedged, so refusals and errors alike
/// must answer in this shape.
fn refuse_unwinding (
  stream : &mut TcpStream,
  restriction : &SkgrepoRestriction,
  msg    : &str,
) {
  let response : String =
    format! (
      "((response-type {}) (restriction \"{}\") (content \"{}\"))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      escape_string (&restriction . name . 0),
      escape_string (msg));
  send_response_with_length_prefix (stream, &response);
  stream_empty_rerender (stream); }

fn escape_string (
  s : &str,
) -> String {
  s . replace ('\\', "\\\\")
    . replace ('"', "\\\"")
    . replace ('\n', "\\n") }
