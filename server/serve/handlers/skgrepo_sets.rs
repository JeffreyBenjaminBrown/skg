use crate::refresh::corrections::BufferCorrectionPolicy;
use crate::serve::ViewsState;
use crate::serve::rerender_stream::{
  PreparedRerenders,
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
use crate::sexp_string::quote_sexp_string;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::SkgrepoSetName;
use crate::types::misc::SkgConfig;
use crate::update_buffer::RerenderOptions;

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

/// Under a skgrepo restriction (the API doc's "Restricted-skgrepo nodes"): a repo-set switch
/// re-renders open views in place rather than closing them. The
/// refresh brings each view under the new restriction: it entombstones
/// the occurrences the restriction keeps out, draws no restricted
/// member, prunes childless tombstones, and draws what a widening
/// reveals; a folder the restriction empties stays, empty, as any
/// empty editable folder does. The rest keeps its
/// viewnode IDs. When the restriction changed, none of that is a
/// buffer correction ('BufferCorrectionPolicy'). Results stream via
/// the rerender-all message flow (lock, per-view, done).
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
  let mut prepared : PreparedRerenders =
    prepare_rerender_views_with_runtime (
      env, runtime, views_state, Some (&restriction),
      BufferCorrectionPolicy::after_a_switch (
        restriction != *skgrepo_restriction ),
      RerenderOptions::ordinary );
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
    . map ( |name| quote_sexp_string (name) )
    . collect::<Vec<String>> ()
    . join (" ");
  let response : String =
    format! (
      "((response-type {}) (restriction {}) (sets ({})))",
      TcpToClient::SkgrepoSets . repr_in_client (),
      quote_sexp_string (&restriction . name . 0),
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
      "((response-type {}) (restriction {}) (content {}))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      quote_sexp_string (name),
      quote_sexp_string (&format! ("Skgrepo restriction: {}", name)) );
  send_response_with_length_prefix (stream, &response); }

/// The unwinding refusal shape (the quiet shape): the endpoint's
/// normal skgrepo-restriction response-type carrying explanatory text
/// and the unchanged skgrepo restriction, followed by an empty rerender
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
      "((response-type {}) (restriction {}) (content {}))",
      TcpToClient::SkgrepoRestriction . repr_in_client (),
      quote_sexp_string (&restriction . name . 0),
      quote_sexp_string (msg));
  send_response_with_length_prefix (stream, &response);
  stream_empty_rerender (stream); }
