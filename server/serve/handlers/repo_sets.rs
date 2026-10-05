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
use crate::repo_sets::ActiveRepoSet;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::RepoSetName;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::update_buffer::repo_switch::convert_and_prune_for_repo_switch;

use std::net::TcpStream;
use std::sync::atomic::AtomicBool;
use std::sync::{Arc, Mutex};

pub fn handle_repo_set_request (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  views_state      : &mut ViewsState,
  active_repo_set : &mut ActiveRepoSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let runtime = env . runtime_snapshot ();
  match request_type_from_request (request) {
    Ok (RequestType::ListRepoSets) =>
      send_repo_sets_response (stream, &runtime . config, active_repo_set),
    Ok (RequestType::ActiveRepoSet) =>
      send_active_repo_set_response (stream, active_repo_set),
    Ok (RequestType::SetActiveRepoSet) =>
      set_active_repo_set (
        stream, request, env, runtime, views_state,
        active_repo_set, enrichment_slot, search_cancelled ),
    Ok (_) =>
      // Reachable only from malformed requests no current client
      // sends, but Emacs may have locked buffers and set its stream
      // guard before any repo-set request, so even these paths
      // answer in the unwinding shape.
      refuse_unwinding (
        stream, active_repo_set, "not a repo-set request"),
    Err (e) =>
      refuse_unwinding (stream, active_repo_set, &e), }}

/// TODO/full-schema/9-2_repo-set-safety.org: a repo-set switch
/// RE-RENDERS open views in place rather than closing them.  Each
/// view gets the convert-and-prune prepass (now-inactive Actives
/// become InactiveVognodes; childless inactive branches, properties of
/// inactive owners, write-protected partners, emptied folders and dead
/// non-vognodes are pruned), then completion with PartnerFolder creation
/// enabled, because a switch can also ACTIVATE repos, revealing
/// members and folders.  Results stream via the rerender-all message
/// flow (lock, per-view, done).
fn set_active_repo_set (
  stream           : &mut TcpStream,
  request          : &str,
  env              : &SkgEnv,
  runtime          : Arc<RuntimeGeneration>,
  views_state      : &mut ViewsState,
  active_repo_set : &mut ActiveRepoSet,
  enrichment_slot  : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled : &Arc<AtomicBool>,
) {
  let name : RepoSetName =
    match value_from_request_sexp ("name", request) {
      Ok (name) => RepoSetName::from (name),
      Err (e) => {
        refuse_unwinding (stream, active_repo_set, &e);
        return; }};
  let active : ActiveRepoSet =
    match ActiveRepoSet::named (&runtime . config, name) {
      Ok (active) => active,
      Err (e) => {
        refuse_unwinding (
          stream, active_repo_set, &e . to_string ());
        return; }};
  if views_state . diff_mode_enabled
     && ! active . is_all () {
    { // Refuse to switch to a restricted set while diff mode is
      // on.  (Switching TO 'all' is always allowed.)  This check
      // precedes every side effect: search-enrichment
      // cancellation, the set assignment, and the rerenders.
      let msg : String = format! (
        "Cannot switch to repo-set {}: git diff mode is on, and it requires active repo-set all. Disable diff mode first.",
        active . name . 0 );
      tracing::info! ( msg = %msg, "Repo-set switch refused" );
      refuse_unwinding (stream, active_repo_set, &msg);
      return; }}
  let mut prepared =
  { let target : ActiveRepoSet = active . clone ();
    let prepass = |viewforest : &mut ViewForest|
      -> Result<(), Box<dyn std::error::Error>> {
      convert_and_prune_for_repo_switch (
        viewforest . as_internal_tree_mut (), &target ) };
    prepare_rerender_views_with_runtime (
      env, runtime, views_state, views_state . diff_mode_enabled,
      Some (&target), Some (&prepass), true ) };
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, Some (&active),
    "repo-set-switch-rerender",
    &approved_pids_from_request (request) ) {
    return; }
  cancel_search_enrichment (enrichment_slot, search_cancelled);
  *active_repo_set = active;
  send_active_repo_set_response (stream, active_repo_set);
  stream_prepared_rerenders (stream, views_state, prepared); }

fn send_repo_sets_response (
  stream      : &mut TcpStream,
  config      : &SkgConfig,
  active      : &ActiveRepoSet,
) {
  // Repo-sets are the prefixes of the privacy order, so the
  // choices are the repos themselves, in that order (each meaning
  // "this repo and everything more public"), plus "all" last (the
  // longest prefix). Order is meaningful; do not sort.
  let mut names : Vec<String> =
    config . ordered_repos () . iter ()
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
      TcpToClient::RepoSets . repr_in_client (),
      escape_string (&active . name . 0),
      names_sexp );
  send_response_with_length_prefix (stream, &response); }

fn send_active_repo_set_response (
  stream : &mut TcpStream,
  active : &ActiveRepoSet,
) {
  let name : &str =
    &active . name . 0;
  let response : String =
    format! (
      "((response-type {}) (active \"{}\") (content \"Active repo-set: {}\"))",
      TcpToClient::ActiveRepoSet . repr_in_client (),
      escape_string (name),
      escape_string (name));
  send_response_with_length_prefix (stream, &response); }

/// The unwinding refusal shape (the quiet shape): the endpoint's
/// normal active-repo-set response-type carrying explanatory text
/// and the UNCHANGED active set, followed by an empty rerender
/// stream.  Emacs locks all Skg buffers and sets its stream guard
/// before sending a switch request; a response-type it has no
/// handler for would leave it wedged, so refusals and errors alike
/// must answer in this shape.
fn refuse_unwinding (
  stream : &mut TcpStream,
  active : &ActiveRepoSet,
  msg    : &str,
) {
  let response : String =
    format! (
      "((response-type {}) (active \"{}\") (content \"{}\"))",
      TcpToClient::ActiveRepoSet . repr_in_client (),
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
