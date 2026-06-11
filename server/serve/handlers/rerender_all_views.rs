use crate::git_ops::read_repo::open_repo;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewUri;
use crate::update_buffer::{rerender_view, RerenderAfterSaveContext};

use futures::executor::block_on;
use std::net::TcpStream;

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  stream_rerender_views (
    stream, env, views_state, Some (active_source_set),
    None, false); }

/// Stream re-rendered views to Emacs.
/// Sends: rerender-lock → rerender-view* → rerender-done.
/// Shared by 'handle_rerender_all_views_request',
/// 'handle_git_diff_toggle_and_rerender', and the source-set switch
/// ('set_active_source_set'), which passes a per-view prepass (the
/// convert-and-prune step) and asks for PartnerCol re-creation
/// (TODO/full-schema/9-2_source-set-safety.org).
pub fn stream_rerender_views (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : Option<&ActiveSourceSet>,
  prepass    : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerCols : bool,
) {
  let uris : Vec<ViewUri> =
    views_state . open_views . views . keys () . cloned () . collect ();

  // 1. Send lock message with all URIs.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp (&uris) ));

  // 2. Compute rerender context once, then stream each view.
  let mut context : RerenderAfterSaveContext =
    RerenderAfterSaveContext::without_save (
      env, views_state . diff_mode_enabled, active_source_set );

  for uri in uris {
    let mut viewforest : ViewForest = match
      views_state . open_views . viewuri_to_view (&uri) {
        Some (f) => f . clone (),
        None => {
          context . errors . push ( format! (
            "View {}: no viewforest found",
            uri . repr_in_client () ));
          continue; } };
    if let Some (prepass) = prepass {
      if let Err (e) = prepass (&mut viewforest) {
        context . errors . push ( format! (
          "View {}: {}",
          uri . repr_in_client (), e ));
        continue; }}
    match block_on ( async {
      let _span : tracing::span::EnteredSpan =
        tracing::info_span! (
          "rerender_view (rerender-all)"
        ) . entered ();
      rerender_view (
        &mut viewforest,
        &mut context,
        None, // streamed rerenders repair silently.
        create_partnerCols
      ) . await } )
    { Ok (text) => {
        views_state . open_views . update_view (&uri, viewforest);
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response (
            TcpToClient::RerenderView,
            & format_single_view_sexp (&uri, &text) )); },
      Err (e) => {
        context . errors . push ( format! (
          "View {}: {}",
          uri . repr_in_client (), e )); }} }

  // 3. Send done message with errors and warnings.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_errors_warnings_sexp (
        &context . errors,
        &context . warnings) )); }

/// Send an EMPTY rerender stream: a "rerender-lock" naming no views,
/// then "rerender-done" with no errors or warnings.  Used after a
/// refusal: Emacs locks every Skg buffer and sets its stream guard
/// BEFORE sending a diff-mode toggle or source-set switch, and only
/// the rerender stream unwinds them.  The empty lock list makes
/// Emacs unlock every buffer; the done message clears the guard.
pub fn stream_empty_rerender (
  stream : &mut TcpStream,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp ( &[] ) ));
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_errors_warnings_sexp ( &[], &[] ) )); }

/// Handle "git diff mode toggle" request.
/// Toggles diff mode, sends the GitDiffMode response with warnings,
/// then streams re-rendered views.
pub fn handle_git_diff_toggle_and_rerender (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  if ! views_state . diff_mode_enabled
     && ! active_source_set . is_all () {
    { // Refuse to ENABLE diff mode under a restricted source-set.
      // (Disabling is always allowed: it only makes state legal.)
      // The refusal takes the quiet shape: the endpoint's normal
      // first message carries the refusal text, then an empty
      // rerender stream unwinds Emacs's preemptive locks.
      // PITFALL: the text must not contain the substring
      // "\nWarning:", which the Emacs diff-toggle handler treats
      // as a window-pop trigger.
      let msg : String = format! (
        "Git diff mode requires active source-set all; current active source-set is {}. Switch the source-set to all first.",
        active_source_set . name . 0 );
      tracing::info! ( msg = %msg, "Git diff mode toggle refused" );
      send_response_with_length_prefix (
        stream,
        & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
      stream_empty_rerender (stream);
      return; }}
  views_state . diff_mode_enabled = ! views_state . diff_mode_enabled;
  let msg : String =
    git_diff_mode_message (views_state . diff_mode_enabled, &env . config);
  tracing::info! ( msg = %msg, "Git diff mode toggled" );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
  stream_rerender_views (
    stream, env, views_state, Some (active_source_set),
    None, false); }

/// Build the human-readable message for a diff-mode toggle,
/// including warnings for sources not tracked in git.
fn git_diff_mode_message (
  enabled : bool,
  config  : &SkgConfig,
) -> String {
  let mut msg : String =
    if enabled
    { "Git diff mode enabled" . to_string () }
    else { "Git diff mode disabled" . to_string () };
  if enabled {
    let warnings : Vec<String> =
      sources_not_tracked_in_git (config);
    if ! warnings . is_empty () {
      msg . push_str ("\n\nWarning: diff mode will be incomplete. \
        These sources are not fully tracked in git:\n");
      for w in &warnings {
        msg . push_str (&format! ("  - {}\n", w)); }} }
  msg }

/// Check each configured source for git-readiness.
/// Returns a list of human-readable warnings for sources
/// that are not in a git repo or have no commits yet.
fn sources_not_tracked_in_git (
  config : &SkgConfig,
) -> Vec<String> {
  let mut warnings : Vec<String> = Vec::new ();
  for (source_name, source_config) in &config . sources {
    let source_path : &std::path::Path =
      std::path::Path::new ( &source_config . path );
    match open_repo (source_path) {
      None => {
        warnings . push ( format! (
          "{}: not in a git repository", source_name )); },
      Some (repo) => {
        if repo . head () . is_err () {
          warnings . push ( format! (
            "{}: git repo has no commits yet", source_name )); } } } }
  warnings }
