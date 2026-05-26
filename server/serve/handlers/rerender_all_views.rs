use crate::git_ops::read_repo::open_repo;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::SkgConfig;
use crate::types::viewnode::ViewNode;
use crate::types::views_state::ViewUri;
use crate::update_buffer::{rerender_view, RerenderAfterSaveContext};

use ego_tree::Tree;
use futures::executor::block_on;
use std::net::TcpStream;

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  stream_rerender_views (
    stream, env, views_state, Some (active_source_set)); }

/// Stream re-rendered views to Emacs.
/// Sends: rerender-lock → rerender-view* → rerender-done.
/// Shared by 'handle_rerender_all_views_request' and (in Change 3)
/// 'handle_git_diff_toggle_and_rerender'.
pub fn stream_rerender_views (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : Option<&ActiveSourceSet>,
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
    let mut viewforest : Tree<ViewNode> = match
      views_state . open_views . viewuri_to_view (&uri) {
        Some (f) => f . clone (),
        None => {
          context . errors . push ( format! (
            "View {}: no viewforest found",
            uri . repr_in_client () ));
          continue; } };
    match block_on ( async {
      let _span : tracing::span::EnteredSpan =
        tracing::info_span! (
          "rerender_view (rerender-all)"
        ) . entered ();
      rerender_view (
        &mut viewforest,
        &mut context,
        false ) . await } )
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

  // 3. Send done message with errors.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_errors_sexp (&context . errors) )); }

/// Handle "git diff mode toggle" request.
/// Toggles diff mode, sends the GitDiffMode response with warnings,
/// then streams re-rendered views.
pub fn handle_git_diff_toggle_and_rerender (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  views_state . diff_mode_enabled = ! views_state . diff_mode_enabled;
  let msg : String =
    git_diff_mode_message (views_state . diff_mode_enabled, &env . config);
  tracing::info! ( msg = %msg, "Git diff mode toggled" );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
  stream_rerender_views (
    stream, env, views_state, Some (active_source_set)); }

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
