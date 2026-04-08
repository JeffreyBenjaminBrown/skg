use crate::git_ops::read_repo::open_repo;
use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{ compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response, tag_text_response};
use crate::types::git::SourceDiff;
use crate::types::memory::{SkgNodeMap, ViewUri};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::viewnode::ViewNode;
use crate::update_buffer::{ rerender_view, seed_skgnodemap_from_pool, merge_skgnodemap_into_pool};

use ego_tree::Tree;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

pub fn handle_rerender_all_views_request (
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) {
  stream_rerender_views (
    stream, typedb_driver, config, conn_state ); }

/// Stream re-rendered views to Emacs.
/// Sends: rerender-lock → rerender-view* → rerender-done.
/// Shared by 'handle_rerender_all_views_request' and (in Change 3)
/// 'handle_git_diff_toggle_and_rerender'.
pub fn stream_rerender_views (
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) {
  let uris : Vec<ViewUri> =
    conn_state . memory . views . keys () . cloned () . collect ();

  // 1. Send lock message with all URIs.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp (&uris) ));

  // 2. Compute diffs once, then stream each view.
  let source_diffs
    : Option<HashMap<SourceName, SourceDiff>>
    = if conn_state . diff_mode_enabled
      { Some ( compute_diff_for_every_source (config)) }
      else { None };
  let deleted_since_head_pid_src_map
    : HashMap<ID, SourceName>
    = source_diffs . as_ref ()
      . map ( |d| deleted_ids_to_source (d))
      . unwrap_or_default ();
  let deleted_by_this_save_pids : HashSet<ID> =
    HashSet::new ();
  let mut errors : Vec<String> = Vec::new ();

  for uri in uris {
    let mut forest : Tree<ViewNode> = match
      conn_state . memory . viewuri_to_view (&uri) {
        Some (f) => f . clone (),
        None => {
          errors . push ( format! (
            "View {}: no viewforest found",
            uri . repr_in_client () ));
          continue; } };
    let mut map : SkgNodeMap =
      seed_skgnodemap_from_pool (&uri, conn_state);
    match block_on ( async {
      let _span : tracing::span::EnteredSpan =
        tracing::info_span! (
          "rerender_view (rerender-all)"
        ) . entered ();
      rerender_view (
        &mut forest, &mut map,
        &source_diffs, config, typedb_driver,
        &mut errors, &deleted_since_head_pid_src_map,
        &deleted_by_this_save_pids,
        false ) . await } )
    { Ok (text) => {
        merge_skgnodemap_into_pool (&map, conn_state);
        conn_state . memory . update_view (&uri, forest);
        send_response_with_length_prefix (
          stream,
          & tag_sexp_response (
            TcpToClient::RerenderView,
            & format_single_view_sexp (&uri, &text) )); },
      Err (e) => {
        errors . push ( format! (
          "View {}: {}",
          uri . repr_in_client (), e )); }} }

  // 3. Send done message with errors.
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_errors_sexp (&errors) )); }

/// Handle "git diff mode toggle" request.
/// Toggles diff mode, sends the GitDiffMode response with warnings,
/// then streams re-rendered views.
pub fn handle_git_diff_toggle_and_rerender (
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  conn_state    : &mut ConnectionState,
) {
  conn_state . diff_mode_enabled = ! conn_state . diff_mode_enabled;
  let msg : String =
    git_diff_mode_message (conn_state . diff_mode_enabled, config);
  tracing::info! ( msg = %msg, "Git diff mode toggled" );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
  stream_rerender_views (
    stream, typedb_driver, config, conn_state ); }

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
