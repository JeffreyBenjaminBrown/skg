use crate::git_ops::read_repo::open_repo;
use crate::serve::ViewsState;
use crate::serve::handlers::collateral_scheduler::CollateralScheduler;
use crate::serve::handlers::scalar_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, send_response_with_length_prefix, tag_sexp_response, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::SkgConfig;
use crate::types::views_state::ViewUri;

use std::net::TcpStream;

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
  collateral_scheduler : &mut CollateralScheduler,
) {
  let uris = collateral_scheduler . replace_for_explicit_rerender (
    views_state, env, active_source_set,
    &approved_pids_from_request (request),
    "rerender-all-views", false);
  stream_queued_rerender (stream, &uris); }

pub(crate) fn stream_queued_rerender (
  stream : &mut TcpStream,
  uris   : &[ViewUri],
) {
  // The command's broad transient lock is no longer application authority.
  // Release it immediately; each retained-session offer independently checks
  // cleanliness and exact buffer/revision/token/generation authority.
  let _ = send_response_with_length_prefix (
    stream,
    &tag_sexp_response (
      TcpToClient::RerenderLock, &format_lock_views_sexp (&[])));
  let response = add_queued_uris (
    &format_errors_warnings_sexp (&[], &[]), uris);
  let _ = send_response_with_length_prefix (
    stream,
    &tag_sexp_response (TcpToClient::RerenderDone, &response));
}

fn add_queued_uris (response : &str, uris : &[ViewUri]) -> String {
  use sexp::{Atom, Sexp};
  let Ok (Sexp::List (mut fields)) = sexp::parse (response) else {
    return response . into (); };
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("queued-view-uris" . into ())),
    Sexp::List (uris . iter () . map (|uri|
      Sexp::Atom (Atom::S (uri . repr_in_client ()))) . collect ()),
  ]));
  Sexp::List (fields) . to_string ()
}

/// Send an EMPTY rerender stream: a "rerender-lock" naming no views,
/// then "rerender-done" with no errors or warnings.  Used after a
/// refusal: Emacs locks every Skg buffer and sets its stream guard
/// BEFORE sending a diff-mode toggle or source-set switch, and only
/// the rerender stream unwinds them.  The empty lock list makes
/// Emacs unlock every buffer; the done message clears the guard.
pub fn stream_empty_rerender (
  stream : &mut TcpStream,
) {
  stream_queued_rerender (stream, &[]); }

/// Handle "git diff mode toggle" request.
/// Toggles diff mode, sends the GitDiffMode response with warnings,
/// then streams re-rendered views.
pub fn handle_git_diff_toggle_and_rerender (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
  collateral_scheduler : &mut CollateralScheduler,
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
      let _ = send_response_with_length_prefix (
        stream,
        & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
      stream_empty_rerender (stream);
      return; }}
  let next_diff_mode : bool = ! views_state . diff_mode_enabled;
  views_state . diff_mode_enabled = next_diff_mode;
  let uris = collateral_scheduler . replace_for_explicit_rerender (
    views_state, env, active_source_set,
    &approved_pids_from_request (request),
    "diff-mode-rerender", false);
  let msg : String =
    git_diff_mode_message (views_state . diff_mode_enabled, &env . config);
  tracing::info! ( msg = %msg, "Git diff mode toggled" );
  let _ = send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
  stream_queued_rerender (stream, &uris); }

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
  for (source_name, source_config) in config . sources . iter () {
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

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn queued_completion_names_every_view_without_streaming_text () {
    let response = add_queued_uris (
      &format_errors_warnings_sexp (&[], &[]),
      &[
        ViewUri::ContentView ("view-a" . into ()),
        ViewUri::SearchView ("dog" . into ()),
      ]);
    assert! (response . contains ("(queued-view-uris (view-a search:dog))"));
    assert! (!response . contains ("content"));
  }
}
