use crate::git_ops::read_repo::open_repo;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide as decide_text_release};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response, tag_text_response};
use crate::source_sets::ActiveSourceSet;
use crate::types::env::SkgEnv;
use crate::types::misc::SkgConfig;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewUri;
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::{rerender_view, RerenderAfterSaveContext};

use futures::executor::block_on;
use std::net::TcpStream;
use std::collections::HashSet;

struct PreparedView {
  uri        : ViewUri,
  text       : String,
  viewforest : ViewForest,
}

pub(crate) struct PreparedRerenders {
  uris     : Vec<ViewUri>,
  views    : Vec<PreparedView>,
  errors   : Vec<String>,
  warnings : Vec<String>,
}

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  stream_rerender_views (
    stream, env, views_state, Some (active_source_set),
    None, false,
    "rerender-all-views",
    &approved_pids_from_request (request)); }

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
  operation          : &str,
  approved_pids      : &HashSet<crate::types::misc::ID>,
) {
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, views_state, views_state . diff_mode_enabled,
    active_source_set, prepass, create_partnerCols );
  if ! authorize_prepared_rerenders (
    stream, env, &mut prepared, active_source_set,
    operation, approved_pids ) {
    return; }
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// The absent-reference command only removes edges to an ID freshly proven to
/// have no node.  It cannot introduce title/body text into any view, so this
/// narrowly scoped post-commit rerender bypasses the text-release challenge.
pub fn stream_rerender_views_after_absent_reference_cleanup (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
  raw_id     : &crate::types::misc::ID,
  affected_owner_pids : &HashSet<crate::types::misc::ID>,
) {
  let prepared = prepare_rerender_views_where (
    env, views_state, views_state . diff_mode_enabled,
    Some (active_source_set), None, false,
    |viewforest| view_can_display_absent_reference_change (
      viewforest, raw_id, affected_owner_pids ));
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// Complete every rerender in memory. Nothing is sent and no registered
/// view is changed, so the text-release decision can precede the lock and
/// the first externally visible mutation.
pub(crate) fn prepare_rerender_views (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  diff_mode_enabled   : bool,
  active_source_set   : Option<&ActiveSourceSet>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerCols  : bool,
) -> PreparedRerenders {
  prepare_rerender_views_where (
    env, views_state, diff_mode_enabled, active_source_set, prepass,
    create_partnerCols, |_| true )
}

/// Prepare only open views selected from their already-held ViewForest.
/// The selector is deliberately evaluated before cloning or rerendering, so a
/// command can leave unrelated clean *and dirty* buffers entirely untouched.
fn prepare_rerender_views_where (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  diff_mode_enabled   : bool,
  active_source_set   : Option<&ActiveSourceSet>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerCols  : bool,
  include             : impl Fn (&ViewForest) -> bool,
) -> PreparedRerenders {
  let uris : Vec<ViewUri> = views_state . open_views . views . iter ()
    .filter (|(_, state)| include (&state . viewforest))
    .map (|(uri, _)| uri . clone ()) . collect ();
  let mut context : RerenderAfterSaveContext =
    RerenderAfterSaveContext::without_save (
      env, diff_mode_enabled, active_source_set );
  let mut rendered_views : Vec<PreparedView> = Vec::new ();
  for uri in &uris {
    let mut viewforest : ViewForest = match
      views_state . open_views . viewuri_to_view (uri) {
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
        rendered_views . push ( PreparedView {
          uri : uri . clone (), text, viewforest } ); },
      Err (e) => {
        context . errors . push ( format! (
          "View {}: {}",
          uri . repr_in_client (), e )); }} }
  PreparedRerenders {
    uris,
    views    : rendered_views,
    errors   : context . errors,
    warnings : context . warnings,
  }
}

/// A cleanup changes an open view only when it currently displays the exact
/// dangling member, or when it displays a graph-backed owner whose relationship
/// list was rewritten.  `pids_from_viewforest` intentionally excludes Unknown,
/// so inspect both representations rather than introducing a new view index.
fn view_can_display_absent_reference_change (
  viewforest          : &ViewForest,
  raw_id              : &crate::types::misc::ID,
  affected_owner_pids : &HashSet<crate::types::misc::ID>,
) -> bool {
  pids_from_viewforest (viewforest) . iter ()
    .any (|pid| affected_owner_pids . contains (pid))
  || viewforest . nodes () . any (|node| matches! (
       &node . value () . kind,
       crate::types::viewnode::ViewNodeKind::Phantom (
         crate::types::viewnode::Phantom::Unknown (unknown))
       if unknown . id == *raw_id ))
}

pub(crate) fn authorize_prepared_rerenders (
  stream            : &mut TcpStream,
  env               : &SkgEnv,
  prepared          : &mut PreparedRerenders,
  active_source_set : Option<&ActiveSourceSet>,
  operation         : &str,
  approved_pids     : &HashSet<crate::types::misc::ID>,
) -> bool {
  let Some (active) = active_source_set else { return true; };
  let candidates : Vec<crate::types::misc::ID> =
    prepared . views . iter ()
    . flat_map ( |view|
      pids_from_viewforest (&view . viewforest) . into_iter () )
    . collect ();
  let release = decide_text_release (
    operation, active, &candidates,
    &env . in_rust_graph_snapshot (), approved_pids );
  match release {
    TextReleaseDecision::Challenge { .. } => {
      send_response_with_length_prefix (
        stream, &challenge_response (&release) . unwrap () );
      stream_empty_rerender (stream);
      false },
    TextReleaseDecision::AllowWithWarning { warning } => {
      prepared . warnings . push (warning);
      true },
    TextReleaseDecision::Allow => true,
  }
}

pub(crate) fn stream_prepared_rerenders (
  stream      : &mut TcpStream,
  views_state : &mut ViewsState,
  prepared    : PreparedRerenders,
) {
  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderLock,
      & format_lock_views_sexp (&prepared . uris) ));
  for view in prepared . views {
    views_state . open_views . update_view (
      &view . uri, view . viewforest);
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response (
        TcpToClient::RerenderView,
        & format_single_view_sexp (&view . uri, &view . text) )); }

  send_response_with_length_prefix (
    stream,
    & tag_sexp_response (
      TcpToClient::RerenderDone,
      & format_errors_warnings_sexp (
        &prepared . errors,
        &prepared . warnings) )); }

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
  request    : &str,
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
  let next_diff_mode : bool = ! views_state . diff_mode_enabled;
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, views_state, next_diff_mode, Some (active_source_set),
    None, false );
  if ! authorize_prepared_rerenders (
    stream, env, &mut prepared, Some (active_source_set),
    "diff-mode-rerender", &approved_pids_from_request (request) ) {
    return; }
  views_state . diff_mode_enabled = next_diff_mode;
  let msg : String =
    git_diff_mode_message (views_state . diff_mode_enabled, &env . config);
  tracing::info! ( msg = %msg, "Git diff mode toggled" );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( TcpToClient::GitDiffMode, &msg ));
  stream_prepared_rerenders (stream, views_state, prepared); }

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

#[cfg(test)]
mod tests {
  use super::view_can_display_absent_reference_change;
  use crate::types::misc::{ID, SourceName};
  use crate::types::tree::forest::ViewForest;
  use crate::types::viewnode::{mk_definitive_viewnode, mk_unknown_viewnode};
  use std::collections::HashSet;

  fn id (text : &str) -> ID { ID::from (text) }

  fn active_view (pid : &str) -> ViewForest {
    let mut view : ViewForest = ViewForest::new ();
    view . append_root (mk_definitive_viewnode (
      id (pid), SourceName::from ("main"), pid . to_string (), None ));
    view
  }

  fn view_with_unknown (raw_id : &str) -> ViewForest {
    let mut view : ViewForest = active_view ("unrelated-owner");
    let root = view . first_root () . unwrap () . id ();
    view . get_mut (root) . unwrap () . append (mk_unknown_viewnode (id (raw_id)));
    view
  }

  #[test]
  fn absent_reference_cleanup_selects_only_affected_owner_or_raw_unknown () {
    let owners : HashSet<ID> = HashSet::from ([id ("changed-owner")]);
    assert! (view_can_display_absent_reference_change (
      &active_view ("changed-owner"), &id ("gone"), &owners),
      "an open owner can display its rewritten relationship" );
    assert! (view_can_display_absent_reference_change (
      &view_with_unknown ("gone"), &id ("gone"), &owners),
      "Unknowns are absent from the PID index but still need removal" );
    assert! (! view_can_display_absent_reference_change (
      &active_view ("unrelated-owner"), &id ("gone"), &owners),
      "an unrelated view must receive neither a lock nor a replacement" );
  }
}
