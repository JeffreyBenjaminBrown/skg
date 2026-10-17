use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  approved_pids_from_request,
  challenge_response,
  decide as decide_text_release};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::ViewId;
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::{
  rerender_view, tdv_buffer_of_view, RerenderAfterSaveContext};

use futures::executor::block_on;
use std::net::TcpStream;
use std::collections::HashSet;

struct PreparedView {
  view_id    : ViewId,
  text       : String,
  viewforest : ViewForest,
}

pub(crate) struct PreparedRerenders {
  runtime  : std::sync::Arc<RuntimeGeneration>,
  view_ids : Vec<ViewId>,
  views    : Vec<PreparedView>,
  errors   : Vec<String>,
  warnings : Vec<String>,
}

pub fn handle_rerender_all_views_request (
  stream     : &mut TcpStream,
  request    : &str,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
) {
  let excluded : HashSet<ViewId> =
    match excluded_view_ids_from_request (request) {
      Ok (excluded) => excluded,
      Err (error) => {
        send_response_with_length_prefix (
          stream, &tag_sexp_response (TcpToClient::RerenderLock,
            &format_lock_views_sexp (&[])));
        send_response_with_length_prefix (
          stream, &tag_sexp_response (TcpToClient::RerenderDone,
            &format_errors_warnings_sexp (&[error], &[])));
        return; } };
  stream_rerender_views_excluding (
    stream, env, views_state, skgrepo_restriction,
    &approved_pids_from_request (request), &excluded); }

fn excluded_view_ids_from_request (
  request : &str,
) -> Result<HashSet<ViewId>, String> {
  let parsed = sexp::parse (request) . map_err (|error| error . to_string ())?;
  let present : bool = match &parsed {
    sexp::Sexp::List (items) => items . iter () . any (|item|
      matches! (item, sexp::Sexp::List (fields)
        if matches! (fields . first (), Some (sexp::Sexp::Atom (
          sexp::Atom::S (name))) if name == "exclude-view-ids"))),
    _ => false, };
  if ! present { return Ok (HashSet::new ()); }
  crate::types::sexp::extract_string_list_from_sexp (
    &parsed, "exclude-view-ids")
    .map (|view_ids| view_ids . into_iter ()
      .map (ViewId::from_client_string) . collect ())
}

#[cfg(test)]
mod import_exclusion_tests {
  use super::*;

  #[test]
  fn dirty_view_exclusions_are_parsed_and_malformed_lists_fail_closed () {
    let request : &str = "((request . \"rerender all views\") (exclude-view-ids \"dirty-view_id\"))";
    let excluded : HashSet<ViewId> =
      excluded_view_ids_from_request (request) . unwrap ();
    assert! (excluded . contains (&ViewId::from_client_string (
      "dirty-view_id" . to_string ())));
    assert! (excluded_view_ids_from_request (
      "((request . \"rerender all views\") (exclude-view-ids (bad)))")
      . is_err ());
  }
}

fn stream_rerender_views_excluding (
  stream : &mut TcpStream,
  env : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  approved_pids : &HashSet<crate::types::misc::ID>,
  excluded : &HashSet<ViewId>,
) {
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, views_state, Some (skgrepo_restriction), None, false);
  prepared . view_ids . retain (|view_id| ! excluded . contains (view_id));
  prepared . views . retain (|view| ! excluded . contains (&view . view_id));
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, Some (skgrepo_restriction),
    "rerender-all-views", approved_pids) {
    return; }
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// Stream re-rendered views to Emacs.
/// Sends: rerender-lock → rerender-view* → rerender-done.
/// Shared by 'handle_rerender_all_views_request' and the skgrepo-set switch
/// ('restrict_repo_set'), which passes a per-view prepass (the
/// convert-and-prune step) and asks for PartnerFolder re-creation
/// (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org).
pub fn stream_rerender_views (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass    : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerFolders : bool,
  operation          : &str,
  approved_pids      : &HashSet<crate::types::misc::ID>,
) {
  let mut prepared : PreparedRerenders = prepare_rerender_views (
    env, views_state, skgrepo_restriction, prepass, create_partnerFolders );
  if ! authorize_prepared_rerenders (
    stream, &mut prepared, skgrepo_restriction,
    operation, approved_pids ) {
    return; }
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// The absent-reference command only removes relationships to an ID freshly proven to
/// have no node.  It cannot introduce title/body text into any view, so this
/// narrowly scoped post-skgsave-commit rerender bypasses the text-release challenge.
pub fn stream_rerender_views_after_absent_reference_cleanup (
  stream     : &mut TcpStream,
  env        : &SkgEnv,
  views_state : &mut ViewsState,
  skgrepo_restriction : &SkgrepoRestriction,
  raw_skgid     : &crate::types::misc::ID,
  affected_recorder_pids : &HashSet<crate::types::misc::ID>,
) {
  let prepared = prepare_rerender_views_where (
    env, env . runtime_snapshot (), views_state,
    Some (skgrepo_restriction), None, false,
    |viewforest| view_can_display_absent_reference_change (
      viewforest, raw_skgid, affected_recorder_pids ));
  stream_prepared_rerenders (stream, views_state, prepared);
}

/// Complete every rerender in memory. Nothing is sent and no registered
/// view is changed, so the text-release decision can precede the lock and
/// the first externally visible mutation.
pub(crate) fn prepare_rerender_views (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerFolders  : bool,
) -> PreparedRerenders {
  let runtime = env . runtime_snapshot ();
  prepare_rerender_views_with_runtime (
    env, runtime, views_state, skgrepo_restriction, prepass,
    create_partnerFolders)
}

pub(crate) fn prepare_rerender_views_with_runtime (
  env                 : &SkgEnv,
  runtime             : std::sync::Arc<RuntimeGeneration>,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerFolders  : bool,
) -> PreparedRerenders {
  prepare_rerender_views_where (
    env, runtime, views_state, skgrepo_restriction, prepass,
    create_partnerFolders, |_| true )
}

/// Prepare only open views selected from their already-held ViewForest.
/// The selector is deliberately evaluated before cloning or rerendering, so a
/// command can leave unrelated clean *and dirty* buffers entirely untouched.
fn prepare_rerender_views_where (
  env                 : &SkgEnv,
  runtime             : std::sync::Arc<RuntimeGeneration>,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  create_partnerFolders  : bool,
  include             : impl Fn (&ViewForest) -> bool,
) -> PreparedRerenders {
  let view_ids : Vec<ViewId> = views_state . open_views . views . iter ()
    .filter (|(_, state)| include (&state . viewforest))
    .map (|(view_id, _)| view_id . clone ()) . collect ();
  let mut context : RerenderAfterSaveContext =
    RerenderAfterSaveContext::without_save_with_runtime (
      env, runtime, skgrepo_restriction );
  // A buffer of the total diff view is reconciled over a graph pair
  // brought up to date from the connection's, which it does not
  // replace.
  context . graph_pair = views_state . graph_pair . clone ();
  let mut rendered_views : Vec<PreparedView> = Vec::new ();
  for view_id in &view_ids {
    let mut viewforest : ViewForest = match
      views_state . open_views . viewid_to_view (view_id) {
        Some (f) => f . clone (),
        None => {
          context . errors . push ( format! (
            "View {}: no viewforest found",
            view_id . repr_in_client () ));
          continue; } };
    if let Some (prepass) = prepass {
      if let Err (e) = prepass (&mut viewforest) {
        context . errors . push ( format! (
          "View {}: {}",
          view_id . repr_in_client (), e ));
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
        create_partnerFolders,
        false,
        tdv_buffer_of_view (views_state, view_id)
      ) } )
    { Ok (text) => {
        rendered_views . push ( PreparedView {
          view_id : view_id . clone (), text, viewforest } ); },
      Err (e) => {
        context . errors . push ( format! (
          "View {}: {}",
          view_id . repr_in_client (), e )); }} }
  PreparedRerenders {
    runtime : context . runtime . clone (),
    view_ids,
    views    : rendered_views,
    errors   : context . errors,
    warnings : context . warnings,
  }
}

/// A cleanup changes an open view only when it currently displays the exact
/// dangling member, or when it displays a graph-member recorder whose relationship
/// list was rewritten.  `pids_from_viewforest` intentionally excludes Unknown,
/// so inspect both representations rather than introducing a new view index.
fn view_can_display_absent_reference_change (
  viewforest             : &ViewForest,
  raw_skgid              : &crate::types::misc::ID,
  affected_recorder_pids : &HashSet<crate::types::misc::ID>,
) -> bool {
  pids_from_viewforest (viewforest) . iter ()
    .any (|pid| affected_recorder_pids . contains (pid))
  || viewforest . nodes () . any (|node| matches! (
       &node . value () . kind,
       crate::types::viewnode::ViewnodeKind::Vognode (crate::types::viewnode::Vognode::Phantom (
         crate::types::viewnode::Phantom::Unknown (unknown)))
       if unknown . skgid == *raw_skgid ))
}

pub(crate) fn authorize_prepared_rerenders (
  stream             : &mut TcpStream,
  prepared           : &mut PreparedRerenders,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  operation          : &str,
  approved_pids      : &HashSet<crate::types::misc::ID>,
) -> bool {
  let Some (restriction) = skgrepo_restriction else { return true; };
  let candidates : Vec<crate::types::misc::ID> =
    prepared . views . iter ()
    . flat_map ( |view|
      pids_from_viewforest (&view . viewforest) . into_iter () )
    . collect ();
  let release = decide_text_release (
    operation, restriction, &candidates,
    &prepared . runtime . graph, approved_pids );
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
      & format_lock_views_sexp (&prepared . view_ids) ));
  for view in prepared . views {
    views_state . open_views . update_view (
      &prepared . runtime . graph,
      &view . view_id, view . viewforest);
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response (
        TcpToClient::RerenderView,
        & format_single_view_sexp (&view . view_id, &view . text) )); }

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
/// BEFORE sending a skgrepo-set switch, and only
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

#[cfg(test)]
mod tests {
  use super::view_can_display_absent_reference_change;
  use crate::types::misc::{ID, SkgrepoName};
  use crate::types::tree::forest::ViewForest;
  use crate::types::viewnode::{mk_editable_viewnode, mk_unknown_viewnode};
  use std::collections::HashSet;

  fn skgid (text : &str) -> ID { ID::from (text) }

  fn unrestricted_view (pid : &str) -> ViewForest {
    let mut view : ViewForest = ViewForest::new ();
    view . append_root (mk_editable_viewnode (
      skgid (pid), SkgrepoName::from ("main"), pid . to_string (), None ));
    view
  }

  fn view_with_unknown (raw_skgid : &str) -> ViewForest {
    let mut view : ViewForest = unrestricted_view ("unrelated-owner");
    let root = view . first_root () . unwrap () . id ();
    view . get_mut (root) . unwrap () . append (mk_unknown_viewnode (skgid (raw_skgid)));
    view
  }

  #[test]
  fn absent_reference_cleanup_selects_only_affected_recorder_or_raw_unknown () {
    let recorders : HashSet<ID> = HashSet::from ([skgid ("changed-owner")]);
    assert! (view_can_display_absent_reference_change (
      &unrestricted_view ("changed-owner"), &skgid ("gone"), &recorders),
      "an open recorder can display its rewritten relationship" );
    assert! (view_can_display_absent_reference_change (
      &view_with_unknown ("gone"), &skgid ("gone"), &recorders),
      "Unknowns are absent from the PID index but still need removal" );
    assert! (! view_can_display_absent_reference_change (
      &unrestricted_view ("unrelated-owner"), &skgid ("gone"), &recorders),
      "an unrelated view must receive neither a lock nor a replacement" );
  }
}
