//! The rerender pipeline every request that rerenders open views
//! outside a save shares: prepare, answer, stream. Each rerender is
//! prepared in memory first ('prepare_rerender_views_where'), so that
//! a text-release decision ('authorize_prepared_rerenders') can
//! precede any visible change, and then streamed: "rerender-lock",
//! one "rerender-view" per view, "rerender-done"
//! ('stream_prepared_rerenders'). The graph straddle the rerenders
//! brought up to date is written back to the connection
//! ('write_graph_straddle_back'), as a save does with its own.

use crate::graph_straddle::types::GraphStraddle;
use crate::serve::ViewsState;
use crate::serve::handlers::text_release::{
  TextReleaseDecision,
  challenge_response,
  decide as decide_text_release};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_errors_warnings_sexp, format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{DiffViewBufferKind, ViewId, ViewIdRecord, ViewState};
use crate::types::views_state::pids_from_viewforest;
use crate::update_buffer::{
  rerender_view, RerenderAfterSaveContext, RerenderOptions};

use futures::executor::block_on;
use std::net::TcpStream;
use std::collections::HashSet;
use std::sync::Arc;

pub(crate) struct PreparedView {
  pub(crate) view_id     : ViewId,
  text                   : String,
  pub(crate) viewforest  : ViewForest,
  view_id_record         : ViewIdRecord,
}

pub(crate) struct PreparedRerenders {
  runtime  : std::sync::Arc<RuntimeGeneration>,
  pub(crate) view_ids : Vec<ViewId>,
  pub(crate) views    : Vec<PreparedView>,
  pub(crate) errors   : Vec<String>,
  warnings : Vec<String>,
  pub(crate) graph_straddle : Option<Arc<GraphStraddle>>, // the connection's, as the rerenders brought it up to date
}

/// Complete every rerender in memory. Nothing is sent and no registered
/// view is changed, so the text-release decision can precede the lock and
/// the first externally visible mutation.
pub(crate) fn prepare_rerender_views (
  env                 : &SkgEnv,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  rerender_options    : fn (Option<DiffViewBufferKind>) -> RerenderOptions,
) -> PreparedRerenders {
  let runtime = env . runtime_snapshot ();
  prepare_rerender_views_with_runtime (
    env, runtime, views_state, skgrepo_restriction, prepass,
    rerender_options)
}

pub(crate) fn prepare_rerender_views_with_runtime (
  env                 : &SkgEnv,
  runtime             : std::sync::Arc<RuntimeGeneration>,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  rerender_options    : fn (Option<DiffViewBufferKind>) -> RerenderOptions,
) -> PreparedRerenders {
  prepare_rerender_views_where (
    env, runtime, views_state, skgrepo_restriction, prepass,
    rerender_options, |_, _| true )
}

/// Prepare only open views selected from their already-held state.
/// The selector is deliberately evaluated before cloning or rerendering, so a
/// command can leave unrelated clean *and dirty* buffers entirely untouched.
/// 'rerender_options' gives the options of each view's 'rerender_view'
/// from the view's buffer kind in the diff view, if any.
pub(crate) fn prepare_rerender_views_where (
  env                 : &SkgEnv,
  runtime             : std::sync::Arc<RuntimeGeneration>,
  views_state         : &ViewsState,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  prepass             : Option<&dyn Fn (&mut ViewForest) -> Result<(), Box<dyn std::error::Error>>>,
  rerender_options    : fn (Option<DiffViewBufferKind>) -> RerenderOptions,
  include             : impl Fn (&ViewId, &ViewState) -> bool,
) -> PreparedRerenders {
  let view_ids : Vec<ViewId> = views_state . open_views . views . iter ()
    .filter (|(view_id, state)| include (view_id, state))
    .map (|(view_id, _)| view_id . clone ()) . collect ();
  let mut context : RerenderAfterSaveContext =
    RerenderAfterSaveContext::without_save_with_runtime (
      env, runtime, skgrepo_restriction );
  // A buffer of the diff view is reconciled over a graph straddle
  // brought up to date from the connection's. Streaming the rerenders
  // writes it back, as a save does.
  context . graph_straddle = views_state . graph_straddle . clone ();
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
    let mut view_id_record : ViewIdRecord =
      views_state . open_views . view_id_record_of (view_id);
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
        &mut view_id_record,
        &mut context,
        None, // streamed rerenders repair silently.
        rerender_options (
          views_state . open_views . diffView_buffer_kind_of_view (view_id) )) } )
    { Ok (text) => {
        rendered_views . push ( PreparedView {
          view_id : view_id . clone (), text, viewforest,
          view_id_record } ); },
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
    graph_straddle : context . graph_straddle,
  }
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
  write_graph_straddle_back (views_state, prepared . graph_straddle);
  for view in prepared . views {
    views_state . open_views . update_view (
      &prepared . runtime . graph,
      &view . view_id, view . viewforest, view . view_id_record );
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

/// Keep the graph straddle a rerender brought up to date as the
/// connection's. A rerender that rendered no buffer of the diff view
/// built none, and leaves the connection's as it was.
pub(crate) fn write_graph_straddle_back (
  views_state    : &mut ViewsState,
  graph_straddle : Option<Arc<GraphStraddle>>,
) {
  if graph_straddle . is_some () {
    views_state . graph_straddle = graph_straddle; }}

/// Send an empty rerender stream: a "rerender-lock" naming no views,
/// then "rerender-done" with no errors or warnings.  Used after a
/// refusal: Emacs locks every Skg buffer and sets its stream guard
/// before sending a skgrepo-set switch, and only
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
