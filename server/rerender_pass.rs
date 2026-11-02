//! One rerender pass: bringing open views up to date with the graph,
//! in memory, against one runtime generation and under one skgrepo
//! restriction, and what the pass shares across the views
//! ('RerenderPassContext'). A save rerenders the saved view and then
//! the collateral ones ('update_views_after_save'); every other
//! request that rerenders open views goes through the rerender
//! pipeline ('server/serve/rerender_stream.rs'). Both rerender each
//! view ('rerender_open_views'), decide the text release over what the
//! views show ('release_candidates'), and register and send the views
//! ('register_and_send_rerendered_views'). Each view is refreshed in
//! the mode its own buffer gives ('RerenderMode', 'refresh_view'): a
//! content view ('server/update_buffer.rs') or a buffer of the diff
//! view ('server/diff_view/buffer_rerender.rs').

use crate::diff_view::buffer_rerender::refresh_diffView_buffer_over_current_straddle;
use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::types::GraphStraddle;
use crate::org_to_text::viewforest_to_string;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::SaveResponse;
use crate::serve::handlers::text_release::{TextReleaseDecision, decide};
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::{
  register_and_send_rerendered_views, write_graph_straddle_back};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::save::ForkSpec;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{Editability, ViewRequest, ViewnodeKind, Vognode};
use crate::types::views_state::{
  DiffViewBufferKind, ViewState, ViewforestId, ViewnodeIdRecord, pids_from_viewforest};
use crate::update_buffer::refresh_content_view;

use ego_tree::NodeId;
use std::collections::HashSet;
use std::error::Error;
use std::net::TcpStream;
use std::sync::Arc;
use std::time::Instant;

/// See the module comment.
pub struct RerenderPassContext<'a> {
  pub env                       : &'a SkgEnv,
  /// The runtime generation every view of the pass is refreshed
  /// against.
  pub runtime                   : Arc<RuntimeGeneration>,
  pub errors                    : Vec<String>,
  pub warnings                  : Vec<String>,
  pub skgrepo_restriction       : &'a SkgrepoRestriction,
  /// The graph straddle a buffer of the diff view is refreshed
  /// over, brought up to date by 'current_graph_straddle_in_slot' when
  /// the first such buffer is rerendered. The caller sets the
  /// connection's, and keeps what it becomes.
  pub graph_straddle            : Option<Arc<GraphStraddle>>,
  /// What the rerender of a buffer of the diff view does beyond its
  /// refresh.
  pub diffView_buffer_rerender  : DiffViewBufferRerender,
}

impl<'a> RerenderPassContext<'a> {
  pub fn new (
    env                      : &'a SkgEnv,
    runtime                  : Arc<RuntimeGeneration>,
    skgrepo_restriction      : &'a SkgrepoRestriction,
    graph_straddle           : Option<Arc<GraphStraddle>>,
    diffView_buffer_rerender : DiffViewBufferRerender,
  ) -> RerenderPassContext<'a> {
    RerenderPassContext {
      env, runtime, skgrepo_restriction, graph_straddle,
      diffView_buffer_rerender,
      errors   : Vec::new (),
      warnings : Vec::new (), } }
}

/// What the rerender of a buffer of the diff view does beyond its
/// refresh.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum DiffViewBufferRerender {
  /// Nothing: the refresh alone.
  RefreshOnly,
  /// The "extend diff view" request's: the buffer is extended to the
  /// changes it does not show. Before the refresh, occurrences are
  /// made editable and the content origins the buffer lacks appended
  /// ('refresh_diffView_buffer_over_current_straddle').
  Extending,
  /// The "rerender all views" request's: 'Extending' for each buffer
  /// a reload named ('ViewState::extend_at_next_rerender_all'), the
  /// refresh alone for the others.
  ExtendingWhereAReloadAsked,
}

impl DiffViewBufferRerender {
  /// Whether the rerender of the buffer of the diff view 'state'
  /// extends it.
  pub fn extends (
    &self,
    state : &ViewState,
  ) -> bool {
    match self {
      DiffViewBufferRerender::RefreshOnly => false,
      DiffViewBufferRerender::Extending => true,
      DiffViewBufferRerender::ExtendingWhereAReloadAsked =>
        state . extend_at_next_rerender_all, } }
}

/// An open view, rerendered in memory: its new text, and the
/// viewforest and vnid record that replace the registered ones once
/// the rerender is released.
pub(crate) struct RerenderedView {
  pub(crate) viewforest_id      : ViewforestId,
  pub(crate) text               : String,
  pub(crate) viewforest         : ViewForest,
  pub(crate) viewnode_id_record : ViewnodeIdRecord,
}

/// PURPOSE:
/// Updates the rendered views and ViewsState
/// for each view affected by the save.
/// Starts with the saved view itself, not the collateral ones,
/// to minimize any freeze the user might experience.
/// .
/// ASSUMES:
/// the graph was already updated. The reverse order would be bad.
pub fn update_views_after_save (
  stream                      : &mut TcpStream,
  saved_view                  : ViewForest,
  collateral_viewforest_ids   : Vec<ViewforestId>,
  env                         : &SkgEnv,
  runtime                     : Arc<RuntimeGeneration>,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                 : &mut ViewsState,
  skgrepo_restriction         : &SkgrepoRestriction,
  text_approved_pids        : &HashSet<ID>,
  fork_specs                  : &[ForkSpec],
  named_diffView_buffer_kind  : Option<DiffViewBufferKind>, // what the request says the saved buffer is, for a view the connection does not hold
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderPassContext = RerenderPassContext::new (
    env, runtime, skgrepo_restriction,
    views_state . graph_straddle . clone (),
    DiffViewBufferRerender::RefreshOnly );
  let saved_diffView_buffer_kind : Option<DiffViewBufferKind> =
    // A held view is what the connection registered; an unheld one, what the request names.
    match viewforest_id_from_request_result . as_ref () . ok ()
      . and_then ( |viewforest_id| views_state . open_views . views . get (viewforest_id) )
    { Some (held) => held . diffView_buffer_kind,
      None => named_diffView_buffer_kind, };
  let mut saved_view_mut : ViewForest = saved_view;
  let mut saved_viewnode_id_record : ViewnodeIdRecord =
    // The parsed buffer carries viewnode IDs; the record says which
    // it knows, before the refresh runs.
    views_state . open_views . viewnode_id_record_for_saved_buffer (
      viewforest_id_from_request_result . as_ref () . ok (),
      &saved_view_mut );
  saved_viewnode_id_record . drop_unknown_viewnode_ids_in_parsed_viewforest (&mut saved_view_mut);
  replace_saved_view_fork_roots (&mut saved_view_mut, fork_specs) ?;
  // The skgsave-commit has happened, so every `(editRequest ...)` in the
  // submitted view is now consumed input. Clear all request carriers at this
  // one boundary before the refresh can echo any of them into the response.
  // `viewRequests` remain: those are fulfilled by the refresh below.
  saved_view_mut . consume_edit_requests_after_save ();
  // Gate the occurrences already in the forests before rendering, so even a
  // rendering error cannot echo protected title/body text. A second decision
  // below covers nodes introduced by the refresh.
  { let restriction : &SkgrepoRestriction = skgrepo_restriction;
    let input_candidates : Vec<ID> = release_candidates (
      std::iter::once (&saved_view_mut) . chain (
        collateral_viewforest_ids . iter () . filter_map ( |viewforest_id|
          views_state . open_views . viewforest_id_to_view (viewforest_id) )));
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &input_candidates,
      &context . runtime . graph, text_approved_pids );
    if matches! (release, TextReleaseDecision::Challenge { .. }) {
      return Ok ( SaveResponse::text_release_challenge (&release) ); }}
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "refresh_view (saved)" ). entered();
      refresh_view ( // saved view first; collateral ones later
        &mut saved_view_mut,
        &mut saved_viewnode_id_record,
        &mut context,
        RerenderMode::of_buffer (saved_diffView_buffer_kind, false) ) ?; }
  let saved_text : String = viewforest_to_string (&saved_view_mut) ?;
  let readiness_summary : Option<BufferReadinessSummary> =
    saved_diffView_buffer_kind . and ( context . graph_straddle . as_ref () )
    . map ( |straddle| buffer_readiness_summary_for (
      env, straddle, &saved_view_mut ) );
  let collateral_views : Vec<RerenderedView> =
    rerender_open_views (&collateral_viewforest_ids, views_state, &mut context);
  // Everything textual is now staged in memory. Decide before changing the
  // open-view registry or streaming the first view.
  let release_candidates : Vec<ID> = release_candidates (
    std::iter::once (&saved_view_mut) . chain (
      collateral_views . iter () . map ( |view| &view . viewforest ) ));
  { let restriction : &SkgrepoRestriction = skgrepo_restriction;
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &release_candidates,
      &context . runtime . graph, text_approved_pids );
    match release {
      decision @ TextReleaseDecision::Challenge { .. } => {
        return Ok ( SaveResponse::text_release_challenge (&decision) ); }
      TextReleaseDecision::AllowWithWarning { warning } =>
        context . warnings . push (warning),
      TextReleaseDecision::Allow => {}, }}
  write_graph_straddle_back (views_state, context . graph_straddle . take ());
  if let Ok (viewforest_id) = viewforest_id_from_request_result {
    views_state . open_views . update_view (
      &context . runtime . graph, viewforest_id, saved_view_mut, saved_viewnode_id_record );
    if let Some (buffer_kind) = saved_diffView_buffer_kind {
      views_state . open_views . mark_as_diffView_buffer (viewforest_id, buffer_kind); }
    register_and_send_rerendered_views (
      stream, views_state, &context . runtime, collateral_views,
      TcpToClient::CollateralView ); }
  Ok ( SaveResponse::saved (
    saved_text, context . errors, context . warnings,
    readiness_summary, context . runtime . generation )) }

/// A root is intentionally rendered raw when the user ordinarily opens an
/// overridden node.  The buffer which *created* a fork is the exception: once
/// the save's skgsave-commit happens, continuing to show its foreign root would make the next
/// edit attempt another fork.  Replace only roots named by this save's exact
/// ForkSpecs; content occurrences are substituted by the refresh.
fn replace_saved_view_fork_roots (
  viewforest : &mut ViewForest,
  fork_specs : &[ForkSpec],
) -> Result<(), Box<dyn Error>> {
  if fork_specs . is_empty () { return Ok (()); }
  let root_gnids : Vec<NodeId> = viewforest . root_gnids ();
  for root_gnid in root_gnids {
    let original : Option<ID> =
      viewforest . get (root_gnid) . and_then (|root| match
        &root . value () . kind {
          ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
            Some (occurrence . gnid . clone ()),
          _ => None, });
    let Some (original) = original else { continue; };
    let Some (spec) = fork_specs . iter ()
      . find (|spec| spec . original_gnid == original)
    else { continue; };
    let clone = & spec . clone . 0;
    let mut root = viewforest . get_mut (root_gnid)
      . ok_or ("replace_saved_view_fork_roots: root not found") ?;
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut root . value () . kind
    { occurrence . gnid = clone . pid . clone ();
      occurrence . home_skgrepo = clone . home_skgrepo . clone ();
      occurrence . title = clone . title . clone ();
      if let Editability::Editable { body, .. } = &mut occurrence . editability
      { *body = clone . body . clone (); }
      occurrence . view_requests . remove (&ViewRequest::Fork);
      occurrence . overridesHere = Some (original); }}
  Ok (())
}

/// Rerender each of the open views 'viewforest_ids' in memory
/// ('rerender_open_view'), in order; a view that fails adds its error
/// to the context and is left out.
pub(crate) fn rerender_open_views (
  viewforest_ids : &[ViewforestId],
  views_state    : &ViewsState,
  context        : &mut RerenderPassContext<'_>,
) -> Vec<RerenderedView> {
  let mut rendered_views : Vec<RerenderedView> = Vec::new ();
  for viewforest_id in viewforest_ids {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ( "rerender_open_view" ) . entered ();
    match rerender_open_view (viewforest_id, views_state, context)
    { Ok (rendered) => rendered_views . push (rendered),
      Err (e) => context . errors . push (e), }}
  rendered_views }

/// Rerender the open view 'viewforest_id' in memory, in the mode its
/// buffer kind in the diff view, if any, gives ('RerenderMode'); the
/// registered view is untouched. An error names the view.
pub(crate) fn rerender_open_view (
  viewforest_id : &ViewforestId,
  views_state   : &ViewsState,
  context       : &mut RerenderPassContext<'_>,
) -> Result<RerenderedView, String> {
  let mut viewforest : ViewForest =
    views_state . open_views . viewforest_id_to_view (viewforest_id)
    . cloned ()
    . ok_or_else ( || format! ( "View {}: no viewforest found",
                                viewforest_id . repr_in_client () )) ?;
  let mut viewnode_id_record : ViewnodeIdRecord =
    views_state . open_views . viewnode_id_record_of (viewforest_id);
  let mode : RerenderMode = RerenderMode::of_buffer (
    views_state . open_views . diffView_buffer_kind_of_view (viewforest_id),
    views_state . open_views . views . get (viewforest_id)
      . is_some_and ( |state| context . diffView_buffer_rerender . extends (state) ));
  let in_the_view : &dyn Fn (Box<dyn Error>) -> String = &|e : Box<dyn Error>| -> String {
    format! ( "View {}: {}", viewforest_id . repr_in_client (), e ) };
  refresh_view (
    &mut viewforest, &mut viewnode_id_record, context, mode )
    . map_err (in_the_view) ?;
  let text : String = viewforest_to_string (&viewforest)
    . map_err (in_the_view) ?;
  Ok ( RerenderedView {
    viewforest_id : viewforest_id . clone (),
    text,
    viewforest,
    viewnode_id_record, } ) }

/// The gnids whose text the views would release: their occurrences'
/// ('pids_from_viewforest'), what the text-release decision reads.
pub(crate) fn release_candidates<'v> (
  viewforests : impl Iterator<Item = &'v ViewForest>,
) -> Vec<ID> {
  viewforests
    . flat_map ( |viewforest| pids_from_viewforest (viewforest) . into_iter () )
    . collect () }

/// The situation a rerender refreshes a view in, from the view
/// itself: a content view, or a buffer of the diff view of this kind,
/// extended to the changes it does not show or not.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RerenderMode {
  ContentView,
  DiffView { buffer_kind : DiffViewBufferKind,
             extend      : bool },
}

impl RerenderMode {
  /// The mode of a view whose buffer kind in the diff view, if it is
  /// one, is 'diffView_buffer'; such a buffer is extended if 'extend'.
  pub fn of_buffer (
    diffView_buffer : Option<DiffViewBufferKind>,
    extend          : bool,
  ) -> RerenderMode {
    match diffView_buffer {
      None => RerenderMode::ContentView,
      Some (buffer_kind) => RerenderMode::DiffView { buffer_kind, extend }, } }
}

/// Bring an open view up to date with the graph; it renders nothing,
/// and the caller renders it. It is refreshed ('refresh_viewforest'),
/// from what it holds and the graph as they are now: a content view in
/// content-view mode ('refresh_content_view'); a buffer of the diff
/// view in diff-view mode, over the graph straddle, brought up to date
/// in 'context' ('refresh_diffView_buffer_over_current_straddle',
/// which says what extending the buffer adds).
pub fn refresh_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord, // the view's, which assigns the viewnode IDs its new viewnodes get
  context            : &mut RerenderPassContext<'_>,
  mode               : RerenderMode,
) -> Result<(), Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "refresh_viewforest" ). entered();
  match mode {
    RerenderMode::ContentView => refresh_content_view (
      viewforest, viewnode_id_record, context ) ?,
    RerenderMode::DiffView { buffer_kind, extend } => refresh_diffView_buffer_over_current_straddle (
      context, buffer_kind, extend, viewforest, viewnode_id_record ) ?, };
  tracing::debug!("refresh_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (( )) }
