//! Bringing views up to date with the graph. Every view is refreshed
//! ('refresh_viewforest', 'server/refresh/'): a content view in
//! content-view mode, a buffer of the diff view in diff-view mode,
//! over the graph straddle ('server/diff_view/buffer_rerender.rs'):
//! - 'render_initial_view' renders a new content view, the refresh of
//!   a stub of its roots, each asking for its containers folder;
//! - 'diffView_buffer_viewforest'
//!   ('server/diff_view/pruned_change_view.rs') renders a buffer of the
//!   diff view, the refresh of a stub of its roots;
//! - 'rerender_view' brings a view that already exists up to date
//!   (after a save, collaterally, after a commit, or at a skgrepo-set
//!   switch), from what it holds;
//! - 'update_views_after_save' rerenders the saved view and then the
//!   collateral ones, streaming each to the client.


use crate::dbs::in_rust_graph::InRustGraph;
use crate::org_to_text::viewforest_to_string;
use crate::refresh::{Truth, refresh_content_viewforest};
use crate::refresh::buffer_corrections::{BufferCorrectionPolicy, BufferCorrection, ViewBufferCorrections};
use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::diff_view::buffer_rerender::rerender_diffView_buffer;
use crate::rerender_pass::RerenderPassContext;
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::SaveResponse;
use crate::serve::handlers::text_release::{
  TextReleaseDecision, decide,
};
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::write_graph_straddle_back;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::views_state::{DiffViewBufferKind, ViewforestId, ViewnodeIdRecord, pids_from_viewforest};
use crate::types::misc::ID;
use crate::types::save::ForkSpec;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::{
  Editability, Viewnode, ViewnodeKind, mk_editable_viewnode, mk_unknown_viewnode};
use crate::types::viewnode::{FolderRelation, Vognode, ViewRequest};

use ego_tree::{NodeId, Tree};
use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;
use std::time::Instant;

/// An open view, rerendered in memory: its new text, what the refresh
/// corrected, and the viewforest and vnid record that replace the
/// registered ones once the rerender is released.
pub(crate) struct RerenderedView {
  pub(crate) viewforest_id      : ViewforestId,
  pub(crate) text               : String,
  pub(crate) buffer_corrections : Vec<BufferCorrection>,
  pub(crate) viewforest         : ViewForest,
  pub(crate) viewnode_id_record : ViewnodeIdRecord,
}

/// PURPOSE:
/// Updates the rendered views and ViewsState
/// for each view affected by the save.
/// Starts with the saved view itself, not the collateral ones,
/// to minimize any freeze the user might experience.
/// What follows from the save's own gestures is no buffer correction
/// ('buffer_correction_policy', 'BufferCorrectionPolicy::AfterSave').
/// .
/// ASSUMES:
/// the graph was already updated. The reverse order would be bad.
pub fn update_views_after_save (
  stream                      : &mut std::net::TcpStream,
  saved_view                  : ViewForest,
  collateral_viewforest_ids   : Vec<ViewforestId>,
  env                         : &SkgEnv,
  runtime                     : Arc<RuntimeGeneration>,
  viewforest_id_from_request_result : &Result<ViewforestId, String>,
  views_state                 : &mut ViewsState,
  skgrepo_restriction         : Option<&SkgrepoRestriction>,
  text_approved_pids        : &HashSet<ID>,
  fork_specs                  : &[ForkSpec],
  buffer_correction_policy    : BufferCorrectionPolicy,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderPassContext = RerenderPassContext::new (
    env, runtime, skgrepo_restriction,
    views_state . graph_straddle . clone (),
    buffer_correction_policy, false );
  let saved_diffView_buffer_kind : Option<DiffViewBufferKind> =
    viewforest_id_from_request_result . as_ref () . ok ()
    . and_then ( |viewforest_id| views_state . open_views . diffView_buffer_kind_of_view (viewforest_id) );
  let mut saved_view_mut : ViewForest = saved_view;
  let mut saved_viewnode_id_record : ViewnodeIdRecord =
    // The parsed buffer carries viewnode IDs; the view's record says which
    // it knows, before the refresh runs.
    viewforest_id_from_request_result . as_ref () . ok ()
    . map ( |viewforest_id| views_state . open_views . viewnode_id_record_of (viewforest_id) )
    . unwrap_or_default ();
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
  if let Some (restriction) = skgrepo_restriction {
    let mut input_candidates : Vec<ID> =
      occurrence_gnids_in_viewforest (&saved_view_mut);
    for viewforest_id in &collateral_viewforest_ids {
      if let Some (viewforest) = views_state . open_views
          . viewforest_id_to_view (viewforest_id) {
        input_candidates . extend (
          occurrence_gnids_in_viewforest (viewforest) ); }}
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &input_candidates,
      &context . runtime . graph, text_approved_pids );
    if matches! (release, TextReleaseDecision::Challenge { .. }) {
      return Ok ( SaveResponse::text_release_challenge (&release) ); }}
  let saved_buffer_corrections : Vec<BufferCorrection> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view ( // saved view first; collateral ones later
        &mut saved_view_mut,
        &mut saved_viewnode_id_record,
        &mut context,
        RerenderMode::of_buffer (saved_diffView_buffer_kind) ) } ?;
  let saved_text : String = viewforest_to_string (&saved_view_mut) ?;
  let readiness_summary : Option<BufferReadinessSummary> =
    saved_diffView_buffer_kind . and ( context . graph_straddle . as_ref () )
    . map ( |straddle| buffer_readiness_summary_for (
      env, straddle, &saved_view_mut ) );
  let mut collateral_views : Vec<RerenderedView> = Vec::new ();
  for curi in &collateral_viewforest_ids {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span!( "rerender_view (collateral)" ). entered();
    match rerender_open_view (curi, views_state, &mut context)
    { Ok (rendered) => collateral_views . push (rendered),
      Err (e) => context . errors . push (e), }}
  // Everything textual is now staged in memory. Decide before changing the
  // open-view registry or streaming the first view.
  let mut release_candidates : Vec<ID> =
    occurrence_gnids_in_viewforest (&saved_view_mut);
  for collateral in &collateral_views {
    release_candidates . extend (
      occurrence_gnids_in_viewforest (&collateral . viewforest) ); }
  if let Some (restriction) = skgrepo_restriction {
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
  let mut buffer_corrections : Vec<ViewBufferCorrections> = Vec::new ();
  if let Ok (viewforest_id) = viewforest_id_from_request_result {
    buffer_corrections . extend ( ViewBufferCorrections::of (viewforest_id, saved_buffer_corrections) );
    views_state . open_views . update_view (
      &context . runtime . graph, viewforest_id, saved_view_mut, saved_viewnode_id_record );
    for rendered in collateral_views {
      buffer_corrections . extend ( ViewBufferCorrections::of (
        &rendered . viewforest_id, rendered . buffer_corrections ));
      views_state . open_views . update_view (
        &context . runtime . graph,
        &rendered . viewforest_id, rendered . viewforest,
        rendered . viewnode_id_record );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::CollateralView,
          & format_single_view_sexp (
            &rendered . viewforest_id, &rendered . text) )); }}
  Ok ( SaveResponse::saved (
    saved_text, context . errors, context . warnings,
    readiness_summary, buffer_corrections )) }

fn occurrence_gnids_in_viewforest (
  viewforest : &ViewForest,
) -> Vec<ID> {
  viewforest . nodes ()
    .filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        Some ( occurrence . gnid . clone () ),
      _ => None,
    } )
    .collect ()
}

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
    views_state . open_views . diffView_buffer_kind_of_view (viewforest_id) );
  let in_the_view = |e : Box<dyn Error>| -> String {
    format! ( "View {}: {}", viewforest_id . repr_in_client (), e ) };
  let buffer_corrections : Vec<BufferCorrection> = rerender_view (
    &mut viewforest, &mut viewnode_id_record, context, mode )
    . map_err (in_the_view) ?;
  let text : String = viewforest_to_string (&viewforest)
    . map_err (in_the_view) ?;
  Ok ( RerenderedView {
    viewforest_id : viewforest_id . clone (),
    text,
    buffer_corrections,
    viewforest,
    viewnode_id_record, } ) }

/// The text of a fresh content view of 'root_gnids', the pids the
/// caller registers for it, and its viewforest: the roots' content,
/// their containers folders and their default folders. The de-novo
/// render ('render_initial_view') is the refresh of a stub forest of
/// the requested roots, each asking for its containers folder, which
/// is drawn with the containerward chain.
pub(crate) fn multi_root_view_via_runtime (
  runtime            : &RuntimeGeneration,
  root_gnids        : &[ID],
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result < (String, Vec<ID>, Tree<Viewnode>), Box<dyn Error> > {
  let (viewforest, buffer_content) : (ViewForest, String) =
    render_initial_view ( runtime, root_gnids, skgrepo_restriction ) ?;
  // The pids the caller registers for this view -- its occurrences,
  // via the one shared skgrepo of which-kinds-count
  // (views_state::pids_from_viewforest), the same helper update_view uses post-save.
  let pids : Vec<ID> =
    pids_from_viewforest (&viewforest)
      . into_iter () . collect ();
  Ok ((buffer_content, pids, viewforest . into_internal_tree ())) }

/// Render a fresh content view: the refresh of a stub
/// ('stub_of_fresh_content_view') in content-view mode, which draws
/// each root's content within the node budget and fulfils the stub's
/// folder requests. Returns the viewforest and its text, with every
/// viewnode given a vnid from a new record. A fresh view's buffer
/// corrections go unread ('BufferCorrectionPolicy::Unread'): there
/// was nothing the user could have made otherwise.
pub fn render_initial_view (
  runtime     : &RuntimeGeneration,
  root_gnids : &[ID],
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(ViewForest, String), Box<dyn Error>> {
  let mut viewforest : ViewForest = stub_of_fresh_content_view (
    root_gnids, &runtime . graph );
  refresh_content_viewforest (
    &mut viewforest, &mut ViewnodeIdRecord::default (),
    &Truth::of_runtime (runtime, restriction),
    &BufferCorrectionPolicy::Unread ) ?;
  let text : String = viewforest_to_string (&viewforest) ?;
  Ok (( viewforest, text )) }

/// One root per requested node: an editable occurrence of the node
/// asking for its containers folder ('(folder containers)') and to be
/// the node's editable occurrence ('editableView'), which extends its
/// content and gives each occurrence that extension draws editable its
/// default folders ('add_default_folders' in 'server/refresh/visit.rs'),
/// as a fresh view always has. A node requested twice is editable at
/// its first root only. A gnid no node answers to is an unknown, which
/// the refresh entombstones.
pub(crate) fn stub_of_fresh_content_view (
  root_gnids : &[ID],
  graph       : &InRustGraph,
) -> ViewForest {
  let mut viewforest : ViewForest = ViewForest::new ();
  let mut editable : HashSet<ID> = HashSet::new ();
  for gnid in root_gnids {
    let root : Viewnode =
      match graph . pid_of (gnid) . and_then ( |pid| graph . get (&pid) ) {
        None => mk_unknown_viewnode ( gnid . clone () ),
        Some (node) => {
          let mut root : Viewnode = mk_editable_viewnode (
            node . pid . clone (), node . home_skgrepo . clone (),
            node . title . clone (), node . body . clone () );
          if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
            &mut root . kind
          { occurrence . view_requests . insert (
              ViewRequest::Folder (FolderRelation::Containers) );
            if editable . insert ( node . pid . clone () ) {
              occurrence . view_requests . insert (ViewRequest::Editable);
            } else { occurrence . write_protect (); }}
          root }, };
    viewforest . append_root (root); }
  viewforest }

/// The situation a rerender refreshes a view in, from the view
/// itself: a content view, or a buffer of the diff view of this kind.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum RerenderMode {
  ContentView,
  DiffView (DiffViewBufferKind),
}

impl RerenderMode {
  /// The mode of a view whose buffer kind in the diff view, if it is
  /// one, is 'diffView_buffer'.
  pub fn of_buffer (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderMode {
    match diffView_buffer {
      None => RerenderMode::ContentView,
      Some (buffer_kind) => RerenderMode::DiffView (buffer_kind), } }
}

/// Bring an open view up to date with the graph, returning its buffer
/// corrections; the caller renders it. It is refreshed
/// ('refresh_viewforest'), from what it holds and the graph as they
/// are now: a content view in content-view mode; a buffer of the diff
/// view in diff-view mode, over the graph straddle, brought up to date
/// in 'context' ('rerender_diffView_buffer', which says what the
/// "integrate unshown changes" request adds).
pub fn rerender_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord, // the view's, which assigns the viewnode IDs its new viewnodes get
  context            : &mut RerenderPassContext<'_>,
  mode               : RerenderMode,
) -> Result<Vec<BufferCorrection>, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "refresh_viewforest" ). entered();
  let result : Vec<BufferCorrection> = match mode {
    RerenderMode::ContentView => refresh_content_view (
      viewforest, viewnode_id_record, context ) ?,
    RerenderMode::DiffView (buffer_kind) => rerender_diffView_buffer (
      context, buffer_kind, viewforest, viewnode_id_record ) ?, };
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (result) }

/// The refresh of an open content view; see 'rerender_view'.
fn refresh_content_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  context            : &mut RerenderPassContext<'_>,
) -> Result<Vec<BufferCorrection>, Box<dyn Error>> {
  refresh_content_viewforest (
    viewforest, viewnode_id_record,
    &Truth::of_runtime (&context . runtime, context . skgrepo_restriction),
    &context . buffer_correction_policy ) }
