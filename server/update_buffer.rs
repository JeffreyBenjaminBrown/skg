//! Bringing views up to date with the graph. A content view is
//! refreshed ('refresh_viewforest', 'server/refresh/'); a buffer of
//! the diff view runs view completion ('complete_viewforest' in
//! 'update_buffer/complete.rs') and then its tail
//! ('finish_viewforest': membership marking,
//! stats, the skgrepo set, the text):
//! - 'render_initial_view' renders a new content view, the refresh of
//!   a stub of its roots, each asking for its containers folder;
//! - 'complete_viewforest_with_graph_straddle'
//!   ('server/diff_view/completion.rs') renders a buffer of the diff
//!   view from its stub, over the graph straddle;
//! - 'rerender_view' brings a view that already exists up to date
//!   (after a save, collaterally, after a commit, or at a skgrepo-set
//!   switch), from what it holds;
//! - 'update_views_after_save' rerenders the saved view and then the
//!   collateral ones, streaming each to the client.

pub mod ancestry;
pub mod skgrepo_switch;
pub mod warnings;
pub mod complete;
pub mod completion_mode;
pub mod reconcile;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;
pub mod warrant;

pub use graphnodestats::{
  set_graphnodestats_in_viewforest,
  set_graphnodestats_in_viewforest_with_skgrepo_set};
pub use viewnodestats::set_viewnodestats_in_viewforest;

use complete::{complete_viewforest, CompletionContext};
use completion_mode::CompletionMode;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::refresh::{Refreshed, Truth, refresh_viewforest};
use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::diff_view::completion::{
  DiffViewRerender, finish_diffView_rerender, prepare_diffView_rerender};
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::org_to_text::viewforest_to_string;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::SaveResponse;
use crate::serve::handlers::text_release::{
  TextReleaseDecision, challenge_response, decide,
};
use crate::serve::protocol::TcpToClient;
use crate::serve::rerender_stream::write_graph_straddle_back;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::skgrepo_sets::{SkgrepoRestriction, entombstone_hidden_occurrences};
use crate::to_org::util::EditableMap;
use crate::types::views_state::{DiffViewBufferKind, ViewforestId, ViewnodeIdRecord};
use crate::types::misc::{ID, SkgrepoName, SkgConfig};
use crate::types::save::ForkSpec;
use crate::types::tree::forest::ViewForest;
use crate::to_org::util::{mark_view_roots_as_members, validate_membership_claims, mark_orphans_under_dead_parents_false};
use crate::update_buffer::warnings::{CompletionWarning, render_completion_warnings};
use crate::types::viewnode::{
  Editability, Viewnode, ViewnodeKind, mk_editable_viewnode, mk_unknown_viewnode};
use crate::types::viewnode::{FolderRelation, Vognode, ViewRequest};

use ego_tree::{Tree, NodeId};
use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;
use std::time::Instant;

pub struct RerenderAfterSaveContext<'a> {
  pub env           : &'a SkgEnv,
  pub runtime       : Arc<RuntimeGeneration>,
  pub graph_snap    : Arc<InRustGraph>,
  pub errors        : Vec<String>,
  pub warnings      : Vec<String>,
  pub skgrepo_restriction            : Option<&'a SkgrepoRestriction>,
  /// The graph straddle a buffer of the diff view is completed
  /// over, brought up to date by 'current_graph_straddle_in_slot' when
  /// the first such buffer is rerendered. A caller that holds the
  /// connection's straddle sets it, and keeps what it becomes.
  pub graph_straddle                 : Option<Arc<GraphStraddle>>,
}

impl<'a> RerenderAfterSaveContext<'a> {
  fn with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext {
      env,
      graph_snap : runtime . graph . clone (),
      runtime,
      errors : Vec::new (),
      warnings : Vec::new (),
      skgrepo_restriction,
      graph_straddle : None,
    }}

  pub fn without_save (
    env                : &'a SkgEnv,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::with_runtime (
      env, env . runtime_snapshot (), skgrepo_restriction ) }

  pub fn without_save_with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::with_runtime (
      env, runtime, skgrepo_restriction ) }
}

struct RenderedCollateralView {
  viewforest_id  : ViewforestId,
  text           : String,
  viewforest     : ViewForest,
  viewnode_id_record : ViewnodeIdRecord,
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
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderAfterSaveContext =
    // Snapshot the in-Rust graph once for this save's rerender pass.
    // Used both by rewriteInPlace_viewnodes_whose_id_is_newly_extra (swap acquiree pids
    // to acquirer pids before rerender) and by content_goal_list
    // resolution during reconcile (neighbors' on-disk contains still
    // point at the acquiree id).
    RerenderAfterSaveContext::with_runtime (
      env, runtime, skgrepo_restriction );
  context . graph_straddle = views_state . graph_straddle . clone ();
  let saved_diffView_buffer_kind : Option<DiffViewBufferKind> =
    viewforest_id_from_request_result . as_ref () . ok ()
    . and_then ( |viewforest_id| views_state . open_views . diffView_buffer_kind_of_view (viewforest_id) );
  let mut saved_view_mut : ViewForest = saved_view;
  let mut saved_viewnode_id_record : ViewnodeIdRecord =
    // The parsed buffer carries viewnode IDs; the view's record says which
    // it knows, before any reconciler runs.
    viewforest_id_from_request_result . as_ref () . ok ()
    . map ( |viewforest_id| views_state . open_views . viewnode_id_record_of (viewforest_id) )
    . unwrap_or_default ();
  saved_viewnode_id_record . drop_unknown_viewnode_ids_in_parsed_viewforest (&mut saved_view_mut);
  replace_saved_view_fork_roots (&mut saved_view_mut, fork_specs) ?;
  // The skgsave-commit has happened, so every `(editRequest ...)` in the
  // submitted view is now consumed input. Clear all request carriers at this
  // one boundary before completion can echo any of them into the response.
  // `viewRequests` remain: those are fulfilled by completion below.
  saved_view_mut . consume_edit_requests_after_save ();
  if saved_diffView_buffer_kind . is_some () {
    // A content view's refresh re-kinds such an occurrence itself.
    let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "rewriteInPlace_viewnodes_whose_id_is_newly_extra" ). entered();
    rewriteInPlace_viewnodes_whose_skgid_is_newly_extra (
      &mut saved_view_mut, &context . graph_snap ) ? }
  // Gate the forests' existing unrestricted nodes before rendering, so even a
  // rendering error cannot echo protected title/body text. A second decision
  // below covers nodes introduced by completion.
  if let Some (restriction) = skgrepo_restriction {
    let mut input_candidates : Vec<ID> =
      unrestricted_skgids_in_viewforest (&saved_view_mut);
    for viewforest_id in &collateral_viewforest_ids {
      if let Some (viewforest) = views_state . open_views
          . viewforest_id_to_view (viewforest_id) {
        input_candidates . extend (
          unrestricted_skgids_in_viewforest (viewforest) ); }}
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &input_candidates,
      &context . graph_snap, text_approved_pids );
    if matches! (release, TextReleaseDecision::Challenge { .. }) {
      return Ok ( SaveResponse {
        saved_view          : String::new (),
        errors              : Vec::new (),
        warnings            : Vec::new (),
        save_point_position : None,
        fork_confirmation   : None,
        hoist_confirmation  : None,
        strandeds_confirmation : None,
        text_release_confirmation : challenge_response (&release),
        readiness_summary   : None,
      } ); }}
  let mut repair_warnings : Vec<CompletionWarning> = Vec::new ();
  let saved_text : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view ( // saved view first; collateral ones later
        &mut saved_view_mut,
        &mut saved_viewnode_id_record,
        &mut context,
        Some (&mut repair_warnings),
        RerenderOptions::ordinary (saved_diffView_buffer_kind) ) } ?;
  let readiness_summary : Option<BufferReadinessSummary> =
    saved_diffView_buffer_kind . and ( context . graph_straddle . as_ref () )
    . map ( |straddle| buffer_readiness_summary_for (
      env, straddle, &saved_view_mut ) );
  context . warnings . extend (
    // Repairs the completion pass made to write-protected PartnerFolders in
    // the saved view, batched per (folder, recorder).
    render_completion_warnings (&repair_warnings) );
  let mut collateral_views : Vec<RenderedCollateralView> = Vec::new ();
  for curi in &collateral_viewforest_ids {
    match rerender_collateral_view (
      curi . clone (), views_state, &mut context )
    { Ok (rendered) => collateral_views . push (rendered),
      Err (e) => context . errors . push (e), }}
  // Everything textual is now staged in memory. Decide before changing the
  // open-view registry or streaming the first view.
  let mut release_candidates : Vec<ID> =
    unrestricted_skgids_in_viewforest (&saved_view_mut);
  for collateral in &collateral_views {
    release_candidates . extend (
      unrestricted_skgids_in_viewforest (&collateral . viewforest) ); }
  if let Some (restriction) = skgrepo_restriction {
    let release : TextReleaseDecision = decide (
      "save-rerender", restriction, &release_candidates,
      &context . graph_snap, text_approved_pids );
    match release {
      decision @ TextReleaseDecision::Challenge { .. } => {
        return Ok ( SaveResponse {
          saved_view          : String::new (),
          errors              : Vec::new (),
          warnings            : Vec::new (),
          save_point_position : None,
          fork_confirmation   : None,
          hoist_confirmation  : None,
          strandeds_confirmation : None,
          text_release_confirmation : challenge_response (&decision),
          readiness_summary   : None,
        } ); }
      TextReleaseDecision::AllowWithWarning { warning } =>
        context . warnings . push (warning),
      TextReleaseDecision::Allow => {}, }}
  write_graph_straddle_back (views_state, context . graph_straddle . take ());
  if let Ok (viewforest_id) = viewforest_id_from_request_result {
    views_state . open_views . update_view (
      &context . graph_snap, viewforest_id, saved_view_mut, saved_viewnode_id_record );
    for rendered in collateral_views {
      views_state . open_views . update_view (
        &context . graph_snap,
        &rendered . viewforest_id, rendered . viewforest,
        rendered . viewnode_id_record );
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::CollateralView,
          & format_single_view_sexp (
            &rendered . viewforest_id, &rendered . text) )); }}
  Ok ( SaveResponse {
    saved_view          : saved_text,
    errors              : context . errors,
    warnings            : context . warnings,
    save_point_position : None,
    fork_confirmation   : None,
    hoist_confirmation  : None,
    strandeds_confirmation : None,
    text_release_confirmation : None,
    readiness_summary, } ) }

fn unrestricted_skgids_in_viewforest (
  viewforest : &ViewForest,
) -> Vec<ID> {
  viewforest . nodes ()
    .filter_map ( |node| match &node . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
        Some ( restriction . skgid . clone () ),
      _ => None,
    } )
    .collect ()
}

/// A root is intentionally rendered raw when the user ordinarily opens an
/// overridden node.  The buffer which *created* a fork is the exception: once
/// the save's skgsave-commit happens, continuing to show its foreign root would make the next
/// edit attempt another fork.  Replace only roots named by this save's exact
/// ForkSpecs; content occurrences are handled by content reconciliation.
fn replace_saved_view_fork_roots (
  viewforest : &mut ViewForest,
  fork_specs : &[ForkSpec],
) -> Result<(), Box<dyn Error>> {
  if fork_specs . is_empty () { return Ok (()); }
  let root_skgids : Vec<NodeId> = viewforest . root_skgids ();
  for root_skgid in root_skgids {
    let original : Option<ID> =
      viewforest . get (root_skgid) . and_then (|root| match
        &root . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =>
            Some (restriction . skgid . clone ()),
          _ => None, });
    let Some (original) = original else { continue; };
    let Some (spec) = fork_specs . iter ()
      . find (|spec| spec . original_skgid == original)
    else { continue; };
    let clone = & spec . clone . 0;
    let mut root = viewforest . get_mut (root_skgid)
      . ok_or ("replace_saved_view_fork_roots: root not found") ?;
    if let ViewnodeKind::Vognode (Vognode::Unrestricted (restriction)) =
      &mut root . value () . kind
    { restriction . skgid = clone . pid . clone ();
      restriction . home_skgrepo = clone . home_skgrepo . clone ();
      restriction . title = clone . title . clone ();
      if let Editability::Editable { body, .. } = &mut restriction . editability
      { *body = clone . body . clone (); }
      restriction . view_requests . remove (&ViewRequest::Fork);
      restriction . overridesHere = Some (original); }}
  Ok (())
}

fn rerender_collateral_view (
  viewforest_id : ViewforestId,
  views_state : &ViewsState,
  context     : &mut RerenderAfterSaveContext<'_>,
) -> Result<RenderedCollateralView, String> {
  let mut viewforest : ViewForest = match
    views_state . open_views . viewforest_id_to_view (&viewforest_id) {
      Some (f) => f . clone (),
      None => {
        return Err ( format! (
          "Collateral view {}: no viewforest found",
          viewforest_id . repr_in_client () )); } };
  let mut viewnode_id_record : ViewnodeIdRecord =
    views_state . open_views . viewnode_id_record_of (&viewforest_id);
  let diffView_buffer_kind : Option<DiffViewBufferKind> =
    views_state . open_views . diffView_buffer_kind_of_view (&viewforest_id);
  if diffView_buffer_kind . is_some () // a content view's refresh re-kinds such an occurrence itself
  && let Err (e) = rewriteInPlace_viewnodes_whose_skgid_is_newly_extra (
    &mut viewforest, &context . graph_snap )
  { return Err ( format! (
      "Collateral view {}: preprocessing failed: {}",
      viewforest_id . repr_in_client (), e )); }
  let text : String =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span!( "rerender_view (collateral)" ). entered();
      rerender_view (
        &mut viewforest,
        &mut viewnode_id_record,
        context,
        None, // Collateral views repair silently.
        RerenderOptions::ordinary (diffView_buffer_kind)
      ) . map_err (
        |e| format!( "Collateral view {}: {}",
                      viewforest_id . repr_in_client (), e)) ? };
  Ok (RenderedCollateralView {
    viewforest_id,
    text,
    viewforest,
    viewnode_id_record,
  }) }

/// Render a fresh content view: the refresh of a stub
/// ('stub_of_fresh_content_view') in content-view mode, which draws
/// each root's content within the node budget and fulfils the stub's
/// folder requests. Returns the viewforest and its text, with every
/// viewnode given a vnid from a new record. A fresh view reports no
/// correction: there was nothing the user could have made otherwise.
pub fn render_initial_view (
  runtime     : &RuntimeGeneration,
  root_skgids : &[ID],
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(ViewForest, String), Box<dyn Error>> {
  let mut viewforest : ViewForest = stub_of_fresh_content_view (
    root_skgids, &runtime . graph );
  let truth : Truth = Truth { graph       : &runtime . graph,
                              config      : &runtime . config,
                              restriction, };
  let refreshed : Refreshed = refresh_viewforest (
    &mut viewforest, &mut ViewnodeIdRecord::default (), &truth,
    CompletionMode::ContentView, runtime . config . initial_node_limit,
    None ) ?;
  Ok (( viewforest, refreshed . text )) }

/// One root per requested node: an editable occurrence of the node
/// asking for its containers folder ('(folder containers)') and to be
/// the node's editable occurrence ('editableView'), which extends its
/// content and gives each occurrence that extension draws editable its
/// default folders ('add_default_folders' in 'server/refresh/ladder.rs'),
/// as a fresh view always has. A node requested twice is editable at
/// its first root only. A gnid no node answers to is an unknown, which
/// the refresh entombstones.
fn stub_of_fresh_content_view (
  root_skgids : &[ID],
  graph       : &InRustGraph,
) -> ViewForest {
  let mut viewforest : ViewForest = ViewForest::new ();
  let mut editable : HashSet<ID> = HashSet::new ();
  for gnid in root_skgids {
    let root : Viewnode =
      match graph . pid_of (gnid) . and_then ( |pid| graph . get (&pid) ) {
        None => mk_unknown_viewnode ( gnid . clone () ),
        Some (node) => {
          let mut root : Viewnode = mk_editable_viewnode (
            node . pid . clone (), node . home_skgrepo . clone (),
            node . title . clone (), node . body . clone () );
          if let ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =
            &mut root . kind
          { occurrence . view_requests . insert (
              ViewRequest::Folder (FolderRelation::Containers) );
            if editable . insert ( node . pid . clone () ) {
              occurrence . view_requests . insert (ViewRequest::Editable);
            } else { occurrence . write_protect (); }}
          root }, };
    viewforest . append_root (root); }
  viewforest }

/// How 'rerender_view' treats a view, beyond what its context says.
/// Each case of rerender has its own constructor, which names every
/// field.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RerenderOptions {
  /// Some for a buffer of the diff view.
  pub diffView_buffer                       : Option<DiffViewBufferKind>,
  /// True only for the "integrate unshown changes" request; matters
  /// only in a buffer of the diff view.
  pub integrate_unshown_changes             : bool,
}

impl RerenderOptions {
  /// Every rerender of an open view but the one below: the saved view
  /// and the collateral views of a save, "rerender all views", a
  /// skgrepo-set switch, the cleanup after deleting references to an
  /// absent node, and the diff view's buffers after a git-commit.
  pub fn ordinary (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      diffView_buffer,
      integrate_unshown_changes             : false, } }

  /// The buffer of the diff view named by an "integrate unshown
  /// changes" request.
  pub fn for_integrating_unshown_changes (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      diffView_buffer,
      integrate_unshown_changes             : true, } }
}

/// Bring an open view up to date with the graph and render it.
/// .
/// A content view ('options . diffView_buffer' None) is refreshed
/// ('refresh_viewforest' in content-view mode), from what it holds and
/// the graph as they are now. When 'warning_sink' is Some (the view
/// the user just saved), it receives the refresh's corrections.
/// .
/// A buffer of the diff view ('options . diffView_buffer' Some) is
/// reconciled over the graph straddle, brought up to date in
/// 'context': view
/// completion reconciles each editable node's change facts, change
/// folders, markers and former members, its ID folder among them,
/// with a shallow model subtree from the straddle
/// ('process_unrestrictedVognode_changes'), keeping every viewnode
/// that stays and its fold state, and its readiness facts are then
/// read from the store. Completion extends what it adds anew by the
/// buffer's sets recomputed after the save, and nothing editable is
/// write-protected again. Only when 'options . integrate_unshown_changes'
/// is true (the "integrate unshown changes" request) does it make occurrences
/// editable or append roots: before completion each node of the
/// recomputed U is made editable at its first occurrence if it has no
/// editable one, and after completion the content origins of the
/// recomputed D that the buffer does not show are appended as roots
/// and completed. Its standing roots stay. The diff view does its
/// part before completion in 'prepare_diffView_rerender' and after it
/// in 'finish_diffView_rerender'.
pub fn rerender_view (
  viewforest    : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord, // the view's, which assigns the viewnode IDs its new viewnodes get
  context       : &mut RerenderAfterSaveContext<'_>,
  warning_sink  : Option<&mut Vec<CompletionWarning>>, // Some only for the view the user just saved.
  options       : RerenderOptions,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  let result : String = match options . diffView_buffer {
    None => refresh_content_view (
      viewforest, viewnode_id_record, context, warning_sink ) ?,
    Some (_) => rerender_diffView_buffer (
      viewforest, viewnode_id_record, context, warning_sink, options ) ?, };
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (result) }

/// The refresh of an open content view; see 'rerender_view'.
fn refresh_content_view (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  context            : &mut RerenderAfterSaveContext<'_>,
  warning_sink       : Option<&mut Vec<CompletionWarning>>,
) -> Result<String, Box<dyn Error>> {
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "refresh_viewforest" ). entered();
  let truth : Truth = Truth { graph       : &context . runtime . graph,
                              config      : &context . runtime . config,
                              restriction : context . skgrepo_restriction, };
  let refreshed : Refreshed = refresh_viewforest (
    viewforest, viewnode_id_record, &truth, CompletionMode::ContentView,
    context . runtime . config . initial_node_limit, None ) ?;
  if let Some (sink) = warning_sink {
    sink . extend ( refreshed . corrections . into_iter ()
                    . map (CompletionWarning::Correction) ); }
  Ok (refreshed . text) }

/// The rerender of a buffer of the diff view; see 'rerender_view'.
fn rerender_diffView_buffer (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  context            : &mut RerenderAfterSaveContext<'_>,
  warning_sink       : Option<&mut Vec<CompletionWarning>>,
  options            : RerenderOptions,
) -> Result<String, Box<dyn Error>> {
  claims_as_the_old_walk_reads_them (viewforest);
  let diffView_rerender : DiffViewRerender =
    prepare_diffView_rerender (
      &mut context . graph_straddle, &context . runtime,
      options . diffView_buffer, options . integrate_unshown_changes,
      viewforest ) ?
    . ok_or ("rerender_diffView_buffer: not a buffer of the diff view") ?;
  { let nodes_before_completion : HashSet<NodeId> =
      viewforest . nodes () . map ( |node| node . id () ) . collect ();
    let editable_before_completion : HashSet<NodeId> =
      viewforest . nodes ()
      . filter ( |node| node . value () . editable_skgid () . is_some () )
      . map ( |node| node . id () ) . collect ();
    let mut editable_map       : EditableMap = EditableMap::new ();
    let mut completion_context : CompletionContext = CompletionContext::for_rerender (
      &context . runtime,
      &context . graph_snap,
      &mut editable_map,
      &mut context . errors,
      warning_sink,
      diffView_rerender . completion_mode (),
      &nodes_before_completion,
      &editable_before_completion );
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "complete_viewforest" ). entered();
      complete_viewforest (
        viewforest, &mut completion_context ) ? };
    finish_diffView_rerender (
      Some (&diffView_rerender), viewforest, &mut completion_context,
      &context . runtime, context . env ) ?; }
  let _span : tracing::span::EnteredSpan = tracing::info_span!(
    "finish_viewforest" ). entered();
  finish_viewforest (
    viewforest,
    viewnode_id_record,
    &context . runtime . graph,
    &context . runtime . config,
    context . skgrepo_restriction ) }

/// The diff view's completion, until it too is a refresh, reads the
/// claims of the children of a write-protected occurrence, a ghost, an
/// unknown or a tombstone, where none claims: as non-members. The
/// printer writes no 'noClaim' there, so a parsed buffer reads them as
/// claiming; this marks each such vognode as claiming nothing.
fn claims_as_the_old_walk_reads_them (
  viewforest : &mut ViewForest,
) {
  let claiming_nothing : Vec<NodeId> =
    viewforest . nodes ()
    . filter ( |node| matches! ( node . value () . kind,
                                 ViewnodeKind::Vognode (_) ))
    . filter ( |node| node . parent () . is_some_and ( |parent|
        match &parent . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
            occurrence . is_writeProtected (),
          ViewnodeKind::Vognode (_) | ViewnodeKind::Tombstone => true,
          _ => false, } ))
    . map ( |node| node . id () )
    . collect ();
  for treeid in claiming_nothing {
    match &mut viewforest . get_mut (treeid) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
        occurrence . claimsMembership = false,
      ViewnodeKind::Vognode (Vognode::Unknown (unknown)) =>
        unknown . claimsMembership = false,
      _ => {}, }}}

/// The post-completion tail of the diff view's renderings: its first
/// rendering ('diffView_buffer_viewforest') and its rerender
/// ('rerender_diffView_buffer', above). Given a viewforest whose UnrestrictedVognode
/// content has already been completed, it:
///   - entombstones the occurrences of nodes the skgrepo restriction
///     hides, so the orphan marks below see their children,
///   - marks view-root and orphan membership,
///   - validates membership against the captured graph,
///   - computes graph- then viewnode stats,
///   - gives a viewnode ID to each viewnode that lacks one, from
///     'viewnode_id_record', which it brings up to date with the forest
///     (the end-of-completion pass, after the prune and after
///     everything this tail adds), and renders to a buffer string.
/// Step order is immaterial between the membership marks and graphnodestats:
/// membership is a view property and graphnodestats reads only the in-Rust graph,
/// never membership, so the final state is identical either way.
pub fn finish_viewforest (
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
) -> Result<String, Box<dyn Error>> {
  if let Some (restriction) = skgrepo_restriction {
    entombstone_hidden_occurrences ( viewforest, graph, restriction ); }
  mark_view_roots_as_members ( viewforest );
  // §A (Jeff's invariant): an Unrestricted survivor left under a non-container parent
  // (a phantom / Deleted / Tombstone) is a non-dead generalized orphan and
  // must become a non-member.
  mark_orphans_under_dead_parents_false ( viewforest );
  // Correct any membership claims whose claimed relation to the parent doesn't
  // hold in the captured graph.
  validate_membership_claims ( viewforest, graph );
  let ( container_to_contents, content_to_containers ) =
    match skgrepo_restriction {
      Some (restriction) =>
        set_graphnodestats_in_viewforest_with_skgrepo_set (
          viewforest, graph, config, restriction ),
      None =>
        set_graphnodestats_in_viewforest (
          viewforest, graph, config ),
    } ?;
  set_viewnodestats_in_viewforest (
    viewforest, graph, &container_to_contents, &content_to_containers, config,
    skgrepo_restriction );
  viewnode_id_record . assign_viewnode_ids (viewforest);
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "viewforest_to_string" ). entered();
    viewforest_to_string ( viewforest, config ) } }

/// When the server first receives the buffer, it replaces
/// each Viewnode's ID with a PID (`replace_gnid_with_pid`).
/// Later it updates the graph, which can involve merging nodes.
/// After a nodeMerge, the acquiree is gone, and what was the Viewnode
/// onto it now an extra_id of the acquirer, not a PID.
/// This is intended to replaces such a
/// viewnode to carry the primary's pid, skgrepo, title, and body.
///
/// The modified viewnode's subtree is left attached.
/// 'reconcile_content_children' will later update it
/// to include the rest of the subscriber's content.
///
/// PITFALL: The function does not actually remember the nodeMerge history.
/// It merely identifies each UnrestrictedVognode in 'viewforest'
/// whose pid is an extra_id of some distinct node in the snapshot.
fn rewriteInPlace_viewnodes_whose_skgid_is_newly_extra (
  viewforest : &mut Tree<Viewnode>,
  graph_snap : &Arc<InRustGraph>,
) -> Result<(), Box<dyn Error>> {
  let treeids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for treeid in treeids {
    let swap : Option<(ID, SkgrepoName, String, Option<String>)> = {
      let n_ref = viewforest . get (treeid) . ok_or (
        "rewriteInPlace_viewnodes_whose_id_is_newly_extra: node not found") ?;
      match &n_ref . value () . kind {
        // Only Unrestricted nodes are rewritten below (a phantom is never an
        // editable occurrence), so only they need a swap computed.
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => { match graph_snap . pid_of (&t . skgid)
               { Some (primary) if primary != t . skgid => {
                     graph_snap . get (&primary) . map ( |r| (
                       primary . clone (),
                       r . home_skgrepo . clone (),
                       r . title . clone (),
                       r . body . clone () )) },
                 _ => None } },
        _ => None } };
    if let Some ((new_pid, new_skgrepo, new_title, new_body)) = swap
    { let mut n_mut = viewforest . get_mut (treeid)
        . ok_or ("rewriteInPlace_viewnodes_whose_id_is_newly_extra: node_mut failed") ?;
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t))
      = &mut n_mut . value () . kind
      { t . skgid = new_pid;
        t . home_skgrepo = new_skgrepo;
        t . title = new_title;
        if let Editability::Editable { body, .. }
        = &mut t . editability
        { *body = new_body; }} }}
  Ok (( )) }

