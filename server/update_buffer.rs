//! Bringing views up to date with the graph. Every rendering of a
//! view runs view completion ('complete_viewforest' in
//! 'update_buffer/complete.rs') and then the shared tail
//! ('finish_viewforest': root role trees, affectsParent marking,
//! stats, the skgrepo set, the text):
//! - 'render_initial_view' draws a new content view from a stub of its
//!   roots;
//! - 'complete_viewforest_with_graph_straddle'
//!   ('server/diff_view/completion.rs') draws a buffer of the diff
//!   view the same way, over the graph straddle;
//! - 'rerender_view' re-completes a view that already exists (after a
//!   save, collaterally, after a commit, or at a skgrepo-set switch),
//!   reconciling what it holds instead of redrawing it;
//! - 'update_views_after_save' rerenders the saved view and then the
//!   collateral ones, streaming each to the client.

pub mod ancestry;
pub mod skgrepo_switch;
pub mod warnings;
pub mod complete;
pub mod reconcile;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use graphnodestats::{
  set_graphnodestats_in_viewforest,
  set_graphnodestats_in_viewforest_with_skgrepo_set};
pub use viewnodestats::set_viewnodestats_in_viewforest;

use complete::{complete_viewforest, CompletionContext};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::diff_view::change_leaves::BufferReadinessSummary;
use crate::diff_view::completion::{
  DiffViewRerender, finish_diffView_rerender, prepare_diffView_rerender};
use crate::diff_view::readiness_requests::buffer_readiness_summary_for;
use crate::graph_straddle::types::GraphStraddle;
use crate::diff_view::pcsvf::DiffViewSource;
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
use crate::skgrepo_sets::{SkgrepoRestriction, apply_skgrepo_set_to_viewforest};
use crate::to_org::expand::role_tree::{
  RoleTreePartners, attach_full_containerward_role_trees_at_treeids_with_skgrepo_set};
use crate::to_org::util::EditableMap;
use crate::types::views_state::{DiffViewBufferKind, ViewId};
use crate::types::misc::{ID, SkgrepoName, SkgConfig};
use crate::types::save::{NodeInstruction, ForkSpec};
use crate::types::tree::generic::do_everywhere_in_tree_dfs_prunable;
use crate::types::tree::forest::ViewForest;
use crate::to_org::util::{mark_view_roots_parent_na, validate_affectsParent_relationships_in_straddle, mark_orphans_under_dead_parents_false};
use crate::update_buffer::warnings::{CompletionWarning, render_completion_warnings};
use crate::types::viewnode::{Editability, Viewnode, ViewnodeKind};
use crate::types::viewnode::{Vognode, PropertyFolder, ViewRequest};
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;
use std::time::Instant;

pub struct RerenderAfterSaveContext<'a> {
  pub env           : &'a SkgEnv,
  pub runtime       : Arc<RuntimeGeneration>,
  pub graph_snap    : Arc<InRustGraph>,
  pub errors        : Vec<String>,
  pub warnings      : Vec<String>,
  /// Pids deleted by this save; not necessarily a subset of git deletes.
  pub deleted_by_this_save_pids      : HashSet<ID>,
  /// Raw extra IDs that belonged to each pid deleted by this save, captured
  /// before graph mutation.  A still-stored relationship can name one of
  /// these IDs after the pid itself has disappeared.
  pub deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>>,
  pub skgrepo_restriction            : Option<&'a SkgrepoRestriction>,
  /// The graph straddle a buffer of the diff view is completed
  /// over, brought up to date by 'current_graph_straddle_in_slot' when
  /// the first such buffer is rerendered. A caller that holds the
  /// connection's straddle sets it, and keeps what it becomes.
  pub graph_straddle                 : Option<Arc<GraphStraddle>>,
}

impl<'a> RerenderAfterSaveContext<'a> {
  fn for_save (
    env               : &'a SkgEnv,
    node_instructions      : &[NodeInstruction],
    deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    let runtime = env . runtime_snapshot ();
    Self::for_save_with_runtime (
      env, runtime, node_instructions,
      deleted_by_this_save_extra_ids, skgrepo_restriction)
  }

  fn for_save_with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    node_instructions : &[NodeInstruction],
    deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    let deleted_by_this_save_pids : HashSet<ID> =
      // PITFALL: Looks dangerous but isn't: Each nodeMerge includes an acquiree deletion. This would make Viewnodes with that ID invalid (since Viewnodes by this point should have PIDs). It doesn't, though, because rewriteInPlace_viewnodes_whose_id_is_newly_extra will rewrite those Viewnodes to instead use the acquirer's PID (i.e. to depict the acquirer now, instead of the acquiree as before) before using this set.
      node_instructions . iter()
      . filter_map( |instr| match instr {
        NodeInstruction::Delete (d) => Some( d . skgid . clone() ),
        _ => None })
      . collect();
    RerenderAfterSaveContext {
      env,
      graph_snap : runtime . graph . clone (),
      runtime,
      errors : Vec::new (),
      warnings : Vec::new (),
      deleted_by_this_save_pids,
      deleted_by_this_save_extra_ids,
      skgrepo_restriction,
      graph_straddle : None,
    }}

  pub fn without_save (
    env                : &'a SkgEnv,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::for_save (
      env, &[], HashMap::new (), skgrepo_restriction ) }

  pub fn without_save_with_runtime (
    env : &'a SkgEnv,
    runtime : Arc<RuntimeGeneration>,
    skgrepo_restriction : Option<&'a SkgrepoRestriction>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::for_save_with_runtime (
      env, runtime, &[], HashMap::new (), skgrepo_restriction ) }
}

struct RenderedCollateralView {
  view_id    : ViewId,
  text       : String,
  viewforest : ViewForest,
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
  node_instructions           : Vec<NodeInstruction>,
  collateral_view_ids         : Vec<ViewId>,
  env                         : &SkgEnv,
  runtime                     : Arc<RuntimeGeneration>,
  viewid_from_request_result  : &Result<ViewId, String>,
  views_state                 : &mut ViewsState,
  skgrepo_restriction         : Option<&SkgrepoRestriction>,
  deleted_by_this_save_extra_ids : HashMap<ID, HashSet<ID>>,
  text_approved_pids        : &HashSet<ID>,
  fork_specs                  : &[ForkSpec],
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderAfterSaveContext =
    // Snapshot the in-Rust graph once for this save's rerender pass.
    // Used both by rewriteInPlace_viewnodes_whose_id_is_newly_extra (swap acquiree pids
    // to acquirer pids before rerender) and by content_goal_list
    // resolution during reconcile (neighbors' on-disk contains still
    // point at the acquiree id).
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "RerenderAfterSaveContext::for_save" ). entered();
      RerenderAfterSaveContext::for_save_with_runtime (
        env, runtime, &node_instructions,
        deleted_by_this_save_extra_ids, skgrepo_restriction ) };
  context . graph_straddle = views_state . graph_straddle . clone ();
  let saved_diffView_buffer_kind : Option<DiffViewBufferKind> =
    viewid_from_request_result . as_ref () . ok ()
    . and_then ( |view_id| views_state . open_views . diffView_buffer_kind_of_view (view_id) );
  let mut saved_view_mut : ViewForest = saved_view;
  replace_saved_view_fork_roots (&mut saved_view_mut, fork_specs) ?;
  // The skgsave-commit has happened, so every `(editRequest ...)` in the
  // submitted view is now consumed input. Clear all request carriers at this
  // one boundary before completion can echo any of them into the response.
  // `viewRequests` remain: those are fulfilled by completion below.
  saved_view_mut . consume_edit_requests_after_save ();
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "rewriteInPlace_viewnodes_whose_id_is_newly_extra" ). entered();
    rewriteInPlace_viewnodes_whose_skgid_is_newly_extra (
      &mut saved_view_mut, &context . graph_snap ) ? };
  // Gate the forests' existing unrestricted nodes before rendering, so even a
  // rendering error cannot echo protected title/body text. A second decision
  // below covers nodes introduced by completion/expansion.
  if let Some (restriction) = skgrepo_restriction {
    let mut input_candidates : Vec<ID> =
      unrestricted_skgids_in_viewforest (&saved_view_mut);
    for view_id in &collateral_view_ids {
      if let Some (viewforest) = views_state . open_views
          . viewid_to_view (view_id) {
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
        &mut context,
        Some (&mut repair_warnings),
        RerenderOptions::for_saved_view (saved_diffView_buffer_kind) ) } ?;
  let readiness_summary : Option<BufferReadinessSummary> =
    saved_diffView_buffer_kind . and ( context . graph_straddle . as_ref () )
    . map ( |straddle| buffer_readiness_summary_for (
      env, straddle, &saved_view_mut ) );
  context . warnings . extend (
    // Repairs the completion pass made to write-protected PartnerFolders in
    // the saved view, batched per (folder, recorder).
    render_completion_warnings (&repair_warnings) );
  let mut collateral_views : Vec<RenderedCollateralView> = Vec::new ();
  for curi in &collateral_view_ids {
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
  if let Ok (view_id) = viewid_from_request_result {
    views_state . open_views . update_view (
      &context . graph_snap, view_id, saved_view_mut);
    for rendered in collateral_views {
      views_state . open_views . update_view (
        &context . graph_snap,
        &rendered . view_id, rendered . viewforest);
      send_response_with_length_prefix (
        stream,
        & tag_sexp_response (
          TcpToClient::CollateralView,
          & format_single_view_sexp (
            &rendered . view_id, &rendered . text) )); }}
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
      restriction . viewStats . overridesHere = Some (original); }}
  Ok (())
}

fn rerender_collateral_view (
  view_id     : ViewId,
  views_state : &ViewsState,
  context     : &mut RerenderAfterSaveContext<'_>,
) -> Result<RenderedCollateralView, String> {
  let mut viewforest : ViewForest = match
    views_state . open_views . viewid_to_view (&view_id) {
      Some (f) => f . clone (),
      None => {
        return Err ( format! (
          "Collateral view {}: no viewforest found",
          view_id . repr_in_client () )); } };
  if let Err (e) = rewriteInPlace_viewnodes_whose_skgid_is_newly_extra (
    &mut viewforest, &context . graph_snap )
  { return Err ( format! (
      "Collateral view {}: preprocessing failed: {}",
      view_id . repr_in_client (), e )); }
  let text : String =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span!( "rerender_view (collateral)" ). entered();
      rerender_view (
        &mut viewforest,
        context,
        None, // Collateral views repair silently.
        RerenderOptions::ordinary (
          views_state . open_views . diffView_buffer_kind_of_view (&view_id) )
      ) . map_err (
        |e| format!( "Collateral view {}: {}",
                      view_id . repr_in_client (), e)) ? };
  Ok (RenderedCollateralView {
    view_id,
    text,
    viewforest,
  }) }

/// Build a DE-NOVO (initial) content view by running the ONE
/// post-save view completion (complete_viewforest) over a stub forest of the
/// requested roots. View completion
/// creates each fresh node's default PartnerFolders
/// (create_partnerFolders_for_fresh_nodes = true), expands content, reconciles
/// folders, and applies the node budget (see 'to_org/expand/editable.rs').
/// The caller (multi_root_view_via_env) then adds containerward role tree and
/// stats.
/// Returns the completed viewforest plus the warning strings the
/// render itself produced, of which there are none: folder-repair
/// warnings stay silent for de novo renders (see the warning-sink
/// filtering at the bottom).
pub fn render_initial_view (
  runtime     : &RuntimeGeneration,
  root_skgids : &[ID],
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(ViewForest, Vec<String>), Box<dyn Error>> {
  // Build the stub roots. Its EditableMap is discarded: view completion below uses
  // a FRESH one, so each root is a first occurrence (make_write-protected_if_repeat treats
  // any pid already in the map as a repeat and would wrongly write-protect them).
  let mut stub_editable_map : EditableMap = EditableMap::new ();
  let mut viewforest : ViewForest =
    crate::to_org::util::stub_viewforest_from_root_skgids (
      root_skgids, &runtime . graph, &runtime . config, &mut stub_editable_map,
      restriction ) ?;
  // De-novo (and ONLY de-novo) asks each view-root for its containerward
  // ancestry, as a self-consuming view request. The during-completion dispatch
  // leaves view-root Containerward alone (extract_view_requests); finish_viewforest
  // fulfills it via the ContainerwardRoleTree attach and drops it. So root containerward is
  // generated once, here, and round-trips in the saved buffer as ordinary content
  // -- a later save never re-generates it. (Roots are already editable by
  // construction -- first occurrence in the editable_map -- so they need no
  // ViewRequest::Editable; adding one would over-trigger the cascade.)
  for root_treeid in viewforest . root_skgids () {
    if let Some (mut node_mut) = viewforest . get_mut (root_treeid) {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
        &mut node_mut . value () . kind
      { t . view_requests . insert ( ViewRequest::RoleTree (RelationRole::CONTAINER) ); }} }
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let mut sink : Vec<CompletionWarning> = Vec::new ();
  let mut context : CompletionContext = CompletionContext::for_content_view (
    runtime, &graph_snap, &mut editable_map, &mut errors, restriction,
    &mut sink );
  complete_viewforest ( &mut viewforest, &mut context ) ?;
  // De-novo rendering REPAIRS silently: the sink's ColRepairs do not
  // correspond to edits the user just made, so a fresh view carries no
  // completion warnings.
  drop (sink);
  Ok (( viewforest, Vec::new () )) }

/// How 'rerender_view' treats a view, beyond what its context says.
/// Each case of rerender has its own constructor, which names every
/// field.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct RerenderOptions {
  /// Create default PartnerFolders for nodes that lack them. Ordinarily
  /// false: folders round-trip from the buffer. True for the
  /// skgrepo-set switch, where pruning removed them and the new set
  /// decides which return.
  pub create_partnerFolders                 : bool,
  /// True only for the view whose edit created the override.
  pub substitute_existing_content_overrides : bool,
  /// Some for a buffer of the diff view.
  pub diffView_buffer                       : Option<DiffViewBufferKind>,
  /// True only for the "integrate unshown changes" request; matters
  /// only in a buffer of the diff view.
  pub integrate_unshown_changes             : bool,
}

impl RerenderOptions {
  /// The view a save came from.
  pub fn for_saved_view (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      create_partnerFolders                 : false,
      substitute_existing_content_overrides : true,
      diffView_buffer,
      integrate_unshown_changes             : false, } }

  /// Every other rerender of an open view: the collateral views of a
  /// save, "rerender all views", the cleanup after deleting references
  /// to an absent node, and the diff view's buffers after a git-commit.
  pub fn ordinary (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      create_partnerFolders                 : false,
      substitute_existing_content_overrides : false,
      diffView_buffer,
      integrate_unshown_changes             : false, } }

  /// A view after its convert-and-prune step at a skgrepo-set switch.
  pub fn for_skgrepo_switch (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      create_partnerFolders                 : true,
      substitute_existing_content_overrides : false,
      diffView_buffer,
      integrate_unshown_changes             : false, } }

  /// The buffer of the diff view named by an "integrate unshown
  /// changes" request.
  pub fn for_integrating_unshown_changes (
    diffView_buffer : Option<DiffViewBufferKind>,
  ) -> RerenderOptions {
    RerenderOptions {
      create_partnerFolders                 : false,
      substitute_existing_content_overrides : false,
      diffView_buffer,
      integrate_unshown_changes             : true, } }
}

/// Strip the stale ID folders, re-complete the viewforest,
/// set graph/view stats, and render to string.
/// .
/// A buffer of the diff view ('options . diffView_buffer' Some) is
/// reconciled over the graph straddle, brought up to date in
/// 'context': view
/// completion reconciles each editable node's change facts, change
/// folders, markers and former members, its ID folder among them,
/// with a fresh drawing from the straddle
/// ('process_unrestrictedVognode_changes'), keeping every viewnode
/// that stays and its fold state, and its readiness facts are then
/// read from the store. Completion draws what it draws anew by the
/// buffer's sets recomputed after the save, and nothing editable is
/// write-protected again. Only when 'options . integrate_unshown_changes'
/// is true (the "integrate unshown changes" request) does it make occurrences
/// editable or append roots: before completion each node of the
/// recomputed U is made editable at its first occurrence if it has no
/// editable one, and after completion the content origins of the
/// recomputed D that the buffer does not draw are appended as roots
/// and completed. Its standing roots stay. The diff view does its
/// part before completion in 'prepare_diffView_rerender' and after it
/// in 'finish_diffView_rerender'.
pub fn rerender_view (
  viewforest    : &mut ViewForest,
  context       : &mut RerenderAfterSaveContext<'_>,
  warning_sink  : Option<&mut Vec<CompletionWarning>>, // Some only for the view the user just saved.
  options       : RerenderOptions,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  if options . diffView_buffer . is_none () {
    // A diff view's step reconciles its ID folders instead.
    tracing::debug!("rerender_view: starting");
    remove_id_folders (viewforest) ?; }
  let diffView_rerender : Option<DiffViewRerender> =
    prepare_diffView_rerender (
      &mut context . graph_straddle, &context . runtime,
      options . diffView_buffer, options . integrate_unshown_changes,
      viewforest ) ?;
  let diffView_source : Option<DiffViewSource> =
    diffView_rerender . as_ref () . map ( |rerender| rerender . source () );
  { tracing::debug!("rerender_view: starting complete_viewforest");
    let nodes_before_completion : HashSet<NodeId> =
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
      &context . deleted_by_this_save_pids,
      &context . deleted_by_this_save_extra_ids,
      context . skgrepo_restriction,
      // Post-save (and rerender-all) reuse the saved buffer's PartnerFolders
      // and pass false. The repo-switch rerender passes true: its prune
      // removed the folders, and the new set decides which defaults return.
      options . create_partnerFolders,
      options . substitute_existing_content_overrides,
      warning_sink,
      diffView_source,
      &nodes_before_completion,
      &editable_before_completion );
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "complete_viewforest" ). entered();
      complete_viewforest (
        viewforest, &mut completion_context ) ? };
    finish_diffView_rerender (
      diffView_rerender . as_ref (), viewforest, &mut completion_context,
      &context . runtime, context . env ) ?; }
  let result : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "finish_viewforest" ). entered();
      finish_viewforest (
        viewforest,
        &context . runtime . graph,
        &context . runtime . config,
        context . skgrepo_restriction,
        diffView_rerender . as_ref ()
          . map ( |rerender| &*rerender . source () . straddle . before )) } ?;
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (result) }

/// The shared post-completion tail for BOTH render paths: the de-novo
/// view (multi_root_view_via_env, server/to_org/render/content_view.rs) and the
/// post-save re-render (rerender_view, above). Given a viewforest whose UnrestrictedVognode
/// content has already been completed, it:
///   - fulfills a ViewRequest::RoleTree (RelationRole::CONTAINER) carried by a view-ROOT (only de-novo
///     sets it; see render_initial_view), building that root's
///     containerward role tree and dropping the request. This is data-driven:
///     post-save roots come from the saved buffer WITHOUT the request, so a save
///     never re-generates the containerward (it round-trips as ordinary content).
///   - marks view-root and orphan affectsParent,
///   - validates affectsParent against the captured graph (the de-novo path could
///     skip it for speed, but running it in both keeps the tails one); in a
///     diff view a role graft's birth holds if either graph of the straddle
///     holds its relationship ('diffView_before_graph'),
///   - computes graph- then viewnode stats,
///   - applies the skgrepo restriction, and renders to a buffer string.
/// Step order is immaterial between the affectsParent marks and graphnodestats:
/// affectsParent is a view property and graphnodestats reads only the in-Rust graph,
/// never affectsParent, so the final state is identical either way.
pub fn finish_viewforest (
  viewforest         : &mut ViewForest,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  skgrepo_restriction : Option<&SkgrepoRestriction>,
  diffView_before_graph : Option<&InRustGraph>, // a diff view's: its straddle's before-graph
) -> Result<String, Box<dyn Error>> {
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "fulfill_root_containerward_requests" ). entered();
    fulfill_root_containerward_requests (
      viewforest, graph, config, skgrepo_restriction ) ? ; }
  mark_view_roots_parent_na ( viewforest );
  // §A (Jeff's invariant): an Unrestricted survivor left under a non-container parent
  // (a phantom / Deleted / DeadViewnode) is a non-dead generalized orphan and
  // must become a non-member.
  mark_orphans_under_dead_parents_false ( viewforest );
  // Correct any affectsParent atoms whose claimed relation to the parent doesn't
  // hold in the captured graph (e.g. user moved a birth=linksToParent node
  // under a new parent it doesn't link to).
  validate_affectsParent_relationships_in_straddle (
    viewforest, graph, diffView_before_graph );
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
  if let Some (restriction) = skgrepo_restriction {
    apply_skgrepo_set_to_viewforest ( viewforest, restriction ); }
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "viewforest_to_string" ). entered();
    viewforest_to_string ( viewforest, config ) } }

/// When the server first receives the buffer, it replaces
/// each Viewnode's ID with a PID (`replace_ids_with_pids`).
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

/// Remove every ID folder. A view draws one only in the diff
/// view, whose step at its node's visit
/// ('process_unrestrictedVognode_changes') reconciles it instead.
fn remove_id_folders (
  viewforest : &mut ViewForest
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_skgid : NodeId =
    viewforest . internal_root_skgid ();
  do_everywhere_in_tree_dfs_prunable (
    viewforest,
    viewforest_root_skgid,
    &mut |mut node : NodeMut<Viewnode>| -> Result<bool, String> {
      if matches! ( &node . value() . kind,
                    ViewnodeKind::PropertyFolder (PropertyFolder::ID) ) {
        node . detach();
        Ok (false) // pruned — don't recurse into detached children
      } else { Ok (true) } } ) ?;
  Ok (( )) }

/// Fulfill a ViewRequest::RoleTree (RelationRole::CONTAINER) carried by a view-ROOT: attach that
/// root's full containerward role tree as its first children, then DROP the
/// request so it does not round-trip into the saved buffer (a later save must not
/// re-generate the containerward -- it round-trips as ordinary content instead).
///
/// Only the de-novo render sets this request (render_initial_view), so
/// this is a no-op post-save. The during-completion view-request dispatch
/// deliberately leaves view-root Containerward alone (extract_view_requests) so it
/// reaches here: this ContainerwardRoleTree-based attach builds a SEPARATE containerward
/// subtree and handles a cyclic root, whereas the dispatch's
/// build_and_integrate_containerward would merge it into existing content and
/// panic on a cyclic root (one whose containerward path cycles back to the root,
/// e.g. a contains b contains a).
fn fulfill_root_containerward_requests (
  viewforest : &mut ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Result<(), Box<dyn Error>> {
  let requesting_root_treeids : Vec<NodeId> =
    viewforest . root_skgids () . into_iter ()
      . filter ( |treeid| viewforest . get (*treeid)
          . map ( |n| match &n . value () . kind {
              ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =>
                t . view_requests . contains (& ViewRequest::RoleTree (RelationRole::CONTAINER)),
              _ => false } )
          . unwrap_or (false) )
      . collect ();
  if requesting_root_treeids . is_empty () { return Ok (( )); }
  attach_full_containerward_role_trees_at_treeids_with_skgrepo_set (
    viewforest, &requesting_root_treeids, graph, config, restriction,
    RoleTreePartners::LiveGraph ) ?;
  for treeid in requesting_root_treeids { // drop the now-fulfilled request
    if let Some (mut node_mut) = viewforest . get_mut (treeid) {
      if let ViewnodeKind::Vognode (Vognode::Unrestricted (t)) =
        &mut node_mut . value () . kind
      { t . view_requests . remove (& ViewRequest::RoleTree (RelationRole::CONTAINER)); }} }
  Ok (( )) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../tests/unit/rerender_options.rs"]
mod tests;
