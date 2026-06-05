pub mod ancestry;
pub mod complete;
pub mod complete_postorder;
pub mod complete_preorder;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use graphnodestats::{
  set_graphnodestats_in_viewforest,
  set_graphnodestats_in_viewforest_with_source_set};
pub use viewnodestats::set_viewnodestats_in_viewforest;

use complete::{complete_viewforest, CompletionContext};
use crate::dbs::in_rust_graph::{ InRustGraph, scheduled_audit::take_pending_audit_warning};
use crate::types::env::SkgEnv;
use crate::org_to_text::viewforest_to_string;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::{ SaveResponse, compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_lock_views_sexp, format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::source_sets::{ActiveSourceSet, apply_source_set_to_viewforest};
use crate::to_org::expand::backpath::attach_containerward_ancestries_at_nodeids_with_source_set;
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::views_state::ViewUri;
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_prunable };
use crate::types::tree::forest::ViewForest;
use crate::to_org::util::{mark_view_roots_parent_absent, validate_parentIs_relationships, mark_orphans_under_dead_parents_independent};
use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::viewnode::{IndefOrDef, ViewNode, ViewNodeKind};
use crate::types::viewnode::{Vognode, QualCol, Qual};

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::sync::Arc;
use std::time::Instant;
use typedb_driver::TypeDBDriver;

pub struct RerenderAfterSaveContext<'a> {
  pub env          : &'a SkgEnv,
  pub source_diffs : Option<HashMap<SourceName, SourceDiff>>,
  pub graph_snap   : Arc<InRustGraph>,
  pub errors       : Vec<String>,
  pub warnings     : Vec<String>,
  /// Files deleted since HEAD, keyed by pid, for diff-mode rendering.
  pub deleted_since_head_pid_src_map : HashMap<ID, SourceName>,
  /// Pids deleted by this save; not necessarily a subset of git deletes.
  pub deleted_by_this_save_pids      : HashSet<ID>,
  pub active_source_set              : Option<&'a ActiveSourceSet>,
}

impl<'a> RerenderAfterSaveContext<'a> {
  fn for_save (
    env               : &'a SkgEnv,
    diff_mode_enabled : bool,
    define_nodes      : &[DefineNode],
    active_source_set : Option<&'a ActiveSourceSet>,
  ) -> RerenderAfterSaveContext<'a> {
    let source_diffs
      : Option<HashMap<SourceName, SourceDiff>>
      = if diff_mode_enabled
        { Some ( compute_diff_for_every_source (&env . config)) }
        else {None};
    let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
      source_diffs . as_ref()
      . map ( |d| deleted_ids_to_source (d))
      . unwrap_or_default();
    let deleted_by_this_save_pids : HashSet<ID> =
      // PITFALL: Can overlap deleted_since_head_pid_src_map, but neither is necessarily a subset of the other. If you delete something that you added since head, it will only be here. And if you deleted something since head but not in this save, it will only be there.
      // PITFALL: Looks dangerous but isn't: Each merge includes an acquiree deletion. This would make ViewNodes with that ID invalid (since ViewNodes by this point should have PIDs). It doesn't, though, because rewriteInPlace_viewnodes_whose_id_is_newly_extra will rewrite those ViewNodes to instead use the acquirer's PID (i.e. to depict the acquirer now, instead of the acquiree as before) before using this set.
      define_nodes . iter()
      . filter_map( |instr| match instr {
        DefineNode::Delete (d) => Some( d . id . clone() ),
        _ => None })
      . collect();
    RerenderAfterSaveContext {
      env,
      source_diffs,
      graph_snap : env . in_rust_graph . load_full (),
      errors : Vec::new (),
      warnings : Vec::new (),
      deleted_since_head_pid_src_map,
      deleted_by_this_save_pids,
      active_source_set,
    }}

  pub fn without_save (
    env               : &'a SkgEnv,
    diff_mode_enabled : bool,
    active_source_set : Option<&'a ActiveSourceSet>,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::for_save (
      env, diff_mode_enabled, &[], active_source_set ) }
}

struct RenderedCollateralView {
  uri        : ViewUri,
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
pub async fn update_views_after_save (
  stream                      : &mut std::net::TcpStream,
  saved_view                  : ViewForest,
  define_nodes                : Vec<DefineNode>,
  diff_mode_enabled           : bool,
  env                         : &SkgEnv,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                 : &mut ViewsState,
  active_source_set           : Option<&ActiveSourceSet>,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut context : RerenderAfterSaveContext =
    // Snapshot the in-Rust graph once for this save's rerender pass.
    // Used both by rewriteInPlace_viewnodes_whose_id_is_newly_extra (swap acquiree pids
    // to acquirer pids before rerender) and by content_goal_list
    // resolution during reconcile (neighbors' on-disk contains still
    // point at the acquiree id).
    RerenderAfterSaveContext::for_save (
      env, diff_mode_enabled, &define_nodes, active_source_set );
  let mut saved_view_mut : ViewForest = saved_view;
  rewriteInPlace_viewnodes_whose_id_is_newly_extra (
    &mut saved_view_mut, &context . graph_snap ) ?;
  let saved_text : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view ( // saved view first; collateral ones later
        &mut saved_view_mut,
        &mut context ) . await } ?;
  if let Ok (uri) = viewuri_from_request_result {
    views_state . open_views . update_view (
      uri, saved_view_mut);
    let collateral_uris : Vec<ViewUri> =
      find_collateral_view_uris (
        uri, &define_nodes, views_state);
    // plan_v2 §8.1 step 3: relax the early (broad) lock to the EXACT collateral
    // set now that the SavePlan is known. Emacs keeps saved + these locked and
    // unlocks everything else it locked early, so the user can edit truly-
    // unaffected buffers during the rest of the pipeline. Symmetric with the
    // save-lock message; sent before the collateral-view stream.
    send_response_with_length_prefix (
      stream,
      & tag_sexp_response (
        TcpToClient::SaveRelaxLock,
        & format_lock_views_sexp ( &collateral_uris )));
    if collateral_uris . is_empty () {
      tracing::debug!("update_views_after_save: no collateral views");
    } else {
      tracing::info!(
        "update_views_after_save: {} collateral view(s): {:?}",
        collateral_uris . len (),
        collateral_uris . iter ()
          . map ( |u| u . repr_in_client () )
          . collect::<Vec<_>> ()); }
    for curi in collateral_uris {
      match rerender_collateral_view (
        curi, views_state, &mut context ) . await
      { Ok (rendered) => {
          views_state . open_views . update_view (
            &rendered . uri, rendered . viewforest);
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::CollateralView,
              & format_single_view_sexp (
                &rendered . uri, &rendered . text) )); },
        Err (e) => { context . errors . push (e); }} }}
  if let Some (w) = take_pending_audit_warning () {
    context . warnings . insert (0, w); }
  Ok ( SaveResponse {
    saved_view          : saved_text,
    errors              : context . errors,
    warnings            : context . warnings,
    save_point_position : None, } ) }

async fn rerender_collateral_view (
  uri         : ViewUri,
  views_state : &ViewsState,
  context     : &mut RerenderAfterSaveContext<'_>,
) -> Result<RenderedCollateralView, String> {
  let mut viewforest : ViewForest = match
    views_state . open_views . viewuri_to_view (&uri) {
      Some (f) => f . clone (),
      None => {
        return Err ( format! (
          "Collateral view {}: no viewforest found",
          uri . repr_in_client () )); } };
  if let Err (e) = rewriteInPlace_viewnodes_whose_id_is_newly_extra (
    &mut viewforest, &context . graph_snap )
  { return Err ( format! (
      "Collateral view {}: preprocessing failed: {}",
      uri . repr_in_client (), e )); }
  let text : String =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span!( "rerender_view (collateral)" ). entered();
      rerender_view (
        &mut viewforest,
        context
      ) . await . map_err (
        |e| format!( "Collateral view {}: {}",
                      uri . repr_in_client (), e)) ? };
  Ok (RenderedCollateralView {
    uri,
    text,
    viewforest,
  }) }

/// Given the saved ViewUri and DefineNodes,
/// return the URIs of other views whose viewforests
/// contain any changed PID. Includes search views --
/// they are just as editable as other kinds.
fn find_collateral_view_uris (
  saved_uri    : &ViewUri,
  define_nodes : &[DefineNode],
  views_state  : &ViewsState,
) -> Vec<ViewUri> {
  let changed_pids : HashSet<ID> =
    define_nodes . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Save ( SaveNode (n)) =>
        Some ( n . pid . clone () ),
      DefineNode::Delete (dn) =>
        Some ( dn . id . clone () ) } )
    . collect ();
  tracing::debug!(
    "find_collateral_view_uris: {} changed PIDs, {} views in ViewsState",
    changed_pids . len (),
    views_state . open_views . views . len ());
  let uris : HashSet<ViewUri> =
    changed_pids . iter ()
    . flat_map ( |pid| views_state . open_views . views_containing (pid) )
    . filter ( |uri| uri != saved_uri )
    . collect ();
  uris . into_iter () . collect () }

/// Phase 8 (§13): build a DE-NOVO (initial) content view by running the ONE
/// post-save driver (complete_viewforest) over a stub forest of the requested
/// roots. The driver
/// creates each fresh node's relation cols (create_relation_cols_for_fresh_nodes
/// = true), expands content, reconciles cols, and applies the §5.5 node budget.
/// When diff_mode, the git diff is computed inline by the driver (per node, at
/// its BFS visit, via process_truenode_diff) -- the same path post-save uses.
/// The caller (multi_root_view_via_env) then adds containerward ancestry and
/// stats.
pub async fn render_initial_view_via_driver (
  env       : &SkgEnv,
  root_ids  : &[ID],
  active    : Option<&ActiveSourceSet>,
  diff_mode : bool,
) -> Result<ViewForest, Box<dyn Error>> {
  // Build the stub roots. Its DefinitiveMap is discarded: the driver below uses
  // a FRESH one, so each root is a first occurrence (make_indef_if_repeat treats
  // any pid already in the map as a repeat and would wrongly indefinitize them).
  let mut stub_defmap : DefinitiveMap = DefinitiveMap::new ();
  let mut viewforest : ViewForest =
    crate::to_org::util::stub_viewforest_from_root_ids (
      root_ids, &env . config, &env . driver, &mut stub_defmap ) . await ?;
  let graph_snap : Arc<InRustGraph> = env . in_rust_graph . load_full ();
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  // §9 reversal (#3): de-novo diff is computed INLINE by the driver, exactly
  // like post-save -- compute the real diffs here and feed them via source_diffs
  // (which drives the inline process_truenode_diff and the diff-aware QualCol /
  // sharing-col reconcilers).
  let real_diffs : Option<HashMap<SourceName, SourceDiff>> =
    if diff_mode { Some ( compute_diff_for_every_source (&env . config) ) }
    else         { None };
  let deleted_src : HashMap<ID, SourceName> =
    real_diffs . as_ref () . map ( |d| deleted_ids_to_source (d) )
      . unwrap_or_default ();
  let empty_deleted_pids : HashSet<ID> = HashSet::new ();
  let mut context : CompletionContext = CompletionContext {
    defmap                         : &mut defmap,
    source_diffs                   : &real_diffs,
    env,
    graph_snap                     : &graph_snap,
    errors                         : &mut errors,
    deleted_since_head_pid_src_map : &deleted_src,
    deleted_by_this_save_pids      : &empty_deleted_pids,
    active_source_set              : active,
    node_budget                    : env . config . initial_node_limit,
    create_relation_cols_for_fresh_nodes : true,
    diff_tantivy_index : if diff_mode { Some (&env . tantivy_index) }
                         else         { None }, };
  complete_viewforest ( &mut viewforest, &mut context ) . await ?;
  Ok ( viewforest ) }

/// Strip stale diff data, re-complete the viewforest,
/// set graph/view stats, and render to string.
pub async fn rerender_view (
  viewforest    : &mut ViewForest,
  context       : &mut RerenderAfterSaveContext<'_>,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  { tracing::debug!("rerender_view: starting");
    strip_stale_diff_state (viewforest) ?; }
  { tracing::debug!("rerender_view: starting complete_viewforest");
    let mut defmap : DefinitiveMap = DefinitiveMap::new ();
    let mut completion_context : CompletionContext = CompletionContext {
      defmap                         : &mut defmap,
      // The real per-source diffs drive ALL diff inline: process_truenode_diff
      // (content axes + phantom flip + TextChanged/IDCol/AliasCol) and the
      // diff-aware QualCol / sharing-col reconcilers, each at its own BFS visit
      // (§9 reversal / #3). The content reconcile itself stays worktree-only.
      source_diffs                   : &context . source_diffs,
      env                            : context . env,
      graph_snap                     : &context . graph_snap,
      errors                         : &mut context . errors,
      deleted_since_head_pid_src_map : &context . deleted_since_head_pid_src_map,
      deleted_by_this_save_pids      : &context . deleted_by_this_save_pids,
      active_source_set              : context . active_source_set,
      node_budget                    : context . env . config . initial_node_limit,
      // Post-save reuses the saved buffer's relation cols; do not re-create them
      // (that would change the buffer and break the save round-trip, §18).
      create_relation_cols_for_fresh_nodes : false,
      // Post-save: phantom sources resolve via the deleted-id map + disk scan
      // (the de-novo path passes the tantivy index instead).
      diff_tantivy_index : None, };
    complete_viewforest (
      viewforest, &mut completion_context ) . await ?; }
  // §9 reversal (#3): the content/scaffold diff was applied INLINE during the
  // BFS above (process_truenode_diff at each Normal node's visit, driven by
  // source_diffs = the real diffs).
  let result : String =
    finish_viewforest (
      viewforest,
      &context . env . config,
      &context . env . driver,
      context . active_source_set,
      false ) . await ?; // post-save: do NOT re-generate root containerward
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  Ok (result) }

/// The shared post-completion tail for BOTH render paths (§20.3): the de-novo
/// view (multi_root_view_via_env, server/to_org/render/content_view.rs) and the
/// post-save re-render (rerender_view, above). Given a viewforest whose TrueNode
/// content + git diff have already been completed, it:
///   - attaches containerward ancestry to every removed-here phantom, and -- ONLY
///     when `attach_root_containerward` is set (de-novo) -- to every view-root.
///     Root containerward is a de-novo-only concern: it is generated once at open
///     and then round-trips in the buffer as ordinary content, so a later SAVE
///     must NOT re-generate it (that is why post-save passes `false`). See the
///     note on attach_containerward_ancestries_to_removedhere_phantoms.
///   - marks view-root and orphan parentIs,
///   - validates parentIs against the in-Rust graph (a no-op when markers
///     already agree, and when the global handle isn't initialized; the de-novo
///     path could skip it for speed, but running it in both keeps the tails one),
///   - computes graph- then view-node stats,
///   - applies the active source set, and renders to a buffer string.
/// Step order is immaterial between the parentIs marks and graphnodestats:
/// parentIs is a view property and graphnodestats reads only the in-Rust graph,
/// never parentIs, so the final state is identical either way.
pub async fn finish_viewforest (
  viewforest              : &mut ViewForest,
  config                  : &SkgConfig,
  driver                  : &TypeDBDriver,
  active_source_set       : Option<&ActiveSourceSet>,
  attach_root_containerward : bool,
) -> Result<String, Box<dyn Error>> {
  if attach_root_containerward {
    attach_containerward_ancestries_to_view_roots (
      viewforest, config, driver, active_source_set ) . await ?; }
  attach_containerward_ancestries_to_removedhere_phantoms (
    viewforest, config, driver, active_source_set ) . await ?;
  mark_view_roots_parent_absent ( viewforest );
  // §A (Jeff's invariant): a Normal survivor left under a non-container parent
  // (a phantom / Deleted / DeadScaffold) is a non-dead generalized orphan and
  // must become Independent.
  mark_orphans_under_dead_parents_independent ( viewforest );
  if let Some (snap) = snapshot_global () {
    // Correct any parentIs markers whose claimed relation to the parent doesn't
    // hold in the in-Rust graph (e.g. user moved a birth=linksToParent node
    // under a new parent it doesn't link to).
    validate_parentIs_relationships ( viewforest, &snap ); }
  let ( container_to_contents, content_to_containers ) =
    match active_source_set {
      Some (active) =>
        set_graphnodestats_in_viewforest_with_source_set (
          viewforest, config, driver, active ) . await,
      None =>
        set_graphnodestats_in_viewforest (
          viewforest, config, driver ) . await,
    } ?;
  set_viewnodestats_in_viewforest (
    viewforest, &container_to_contents, &content_to_containers, config );
  if let Some (active) = active_source_set {
    apply_source_set_to_viewforest ( viewforest, active ); }
  viewforest_to_string ( viewforest, config ) }

/// When the server first receives the buffer, it replaces
/// each ViewNode's ID with a PID (`replace_ids_with_pids`).
/// Later it updates the graph, which can involve merging nodes.
/// After a merge, the acquiree is gone, and what was the ViewNode
/// onto it now an extra_id of the acquirer, not a PID.
/// This is intended to replaces such a
/// viewnode to carry the primary's pid, source, title, and body.
///
/// The modified viewnode's subtree is left attached.
/// 'reconcile_content_children' will later update it
/// to include the rest of the subscriber's content.
///
/// PITFALL: The function does not actually remember the merge history.
/// It merely identifies each TrueNode in 'viewforest'
/// whose pid is an extra_id of some distinct node in the snapshot.
fn rewriteInPlace_viewnodes_whose_id_is_newly_extra (
  viewforest : &mut Tree<ViewNode>,
  graph_snap : &Arc<InRustGraph>,
) -> Result<(), Box<dyn Error>> {
  let nodeids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for nid in nodeids {
    let swap : Option<(ID, SourceName, String, Option<String>)> = {
      let n_ref = viewforest . get (nid) . ok_or (
        "rewriteInPlace_viewnodes_whose_id_is_newly_extra: node not found") ?;
      match &n_ref . value () . kind {
        // Only Normal nodes are rewritten below (a phantom is never an
        // editable instance), so only they need a swap computed.
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => { match graph_snap . pid_of (&t . id)
               { Some (primary) if primary != t . id => {
                     graph_snap . get (&primary) . map ( |r| (
                       primary . clone (),
                       r . source . clone (),
                       r . title . clone (),
                       r . body . clone () )) },
                 _ => None } },
        _ => None } };
    if let Some ((new_pid, new_source, new_title, new_body)) = swap
    { let mut n_mut = viewforest . get_mut (nid)
        . ok_or ("rewriteInPlace_viewnodes_whose_id_is_newly_extra: node_mut failed") ?;
      if let ViewNodeKind::Vognode (Vognode::Normal (t))
      = &mut n_mut . value () . kind
      { t . id = new_pid;
        t . source = new_source;
        t . title = new_title;
        if let IndefOrDef::Definitive { body, .. }
        = &mut t . indef_or_def
        { *body = new_body; }} }}
  Ok (( )) }

/// Reset all traces of a prior diff-view rendering so the viewforest
/// is ready for a fresh pass. Three independent traversals today;
/// could be fused if traversal cost ever mattered.
fn strip_stale_diff_state (
  viewforest : &mut ViewForest
) -> Result<(), Box<dyn Error>> {
  remove_branches_that_git_marked_removed (viewforest) ?;
  remove_diff_only_scaffolds (viewforest) ?;
  clear_diff_metadata (viewforest) ?;
  Ok (( )) }

/// Strip every stale diff phantom from the viewforest, REGARDLESS of its
/// relation to its parent (Jeff's progress.org §9 TODO): a phantom anywhere
/// but a forest root is removed, whatever its parentIs. Disposal depends on
/// whether it has children:
///   - childless phantom -> detached (deleted) and its branch pruned;
///   - phantom WITH children -> demoted to DeadScaffold, so any real user
///     subtree parented under it survives (the §3.4 postorder prune sweep
///     later removes the DeadScaffold if it ends up childless). We recurse
///     into it so nested phantoms are stripped too.
/// Exception: a forest root (top-level view node) is NOT stripped -- stripping
/// it would empty the view, and unlike a content phantom the diff overlay does
/// not regenerate a root. (This is the only surviving relation-to-parent test;
/// it is about tree position and regeneration, not parentIs.)
///
/// Previously this stripped only content phantoms (parentIs == Affected),
/// preserving Independent/Absent ones. But a phantom dragged out of position
/// is a confusing lie -- it claims a node is missing somewhere it never was --
/// so we now drop it too; parentIs is no longer read here at all.
///
/// PITFALL:
/// Beware, ye who would preserve phantom nodes across save-buffer ops:
/// Phantom nodes can go stale in many ways.
/// Maybe it was RemovedHere and now it is entirely Removed, or vice-versa.
/// Maybe it lies, because the user moved the phantom to a place it never was.
/// Maybe it duplicates another phantom node here.
/// Maybe it duplicates a non-phantom node here,
/// in which case its Removed* label is again a lie
/// -- it is no longer missing here.
/// Etc. Much easier to just regenerate them at each save.
fn remove_branches_that_git_marked_removed (
  viewforest : &mut ViewForest
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . internal_root_id ();
  do_everywhere_in_tree_dfs_prunable (
    viewforest,
    viewforest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_viewforest_root_child : bool = {
        match node . parent() {
          Some (p) =>
            p . id () == viewforest_root_id,
          None => false } };
      let is_phantom : bool =
        matches! ( &node . value() . kind,
          ViewNodeKind::Vognode (Vognode::DiffPhantom (_)) );
      if ! is_phantom || is_viewforest_root_child {
        return Ok (true); } // not a strippable phantom: recurse normally
      if node . has_children () {
        // Keep any real subtree the user parented here; mark this dead and
        // recurse so nested phantoms are still stripped.
        node . value() . kind = ViewNodeKind::DeadScaffold;
        Ok (true)
      } else {
        node . detach();
        Ok (false) // Prune: branch removed, so don't recurse
      }} )? ;
  Ok (( )) }

/// Remove scaffolds that exist only to display diff information:
/// TextChanged and IDCol.
/// These are regenerated from scratch by 'process_truenode_diff' (the inline
/// per-node diff) at each node's BFS visit, so stale ones must be stripped first.
/// AliasCol is NOT removed: it may have been requested by the user
/// (not just injected by diff mode), and tracking which case applies
/// is not worth the complexity. Phantom Alias children (injected by
/// diff mode) are cleaned up by reconcile_alias_col_children during the postorder
/// pass: its goal list won't include them, so they are detached.
fn remove_diff_only_scaffolds (
  viewforest : &mut ViewForest
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . internal_root_id ();
  do_everywhere_in_tree_dfs_prunable (
    viewforest,
    viewforest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_diff_scaffold : bool =
        matches! ( &node . value() . kind,
          ViewNodeKind::Qual (Qual::TextChanged { .. }) |
          ViewNodeKind::QualCol (QualCol::ID) );
      if is_diff_scaffold {
        node . detach();
        Ok (false) // pruned — don't recurse into detached children
      } else { Ok (true) } } ) ?;
  Ok (( )) }

/// Clear diff metadata from all TrueNodes in the viewforest.
/// Diff-only scaffolds (TextChanged, IDCol) are
/// removed by 'remove_diff_only_scaffolds' before this runs.
fn clear_diff_metadata (
  viewforest : &mut ViewForest
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . internal_root_id ();
  do_everywhere_in_tree_dfs (
    viewforest,
    viewforest_root_id,
    true,
    &mut |mut node : NodeMut<ViewNode>| -> Result<(), String>
      { // Ignores scaffolds: some (Alias, ID) carry diff data,
        // but they are regenerated from scratch by their postorder
        // completers, so clearing them here is unnecessary.
        match &mut node . value() . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t)) => {
            t . existence  = ExistenceAxes::default ();
            t . membership = MembershipAxes::default ();
            t . not_in_git = false; }
          ViewNodeKind::Vognode (Vognode::DiffPhantom (p)) => {
            p . existence  = ExistenceAxes::default ();
            p . membership = MembershipAxes::default ();
            p . not_in_git = false; }
          _ => {} }
        Ok (( )) } ) ?;
  Ok (( )) }

/// For each view-root, attach its full containerward ancestry as that root's
/// first children. Called by finish_viewforest ONLY for the de-novo render (the
/// `attach_root_containerward` arm): root containerward is generated once at open
/// and then round-trips in the saved buffer as ordinary content, so a later SAVE
/// must NOT re-generate it. (We deliberately use this AncestryTree-based attach,
/// not the ViewRequest::Containerward / build_and_integrate_containerward path:
/// the latter runs during completion and panics on a cyclic root -- one whose
/// containerward path cycles back to the root itself, e.g. a contains b contains
/// a -- whereas this tail attach handles that case. See progress.org §17.)
async fn attach_containerward_ancestries_to_view_roots (
  viewforest : &mut ViewForest,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
  active     : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let view_root_nodeids : Vec<NodeId> = viewforest . root_ids ();
  attach_containerward_ancestries_at_nodeids_with_source_set (
    viewforest, &view_root_nodeids, config, driver, active ) . await }

/// For every RemovedHere phantom in the viewforest, fetch its containerward
/// ancestry from TypeDB and insert it as indefinitive Content children.
/// Short-circuits when no RemovedHere phantoms exist.
async fn attach_containerward_ancestries_to_removedhere_phantoms (
  viewforest    : &mut ViewForest,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  active        : Option<&ActiveSourceSet>,
) -> Result<(), Box<dyn Error>> {
  let phantom_nodeids : Vec<NodeId> = {
    let mut result : Vec<NodeId> = Vec::new ();
    for edge in viewforest . root () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        let is_removedhere : bool = match &node_ref . value () . kind {
          ViewNodeKind::Vognode (Vognode::Normal (t)) =>
            t . is_removedhere_phantom (),
          ViewNodeKind::Vognode (Vognode::DiffPhantom (p)) =>
            p . is_removedhere_phantom (),
          _ => false };
        if is_removedhere
        { result . push ( node_ref . id () ); }} }
    result };
  attach_containerward_ancestries_at_nodeids_with_source_set (
    viewforest, &phantom_nodeids, config, typedb_driver, active ) . await }
