pub mod complete;
pub mod complete_postorder;
pub mod complete_preorder;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use complete::complete_viewforest;
pub use graphnodestats::set_graphnodestats_in_viewforest;
pub use viewnodestats::set_viewnodestats_in_viewforest;

use crate::dbs::in_rust_graph::{ InRustGraph, scheduled_audit::take_pending_audit_warning};
use crate::types::env::SkgEnv;
use crate::org_to_text::viewforest_to_string;
use crate::serve::ViewsState;
use crate::serve::handlers::save_buffer::{ SaveResponse, compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::to_org::expand::backpath::attach_containerward_ancestries_at_nodeids;
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::views_state::ViewUri;
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_prunable };
use crate::to_org::util::{mark_view_roots_independent, validate_birth_relationships};
use crate::dbs::in_rust_graph::snapshot_global;
use crate::types::viewnode::{IndefOrDef, ViewNode, ViewNodeKind, Scaffold};

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
  /// Files deleted since HEAD, keyed by pid, for diff-mode rendering.
  pub deleted_since_head_pid_src_map : HashMap<ID, SourceName>,
  /// Pids deleted by this save; not necessarily a subset of git deletes.
  pub deleted_by_this_save_pids      : HashSet<ID>,
}

impl<'a> RerenderAfterSaveContext<'a> {
  fn for_save (
    env               : &'a SkgEnv,
    diff_mode_enabled : bool,
    save_instructions : &[DefineNode],
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
      // Merge acquirees are deliberately NOT included: post-merge they are extra-ids of their acquirers, not genuine deletions. The preprocessing pass 'resolve_extra_ids_in_viewforest' below rewrites their viewnodes to point at the acquirer before rerender runs, so the (deleted ...) path never fires for them.
      save_instructions . iter()
      . filter_map( |instr| match instr {
        DefineNode::Delete (d) => Some( d . id . clone() ),
        _ => None })
      . collect();
    RerenderAfterSaveContext {
      env,
      source_diffs,
      graph_snap : env . in_rust_graph . load_full (),
      errors : Vec::new (),
      deleted_since_head_pid_src_map,
      deleted_by_this_save_pids,
    }}

  pub fn without_save (
    env               : &'a SkgEnv,
    diff_mode_enabled : bool,
  ) -> RerenderAfterSaveContext<'a> {
    RerenderAfterSaveContext::for_save (
      env, diff_mode_enabled, &[] ) }
}

struct RenderedCollateralView {
  uri        : ViewUri,
  text       : String,
  viewforest : Tree<ViewNode>,
}

/// PURPOSE:
/// For each view affected by the save
/// (starting, importantly, with the saved view itself,
/// and only processing collateral views after that),
/// this updates the rendered views and ViewsState.
/// .
/// ASSUMES:
/// the graph was already updated. The reverse order would be bad.
pub async fn update_views_after_save (
  stream                      : &mut std::net::TcpStream,
  saved_view                  : Tree<ViewNode>,
  save_instructions           : Vec<DefineNode>,
  diff_mode_enabled           : bool,
  env                         : &SkgEnv,
  viewuri_from_request_result : &Result<ViewUri, String>,
  views_state                  : &mut ViewsState,
) -> Result<SaveResponse, Box<dyn Error>> {
  // Snapshot the in-Rust graph once for this save's rerender pass.
  // Used both by resolve_extra_ids_in_viewforest (swap acquiree pids
  // to acquirer pids before rerender) and by content_goal_list
  // resolution during reconcile (neighbors' on-disk contains still
  // point at the acquiree id).
  let mut context : RerenderAfterSaveContext =
    RerenderAfterSaveContext::for_save (
      env, diff_mode_enabled, &save_instructions );
  let mut saved_view_mut : Tree<ViewNode> = saved_view;
  resolve_extra_ids_in_viewforest (
    &mut saved_view_mut, &context . graph_snap ) ?;
  let saved_text : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view (
        &mut saved_view_mut,
        &mut context,
        true ) . await } ?;
  if let Ok (uri) = viewuri_from_request_result {
    views_state . open_views . update_view (
      uri, saved_view_mut);
    let collateral_uris : Vec<ViewUri> =
      find_collateral_view_uris (
        uri, &save_instructions, views_state);
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
    context . errors . insert (0, w); }
  Ok ( SaveResponse { saved_view : saved_text,
                       errors : context . errors } ) }

async fn rerender_collateral_view (
  uri         : ViewUri,
  views_state : &ViewsState,
  context     : &mut RerenderAfterSaveContext<'_>,
) -> Result<RenderedCollateralView, String> {
  let mut viewforest : Tree<ViewNode> = match
    views_state . open_views . viewuri_to_view (&uri) {
      Some (f) => f . clone (),
      None => {
        return Err ( format! (
          "Collateral view {}: no viewforest found",
          uri . repr_in_client () )); } };
  if let Err (e) = resolve_extra_ids_in_viewforest (
    &mut viewforest, &context . graph_snap )
  { return Err ( format! (
      "Collateral view {}: preprocessing failed: {}",
      uri . repr_in_client (), e )); }
  let text : String =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span!( "rerender_view (collateral)" ). entered();
      rerender_view (
        &mut viewforest,
        context,
        false
      ) . await . map_err (
        |e| format!( "Collateral view {}: {}",
                      uri . repr_in_client (), e)) ? };
  Ok (RenderedCollateralView {
    uri,
    text,
    viewforest,
  }) }

/// Given the saved ViewUri and save instructions,
/// return the URIs of other views whose viewforests
/// contain any changed PID. Includes search views --
/// they are just as editable as other kinds.
fn find_collateral_view_uris (
  saved_uri         : &ViewUri,
  save_instructions : &[DefineNode],
  views_state        : &ViewsState,
) -> Vec<ViewUri> {
  let changed_pids : HashSet<ID> =
    save_instructions . iter ()
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

/// Strip stale diff data, re-complete the viewforest,
/// set graph/view stats, and render to string.
pub async fn rerender_view (
  viewforest    : &mut Tree<ViewNode>,
  context       : &mut RerenderAfterSaveContext<'_>,
  is_saved_view : bool,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  tracing::debug!("rerender_view: starting");
  strip_stale_diff_state (viewforest) ?;
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  tracing::debug!("rerender_view: starting complete_viewforest");
  complete_viewforest (
    viewforest, &mut defmap,
    &context . source_diffs, context . env, &context . graph_snap,
    &mut context . errors, &context . deleted_since_head_pid_src_map,
    &context . deleted_by_this_save_pids,
    is_saved_view ) . await ?;
  mark_view_roots_independent (viewforest);
  if let Some (snap) = snapshot_global () {
    // Correct any birth markers whose claimed relation to the
    // parent doesn't hold in the in-Rust graph (e.g. user moved a
    // birth=linksTo node under a new parent it doesn't link to).
    // No-op when the global graph handle isn't initialized (tests
    // that bypass startup).
    validate_birth_relationships (viewforest, &snap); }
  attach_containerward_ancestries_to_removedhere_phantoms (
    viewforest, &context . env . config,
    &context . env . driver ) . await ?;
  tracing::debug!("rerender_view: complete_viewforest done ({:.3}s), starting graphnodestats",
            t_rerender . elapsed () . as_secs_f64 ());
  let ( container_to_contents, content_to_containers ) =
    set_graphnodestats_in_viewforest (
      viewforest,
      &context . env . config,
      &context . env . driver ) . await ?;
  tracing::debug!("rerender_view: graphnodestats done ({:.3}s), rendering to string",
            t_rerender . elapsed () . as_secs_f64 ());
  set_viewnodestats_in_viewforest (
    viewforest,
    &container_to_contents,
    &content_to_containers,
    &context . env . config );
  let result : Result<String, Box<dyn Error>> =
    viewforest_to_string (viewforest, &context . env . config);
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  result }

/// For each TrueNode in VIEWFOREST whose pid is an extra_id of
/// some *different* primary in the graph snapshot, rewrite the
/// viewnode to carry the primary's pid, source, title, and body.
/// The viewnode's subtree is left attached.
///
/// Why it's safe to leave the subtree attached: the canonical case
/// triggering this swap is a merge, where R absorbed E. Per
/// three_merged_nodecompletes, R's updated `contains` is
/// [preserver] ++ R's old contains ++ E's old contains. Every child
/// that was a content-child of E in the view is still a legitimate
/// content-child of R, so reconcile_content_children (during the
/// subsequent rerender) will retain them, insert any additional
/// content-children (preserver, R's originals), and move non-content
/// children to precede the content group. Clearing the subtree here
/// would just make reconcile redo that work.
///
/// Why this exists in addition to `replace_ids_with_pids` (which
/// runs at buffer-parse time in add_missing_info_to_viewforest):
/// that pass runs *before* the save pipeline executes the merge.
/// At parse time the acquiree is still its own primary pid; nothing
/// to rewrite. The merge then creates the acquiree -> acquirer
/// extra-id binding. This second pass catches the viewforest up to
/// that mutation before rerender walks it.
///
/// Efficiency opportunity intentionally declined: we could track the
/// specific ego_tree NodeIds of acquiree viewnodes at
/// merge-execution time and rewrite only those. Tree size is small
/// on human-scale views, and threading a NodeId set through the
/// save pipeline would outweigh the savings. A full walk also
/// naturally catches stale extra-ids left by prior merges that
/// didn't execute in this save.
fn resolve_extra_ids_in_viewforest (
  viewforest : &mut Tree<ViewNode>,
  graph_snap : &Arc<InRustGraph>,
) -> Result<(), Box<dyn Error>> {
  let nodeids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |n| n . id () )
    . collect ();
  for nid in nodeids {
    let swap : Option<(ID, SourceName, String, Option<String>)> = {
      let n_ref = viewforest . get (nid)
        . ok_or ("resolve_extra_ids_in_viewforest: node not found") ?;
      match &n_ref . value () . kind {
        ViewNodeKind::True (t) => {
          match graph_snap . pid_of (&t . id) {
            Some (primary) if primary != t . id => {
              graph_snap . get (&primary) . map ( |r| (
                primary . clone (),
                r . source . clone (),
                r . title . clone (),
                r . body . clone () )) },
            _ => None } },
        _ => None } };
    if let Some ((new_pid, new_source, new_title, new_body)) = swap {
      let mut n_mut = viewforest . get_mut (nid)
        . ok_or ("resolve_extra_ids_in_viewforest: node_mut failed") ?;
      if let ViewNodeKind::True (t) = &mut n_mut . value () . kind {
        t . id = new_pid;
        t . source = new_source;
        t . title = new_title;
        if let IndefOrDef::Definitive { body, .. }
          = &mut t . indef_or_def {
            *body = new_body; }}}}
  Ok (( )) }

/// Reset all traces of a prior diff-view rendering so the viewforest
/// is ready for a fresh pass. Three independent traversals today;
/// could be fused if traversal cost ever mattered.
fn strip_stale_diff_state (
  viewforest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  remove_branches_that_git_marked_removed (viewforest) ?;
  remove_diff_only_scaffolds (viewforest) ?;
  clear_diff_metadata (viewforest) ?;
  Ok (( )) }

/// Strip from the viewforest every branch whose root
///   is marked as removed or removed-here.
/// Exception: Does not strip:
///   - top-level branches (children of BufferRoot)
///   - non-content nodes (birth != ContentOf)
/// Uses single-pass DFS: removes branch roots as encountered,
/// skipping recursion into removed branches.
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
  viewforest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    viewforest,
    viewforest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_viewforest_root_child : bool = {
        match node . parent() {
          Some (mut p) =>
            matches! ( &p . value() . kind,
                       ViewNodeKind::Scaff (Scaffold::BufferRoot)),
          None => false } };
      let should_remove : bool =
        match &node . value() . kind {
          ViewNodeKind::True (t) =>
            t . is_phantom ()
            && ! is_viewforest_root_child
            && ! t . parent_ignores_it(),
          _ => false };
      if should_remove {
        node . detach();
        Ok (false) // Prune: branch removed, so don't recurse
      } else { Ok (true) }} )? ; // recurse into children
  Ok (( )) }

/// Remove scaffolds that exist only to display diff information:
/// TextChanged and IDCol.
/// These are regenerated from scratch by 'maybe_prepend_diff_view_scaffolds'
/// and their postorder completers, so stale ones must be stripped first.
/// AliasCol is NOT removed: it may have been requested by the user
/// (not just injected by diff mode), and tracking which case applies
/// is not worth the complexity. Phantom Alias children (injected by
/// diff mode) are cleaned up by reconcile_alias_col_children during the postorder
/// pass: its goal list won't include them, so they are detached.
fn remove_diff_only_scaffolds (
  viewforest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    viewforest,
    viewforest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_diff_scaffold : bool =
        matches! ( &node . value() . kind,
          ViewNodeKind::Scaff (Scaffold::TextChanged { .. }) |
          ViewNodeKind::Scaff (Scaffold::IDCol) );
      if is_diff_scaffold {
        node . detach();
        Ok (false) // pruned — don't recurse into detached children
      } else { Ok (true) } } ) ?;
  Ok (( )) }

/// Clear diff metadata from all TrueNodes in the viewforest.
/// Diff-only scaffolds (TextChanged, IDCol) are
/// removed by 'remove_diff_only_scaffolds' before this runs.
fn clear_diff_metadata (
  viewforest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let viewforest_root_id : NodeId =
    viewforest . root() . id();
  do_everywhere_in_tree_dfs (
    viewforest,
    viewforest_root_id,
    true,
    &mut |mut node : NodeMut<ViewNode>| -> Result<(), String>
      { // Ignores scaffolds: some (Alias, ID) carry diff data,
        // but they are regenerated from scratch by their postorder
        // completers, so clearing them here is unnecessary.
        if let ViewNodeKind::True (t)
          = &mut node . value() . kind
          { t . existence  = ExistenceAxes::default ();
            t . membership = MembershipAxes::default ();
            t . not_in_git = false; }
        Ok (( )) } ) ?;
  Ok (( )) }

/// For every RemovedHere phantom in the viewforest,
/// fetch its containerward ancestry from TypeDB
/// and insert it as indefinitive ContainerOf children.
/// Short-circuits when no RemovedHere phantoms exist.
async fn attach_containerward_ancestries_to_removedhere_phantoms (
  viewforest    : &mut Tree<ViewNode>,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let phantom_nodeids : Vec<NodeId> = {
    let mut result : Vec<NodeId> = Vec::new ();
    for edge in viewforest . root () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        if let ViewNodeKind::True (t) = &node_ref . value () . kind {
          if t . is_removedhere_phantom () {
            result . push ( node_ref . id () ); }} }}
    result };
  attach_containerward_ancestries_at_nodeids (
    viewforest, &phantom_nodeids, config, typedb_driver ) . await }
