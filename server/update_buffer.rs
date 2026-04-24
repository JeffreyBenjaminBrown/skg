pub mod complete;
pub mod complete_postorder;
pub mod complete_preorder;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use complete::complete_viewforest;
pub use graphnodestats::set_graphnodestats_in_viewforest;
pub use viewnodestats::set_viewnodestats_in_viewforest;

use crate::dbs::memory::{memory_coherent_with_save_instructions, scheduled_audit::take_pending_audit_warning};
use crate::dbs::typedb::ancestry::{ AncestryTree, ancestry_by_id_from_ids_async};
use crate::org_to_text::viewforest_to_string;
use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{ SaveResponse, compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::to_org::expand::backpath::insert_containerward_ancestry_tree_recursive;
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{ExistenceAxes, MembershipAxes, SourceDiff};
use crate::types::memory::ViewUri;
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, Merge, SaveNode};
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_prunable };
use crate::to_org::util::{mark_view_roots_independent, validate_birth_relationships};
use crate::dbs::memory::snapshot_global;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::time::Instant;
use typedb_driver::TypeDBDriver;

/// PURPOSE:
/// For each view affected by the save
/// (starting, importantly, with the saved view itself,
/// and only processing collateral views after that),
/// this updates the view and the memory.
/// .
/// ASSUMES:
/// the graph was already updated.
/// Updating views before updating the graph would be bad.
pub async fn update_views_after_save (
  stream                      : &mut std::net::TcpStream,
  saved_view                  : Tree<ViewNode>,
  save_instructions           : Vec<DefineNode>,
  merge_instructions          : &[Merge],
  diff_mode_enabled           : bool,
  config                      : &SkgConfig,
  typedb_driver               : &TypeDBDriver,
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &mut ConnectionState,
) -> Result<SaveResponse, Box<dyn Error>> {
  // PITFALL: Memory must already reflect every Save and Delete in
  // 'save_instructions' by the time this function runs. Violating
  // this invariant (e.g. by reordering the save pipeline so that
  // 'update_views_after_save' runs before 'apply_definenodes')
  // would let the rerender read stale NodeCompletes from memory.
  debug_assert! (
    memory_coherent_with_save_instructions (&save_instructions) . is_ok (),
    "update_views_after_save: in-Rust memory not coherent with save_instructions" );
  let source_diffs
    : Option<HashMap<SourceName, SourceDiff>>
    = if diff_mode_enabled
      { Some ( compute_diff_for_every_source (config)) }
      else {None};
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    source_diffs . as_ref()
    . map ( |d| deleted_ids_to_source (d))
    . unwrap_or_default();
  let mut deleted_by_this_save_pids : HashSet<ID> =
    // PITFALL: Can overlap deleted_since_head_pid_src_map, but neither is necessarily a subset of the other. If you delete something that you added since head, it will only be here. And if you deleted something since head but not in this save, it will only be there.
    save_instructions . iter()
    . filter_map( |instr| match instr {
      DefineNode::Delete (d) => Some( d . id . clone() ),
      _ => None })
    . collect();
  for m in merge_instructions {
    deleted_by_this_save_pids . insert (
      m . acquiree_to_delete . id . clone() ); }
  let mut errors : Vec<String> = Vec::new ();
  let mut saved_view_mut : Tree<ViewNode> = saved_view;
  let saved_text : String =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "rerender_view (saved)" ). entered();
      rerender_view (
        &mut saved_view_mut,
        &source_diffs,
        config,
        typedb_driver,
        &mut errors,
        &deleted_since_head_pid_src_map,
        &deleted_by_this_save_pids,
        true ) . await } ?;
  if let Ok (uri) = viewuri_from_request_result {
    conn_state . memory . update_view (
      uri, saved_view_mut);
    let collateral_uris : Vec<ViewUri> =
      find_collateral_view_uris (
        uri, &save_instructions, conn_state);
    if collateral_uris . is_empty () {
      tracing::debug!("update_views_after_save: no collateral views");
    } else {
      tracing::info!(
        "update_views_after_save: {} collateral view(s): {:?}",
        collateral_uris . len (),
        collateral_uris . iter ()
          . map ( |u| u . repr_in_client () )
          . collect::<Vec<_>> ()); }
    for curi in collateral_uris { // Same rerender_view pipeline as the saved view, but with is_saved_view=false. Each rerender fetches from the in-Rust memory directly. Streamed to Emacs immediately.
      let mut viewforest : Tree<ViewNode> = match
        conn_state . memory . viewuri_to_view (&curi) {
          Some (f) => f . clone (),
          None => {
            errors . push ( format! (
              "Collateral view {}: no viewforest found",
              curi . repr_in_client () ));
            continue; } };
      match { let _span : tracing::span::EnteredSpan =
                tracing::info_span!( "rerender_view (collateral)"
                ). entered();
        rerender_view (
          &mut viewforest,
          &source_diffs, config, typedb_driver,
          &mut errors, &deleted_since_head_pid_src_map,
          &deleted_by_this_save_pids,
          false ) . await }
      { Ok (text) => {
          conn_state . memory . update_view (&curi, viewforest);
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::CollateralView,
              & format_single_view_sexp (&curi, &text) )); },
        Err (e) => {
          errors . push ( format! (
            "Collateral view {}: {}",
            curi . repr_in_client (), e )); }} }}
  if let Some (w) = take_pending_audit_warning () {
    errors . insert (0, w); }
  Ok ( SaveResponse { saved_view : saved_text,
                       errors } ) }

/// Given the saved ViewUri and save instructions,
/// return the URIs of other views whose viewforests
/// contain any changed PID. Includes search views --
/// they are just as editable as other kinds.
fn find_collateral_view_uris (
  saved_uri         : &ViewUri,
  save_instructions : &[DefineNode],
  conn_state        : &ConnectionState,
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
    "find_collateral_view_uris: {} changed PIDs, {} views in memory",
    changed_pids . len (),
    conn_state . memory . views . len ());
  let uris : HashSet<ViewUri> =
    changed_pids . iter ()
    . flat_map ( |pid| conn_state . memory . views_containing (pid) )
    . filter ( |uri| uri != saved_uri )
    . collect ();
  uris . into_iter () . collect () }

/// Strip stale diff data, re-complete the viewforest,
/// set graph/view stats, and render to string.
pub async fn rerender_view (
  viewforest                         : &mut Tree<ViewNode>,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  config                         : &SkgConfig,
  typedb_driver                  : &TypeDBDriver,
  errors                         : &mut Vec<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &HashSet<ID>,
  is_saved_view                  : bool,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : Instant = Instant::now ();
  tracing::debug!("rerender_view: starting");
  strip_stale_diff_state (viewforest) ?;
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  tracing::debug!("rerender_view: starting complete_viewforest");
  complete_viewforest (
    viewforest, &mut defmap,
    source_diffs, config, typedb_driver,
    errors, deleted_since_head_pid_src_map,
    deleted_by_this_save_pids,
    is_saved_view ) . await ?;
  mark_view_roots_independent (viewforest);
  if let Some (snap) = snapshot_global () {
    // Correct any birth markers whose claimed relation to the
    // parent doesn't hold in memory (e.g. user moved a
    // birth=linksTo node under a new parent it doesn't link to).
    // No-op when the global graph handle isn't initialized (tests
    // that bypass startup).
    validate_birth_relationships (viewforest, &snap); }
  attach_containerward_ancestries_to_removedhere_phantoms (
    viewforest, config, typedb_driver ) . await ?;
  tracing::debug!("rerender_view: complete_viewforest done ({:.3}s), starting graphnodestats",
            t_rerender . elapsed () . as_secs_f64 ());
  let ( container_to_contents, content_to_containers ) =
    set_graphnodestats_in_viewforest (
      viewforest, config, typedb_driver ) . await ?;
  tracing::debug!("rerender_view: graphnodestats done ({:.3}s), rendering to string",
            t_rerender . elapsed () . as_secs_f64 ());
  set_viewnodestats_in_viewforest (
    viewforest,
    &container_to_contents,
    &content_to_containers,
    config );
  let result : Result<String, Box<dyn Error>> =
    viewforest_to_string (viewforest, config);
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  result }

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
/// diff mode) are cleaned up by complete_alias_col during the postorder
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
  viewforest        : &mut Tree<ViewNode>,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let phantom_nodes : Vec<(NodeId, ID)> = {
    let mut result : Vec<(NodeId, ID)> = Vec::new ();
    for edge in viewforest . root () . traverse () {
      if let ego_tree::iter::Edge::Open (node_ref) = edge {
        if let ViewNodeKind::True (t) = &node_ref . value () . kind {
          if t . is_removedhere_phantom () {
            result . push (
              ( node_ref . id (),
                t . id . clone () )); }} }}
    result };
  if phantom_nodes . is_empty () { return Ok (( )); }
  let ids : Vec<ID> =
    phantom_nodes . iter ()
    . map ( |( _, id )| id . clone () )
    . collect ();
  let ancestry_map : HashMap<ID, AncestryTree> =
    ancestry_by_id_from_ids_async (
      &ids, &config.db_name, typedb_driver,
      config.max_ancestry_depth
    ) . await;
  for ( phantom_nid, pid ) in phantom_nodes {
    if let Some (ancestry) = ancestry_map . get (&pid) {
      if let AncestryTree::Inner ( _, children ) = ancestry {
        for child in children . iter () . rev () {
          insert_containerward_ancestry_tree_recursive (
            child, phantom_nid,
            viewforest, config, typedb_driver
          ) . await ?; }} }}
  Ok (( )) }
