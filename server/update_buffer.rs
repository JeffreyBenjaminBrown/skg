pub mod complete;
pub mod complete_child_first;
pub mod complete_parent_first;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use complete::complete_viewtree;
pub use graphnodestats::set_graphnodestats_in_forest;
pub use viewnodestats::set_viewnodestats_in_forest;

use crate::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use crate::org_to_text::viewnode_forest_to_string;
use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{ SaveResponse, compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{ format_single_view_sexp, send_response_with_length_prefix, tag_sexp_response};
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{SourceDiff, NodeDiffStatus};
use crate::types::memory::ViewUri;
use crate::types::memory::{SkgNodeMap, skgnode_map_from_save_instructions};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, Merge, SaveNode};
use crate::types::skgnode::SkgNode;
use crate::types::tree::generic::{ do_everywhere_in_tree_dfs, do_everywhere_in_tree_dfs_prunable };
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
  saveview_skgnodes_pre_save  : SkgNodeMap,
  diff_mode_enabled           : bool,
  config                      : &SkgConfig,
  typedb_driver               : &TypeDBDriver,
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &mut ConnectionState,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut skgnode_map : SkgNodeMap = {
    let mut it : SkgNodeMap =
      skgnode_map_from_save_instructions (& save_instructions);
    for (pid, skgnode) in saveview_skgnodes_pre_save {
      it . entry (pid) // The 'or_insert_with' means user edits from skgnode_map_from_save_instructions are given priority.
        . or_insert_with ( || skgnode ); }
    it };
  for m in merge_instructions {
    // Reload acquirers from disk so the map reflects the post-merge .skg files.
    let pid    : &ID         = &m . updated_acquirer . 0 . pid;
    let source : &SourceName = &m . updated_acquirer . 0 . source;
    let skgnode : SkgNode = skgnode_from_pid_and_source (
      config, pid . clone(), source ) ?;
    skgnode_map . insert ( pid . clone(), skgnode ); }
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
        &mut skgnode_map,
        &source_diffs,
        config,
        typedb_driver,
        &mut errors,
        &deleted_since_head_pid_src_map,
        &deleted_by_this_save_pids,
        true ) . await } ?;
  if let Ok (uri) = viewuri_from_request_result {
    merge_skgnodemap_into_pool (&skgnode_map, conn_state);
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
    for curi in collateral_uris { // Same rerender_view pipeline as the saved view, but with is_saved_view=false. Each view builds on the pool enriched by prior views, and is streamed to Emacs immediately.
      let mut forest : Tree<ViewNode> = match
        conn_state . memory . viewuri_to_view (&curi) {
          Some (f) => f . clone (),
          None => {
            errors . push ( format! (
              "Collateral view {}: no viewforest found",
              curi . repr_in_client () ));
            continue; } };
      let mut map : SkgNodeMap =
        seed_skgnodemap_from_pool (&curi, conn_state);
      match { let _span : tracing::span::EnteredSpan =
                tracing::info_span!( "rerender_view (collateral)"
                ). entered();
        rerender_view (
          &mut forest, &mut map,
          &source_diffs, config, typedb_driver,
          &mut errors, &deleted_since_head_pid_src_map,
          &deleted_by_this_save_pids,
          false ) . await }
      { Ok (text) => {
          merge_skgnodemap_into_pool (&map, conn_state);
          conn_state . memory . update_view (&curi, forest);
          send_response_with_length_prefix (
            stream,
            & tag_sexp_response (
              TcpToClient::CollateralView,
              & format_single_view_sexp (&curi, &text) )); },
        Err (e) => {
          errors . push ( format! (
            "Collateral view {}: {}",
            curi . repr_in_client (), e )); }} }}
  Ok ( SaveResponse { saved_view : saved_text,
                       errors } ) }

/// Given the saved ViewUri and save instructions,
/// return the URIs of other views whose forests
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

/// Build a SkgNodeMap for a view by pulling every PID
/// in that view's forest from the pool.
pub fn seed_skgnodemap_from_pool (
  uri        : &ViewUri,
  conn_state : &ConnectionState,
) -> SkgNodeMap {
  let mut it : SkgNodeMap = SkgNodeMap::new ();
  for pid in conn_state . memory . viewuri_to_pids (uri) {
    if let Some (skgnode)
      = conn_state . memory . pool . get (&pid)
      { it . insert ( pid, skgnode . clone () ); } }
  it }

/// Merge a completed SkgNodeMap into the pool.
pub fn merge_skgnodemap_into_pool (
  map        : &SkgNodeMap,
  conn_state : &mut ConnectionState,
) {
  for (pid, skgnode) in map {
    conn_state . memory . pool . insert (
      pid . clone (), skgnode . clone () ); } }

/// Strip stale diff data, re-complete the viewtree,
/// set graph/view stats, and render to string.
pub async fn rerender_view (
  forest                         : &mut Tree<ViewNode>,
  map                            : &mut SkgNodeMap,
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
  remove_branches_that_git_marked_removed (forest) ?;
  remove_diff_only_scaffolds (forest) ?;
  clear_diff_metadata (forest) ?;
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  tracing::debug!("rerender_view: starting complete_viewtree");
  complete_viewtree (
    forest, map, &mut defmap,
    source_diffs, config, typedb_driver,
    errors, deleted_since_head_pid_src_map,
    deleted_by_this_save_pids,
    is_saved_view ) . await ?;
  tracing::debug!("rerender_view: complete_viewtree done ({:.3}s), starting graphnodestats",
            t_rerender . elapsed () . as_secs_f64 ());
  let ( container_to_contents, content_to_containers ) =
    set_graphnodestats_in_forest (
      forest, map, config, typedb_driver ) . await ?;
  tracing::debug!("rerender_view: graphnodestats done ({:.3}s), rendering to string",
            t_rerender . elapsed () . as_secs_f64 ());
  set_viewnodestats_in_forest (
    forest,
    &container_to_contents,
    &content_to_containers,
    config );
  let result : Result<String, Box<dyn Error>> =
    viewnode_forest_to_string (forest, config);
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  result }

/// Strip from the forest every branch whose root
///   is marked as removed or removed-here.
/// Exception: Does not strip:
///   - top-level branches (children of BufferRoot)
///   - non-content nodes (birth != ContentOf)
/// Uses single-pass DFS: removes branch roots as encountered,
/// skipping recursion into removed branches.
pub fn remove_branches_that_git_marked_removed (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_forest_root_child : bool = {
        match node . parent() {
          Some (mut p) =>
            matches! ( &p . value() . kind,
                       ViewNodeKind::Scaff (Scaffold::BufferRoot)),
          None => false } };
      let should_remove : bool =
        match &node . value() . kind {
          ViewNodeKind::True (t) => {
            matches! ( t . diff,
                       Some (NodeDiffStatus::Removed) |
                       Some (NodeDiffStatus::RemovedHere))
            && ! is_forest_root_child
            && ! t . parent_ignores_it() },
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
/// diff mode) are cleaned up by completeAliasCol during the postorder
/// pass: its goal list won't include them, so they are detached.
pub fn remove_diff_only_scaffolds (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs_prunable (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<bool, String> {
      let is_diff_scaffold : bool =
        matches! ( &node . value() . kind,
          ViewNodeKind::Scaff (Scaffold::TextChanged) |
          ViewNodeKind::Scaff (Scaffold::IDCol) );
      if is_diff_scaffold {
        node . detach();
        Ok (false) // pruned — don't recurse into detached children
      } else { Ok (true) } } ) ?;
  Ok (( )) }

/// Clear diff metadata from all TrueNodes in the forest.
/// Diff-only scaffolds (TextChanged, IDCol) are
/// removed by 'remove_diff_only_scaffolds' before this runs.
pub fn clear_diff_metadata (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<(), String>
      { // Ignores scaffolds: some (Alias, ID) carry diff data,
        // but they are regenerated from scratch by their postorder
        // completers, so clearing them here is unnecessary.
        if let ViewNodeKind::True (t)
          = &mut node . value() . kind
          { t . diff = None; }
        Ok (( )) } ) ?;
  Ok (( )) }
