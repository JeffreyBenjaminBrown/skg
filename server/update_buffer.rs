pub mod complete;
pub mod complete_child_first;
pub mod complete_parent_first;
pub mod graphnodestats;
pub mod util;
pub mod viewnodestats;

pub use complete::complete_viewtree;
pub use graphnodestats::set_graphnodestats_in_forest;
pub use viewnodestats::set_viewnodestats_in_forest;

use crate::org_to_text::viewnode_forest_to_string;
use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{
  SaveResponse,
  compute_diff_for_every_source, deleted_ids_to_source};
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{SourceDiff, NodeDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::memory::{SkgNodeMap, skgnode_map_from_save_instructions};
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs,
  do_everywhere_in_tree_dfs_prunable};
use crate::types::memory::ViewUri;
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold};

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
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
  saved_view                  : Tree<ViewNode>,
  save_instructions           : Vec<DefineNode>,
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
  let source_diffs
    : Option<HashMap<SourceName, SourceDiff>>
    = if diff_mode_enabled
      { Some ( compute_diff_for_every_source (config)) }
      else {None};
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    source_diffs . as_ref()
    . map ( |d| deleted_ids_to_source (d))
    . unwrap_or_default();
  let deleted_by_this_save_pids : HashSet<ID> = // PITFALL: Can overlap deleted_since_head_pid_src_map, but neither is necessarily a subset of the other. If you delete something that you added since head, it will only be here. And if you deleted something since head but not in this save, it will only be there.
    save_instructions . iter()
    . filter_map( |instr| match instr {
      DefineNode::Delete (d) => Some( d . id . clone() ),
      _ => None })
    . collect();
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
        &deleted_by_this_save_pids ) . await } ?;
  let mut collateral_views : Vec<(ViewUri, String)> = Vec::new ();
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
    for curi in collateral_uris { // Identical pipeline, with each view building on the pool enriched by prior views.
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
          &deleted_by_this_save_pids ) . await }
      { Ok (text) => {
          merge_skgnodemap_into_pool (&map, conn_state);
          conn_state . memory . update_view (&curi, forest);
          collateral_views . push (( curi, text )); },
        Err (e) => {
          errors . push ( format! (
            "Collateral view {}: {}",
            curi . repr_in_client (), e )); } } } }
  Ok ( SaveResponse { saved_view : saved_text,
                       errors, collateral_views } ) }

/// Given the saved ViewUri and save instructions,
/// return the URIs of other views whose forests
/// contain any changed PID.
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
    . filter ( |uri| ! uri . is_search () ) // Search views don't need collateral updates: the search results are fixed until the user searches again.
    . collect ();
  uris . into_iter () . collect () }

/// Build a SkgNodeMap for a view by pulling every PID
/// in that view's forest from the pool.
fn seed_skgnodemap_from_pool (
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
fn merge_skgnodemap_into_pool (
  map        : &SkgNodeMap,
  conn_state : &mut ConnectionState,
) {
  for (pid, skgnode) in map {
    conn_state . memory . pool . insert (
      pid . clone (), skgnode . clone () ); } }

/// Strip stale diff data, re-complete the viewtree,
/// set graph/view stats, and render to string.
async fn rerender_view (
  forest                         : &mut Tree<ViewNode>,
  map                            : &mut SkgNodeMap,
  source_diffs                   : &Option<HashMap<SourceName, SourceDiff>>,
  config                         : &SkgConfig,
  typedb_driver                  : &TypeDBDriver,
  errors                         : &mut Vec<String>,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  deleted_by_this_save_pids      : &HashSet<ID>,
) -> Result<String, Box<dyn Error>> {
  let t_rerender : std::time::Instant = std::time::Instant::now ();
  tracing::debug!("rerender_view: starting");
  remove_branches_that_git_marked_removed (forest) ?;
  clear_diff_metadata (forest) ?;
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  tracing::debug!("rerender_view: starting complete_viewtree");
  complete_viewtree (
    forest, map, &mut defmap,
    source_diffs, config, typedb_driver,
    errors, deleted_since_head_pid_src_map,
    deleted_by_this_save_pids ) . await ?;
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
    &content_to_containers );
  let result : Result<String, Box<dyn Error>> =
    viewnode_forest_to_string (forest);
  tracing::debug!("rerender_view: done ({:.3}s)",
            t_rerender . elapsed () . as_secs_f64 ());
  result }

/// Strip from the forest every branch whose root
///   is marked as removed or removed-here.
/// Exception: Does not strip:
///   - top-level branches (children of BufferRoot)
///   - parent_ignores nodes
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
            && ! t . parent_ignores },
          _ => false };
      if should_remove {
        node . detach();
        Ok (false) // Prune: branch removed, so don't recurse
      } else { Ok (true) }} )? ; // recurse into children
  Ok (( )) }

/// Clear diff metadata from all TrueNodes in the forest.
/// Scaffolds with diff fields (Alias, ID) are already removed
/// by remove_diff_only_scaffolds before this runs.
pub fn clear_diff_metadata (
  forest : &mut Tree<ViewNode>
) -> Result<(), Box<dyn Error>> {
  let forest_root_id : NodeId =
    forest . root() . id();
  do_everywhere_in_tree_dfs (
    forest,
    forest_root_id,
    &mut |mut node : NodeMut<ViewNode>| -> Result<(), String>
      { // IGNORES scaffolds, even though some scaffolds *can* have diff data. Since all such kinds are regenerated from scratch, they don't need processing here. See remove_regenerable_scaffolds.
        if let ViewNodeKind::True (t)
          = &mut node . value() . kind
          { t . diff = None; }
        Ok (( )) } ) ?;
  Ok (( )) }
