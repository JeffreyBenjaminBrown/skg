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
  SaveResult, SaveResponse,
  compute_diff_for_every_source, deleted_ids_to_source};
use crate::serve::timing_log::timed_async;
use crate::to_org::util::DefinitiveMap;
use crate::types::git::{SourceDiff, NodeDiffStatus};
use crate::types::misc::{ID, SourceName, SkgConfig};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::memory::{SkgNodeMap, skgnode_map_from_save_instructions};
use crate::types::tree::generic::{
  do_everywhere_in_tree_dfs,
  do_everywhere_in_tree_dfs_prunable};
use crate::types::viewnode::{ViewNode, ViewNodeKind, Scaffold, ViewUri};

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// After updating the graph (save instructions + merges),
/// rerender the saved view, update memory, and
/// rerender collateral views.
pub async fn update_views_after_save (
  saved_view                  : Tree<ViewNode>, // other, collateral views might be adjusted too
  save_instructions           : Vec<DefineNode>,
  saveview_skgnodes_pre_save  : SkgNodeMap,
  diff_mode_enabled           : bool,
  config                      : &SkgConfig,
  typedb_driver               : &TypeDBDriver,
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &mut ConnectionState,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut errors : Vec < String > = Vec::new ();
  let mut skgnode_map : SkgNodeMap = {
    let mut it : SkgNodeMap =
      skgnode_map_from_save_instructions (& save_instructions);
    for (pid, skgnode) in saveview_skgnodes_pre_save { // Use these to fill in missing values, giving preference to the user's edits.
      it . entry (pid)
        . or_insert_with ( || skgnode ); }
    it };
  let source_diffs // Used to execute view requests.
    : Option<HashMap<SourceName, SourceDiff>>
    = if diff_mode_enabled
      { Some ( compute_diff_for_every_source (config)) }
      else {None};
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    // only nonempty when git diff view is enabled
    source_diffs . as_ref()
    . map ( |d| deleted_ids_to_source (d))
    . unwrap_or_default();
  let deleted_by_this_save_pids : HashSet<ID> = // PITFALL: Can overlap deleted_since_head_pid_src_map, but neither is necessarily a subset of the other. If you delete something that you added since head, it will only be here. And if you deleted something since head but not in this save, it will only be there.
    save_instructions . iter()
    . filter_map( |instr| match instr {
      DefineNode::Delete( d ) => Some( d.id.clone() ),
      _ => None })
    . collect();
  let mut saved_view_mut : Tree<ViewNode> = saved_view;
  let buffer_content : String =
    timed_async ( config, "rerender_view",
                  rerender_view (
                    &mut saved_view_mut,
                    &mut skgnode_map,
                    &source_diffs,
                    config,
                    typedb_driver,
                    &mut errors,
                    &deleted_since_head_pid_src_map,
                    &deleted_by_this_save_pids )) . await ?;
  let save_result : SaveResult = SaveResult {
    response : SaveResponse {
      saved_view : buffer_content, errors,
      collateral_views : Vec::new () },
    skgnodemap_after_completion : skgnode_map,
    save_instructions,
    completed_forest : saved_view_mut };
  let collateral_views : Vec<(ViewUri, String)> =
    update_memory_for_saved_view (
      viewuri_from_request_result,
      &save_result,
      conn_state,
      typedb_driver,
      config ). await;
  Ok ( SaveResponse {
    saved_view       : save_result . response . saved_view,
    errors           : save_result . response . errors,
    collateral_views } ) }

/// Update the SkgnodesInMemory's pool of skgnodes,
/// the view of the saved buffer, and the collateral views.
/// Returns empty if viewuri_from_request_result was Err.
/// TODO : On error, should instead
/// unwind the entire save and report the error.
async fn update_memory_for_saved_view (
  viewuri_from_request_result : &Result<ViewUri, String>,
  save_result                 : &SaveResult,
  conn_state                  : &mut ConnectionState,
  typedb_driver               : &TypeDBDriver,
  config                      : &SkgConfig,
) -> Vec<(ViewUri, String)> {
  let view_uri : &ViewUri = match viewuri_from_request_result {
    Ok ( uri ) => uri,
    Err ( _ ) =>
      return Vec::new () };
  for (pid, skgnode) // update the skgnode pool
    in &save_result . skgnodemap_after_completion
    { conn_state . memory . pool . insert ( pid . clone (),
                                            skgnode . clone () ); }
  conn_state . memory . update_view (
    view_uri,
    save_result . completed_forest . clone () );
  rerender_collateral_views (
    view_uri,
    save_result,
    conn_state,
    typedb_driver,
    config ). await }

/// When the user saves a buffer, the nodes they edited
/// might also appear in other open Emacs buffers. Those are
/// "collateral views", which this updates.
async fn rerender_collateral_views (
  view_uri      : &ViewUri,
  save_result   : &SaveResult,
  conn_state    : &mut ConnectionState,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> Vec<(ViewUri, String)> {
  let changed_pids : HashSet<ID> =
    save_result . save_instructions . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Save ( SaveNode (n)) =>
        n . ids . first () . cloned (),
      DefineNode::Delete ( dn ) =>
        Some ( dn . id . clone () ) } )
    . collect ();
  let deleted_by_this_save_pids : HashSet<ID> =
    save_result . save_instructions . iter ()
    . filter_map ( |instr| match instr {
      DefineNode::Delete ( dn ) =>
        Some ( dn . id . clone () ),
      _ => None } )
    . collect ();
  let collateral_views : HashSet<ViewUri> =
    changed_pids . iter ()
    . flat_map ( |pid| conn_state . memory . views_containing (pid) )
    . filter ( |uri| // this, the saved view, is already up to date
               uri != view_uri )
    . collect ();
  let mut updates : Vec<(ViewUri, String)> =
    Vec::new ();
  for uri in collateral_views {
    match complete_collateral_view (
      &uri, conn_state, typedb_driver,
      config, &deleted_by_this_save_pids ). await
    { Ok ( (uri, text) ) =>
        { updates . push ( (uri, text) ); },
      Err (e) =>
        { eprintln! (
            "Warning: Failed to complete collateral view: {}",
            e ); }} }
  updates }

/// Complete a single collateral view by re-running the
/// completion pipeline on the stored viewforest.
/// Each Viewnode with a PID in deleted_by_this_save_pids,
/// is degraded to DeletedNode.
/// .
/// TODO | PITFALL:
/// The server completes collateral views from its stored
/// viewforest rather than requesting the buffer from Emacs.
/// This works correctly only if the user adheres to the
/// recommendation that at most one buffer has unsaved
/// changes at a time. In the future, hopefully, this
/// recommendation will be retracted, and we will instead
/// update by parsing the raw buffer text of the collateral view.
async fn complete_collateral_view (
  uri           : &ViewUri,
  conn_state    : &mut ConnectionState,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  deleted_by_this_save_pids  : &HashSet<ID>,
) -> Result<(ViewUri, String), Box<dyn Error>> {
  let mut viewforest : Tree<ViewNode> =
    conn_state . memory . viewuri_to_view (uri)
    . ok_or_else ( || format! (
      "complete_collateral_view: no viewforest for {}", uri.0 )) ?
    . clone ();
  let mut skgnodemap : SkgNodeMap = {
    let mut it : SkgNodeMap = SkgNodeMap::new ();
    for pid in conn_state . memory . viewuri_to_pids ( uri ) {
      if let Some (skgnode)
        = conn_state . memory . pool . get (&pid)
        { it . insert ( pid, skgnode . clone () ); } }
    it };
  let source_diffs : Option<HashMap<SourceName, SourceDiff>> =
    if conn_state . diff_mode_enabled
    { Some ( compute_diff_for_every_source ( config )) }
    else { None };
  let deleted_since_head_pid_src_map : HashMap<ID, SourceName> =
    source_diffs . as_ref ()
    . map ( |d| deleted_ids_to_source ( d ))
    . unwrap_or_default ();
  let text : String = {
    let mut errors : Vec<String> = Vec::new ();
    rerender_view (
      &mut viewforest, &mut skgnodemap,
      &source_diffs, config, typedb_driver,
      &mut errors, &deleted_since_head_pid_src_map,
      deleted_by_this_save_pids ). await ? };
  for (pid, skgnode) in skgnodemap {
    conn_state . memory . pool . insert (
      pid, skgnode ); }
  conn_state . memory . update_view (
    uri, viewforest );
  Ok (( uri . clone (),
        text )) }

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
  remove_branches_that_git_marked_removed (forest) ?;
  clear_diff_metadata (forest) ?;
  let mut defmap : DefinitiveMap = DefinitiveMap::new ();
  complete_viewtree (
    forest, map, &mut defmap,
    source_diffs, config, typedb_driver,
    errors, deleted_since_head_pid_src_map,
    deleted_by_this_save_pids ). await ?;
  let ( container_to_contents, content_to_containers ) =
    set_graphnodestats_in_forest (
      forest, map, config, typedb_driver ). await ?;
  set_viewnodestats_in_forest (
    forest,
    &container_to_contents,
    &content_to_containers );
  viewnode_forest_to_string (forest) }

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
          Some ( mut p ) =>
            matches! ( &p . value() . kind,
                       ViewNodeKind::Scaff ( Scaffold::BufferRoot )),
          None => false } };
      let should_remove : bool =
        match &node . value() . kind {
          ViewNodeKind::True (t) => {
            matches! ( t . diff,
                       Some ( NodeDiffStatus::Removed ) |
                       Some ( NodeDiffStatus::RemovedHere ))
            && ! is_forest_root_child
            && ! t . parent_ignores },
          _ => false };
      if should_remove {
        node . detach();
        Ok ( false ) // Prune: branch removed, so don't recurse
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
