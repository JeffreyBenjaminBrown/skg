use crate::types::git::{RelationshipAxes, NodeChanges};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_repo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, RepoName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::git::{RepoDiff, axes_from_per_stage_diffs, per_stage_node_changes_for_activeNode};
use crate::types::tree::generic::error_unless_node_satisfies;
use crate::update_buffer::ancestry::pid_and_repo_from_required_ancestor;
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::viewnode::{QualFolder, Qual};
use crate::update_buffer::util::complete_relevant_children_in_viewnodetree;
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an IDFolder's children against
///   the IDs on disk (via the map) for its parent ActiveNode.
///
/// - Verify this node is an IDFolder
/// - Verify its parent is an ActiveNode
/// - Fetch the corresponding NodeComplete from the map
/// - Read its IDs into a goal list
/// - In diff view, also build a diff-status map from NodeChanges.ids_diff
/// - Reconcile children via complete_relevant_children_in_viewnodetree
pub fn reconcile_idFolder_children (
  idfolder_node_id : NodeId,
  tree          : &mut Tree<ViewNode>,
  graph         : &InRustGraph,
  repo_diffs  : &Option<HashMap<RepoName, RepoDiff>>,
  config        : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, idfolder_node_id,
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::QualFolder (QualFolder::ID) ),
    "reconcile_idFolder_children: Node is not an IDFolder" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: parent Active vognode read through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 0).
  let (parent_pid, parent_repo) : (ID, RepoName) =
    pid_and_repo_from_required_ancestor(
      tree, idfolder_node_id, 0,
      "reconcile_idFolder_children" ) ?;
  let parent_nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_repo (
      graph, config, &parent_pid, &parent_repo )
    . map_err ( |_| "reconcile_idFolder_children: parent NodeComplete not found" ) ?;
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_activeNode (
      repo_diffs, &parent_pid, &parent_repo );
  let (goal_list, axes_map)
    : (Vec<ID>, HashMap<ID, RelationshipAxes>) =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      // No git diff view, or no changes for this file in either stage.
      let goals : Vec<ID> =
        parent_nodecomplete . all_ids()
          . cloned()
          . collect();
      ( goals, HashMap::new() )
    } else {
      let merged : Vec<(ID, RelationshipAxes)> =
        axes_from_per_stage_diffs (
          staged_nc   . map ( |c| c . ids_diff . as_slice () ),
          unstaged_nc . map ( |c| c . ids_diff . as_slice () ) );
      let goals : Vec<ID> =
        merged . iter () . map ( |(id, _)| id . clone () ) . collect ();
      let amap : HashMap<ID, RelationshipAxes> =
        merged . into_iter () . collect ();
      ( goals, amap ) };
  let is_id : fn (&ViewNode) -> bool =
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::Qual (Qual::ID { .. } ) );
  let view_id_text : fn (&ViewNode) -> Result<ID, String> =
    |viewnode| match &viewnode . kind {
      ViewNodeKind::Qual (Qual::ID { id, .. } ) =>
        Ok ( id . clone() ),
      _ => Err ( "reconcile_idFolder_children: relevant child is not an ID scaffold"
                 . to_string() ), };
  let create_id = |id: &ID| -> Result<ViewNode, String> {
    let relationship_axes : RelationshipAxes =
      axes_map . get (id) . copied () . unwrap_or_default ();
    Ok ( ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::Qual (
        Qual::ID {
          id: id . clone(), relationship_axes } ) } ) };
  complete_relevant_children_in_viewnodetree(
    tree,
    idfolder_node_id,
    is_id,
    view_id_text,
    &goal_list,
    create_id )?;
  Ok( () ) }
