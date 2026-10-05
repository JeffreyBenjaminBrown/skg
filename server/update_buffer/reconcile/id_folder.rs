use crate::types::git::{RelationshipAxes, NodeChanges};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::git::{SkgrepoDiff, axes_from_per_stage_diffs, per_stage_node_changes_for_unrestrictedVognode};
use crate::types::tree::generic::error_unless_node_satisfies;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::{PropertyFolder, Property};
use crate::update_buffer::util::complete_relevant_children_in_viewforest;
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an IDFolder's children against
///   the IDs on disk (via the map) for its parent UnrestrictedVognode.
///
/// - Verify this node is an IDFolder
/// - Verify its parent is an UnrestrictedVognode
/// - Fetch the corresponding Graphnode from the map
/// - Read its IDs into a goal list
/// - In diff view, also build a diff-status map from NodeChanges.ids_diff
/// - Reconcile children via complete_relevant_children_in_viewforest
pub fn reconcile_idFolder_children (
  idfolder_treeid : NodeId,
  tree            : &mut Tree<Viewnode>,
  graph           : &InRustGraph,
  skgrepo_diffs   : &Option<HashMap<SkgrepoName, SkgrepoDiff>>,
  config          : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, idfolder_treeid,
    |viewnode| matches!( &viewnode . kind,
                         ViewnodeKind::PropertyFolder (PropertyFolder::ID) ),
    "reconcile_idFolder_children: Node is not an IDFolder" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: parent Unrestricted vognode read through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 0).
  let (parent_pid, parent_skgrepo) : (ID, SkgrepoName) =
    pid_and_skgrepo_from_required_ancestor(
      tree, idfolder_treeid, 0,
      "reconcile_idFolder_children" ) ?;
  let parent_graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph, config, &parent_pid, &parent_skgrepo )
    . map_err ( |_| "reconcile_idFolder_children: parent Graphnode not found" ) ?;
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_unrestrictedVognode (
      skgrepo_diffs, &parent_pid, &parent_skgrepo );
  let (goal_list, axes_map)
    : (Vec<ID>, HashMap<ID, RelationshipAxes>) =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      // No git diff view, or no changes for this file in either stage.
      let goals : Vec<ID> =
        parent_graphnode . all_skgids()
          . cloned()
          . collect();
      ( goals, HashMap::new() )
    } else {
      let merged : Vec<(ID, RelationshipAxes)> =
        axes_from_per_stage_diffs (
          staged_nc   . map ( |c| c . ids_diff . as_slice () ),
          unstaged_nc . map ( |c| c . ids_diff . as_slice () ) );
      let goals : Vec<ID> =
        merged . iter () . map ( |(skgid, _)| skgid . clone () ) . collect ();
      let amap : HashMap<ID, RelationshipAxes> =
        merged . into_iter () . collect ();
      ( goals, amap ) };
  let is_skgid : fn (&Viewnode) -> bool =
    |viewnode| matches!( &viewnode . kind,
                         ViewnodeKind::Property (Property::ID { .. } ) );
  let view_id_text : fn (&Viewnode) -> Result<ID, String> =
    |viewnode| match &viewnode . kind {
      ViewnodeKind::Property (Property::ID { skgid, .. } ) =>
        Ok ( skgid . clone() ),
      _ => Err ( "reconcile_idFolder_children: relevant child is not an ID property"
                 . to_string() ), };
  let create_skgid = |skgid: &ID| -> Result<Viewnode, String> {
    let relationship_axes : RelationshipAxes =
      axes_map . get (skgid) . copied () . unwrap_or_default ();
    Ok ( Viewnode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewnodeKind::Property (
        Property::ID {
          skgid: skgid . clone(), relationship_axes } ) } ) };
  complete_relevant_children_in_viewforest(
    tree,
    idfolder_treeid,
    is_skgid,
    view_id_text,
    &goal_list,
    create_skgid )?;
  Ok( () ) }
