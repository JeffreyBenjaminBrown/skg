use crate::types::git::{MembershipAxes, NodeChanges};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::git::{SourceDiff, axes_from_per_stage_diffs, per_stage_node_changes_for_truenode};
use crate::types::tree::generic::error_unless_node_satisfies;
use crate::update_buffer::ancestry::pid_and_source_from_required_ancestor;
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::viewnode::{QualCol, Qual};
use crate::update_buffer::util::complete_relevant_children_in_viewnodetree;
use ego_tree::{NodeId, Tree};
use std::collections::HashMap;
use std::error::Error;

/// Reconciles an IDCol's children against
///   the IDs on disk (via the map) for its parent TrueNode.
///
/// - Verify this node is an IDCol
/// - Verify its parent is a TrueNode
/// - Fetch the corresponding NodeComplete from the map
/// - Read its IDs into a goal list
/// - In diff view, also build a diff-status map from NodeChanges.ids_diff
/// - Reconcile children via complete_relevant_children_in_viewnodetree
pub fn reconcile_id_col_children (
  idcol_node_id : NodeId,
  tree          : &mut Tree<ViewNode>,
  source_diffs  : &Option<HashMap<SourceName, SourceDiff>>,
  config        : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  error_unless_node_satisfies(
    tree, idcol_node_id,
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::QualCol (QualCol::ID) ),
    "reconcile_id_col_children: Node is not an IDCol" )
    . map_err( |e| -> Box<dyn Error> { e . into() } )?;
  // TODO/DONE/local-view-update/propagate-death-leafward/plan.org §4: parent Normal vognode read through the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 ancestry table (index 0).
  let (parent_pid, parent_source) : (ID, SourceName) =
    pid_and_source_from_required_ancestor(
      tree, idcol_node_id, 0,
      "reconcile_id_col_children" ) ?;
  let parent_nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source (
      config, &parent_pid, &parent_source )
    . map_err ( |_| "reconcile_id_col_children: parent NodeComplete not found" ) ?;
  let (staged_nc, unstaged_nc)
    : (Option<&NodeChanges>, Option<&NodeChanges>) =
    per_stage_node_changes_for_truenode (
      source_diffs, &parent_pid, &parent_source );
  let (goal_list, axes_map)
    : (Vec<ID>, HashMap<ID, MembershipAxes>) =
    if staged_nc . is_none () && unstaged_nc . is_none () {
      // No git diff view, or no changes for this file in either stage.
      let goals : Vec<ID> =
        parent_nodecomplete . all_ids()
          . cloned()
          . collect();
      ( goals, HashMap::new() )
    } else {
      let merged : Vec<(ID, MembershipAxes)> =
        axes_from_per_stage_diffs (
          staged_nc   . map ( |c| c . ids_diff . as_slice () ),
          unstaged_nc . map ( |c| c . ids_diff . as_slice () ) );
      let goals : Vec<ID> =
        merged . iter () . map ( |(id, _)| id . clone () ) . collect ();
      let amap : HashMap<ID, MembershipAxes> =
        merged . into_iter () . collect ();
      ( goals, amap ) };
  let is_id : fn (&ViewNode) -> bool =
    |viewnode| matches!( &viewnode . kind,
                         ViewNodeKind::Qual (Qual::ID { .. } ) );
  let view_id_text : fn (&ViewNode) -> ID =
    |viewnode| match &viewnode . kind {
      ViewNodeKind::Qual (Qual::ID { id, .. } ) =>
        id . clone(),
      _ => unreachable!(), };
  let create_id = |id: &ID| -> ViewNode {
    let membership : MembershipAxes =
      axes_map . get (id) . copied () . unwrap_or_default ();
    ViewNode {
      focused     : false,
      folded      : false,
      body_folded : false,
      kind        : ViewNodeKind::Qual (
        Qual::ID {
          id: id . clone(), membership } ) } };
  complete_relevant_children_in_viewnodetree(
    tree,
    idcol_node_id,
    is_id,
    view_id_text,
    &goal_list,
    create_id )?;
  Ok( () ) }
