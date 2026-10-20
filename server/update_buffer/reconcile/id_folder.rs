use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::tree::generic::error_unless_node_satisfies;
use crate::update_buffer::ancestry::pid_and_skgrepo_from_required_ancestor;
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::{ChangeFacts, PropertyFolder, Property};
use crate::update_buffer::util::complete_relevant_children_in_viewforest;
use ego_tree::{NodeId, Tree};
use std::error::Error;

/// Reconciles an IDFolder's children against
///   the IDs on disk (via the map) for its parent UnrestrictedVognode.
///
/// - Verify this node is an IDFolder
/// - Verify its parent is an UnrestrictedVognode
/// - Fetch the corresponding Graphnode from the map
/// - Read its IDs into a goal list
/// - Reconcile children via complete_relevant_children_in_viewforest
pub fn reconcile_idFolder_children (
  idfolder_treeid : NodeId,
  tree            : &mut Tree<Viewnode>,
  graph           : &InRustGraph,
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
  let goal_list : Vec<ID> =
    parent_graphnode . all_skgids()
      . cloned()
      . collect();
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
    Ok ( Viewnode {
      focused         : false,
      org_folded      : false,
      org_body_folded : false,
      kind            : ViewnodeKind::Property (
        Property::ID {
          skgid: skgid . clone(),
          change_facts: ChangeFacts::default (),
          readiness_request: None } ) } ) };
  complete_relevant_children_in_viewforest(
    tree,
    idfolder_treeid,
    is_skgid,
    view_id_text,
    &goal_list,
    create_skgid )?;
  Ok( () ) }
