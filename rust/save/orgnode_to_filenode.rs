// PURPOSE:
// Translates the OrgNode type to the FileNode type.

// PITFALL:
// The correspondence between those two types is imperfect, so it is as if the OrgNode is transformed before translation, in two respects:
//   (1) Since the ID of an OrgNode is optional and the ID of a FileNode is mandatory, anything without an ID is assigned one, at random.
//   (2) If an ID is repeated, the first node to contain it is processed normally, but all future nodes are ignored, except that each is counted as a branch under the node that contains them.

use std::vec::Vec;
use uuid::Uuid;

use crate::types::{ID,OrgNode,FileNode};

pub fn orgnodes_to_filenodes (
  branches: &[OrgNode]
) -> (Vec<FileNode>, Option<ID>) {

  let mut all_nodes = Vec::new();
  let mut focused_id = None;
  for branch in branches {
    let (mut nodes, focused_node) =
      orgnode_to_filenodes(branch);
    all_nodes.extend(nodes);
    if focused_node.is_some() {
      focused_id = focused_node;
    } }
  (all_nodes, focused_id) }

pub fn orgnode_to_filenodes (
  branch: &OrgNode)
  -> (Vec<FileNode>, Option<ID>) {

  let mut nodes = Vec::new();
  let mut focused_id = None;
  orgnode_to_filenodes_internal(
    branch, &mut nodes, &mut focused_id);
  (nodes, focused_id) }

fn orgnode_to_filenodes_internal (
  // PITFALL: Mutates arguments 2 and 3, returns nothing.
  branch: &OrgNode,
  nodes_acc: &mut Vec<FileNode>,
  focused_id: &mut Option<ID>
) {

  if branch.repeated { // Skip nodes marked as repeated. Do not modify nodes_acc.
    // TRICKY: Even if this is the first appearance of that node in the s-exp, it can still be marked `repeated`, if the user moved it. The user has done no harm -- it can belong to the new parent. But whatever edits the user made to or under it should be ignored.
    return; }
  if let Some(id) = &branch.id {
    // Skip repeated nodes, even if not marked as such.
    // TODO: This should use a set, not a vector, because it is frequently searched.
    if nodes_acc.iter().any(
      |node| node.ids.contains(id)) {
      return; } }
  let node = FileNode {
    title: branch.heading.clone(),
    ids: vec![
      match &branch.id {
        Some(id) => id.clone(),
        None => ID::new(
          Uuid::new_v4() . to_string() ) } ],
    body: branch.body.clone(),
    contains: branch.branches.iter()
      // Do not exclude repeated nodes here. They are still valid contents.
      .filter_map(
        // TODO: Why is this a filter_map, not a map?
        |child| child.id.clone() )
      .collect(),
    subscribes_to                : Vec::new(),
    hides_from_its_subscriptions : Vec::new(),
    overrides_view_of_of         : Vec::new(),
    path                         : "".into(),
  };
  if branch.focused {
    // This would clobber any earlier focused node, but that's fine, because there should be only one.
    *focused_id = Some(
      node . ids[0] . clone()); }
  nodes_acc.push(node);
  for child in &branch.branches { // recurse
    orgnode_to_filenodes_internal(
      child, nodes_acc, focused_id); } }
