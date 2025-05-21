// PURPOSE:
// Translates the OrgNode type to the FileNode type.

// PITFALL:
// The correspondence between those two types is imperfect, so it is as if the OrgNode is transformed before translation, in two respects:
//   (1) Since the ID of an OrgNode is optional and the ID of a FileNode is mandatory, anything without an ID is assigned one, at random.
//   (2) If an ID is repeated, the first node to contain it is processed normally, but all future nodes are ignored, except that each is counted as a branch under for the node that contains them.

use std::vec::Vec;
use uuid::Uuid;

use crate::types::{ID,OrgNode,FileNode};

pub fn orgnodes_to_filenodes (
  branches: &[OrgNode]
) -> Vec<FileNode> {
  let mut all_nodes = Vec::new();
  for branch in branches {
    all_nodes.extend(
      orgnode_to_filenodes(
        branch) ) }
  all_nodes }

pub fn orgnode_to_filenodes (
  branch: &OrgNode)
  -> Vec<FileNode> {
  let mut nodes = Vec::new();
  orgnode_to_filenodes_internal(
    branch, &mut nodes);
  nodes }

fn orgnode_to_filenodes_internal (
  branch: &OrgNode,
  nodes_acc: &mut Vec<FileNode> // accumulator
) {
  // PITFALL: Mutates second argument, returns nothing.
  if branch.repeated { // skip nodes marked as repeated
    return; }
  if let Some(id) = &branch.id {
    // Skip repeated nodes, even if not marked as such.
    // TODO: This should use a set, not a vector, because it is frequently searched.
    if nodes_acc.iter().any(|node| node.ids.contains(id)) {
      return; } }
  let node = FileNode {
    title: branch.heading.clone(),
    ids: vec![ match &branch.id {
      Some(id) => id.clone(),
      None => ID::new(
        Uuid::new_v4() . to_string() ) } ],
    body: branch.body.clone(),
    contains: branch.branches.iter()
      // Do not exclude repeated nodes here.
      .filter_map( |child| child.id.clone() )
      .collect(),
    subscribes_to                : Vec::new(),
    hides_from_its_subscriptions : Vec::new(),
    overrides_view_of_of         : Vec::new(),
    path                         : "".into(),
  };
  nodes_acc.push(node);

  for child in &branch.branches { // recurse
    orgnode_to_filenodes_internal(
      child, nodes_acc); } }
