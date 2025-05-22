// PURPOSE:
// Translates the OrgNode type to the FileNode type.

// PITFALL:
// The correspondence between those two types is imperfect, so it is as if the OrgNode is transformed before translation, in two respects:
//   (1) Since the ID of an OrgNode is optional and the ID of a FileNode is mandatory, anything without an ID is assigned one, at random.
//   (2) If an ID is repeated, the first node to contain it is processed normally, but all future nodes are ignored, except that each is counted as a branch under the node that contains them.

use std::collections::HashSet;
use uuid::Uuid;

use crate::types::{ID,OrgNode,FileNode};

pub fn orgnode_to_filenodes (
  branch: &OrgNode)
  -> (HashSet<FileNode>, Option<ID>) {

  let mut nodes = HashSet::new();
  let mut focused_id = None;
  orgnode_to_filenodes_internal(
    &assign_id_where_missing_in_orgnode_recursive(
      // TRICKY: It might seem more natural to assign missing IDs only when creating the FileNode. That would not work because `orgnode_to_filenodes_internal` has to know the ID of a node's contents in order to create the FileNode, and those contents are not themselves FileNodes yet. (Alternatively, the contents could be processed before the container, but that would confuse the detection of repeated nodes.)
      branch),
    &mut nodes,
    &mut focused_id);
  (nodes, focused_id) }

fn orgnode_to_filenodes_internal (
  // PITFALL: Mutates arguments 2 and 3, returns nothing.
  branch: &OrgNode,
  nodes_acc: &mut HashSet<FileNode>,
  focused_id: &mut Option<ID> ) {

  if branch.repeated { // Skip nodes marked as repeated. Do not modify nodes_acc.
    // TRICKY: Even if this is the first appearance of that node in the s-exp, it can still be marked `repeated`, if the user moved it. The user has done no harm -- it can belong to the new parent. But whatever edits the user made to or under it should be ignored.
    return; }
  if let Some(id) = &branch.id {
    // Skip repeated nodes, even if not marked as such.
    if nodes_acc.iter().any(
      |node| node.ids.contains(id)) {
      return; } }
  let node = FileNode {
    title: branch.heading.clone(),
    ids: vec![
      branch.id.clone().expect(
        "FileNode with no ID found in `orgnode_to_filenodes_internal`. It should have already had an ID assigned by `assign_id_where_missing_in_orgnode_recursive` in `orgnode_to_filenodes` (the non-internal version)." ) ],
    body: branch.body.clone(),
    contains: branch.branches.iter()
      // Do not exclude repeated nodes here. They are still valid contents.
      .filter_map( // filter_map is robust to receiving None values, but this should not receive any.
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
  nodes_acc.insert(node);
  for child in &branch.branches { // recurse
    orgnode_to_filenodes_internal(
      child, nodes_acc, focused_id); } }

pub fn assign_id_where_missing_in_orgnode_recursive(
  node: &OrgNode)
  -> OrgNode {

  OrgNode {
    id: Some(
      node.id.clone().unwrap_or_else(
        || ID::new( Uuid::new_v4()
                    . to_string())) ),
    heading: node.heading.clone(),
    body: node.body.clone(),
    folded: node.folded,
    focused: node.focused,
    repeated: node.repeated,
    branches: node.branches
      .iter()
      .map ( assign_id_where_missing_in_orgnode_recursive)
      .collect()
  } }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::{ID, OrgNode};

  #[test]
  fn test_assign_id_where_missing_in_orgnode_recursive() {
    let node_c = OrgNode {
      // Innermost node, with no ID
      id: None,
      heading: "c".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![],
    };
    let node_b = OrgNode {
      // Intermediate node, with ID
      id: Some(ID::from("b")),
      heading: "b".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![node_c],
    };
    let node_a = OrgNode {
      // Outermost node, with no ID
      id: None,
      heading: "a".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![node_b],
    };
    let result =
      assign_id_where_missing_in_orgnode_recursive(
        &node_a);
    assert!(result.id.is_some(),
            "Node A should have an ID after processing");
    assert_eq!(result.branches.len(), 1,
               "Node A should have one child");
    let result_b = &result.branches[0];
    assert_eq!(result_b.id, Some(ID::from("b")),
               "Node B should keep its original ID 'b'");
    assert_eq!(result_b.branches.len(), 1,
               "Node B should have one child");
    let result_c = &result_b.branches[0];
    assert!(result_c.id.is_some(),
            "Node C should have an ID after processing");
    assert_eq!(result.heading, "a");
    assert_eq!(result_b.heading, "b");
    assert_eq!(result_c.heading, "c");
  } }
