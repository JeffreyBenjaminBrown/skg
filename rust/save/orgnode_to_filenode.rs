// PURPOSE:
// Translates the OrgNode type to the FileNode type.

// PITFALL:
// The correspondence between those two types is imperfect in two respects:
//   (1) Since the ID of an OrgNode is optional and the ID of a FileNode is mandatory, anything without an ID is assigned one, at random. Confusingly, this is done to the OrgNodes before transforming them into FileNodes, by `assign_id_where_missing_in_orgnode_recursive`. See the long comment at its call site for why.
//   (2) If an ID is repeated, the first node to contain a branch with that ID is processed normally. All future such containers are ignored, except that each is counted as a branch under the node that contains them.

// TODO: There is still the problem that the user might copy a node not marked repeated, i.e. the one that's supposed to be the source of truth, and paste it somewhere else in the document. Rust won't know which one to treat as the source of truth. This could result in data loss.

use std::collections::HashSet;
use uuid::Uuid;

use crate::types::{ID, FileNode, OrgNode};

pub fn orgnode_to_filenodes (
  // Uses `assign_id_where_missing_in_orgnode_recursive`
  // to create random IDs where needed,
  // then runs `orgnode_to_filenodes_internal`.
  branch : &OrgNode )
  -> ( HashSet <FileNode>,
       Option  <ID>,    // the focused node
       HashSet <ID> ) { // the folded nodes

  let mut filenodes  : HashSet<FileNode> = HashSet::new ();
  let mut focused_id : Option<ID>        = None;
  let mut folded_ids : HashSet<ID>       = HashSet::new ();
  orgnode_to_filenodes_internal (
    & assign_id_where_missing_in_orgnode_recursive (
      // TRICKY: It might seem more natural to assign missing IDs only when creating the FileNode. That would not work because `orgnode_to_filenodes_internal` has to know the ID of a node's contents in order to create the FileNode, and those contents are not themselves FileNodes yet. (Alternatively, the contents could be processed before the container, but that would confuse the detection of repeated nodes.)
      branch ),
    &mut filenodes,
    &mut focused_id,
    &mut folded_ids );
  (filenodes, focused_id, folded_ids) }

fn orgnode_to_filenodes_internal (
  branch        : &    OrgNode,
  filenodes_acc : &mut HashSet <FileNode>,
  focused_id    : &mut Option  <ID>,
  folded_ids    : &mut HashSet <ID> ) {

  if branch.repeated { // Skip nodes marked as repeated.
    // TRICKY: Even if this is the first appearance of that node in the s-exp, it might still be marked `repeated`, because the user might have moved it. If so, the user has done no harm -- it can belong to the new parent. But whatever edits the user made to or under it should be ignored.
    return; }
  if let Some (id) = &branch.id {
    // Skip repeated nodes, even if not marked as such.
    if filenodes_acc . iter() . any (
      |node| node.ids.contains (id) ) {
      return; }}
  let filenode = FileNode {
    title: branch.heading.clone (),
    ids: vec! [ // FileNodes can have multiple IDs, but OrgNodes can't.
      branch . id . clone () . expect (
        "FileNode with no ID found in `orgnode_to_filenodes_internal`. It should have already had an ID assigned by `assign_id_where_missing_in_orgnode_recursive` in `orgnode_to_filenodes` (the non-internal version)." ) ],
    body: branch.body.clone (),
    contains: branch.branches.iter ()
      // Do not exclude repeated nodes here. They are still valid contents.
      . filter_map ( // `filter_map` is robust to receiving `None` values, but this should not receive any, thanks to `assign_id_where_missing_in_orgnode_recursive`.
        |child| child.id.clone () )
      . collect (),
    subscribes_to                : Vec::new (),
    hides_from_its_subscriptions : Vec::new (),
    overrides_view_of            : Vec::new (),
    path                         : "".into (), };
  if branch.focused {
    // This would clobber any earlier focused node, but that's fine, because there should be only one.
    *focused_id = Some (
      filenode . ids [0] . clone () ); }
  if branch.folded {
    folded_ids.insert (
      filenode . ids [0] . clone () ); }
  filenodes_acc.insert (filenode);
  for child in &branch.branches { // recurse
    orgnode_to_filenodes_internal (
      child, filenodes_acc, focused_id, folded_ids ); } }

pub fn assign_id_where_missing_in_orgnode_recursive (
  orgnode : &OrgNode )
  -> OrgNode {
  // Returns something almost identical,
  // but replacing each `None`-valued ID with a random UUID.

  OrgNode {
    id : Some (
      orgnode . id . clone () . unwrap_or_else (
        || ID::new ( Uuid::new_v4 ()
                     . to_string () )) ),
    heading  : orgnode.heading.clone(),
    body     : orgnode.body.clone(),
    folded   : orgnode.folded,
    focused  : orgnode.focused,
    repeated : orgnode.repeated,
    branches : orgnode.branches
      . iter ()
      . map ( assign_id_where_missing_in_orgnode_recursive )
      . collect ()
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
