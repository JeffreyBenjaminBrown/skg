// PURPOSE:
// Translates the OrgNode type to the FileNode type.

// PITFALL:
// The correspondence between those two types is imperfect in two respects:
//   (1) Since the ID of an OrgNode is optional and the ID of a FileNode is mandatory, anything without an ID is assigned one, at random.
//   (2) If an ID is repeated, the first node to contain a branch with that ID is processed normally. All future such containers are ignored, except that each is counted as a branch under the node that contains them.

// TODO: There is still the problem that the user might copy a node not marked repeated, i.e. the one that's supposed to be the source of truth, and paste it somewhere else in the document. Rust won't know which one to treat as the source of truth. This could result in data loss.

use crate::save::assign_ids::assign_ids_recursive;
use crate::types::{ID, FileNode, OrgNode};

use std::collections::HashSet;

pub fn orgnode_to_filenodes (
  // Uses `assign_ids_recursive`
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
    & assign_ids_recursive (
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

  // Extract ContentNode or error if this is an AliasNode
  let content_node = match branch {
    OrgNode::Content(content_node) => content_node,
    OrgNode::Aliases(_) => {
      panic!("Found AliasNode in orgnode_to_filenodes_internal. All AliasNodes should have been filtered out by this point.");
    }};
  if content_node.repeated { // Skip nodes marked as repeated.
    // TRICKY: Even if this is the first appearance of that node in the org text, it might still be marked `repeated`, because the user might have moved it. If so, the user has done no harm -- it can belong to the new parent. But whatever edits the user made to or under it should be ignored.
    return; }
  if let Some (id) = &content_node.id {
    // Skip repeated nodes, even if not marked as such.
    if filenodes_acc . iter() . any (
      |node| node.ids.contains (id) ) {
      return; }}
  let filenode = FileNode {
    title: content_node.heading.clone (),
    ids: vec! [ // FileNodes can have multiple IDs, but OrgNodes can't.
      content_node . id . clone () . expect (
        "FileNode with no ID found in `orgnode_to_filenodes_internal`. It should have already had an ID assigned by `assign_ids_recursive` in `orgnode_to_filenodes` (the non-internal version)." ) ],
    body: content_node.body.clone (),
    contains: content_node.branches.iter ()
      // Do not exclude repeated nodes here. They are still valid contents.
      . filter_map ( // `filter_map` is robust to receiving `None` values, but this should not receive any, thanks to `assign_ids_recursive`.
        |child| { match child {
          OrgNode::Content(child_content) =>
            child_content.id.clone (),
          OrgNode::Aliases(_) => {
            panic!("Found AliasNode when extracting child IDs. All AliasNodes should have been filtered out by this point.");
          }} })
      . collect (),
    subscribes_to                : Vec::new (),
    hides_from_its_subscriptions : Vec::new (),
    overrides_view_of            : Vec::new (), };
  if content_node.focused {
    // This would clobber any earlier focused node, but that's fine, because there should be only one.
    *focused_id = Some (
      filenode . ids [0] . clone () ); }
  if content_node.folded {
    folded_ids.insert (
      filenode . ids [0] . clone () ); }
  filenodes_acc.insert (filenode);
  for child in &content_node.branches { // recurse
    orgnode_to_filenodes_internal (
      child, filenodes_acc, focused_id, folded_ids ); } }
