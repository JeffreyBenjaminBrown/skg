// PURPOSE:
// Translates the OrgNodeInterp type to the SkgNode type.

// PITFALL:
// The correspondence between those two types is imperfect in two respects:
//   (1) Since the ID of an OrgNodeInterp is optional and the ID of a SkgNode is mandatory, anything without an ID is assigned one, at random.
//   (2) If an ID is repeated, the first node to contain a branch with that ID is processed normally. All future such containers are ignored, except that each is counted as a branch under the node that contains them.

// TODO ? There is still the problem that the user might copy a node not marked repeated, i.e. the one that's supposed to be the source of truth, and paste it somewhere else in the document. Rust won't know which one to treat as the source of truth. This could result in data loss.

use crate::types::{ID, SkgNode, OrgNodeInterp};

use std::collections::HashSet;

pub fn orgNodeInterpretation_to_nodes (
  // Uses `assign_ids_recursive`
  // to create random IDs where needed,
  // then runs `orgNodeInterpretation_to_nodes_internal`.
  branch : &OrgNodeInterp )
  -> ( HashSet <SkgNode>,
       Option  <ID>,    // the focused node
       HashSet <ID> ) { // the folded nodes

  let mut nodes      : HashSet<SkgNode> = HashSet::new ();
  let mut focused_id : Option<ID>       = None;
  let mut folded_ids : HashSet<ID>      = HashSet::new ();
  orgNodeInterpretation_to_nodes_internal (
    branch,
    &mut nodes,
    &mut focused_id,
    &mut folded_ids );
  (nodes, focused_id, folded_ids) }

fn orgNodeInterpretation_to_nodes_internal (
  branch     : &    OrgNodeInterp,
  nodes_acc  : &mut HashSet <SkgNode>,
  focused_id : &mut Option  <ID>,
  folded_ids : &mut HashSet <ID> ) {

  let content_node = match branch {
    OrgNodeInterp::Content(content_node) => content_node,
    OrgNodeInterp::Aliases(_) => {
      panic!("Found Aliases node in orgNodeInterpretation_to_nodes_internal. All Aliases nodes should have been filtered out by this point."); },
    OrgNodeInterp::Ignored => { return; }};
  if content_node.repeated { // Skip nodes marked as repeated.
    // TRICKY: Even if this is the first appearance of that node in the org text, it might still be marked `repeated`, because the user might have moved it. If so, the user has done no harm -- it can belong to the new parent. But whatever edits the user made to or under it should be ignored.
    return; }
  if let Some (id) = &content_node.id {
    // Skip repeated nodes, even if not marked as such.
    if nodes_acc . iter() . any (
      |node| node.ids.contains (id) ) {
      return; }}
  let node = SkgNode {
    title: content_node.title.clone (),
    aliases: content_node.aliases.clone(),
    ids: vec! [ // Nodes can have multiple IDs, but OrgNodeInterps can't.
      content_node . id . clone () . expect (
        "SkgNode with no ID found in `orgNodeInterpretation_to_nodes_internal`. It should have already had an ID assigned by `assign_ids_recursive` in `orgNodeInterpretation_to_nodes` (the non-internal version)." ) ],
    body: content_node.body.clone (),
    contains: content_node.branches.iter ()
      // Do not exclude repeated nodes here. They are still valid contents.
      . filter_map ( // `filter_map` is robust to receiving `None` values, but this should not receive any, thanks to `assign_ids_recursive`.
        |child| { match child {
          OrgNodeInterp::Content(child_content) =>
            child_content.id.clone (),
          OrgNodeInterp::Aliases(_) => {
            panic!("Found Aliases node when extracting child IDs. All Aliases nodes should have been filtered out by this point."); },
          OrgNodeInterp::Ignored => { None }} })
      . collect (),
    subscribes_to                : Vec::new (),
    hides_from_its_subscriptions : Vec::new (),
    overrides_view_of            : Vec::new (), };
  if content_node.focused {
    // This would clobber any earlier focused node, but that's fine, because there should be only one.
    *focused_id = Some (
      node . ids [0] . clone () ); }
  if content_node.folded {
    folded_ids.insert (
      node . ids [0] . clone () ); }
  nodes_acc.insert (node);
  for child in &content_node.branches { // recurse
    orgNodeInterpretation_to_nodes_internal (
      child, nodes_acc, focused_id, folded_ids ); } }
