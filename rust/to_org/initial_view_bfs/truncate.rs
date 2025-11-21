/// Truncation logic for BFS rendering when node limit is hit.

use crate::media::tree::collect_generation_ids_in_forest;
use crate::types::{OrgNode, SkgNode};

use ego_tree::{Tree, NodeId, NodeMut};
use std::error::Error;

/// Truncate all nodes after a given node in a generation.
pub fn truncate_after_node_in_generation(
  forest     : &mut Vec<Tree<(SkgNode, OrgNode)>>,
  generation : usize,
  tree_idx   : usize, // index into 'forest' to the tree containing 'node_id'
  node_id    : NodeId, // truncate everything after this node in its generation
) -> Result<(), Box<dyn Error>> {
  let nodes_in_gen_grouped : Vec<(usize, Vec<NodeId>)> =
    collect_generation_ids_in_forest (forest, generation)?;
  let mut nodes_to_truncate : Vec<(usize, Vec<NodeId>)> =
    Vec::new();
  for (idx, ids) in nodes_in_gen_grouped {
    // Build 'nodes_to_truncate' from 'nodes_in_gen_grouped'.
    if idx < tree_idx {
      // Skip this tree entirely, because it precedes the target tree,
      // so its children are already fully rendered.
      continue;
    } else if idx == tree_idx {
      // This is the target tree. Include nodes after node_id.
      let mut found_target : bool = false;
      let mut remaining_ids : Vec<NodeId> = Vec::new();
      for id in ids {
        if found_target {
          remaining_ids.push(id);
        } else if id == node_id {
          // Don't add this one, but add everything after it.
          found_target = true; }}
      if !remaining_ids . is_empty() {
        nodes_to_truncate . push ((idx, remaining_ids)); }
    } else {
      // This one follows the target tree, so keep all of it.
      nodes_to_truncate.push((idx, ids)); }}
  for (idx, ids) in nodes_to_truncate {
    // Truncate everything in 'nodes_to_truncate'.
    for id in ids {
      truncate_node (forest, idx, id); }}
  Ok (( )) }

/// Mark the OrgNode as indefinitive and clear its body.
/// Note: These nodes should not have children yet, as we stop before
/// processing generations beyond the limit.
fn truncate_node(
  forest: &mut Vec<Tree<(SkgNode, OrgNode)>>,
  tree_idx: usize,
  node_id: NodeId,
) {
  let mut node_mut: NodeMut<(SkgNode, OrgNode)> =
    forest[tree_idx] . get_mut(node_id) . unwrap();
  node_mut . value() . 1 . metadata.code.indefinitive = true;
  node_mut . value() . 1 . body = None; }
