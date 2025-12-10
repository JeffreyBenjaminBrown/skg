/// Truncation logic for BFS rendering when node limit is hit.

use crate::media::tree::collect_generation_ids;
use crate::to_org::bfs_shared::{
  nodes_after_in_generation,
  rewrite_to_indefinitive };
use crate::to_org::util::{get_pid_in_pairtree, VisitedMap};
use crate::types::trees::PairTree;

use ego_tree::NodeId;
use std::error::Error;

/// Truncate all nodes after a given node in a generation.
/// Marks them as indefinitive.
/// Generation is relative to effective_root (which is generation 1).
pub fn truncate_after_node_in_generation_in_tree (
  tree           : &mut PairTree,
  generation     : usize,
  node_id        : NodeId, // truncate after this one
  effective_root : NodeId,
  visited        : &mut VisitedMap,
) -> Result < (), Box<dyn Error> > {
  let nodes_to_truncate : Vec < NodeId > =
    nodes_after_in_generation (
      tree, generation, node_id, Some ( effective_root ) ) ?;
  for id in nodes_to_truncate {
    if let Ok ( pid ) = get_pid_in_pairtree ( tree, id ) {
      visited . remove ( &pid ); }
    rewrite_to_indefinitive ( tree, id ) ?; }
  Ok (( )) }

/// Truncate all nodes after a given node in a generation
/// across the forest.
/// For trees before tree_idx: skip (already fully rendered).
/// For tree at tree_idx: truncate nodes after node_id.
/// For trees after tree_idx: truncate entire generation.
pub fn truncate_after_node_in_generation_in_forest(
  forest     : &mut Vec<PairTree>,
  generation : usize,
  tree_idx   : usize, // index into 'forest' to the tree containing 'node_id'
  node_id    : NodeId, // truncate everything after this node in its generation
) -> Result<(), Box<dyn Error>> {
  for (idx, tree) in forest . iter_mut () . enumerate () {
    if idx < tree_idx {
      // Precedes target tree, so already fully rendered.
      continue; }
    let nodes_to_truncate : Vec < NodeId > =
      if idx == tree_idx { // In target tree, truncate only after node_id.
        nodes_after_in_generation (
          tree, generation, node_id, None ) ?
      } else { // After target tree, truncate entire generation.
        collect_generation_ids (
          tree, generation, None ) ? };
    for id in nodes_to_truncate {
      rewrite_to_indefinitive ( tree, id )
        . unwrap_or_else (
          |e| eprintln! (
            "truncate_after_node_in_generation_in_forest: {}", e )); } }
  Ok (( )) }
