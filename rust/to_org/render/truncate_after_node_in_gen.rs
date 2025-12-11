/// Truncate after a node in a generation of a tree or forest,
/// possibly limiting scope to an effective branch of a tree.
/// Truncated nodes are re-rendered using 'rewrite_to_indefinitive'.

use crate::media::tree::collect_generation_ids;
use crate::to_org::util::{
  get_pid_in_pairtree, VisitedMap,
  nodes_after_in_generation, rewrite_to_indefinitive };
use crate::types::trees::PairTree;

use ego_tree::NodeId;
use std::error::Error;

/// PURPOSE: See file header comment.
/// This one is for a single branch of a single tree.
/// Effective_root is generation 1.
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

/// PURPOSE: See file header comment.
/// This one is for an entire forest.
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
