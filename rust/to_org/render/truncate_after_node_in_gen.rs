use crate::media::tree::collect_generation_ids;
use crate::to_org::util::{
  make_and_append_child_pair,
  get_pid_in_pairtree, VisitedMap,
  nodes_after_in_generation, rewrite_to_indefinitive };
use crate::types::misc::{ID, SkgConfig};
use crate::types::trees::PairTree;

use ego_tree::NodeId;
use std::cmp::min;
use std::error::Error;
use typedb_driver::TypeDBDriver;


// ==========================================================
// add_last_generation_and_edit_previous
// (tree and forest versions)
//
// Render the last generation because limit is hit. In order:
// - fetch and add children up to the limit as indefinitive nodes
// - complete the sibling group of the limit node
//   - but omit rest of generation (later would-be sibling groups)
// - truncate the preceding generation after the limit-hitting node's parent
// ==========================================================

/// PURPOSE: See section header comment.
/// Generation is relative to effective_root (which is generation 1).
pub async fn add_last_generation_and_edit_previous_in_tree (
  tree           : &mut PairTree,
  generation     : usize,
  children       : &[(NodeId, ID)],
  space_left     : usize,
  effective_root : NodeId, // it had the definitive view request
  visited        : &mut VisitedMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if space_left < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index : usize =
    min ( space_left, children . len () ) - 1;
  let limit_parent_id : NodeId =
    children [last_addable_index] . 0;
  for (idx, (parent_nid, child_id))
    in children . iter () . enumerate () {
      if ( idx > last_addable_index && // past limit
           *parent_nid != limit_parent_id ) // in new sibling group
      { break; }
      else {
        let new_node_id : NodeId =
          make_and_append_child_pair (
            tree, *parent_nid, child_id, config, driver ) . await ?;
        rewrite_to_indefinitive ( tree, new_node_id ) ?; }}
  truncate_after_node_in_generation_in_tree (
    tree, generation - 1, limit_parent_id,
    effective_root, visited ) ?;
  Ok (( )) }

/// PURPOSE: See section header comment.
pub async fn add_last_generation_and_edit_previous_in_forest (
  forest           : &mut Vec<PairTree>,
  generation       : usize,
  children_to_add  : &[(usize, NodeId, ID)],
  nodes_rendered   : usize,
  limit            : usize,
  config           : &SkgConfig,
  driver           : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let space_left_before_limit : usize = limit - nodes_rendered;
  if space_left_before_limit < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index =
    min (space_left_before_limit, children_to_add . len()) - 1;
  let (limit_tree_idx, limit_parent_id) : (usize, NodeId) = {
    // identify where limit is hit
    let (tree_idx, parent_id, _child_id) =
      &children_to_add [last_addable_index];
    (*tree_idx, *parent_id) };
  for (idx, (tree_idx, parent_id, child_id))
    in children_to_add . iter() . enumerate() {
      if idx > last_addable_index &&
        ( *tree_idx != limit_tree_idx ||
           *parent_id != limit_parent_id )
      // That 'in a new tree' condition is needed because ego_tree does not permit comparison of node_ids from different trees.
      { break; }
      let child_node_id : NodeId =
        make_and_append_child_pair (
          &mut forest[*tree_idx], *parent_id, child_id,
          config, driver
        ). await ?;
      rewrite_to_indefinitive (
        &mut forest[*tree_idx], child_node_id ) ?; }
  truncate_after_node_in_generation_in_forest(
    forest, generation, limit_tree_idx, limit_parent_id )?;
  Ok (( )) }


// ========================================
// truncate_after_node_in_generation
// (tree and forest versions)
//
// Truncate after a node in a generation of a tree or forest,
// possibly limiting scope to an effective branch of a tree.
// Truncated nodes are re-rendered using 'rewrite_to_indefinitive'.
// ========================================

/// PURPOSE: See section header comment.
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

/// PURPOSE: See section header comment.
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
