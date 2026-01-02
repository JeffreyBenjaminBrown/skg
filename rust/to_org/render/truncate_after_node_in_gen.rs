use crate::to_org::util::{
  VisitedMap,
  get_pid_in_pairtree,
  makeIndefinitiveAndClobber,
  make_and_append_child_pair,
  nodes_after_in_generation };
use crate::types::misc::{ID, SkgConfig};
use crate::types::trees::PairTree;

use ego_tree::NodeId;
use std::cmp::min;
use std::error::Error;
use typedb_driver::TypeDBDriver;


/// PURPOSE:
/// Render the last generation because limit is hit. In order:
/// - fetch and add children up to the limit as indefinitive nodes
/// - complete the sibling group of the limit node
///   - but omit rest of generation (later would-be sibling groups)
/// - truncate the preceding generation after the limit-hitting node's parent
///
/// effective_root is generation 0.
pub async fn add_last_generation_and_truncate_some_of_previous (
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
        makeIndefinitiveAndClobber ( tree, new_node_id ) ?; }}
  truncate_after_node_in_generation_in_tree (
    tree, generation - 1, limit_parent_id,
    effective_root, visited ) ?;
  Ok (( )) }

/// PURPOSE:
/// Truncate after a node in a generation of a tree or forest,
/// possibly limiting scope to an effective branch of a tree.
/// Truncated nodes are re-rendered using 'makeIndefinitiveAndClobber'.
/// Effective_root is generation 0.
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
    makeIndefinitiveAndClobber ( tree, id ) ?; }
  Ok (( )) }
