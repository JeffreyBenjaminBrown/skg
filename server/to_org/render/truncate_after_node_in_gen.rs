use crate::to_org::util::{
  DefinitiveMap,
  get_pid_in_pairtree,
  get_pid_in_tree,
  makeIndefinitiveAndClobber,
  makeIndefinitiveAndClobber_v2,
  make_and_append_child_pair,
  make_and_append_child_pair_v2,
  nodes_after_in_generation,
  nodes_after_in_generation_v2 };
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNodeMap;
use crate::types::tree::PairTree;

use ego_tree::{Tree, NodeId};
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
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if space_left < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index : usize =
    min ( space_left, children . len () ) - 1;
  let limit_parent_treeid : NodeId =
    children [last_addable_index] . 0;
  for (idx, (parent_treeid, child_skgid))
    in children . iter () . enumerate () {
      if ( idx > last_addable_index && // past limit
           *parent_treeid != limit_parent_treeid ) // in new sibling group
      { break; }
      else {
        let new_treeid : NodeId =
          make_and_append_child_pair (
            tree, *parent_treeid, child_skgid, config, driver ) . await ?;
        makeIndefinitiveAndClobber ( tree, new_treeid ) ?; }}
  truncate_after_node_in_generation_in_tree (
    tree, generation - 1, limit_parent_treeid,
    effective_root, visited ) ?;
  Ok (( )) }

/// V2: Add last generation and truncate preceding generation in Tree<OrgNode> + SkgNodeMap.
/// Render children up to the limit as indefinitive, complete sibling group,
/// and truncate preceding generation after the limit node's parent.
pub async fn add_last_generation_and_truncate_some_of_previous_v2 (
  tree           : &mut Tree<OrgNode>,
  map            : &mut SkgNodeMap,
  generation     : usize,
  children       : &[(NodeId, ID)],
  space_left     : usize,
  effective_root : NodeId,
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if space_left < 1 {
    return Ok (( )); }
  let last_addable_index : usize =
    min ( space_left, children . len () ) - 1;
  let limit_parent_treeid : NodeId =
    children [last_addable_index] . 0;
  for (idx, (parent_treeid, child_skgid))
    in children . iter () . enumerate () {
      if ( idx > last_addable_index &&
           *parent_treeid != limit_parent_treeid )
      { break; }
      else {
        let new_treeid : NodeId =
          make_and_append_child_pair_v2 (
            tree, map, *parent_treeid, child_skgid, config, driver ) . await ?;
        makeIndefinitiveAndClobber_v2 ( tree, new_treeid ) ?; }}
  truncate_after_node_in_generation_in_tree_v2 (
    tree, generation - 1, limit_parent_treeid,
    effective_root, visited ) ?;
  Ok (( )) }

/// PURPOSE:
/// Truncate after a node in a generation of a tree or forest,
/// possibly limiting scope to an effective branch of a tree.
/// Truncated nodes are re-rendered using 'makeIndefinitiveAndClobber'.
/// Effective_root is generation 0.
fn truncate_after_node_in_generation_in_tree (
  tree           : &mut PairTree,
  generation     : usize,
  node_id        : NodeId, // truncate after this one
  effective_root : NodeId,
  visited        : &mut DefinitiveMap,
) -> Result < (), Box<dyn Error> > {
  let nodes_to_truncate : Vec < NodeId > =
    nodes_after_in_generation (
      tree, generation, node_id, Some ( effective_root ) ) ?;
  for id in nodes_to_truncate {
    if let Ok ( pid ) = get_pid_in_pairtree ( tree, id ) {
      visited . remove ( &pid ); }
    makeIndefinitiveAndClobber ( tree, id ) ?; }
  Ok (( )) }

/// V2: Truncate after a node in a generation of Tree<OrgNode>.
/// Truncated nodes are marked indefinitive and removed from visited map.
fn truncate_after_node_in_generation_in_tree_v2 (
  tree           : &mut Tree<OrgNode>,
  generation     : usize,
  node_id        : NodeId,
  effective_root : NodeId,
  visited        : &mut DefinitiveMap,
) -> Result < (), Box<dyn Error> > {
  let nodes_to_truncate : Vec < NodeId > =
    nodes_after_in_generation_v2 (
      tree, generation, node_id, Some ( effective_root ) ) ?;
  for id in nodes_to_truncate {
    if let Ok ( pid ) = get_pid_in_tree ( tree, id ) {
      visited . remove ( &pid ); }
    makeIndefinitiveAndClobber_v2 ( tree, id ) ?; }
  Ok (( )) }
