use crate::to_org::util::{
  DefinitiveMap,
  get_id_from_treenode,
  makeIndefinitiveAndClobber,
  make_and_append_child_pair,
  nodes_after_in_generation };
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::ViewNode;
use crate::types::skgnodemap::SkgNodeMap;

use ego_tree::{Tree, NodeId};
use std::cmp::min;
use std::error::Error;
use neo4rs::Graph;


/// PURPOSE:
/// Render the last generation because limit is hit. In order:
/// - fetch and add children up to the limit as indefinitive nodes
/// - complete the sibling group of the limit node
///   - but omit rest of generation (later would-be sibling groups)
/// - truncate the preceding generation after the limit-hitting node's parent
pub async fn add_last_generation_and_truncate_some_of_previous (
  tree           : &mut Tree<ViewNode>,
  map            : &mut SkgNodeMap,
  generation     : usize,
  children       : &[(NodeId, ID)],
  space_left     : usize,
  effective_root : NodeId, // gen 0; had the definitive view req
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
  graph          : &Graph,
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
            tree, map, *parent_treeid, child_skgid, config, graph ) . await ?;
        makeIndefinitiveAndClobber ( tree, map, new_treeid, config ) ?; }}
  truncate_after_node_in_generation_in_tree (
    tree, map, generation - 1, limit_parent_treeid,
    effective_root, visited, config ) ?;
  Ok (( )) }

/// PURPOSE:
/// Truncate after a node in a generation of a tree or forest,
/// possibly limiting scope to an effective branch of a tree.
/// Truncated nodes are re-rendered using 'makeIndefinitiveAndClobber'.
fn truncate_after_node_in_generation_in_tree (
  tree           : &mut Tree<ViewNode>,
  map            : &mut SkgNodeMap,
  generation     : usize,
  node_id        : NodeId, // truncate after this one
  effective_root : NodeId, // gen 0
  visited        : &mut DefinitiveMap,
  config         : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let nodes_to_truncate : Vec < NodeId > =
    nodes_after_in_generation (
      tree, generation, node_id, Some ( effective_root ) ) ?;
  for id in nodes_to_truncate {
    if let Ok ( pid ) = get_id_from_treenode ( tree, id ) {
      visited . remove ( &pid ); }
    makeIndefinitiveAndClobber ( tree, map, id, config ) ?; }
  Ok (( )) }
