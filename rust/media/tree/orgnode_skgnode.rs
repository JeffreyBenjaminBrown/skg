/// Functions for pairing OrgNode trees with SkgNode data.

use crate::to_org::util::forest_root_pair;
use crate::types::misc::ID;
use crate::types::orgnode::OrgNode;
use crate::types::save::SaveInstruction;
use crate::types::skgnode::SkgNode;
use crate::types::trees::PairTree;

use ego_tree::{Tree, NodeId, NodeMut};
use std::collections::HashMap;

/// Converts an OrgNode forest to a PairTree forest
/// (both represented as Trees, via ForestRoot).
///
/// Definitive nodes that generated SaveInstructions get Some(skgnode).
/// Indefinitive nodes (views) get None.
pub fn pair_orgnode_forest_with_save_instructions (
  orgnode_tree : &Tree<OrgNode>,
  instructions : &[SaveInstruction],
) -> PairTree {
  let skgnode_map : HashMap<ID, SkgNode> =
    instructions . iter ()
    . filter_map ( |(skgnode, _action)| {
      skgnode . ids . first ()
        . map ( |pid| (pid.clone(), skgnode.clone()) ) } )
    . collect ();
  let mut pair_tree : PairTree = Tree::new (
    // PITFALL: Discards the forest's root OrgNode.
    forest_root_pair () );
  let forest_root_id : NodeId = pair_tree . root () . id ();
  for tree_root in orgnode_tree.root().children() {
    add_paired_subtree_as_child (
      &mut pair_tree,
      forest_root_id,
      orgnode_tree, tree_root . id (),
      &skgnode_map ); }
  pair_tree }

/// Add an OrgNode subtree as a child of a parent in the PairTree,
/// pairing each node with its SkgNode from the map.
fn add_paired_subtree_as_child (
  pair_tree       : &mut PairTree,
  parent_id       : NodeId,
  orgnode_tree    : &Tree<OrgNode>,
  orgnode_node_id : NodeId,
  skgnode_map     : &HashMap<ID, SkgNode>,
) {
  let orgnode : OrgNode =
    orgnode_tree . get ( orgnode_node_id ) . unwrap ()
    . value () . clone ();
  let skgnode : Option<SkgNode> =
    orgnode . metadata . id . as_ref ()
    . and_then (
      |id| skgnode_map . get (id) . cloned () );
  let new_node_id : NodeId = {
    let mut parent_mut : NodeMut < _ > =
      pair_tree . get_mut ( parent_id ) . unwrap ();
    parent_mut . append ( // add new node
      (skgnode, orgnode) ) . id () };
  { // recurse in new node
    let child_ids : Vec < NodeId > =
      orgnode_tree . get ( orgnode_node_id ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_id in child_ids {
      add_paired_subtree_as_child (
        pair_tree, new_node_id,
        orgnode_tree, child_id,
        skgnode_map ); }} }
