/// Functions for pairing OrgNode trees with SkgNode data.

use crate::types::misc::ID;
use crate::types::orgnode::OrgNode;
use crate::types::save::SaveInstruction;
use crate::types::skgnode::SkgNode;
use crate::types::trees::PairTree;

use ego_tree::{Tree, NodeId};
use std::collections::HashMap;

/// Convert an OrgNode forest to a NodePair forest
/// using SkgNodes from SaveInstructions.
///
/// Definitive nodes that generated SaveInstructions get Some(skgnode).
/// Indefinitive nodes (views) get None.
///
/// This avoids redundant disk fetches: the SkgNodes from SaveInstructions
/// already reflect what will be saved, supplemented with disk data.
pub fn pair_forest_with_save_instructions (
  orgnode_forest : Vec<Tree<OrgNode>>,
  instructions   : &[SaveInstruction],
) -> Vec<PairTree> {
  let skgnode_map : HashMap<ID, SkgNode> =
    instructions . iter ()
    . filter_map ( |(skgnode, _action)| {
      skgnode . ids . first ()
        . map ( |pid| (pid.clone(), skgnode.clone()) ) } )
    . collect ();
  orgnode_forest . into_iter ()
    . map ( |tree| pairtree_from_orgnodetree_and_map (
      tree, &skgnode_map ) )
    . collect () }

fn pairtree_from_orgnodetree_and_map (
  orgnode_tree : Tree<OrgNode>,
  skgnode_map  : &HashMap<ID, SkgNode>,
) -> PairTree {
  let old_root_id : NodeId =
    orgnode_tree . root () . id ();
  let root_orgnode : OrgNode =
    orgnode_tree . root () . value () . clone ();
  let root_skgnode : Option<SkgNode> =
    root_orgnode . metadata . id . as_ref ()
    . and_then (
      |id| skgnode_map . get (id) . cloned () );
  let mut new_tree : PairTree =
    Tree::new ((root_skgnode, root_orgnode));
  let new_root_id : NodeId =
    new_tree . root () . id ();
  pair_children_optional_recursive (
    &orgnode_tree, old_root_id,
    &mut new_tree, new_root_id, skgnode_map );
  new_tree }

fn pair_children_optional_recursive (
  old_tree    : &Tree<OrgNode>,
  old_node_id : NodeId,
  new_tree    : &mut PairTree,
  new_node_id : NodeId,
  skgnode_map : &HashMap<ID, SkgNode>,
) {
  let child_orgnodes : Vec<(NodeId, OrgNode)> =
    old_tree . get ( old_node_id ) . unwrap ()
    . children ()
    . map ( |c| (c.id(), c.value().clone()) )
    . collect ();
  for (old_child_id, child_orgnode) in child_orgnodes {
    let child_skgnode : Option<SkgNode> =
      child_orgnode . metadata . id . as_ref ()
      . and_then (
        |id| skgnode_map . get ( id ) . cloned () );
    let new_child_id : NodeId = {
      let mut parent_mut =
        new_tree . get_mut ( new_node_id ) . unwrap ();
      parent_mut . append (
        (child_skgnode, child_orgnode))
        . id () };
    pair_children_optional_recursive (
      old_tree, old_child_id,
      new_tree, new_child_id, skgnode_map ); }}
