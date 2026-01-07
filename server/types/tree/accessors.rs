/// Node access utilities for ego_tree::Tree.

use crate::types::orgnode::{Interp, OrgNode};
use super::PairTree;

use ego_tree::{Tree, NodeId, NodeMut};
use std::error::Error;

/// Read a value from an ancestor of a node in a tree, applying a function to it.
/// The `generation` parameter specifies how many generations to climb up:
/// 0 = the node itself, 1 = parent, 2 = grandparent, etc.
/// Returns an error if the node is not found or if we can't climb that high.
pub fn read_at_ancestor_in_tree<T, F, R>(
  tree: &Tree<T>,
  treeid: NodeId,
  generation: usize,
  f: F
) -> Result<R, String>
where F: FnOnce(&T) -> R, {
  let mut node_ref = tree.get(treeid)
    .ok_or("node not found")?;
  for _ in 0..generation {
    node_ref = node_ref.parent()
      .ok_or("cannot climb that many generations")?; }
  Ok(f(node_ref.value() )) }

/// Write to an ancestor of a node in a tree, applying a mutating function to it.
/// The `generation` parameter specifies how many generations to climb up:
/// 0 = the node itself, 1 = parent, 2 = grandparent, etc.
/// Returns an error if the node is not found or if we can't climb that high.
fn write_at_ancestor_in_tree<T, F, R>(
  tree: &mut Tree<T>,
  treeid: NodeId,
  generation: usize,
  f: F
) -> Result<R, String>
where F: FnOnce(&mut T) -> R, {
  let target_id = {
    // Climb to target node via immutable reference
    let mut node_ref = tree.get(treeid)
      .ok_or("node not found")?;
    for _ in 0..generation {
      node_ref = node_ref.parent()
        .ok_or("cannot climb that many generations")?; }
    node_ref.id() };
  let mut node_mut = tree.get_mut(target_id)
    .ok_or("target node not found")?;
  Ok(f(node_mut.value() )) }

/// Read a node's value from a tree, applying a function to it.
/// Returns an error if the node is not found.
pub fn read_at_node_in_tree<T, F, R>(
    tree: &Tree<T>,
    treeid: NodeId,
    f: F
) -> Result<R, String>
where F: FnOnce(&T) -> R, {
  read_at_ancestor_in_tree(
    tree, treeid, 0, f) }

/// Write to a node's value in a tree, applying a mutating function to it.
/// Returns an error if the node is not found.
pub fn write_at_node_in_tree<T, F, R>(
    tree: &mut Tree<T>,
    treeid: NodeId,
    f: F
) -> Result<R, String>
where F: FnOnce(&mut T) -> R, {
  write_at_ancestor_in_tree(tree, treeid, 0, f) }

/// Access a node mutably for structural operations (prepend, append, detach, etc.).
/// Unlike write_at_node_in_tree which only provides &mut T (the value),
/// this provides NodeMut<T> so you can modify tree structure.
/// Returns an error if the node is not found.
pub fn with_node_mut<T, F, R>(
  tree    : &mut Tree<T>,
  node_id : NodeId,
  f       : F
) -> Result<R, String>
where F: FnOnce(NodeMut<T>) -> R {
  let node_mut : NodeMut<T> = tree . get_mut ( node_id )
    . ok_or ( "with_node_mut: node not found" ) ?;
  Ok ( f ( node_mut ) ) }

///
/// accessors specific to trees of OrgNodes and (maybe) SkgNodes
///

/// Find the unique child of a node with a given Interp (for PairTree).
/// Returns None if no child has the interp,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_child_with_interp (
  tree    : &PairTree,
  node_id : NodeId,
  interp  : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<super::NodePair> =
    tree.get(node_id)
    .ok_or("unique_child_with_interp: node not found")?;
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| c.value().orgnode.metadata.code.interp == interp)
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", interp, n).into()),
  }
}

/// Find the unique child of a node with a given Interp (for Tree<OrgNode>).
/// Returns None if no child has the interp,
/// Some(child_id) if exactly one does,
/// or an error if multiple children have it.
pub fn unique_orgnode_child_with_interp (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
  interp  : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let node_ref : ego_tree::NodeRef<OrgNode> =
    tree . get(node_id) . ok_or(
      "unique_orgnode_child_with_interp: node not found")?;
  unique_orgnode_child_with_interp_from_ref (
    &node_ref, interp ) }

/// Like unique_orgnode_child_with_interp, but takes a NodeRef directly.
/// Useful when you already have the NodeRef and don't want to look it up again.
pub fn unique_orgnode_child_with_interp_from_ref (
  node_ref : &ego_tree::NodeRef<OrgNode>,
  interp   : Interp,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let matches : Vec<NodeId> = node_ref.children()
    .filter(|c| c.value().metadata.code.interp == interp)
    .map(|c| c.id())
    .collect();
  match matches.len() {
    0 => Ok(None),
    1 => Ok(Some(matches[0])),
    n => Err(format!(
      "Expected at most one {:?} child, found {}", interp, n).into()),
  }
}
