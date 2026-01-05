/// Node access utilities for ego_tree::Tree.

use ego_tree::{Tree, NodeId};

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
pub fn write_at_ancestor_in_tree<T, F, R>(
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
