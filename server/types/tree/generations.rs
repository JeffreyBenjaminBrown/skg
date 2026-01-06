/// Generation utilities for ego_tree::Tree.
/// Functions for navigating and collecting nodes at specific generations.

use ego_tree::{Tree, NodeRef, NodeId};
use std::error::Error;

/// See 'first_in_nth_generation_of_descendents'.
/// The difference is that that takes any node,
/// whereas this takes a whole tree and starts from its root.
pub fn first_in_generation<T>(
  tree: &Tree<T>,
  generation: usize,
) -> Result<Option<NodeRef<'_, T>>, Box<dyn Error>> {
  first_in_nth_generation_of_descendents(
    tree.root(), generation) }

/// Returns the first (in the DFS sense)
/// node at the Nth generation of descendants from a given node.
///
/// # Arguments
/// * `node` - The starting node (generation 0)
/// * `generations` - The depth (number of generations) to search
///
/// # Returns
/// * `Some(NodeRef)` if a node exists at that generation
/// * `None` if no node exists at that generation
fn first_in_nth_generation_of_descendents<T>(
  node: NodeRef<'_, T>,
  generations: usize,
) -> Result<Option<NodeRef<'_, T>>, Box<dyn Error>> {
  fn dfs_find_at_depth<T>(
    node: NodeRef<'_, T>,
    current_depth: usize,
    target_depth: usize,
  ) -> Option<NodeRef<'_, T>> {
    if current_depth == target_depth {
      return Some(node); }
    for child in node.children() {
      if let Some(found) =
        dfs_find_at_depth(
          child, current_depth + 1, target_depth)
      { return Some(found); }}
    None }
  Ok ( dfs_find_at_depth (
    node, 0, generations )) }

/// Returns the next node in the same generation as the given node.
///
/// Algorithm:
/// - If N has a next sibling, return it.
/// - Otherwise, climb to N's parent and try each of parent's subsequent (but not previous!) siblings,
///   using first_in_nth_generation_of_descendents with generations=1
/// - If that doesn't work, climb to N's grandparent and try each subsequent sibling,
///   using first_in_nth_generation_of_descendents with generations=2
/// - Once we can't climb any higher, return None.
pub fn next_in_generation<T>(
  node: NodeRef<'_, T>
) -> Option<NodeRef<'_, T>> {
  if let Some(next_sibling) = node.next_sibling() {
    return Some(next_sibling); }
  let mut ancestor: NodeRef<'_, T> = node;
  let mut generations_climbed: usize = 0;
  loop {
    ancestor = match ancestor.parent() {
      Some(parent) => parent,
      None => return None, // Can't climb any higher
    };
    generations_climbed += 1;
    // We'll try each of the ancestor's subsequent siblings,
    // but not the siblings that preceded it.
    let mut sibling_after_ancestor: Option<NodeRef<'_, T>> =
      ancestor.next_sibling ();
    while let Some(next_sib) = sibling_after_ancestor {
      // Try to descend back down (generations_climbed) levels
      if let Ok(Some(result)) =
        first_in_nth_generation_of_descendents(
          next_sib,
          generations_climbed )
      { return Some(result); }
      sibling_after_ancestor = next_sib.next_sibling(); }
    // None found from this ancestor's next siblings. Climb yet higher.
  }}

/// Collects all NodeIds at a given generation in a tree,
/// relative to an effective root (which might be the true root).
/// Generation 0 = the effective root; 1 = its children; etc.
/// If effective_root is None, uses the true tree root.
/// Returns an empty Vec if the generation doesn't exist.
/// Returns an error if effective_root is not in the tree.
pub fn collect_generation_ids<T>(
  tree           : &Tree<T>,
  generation     : usize,
  effective_root : Option<NodeId>,
) -> Result<Vec<NodeId>, Box<dyn Error>> {
  let effective_root_noderef : NodeRef<'_, T> =
    match effective_root {
      None => tree.root(),
      Some(nid) => tree.get(nid)
        . ok_or("collect_generation_ids: effective_root not in tree")? };
  let mut result : Vec<NodeId> = Vec::new();
  fn collect_at_depth<T>(
    node: NodeRef<'_, T>,
    current_depth: usize,
    target_depth: usize,
    result: &mut Vec<NodeId>,
  ) {
    if current_depth == target_depth {
      result.push (node.id());
      return; }
    for child in node.children() {
      collect_at_depth (
        child, current_depth + 1, target_depth, result); }}
  collect_at_depth (
    effective_root_noderef, 0, generation, &mut result);
  Ok (result) }
