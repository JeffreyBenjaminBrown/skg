/// Utilities for working with ego_tree::Tree.

use ego_tree::{Tree, NodeRef};
use std::error::Error;

/// Returns the first node in the Nth generation of the tree.
/// Generation 1 is the root; generation 2 is the next one; etc.
/// Returns None if the Nth generation doesn't exist in the tree.
/// Returns an error if generation < 1.
pub fn first_in_generation<T>(
  tree: &Tree<T>,
  generation: usize,
) -> Result<Option<NodeRef<'_, T>>, Box<dyn Error>> {
  first_in_nth_generation_of_descendents(
    tree.root(), generation) }

/// Returns the first node at the Nth generation of descendants from a given node.
///
/// # Arguments
/// * `node` - The starting node (generation 1)
/// * `generations` - The depth (number of generations) to search
///
/// # Returns
/// * `Some(NodeRef)` if a node exists at that generation
/// * `None` if no node exists at that generation
/// * Error if generations < 1
///
/// # Examples
/// - generations=1: returns the node itself
/// - generations=2: returns the node's first child (if any)
/// - generations=3+: DFS search for first node at that depth
pub fn first_in_nth_generation_of_descendents<T>(
  node: NodeRef<'_, T>,
  generations: usize,
) -> Result<Option<NodeRef<'_, T>>, Box<dyn Error>> {
  if generations < 1 {
    return Err("Generations must be >= 1".into()); }
  let depth_difference: usize = (
    generations - 1 );
  fn dfs_find_at_depth<T>(
    node: NodeRef<'_, T>,
    current_depth: usize,
    depth_difference: usize,
  ) -> Option<NodeRef<'_, T>> {
    if current_depth == depth_difference {
      return Some(node); }
    for child in node.children() {
      if let Some(found) =
        dfs_find_at_depth(
          child, current_depth + 1, depth_difference)
      { return Some(found); }}
    None }
  Ok ( dfs_find_at_depth (
    node, 0, depth_difference )) }

/// Runs 'first_in_nth_generation' on (potentially) each tree,
/// stopping and returning at the first success.
pub fn first_in_generation_in_forest<'a, T>(
  forest: &'a [Tree<T>],
  generations: usize,
) -> Result<Option<NodeRef<'a, T>>, Box<dyn Error>> {
  for tree in forest {
    if let Some(result)
      = first_in_nth_generation_of_descendents(
        tree.root(), generations)?
    { return Ok(Some(result)); }}
  Ok(None) }

/// Returns the next node in the same generation as the given node.
///
/// Algorithm:
/// - If N has a next sibling, return it
/// - Otherwise, climb to N's parent and try each of parent's subsequent (but not previous!) siblings,
///   using first_in_nth_generation_of_descendents with generations=2
/// - If that doesn't work, climb to N's grandparent and try each subsequent sibling,
///   using first_in_nth_generation_of_descendents with generations=3
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
      // Try to descend back down (generations_climbed + 1) levels
      if let Ok(Some(result)) =
        first_in_nth_generation_of_descendents(
          next_sib,
          generations_climbed + 1 )
      { return Some(result); }
      sibling_after_ancestor = next_sib.next_sibling(); }
    // None found from this ancestor's next siblings. Climb yet higher.
  }}

/// Runs 'next_in_generation' on the current tree,
/// and failing that, 'first_in_generation'
/// on the subsequent trees.
pub fn next_in_generation_in_forest<'a, T>(
  forest: &'a [Tree<T>],
  tree_index: usize, // where in 'forest' to find the tree containing 'node'
  node: NodeRef<'a, T>
) -> Option<NodeRef<'a, T>> {
  if let Some(result) // try this tree
    = next_in_generation(node)
  { return Some(result); }
  { // try subsequent trees
    let depth : usize = {
      let mut depth_count: usize = 1;
      let mut ancestor: NodeRef<'_, T> = node;
      while let Some(parent) = ancestor.parent() {
        depth_count += 1;
        ancestor = parent; }
      depth_count };
    for tree in &forest[tree_index + 1..] {
      if let Ok(Some(result))
        = first_in_nth_generation_of_descendents(
          tree.root(), depth )
      { return Some(result); }} }
  None }
