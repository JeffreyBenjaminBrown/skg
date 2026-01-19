use ego_tree::{Tree, NodeId, NodeMut, NodeRef};

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
  let mut node_ref : NodeRef<T> = tree.get(treeid)
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
  let target_id : NodeId = {
    // Climb to target node via immutable reference
    let mut node_ref : NodeRef<T> = tree.get(treeid)
      .ok_or("node not found")?;
    for _ in 0..generation {
      node_ref = node_ref.parent()
        .ok_or("cannot climb that many generations")?; }
    node_ref.id() };
  let mut node_mut : NodeMut<T> = tree.get_mut(target_id)
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

/// Apply a function to every node in a subtree, DFS preorder traversal.
/// Starts at `start_node_id` and recursively visits all descendants.
/// The function receives `NodeMut<T>`, which allows:
/// - Mutating the node's value via `.value()`
/// - Navigating to parent via `.parent()` (immutable)
/// - Accessing the tree via `.tree()` (immutable)
///
/// Example: Convert TrueNodes to Aliases when parent is AliasCol
/// ```ignore
/// do_everywhere_in_tree_dfs(&mut tree, root_id, |mut node| {
///   if let Some(parent) = node.parent() {
///     if matches!(parent.value(), OrgNode { kind: Scaff(AliasCol), .. }) {
///       // Convert to Alias
///       let val = node.value();
///       // mutate val
///     }
///   }
///   Ok(())
/// })
/// ```
pub fn do_everywhere_in_tree_dfs<T, F>(
  tree          : &mut Tree<T>,
  start_node_id : NodeId,
  f             : &mut F
) -> Result<(), String>
where F: FnMut(NodeMut<T>) -> Result<(), String> {
  let child_ids : Vec<NodeId> = {
    // Collect early so no borrow conflicts
    let node_ref : NodeRef<T> =
      tree . get ( start_node_id ) . ok_or ( "do_everywhere_in_tree_dfs: start node not found" ) ?;
    node_ref . children ()
      . map ( |c| c . id( ))
      . collect () };
  { // do F here
    let node_mut : NodeMut<T> = tree . get_mut ( start_node_id )
      . ok_or ( "do_everywhere_in_tree_dfs: node not found" ) ?;
    f ( node_mut ) ?; }
  for child_id in child_ids { // recurse
    do_everywhere_in_tree_dfs (
      tree, child_id, f ) ?; }
  Ok (( ))}

/// Compare two trees for structural and value equality.
/// Returns true if both trees have the same structure and all node values are equal.
pub fn eq_trees<T: PartialEq>(
  node1: ego_tree::NodeRef<T>,
  node2: ego_tree::NodeRef<T>
) -> bool {
  if node1.value() != node2.value() {
    return false;
  }
  let children1: Vec<_> = node1.children().collect();
  let children2: Vec<_> = node2.children().collect();
  if children1.len() != children2.len() {
    return false;
  }
  children1.iter().zip(children2.iter())
    .all(|(c1, c2)| eq_trees(*c1, *c2))
}
