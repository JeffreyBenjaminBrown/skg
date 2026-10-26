use ego_tree::{Tree, NodeId, NodeMut, NodeRef};

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
where F: FnOnce (&mut T) -> R, {
  let target_gnid : NodeId = {
    // Climb to target node via immutable reference
    let mut node_ref : NodeRef<T> = tree . get (treeid)
      . ok_or ("node not found")?;
    for _ in 0..generation {
      node_ref = node_ref . parent()
        . ok_or ("cannot climb that many generations")?; }
    node_ref . id() };
  let mut node_mut : NodeMut<T> = tree . get_mut (target_gnid)
    . ok_or ("target node not found")?;
  Ok(f(node_mut . value() )) }

/// Write to a node's value in a tree, applying a mutating function to it.
/// Returns an error if the node is not found.
pub fn write_at_node_in_tree<T, F, R>(
    tree: &mut Tree<T>,
    treeid: NodeId,
    f: F
) -> Result<R, String>
where F: FnOnce (&mut T) -> R, {
  write_at_ancestor_in_tree(tree, treeid, 0, f) }

/// Access a node mutably for structural operations (prepend, append, detach, etc.).
/// Unlike write_at_node_in_tree which only provides &mut T (the value),
/// this provides NodeMut<T> so you can modify tree structure.
/// Returns an error if the node is not found.
pub fn with_node_mut<T, F, R>(
  tree    : &mut Tree<T>,
  treeid    : NodeId,
  f       : F
) -> Result<R, String>
where F: FnOnce(NodeMut<T>) -> R {
  let node_mut : NodeMut<T> = tree . get_mut (treeid)
    . ok_or ("with_node_mut: node not found") ?;
  Ok ( f (node_mut) ) }

/// Apply a function to every node in a subtree, DFS traversal.
/// Starts at `start_treeid` and recursively visits all descendants.
/// The function receives `NodeMut<T>`, which allows:
/// - Mutating the node's value via `.value()`
/// - Navigating to parent via `.parent()` (immutable)
/// - Accessing the tree via `.tree()` (immutable)
/// If `preorder` is true, runs the function before descendants.
/// If `preorder` is false, runs it after descendants.
///
/// Example: Convert Occurrences to Aliases when parent is an alias folder
/// ```ignore
/// do_everywhere_in_tree_dfs(&mut tree, root_id, true, |mut node| {
///   if let Some(parent) = node.parent() {
///     if matches!(parent.value(), Viewnode { kind: ViewnodeKind::Folder (Folder::Alias), .. }) {
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
  start_treeid  : NodeId,
  preorder      : bool,
  f             : &mut F
) -> Result<(), String>
where F: FnMut(NodeMut<T>) -> Result<(), String> {
  let child_gnids : Vec<NodeId> = {
    // Collect early so no borrow conflicts
    let node_ref : NodeRef<T> =
      tree . get (start_treeid) . ok_or ("do_everywhere_in_tree_dfs: start node not found") ?;
    node_ref . children ()
      . map ( |c| c . id( ))
      . collect () };
  if preorder {
    let node_mut : NodeMut<T> = tree . get_mut (start_treeid)
      . ok_or ("do_everywhere_in_tree_dfs: node not found") ?;
    f (node_mut) ?; }
  for child_gnid in child_gnids { // recurse
    do_everywhere_in_tree_dfs (
      tree, child_gnid, preorder, f ) ?; }
  if !preorder {
    let node_mut : NodeMut<T> = tree . get_mut (start_treeid)
      . ok_or ("do_everywhere_in_tree_dfs: node not found") ?;
    f (node_mut) ?; }
  Ok (( ))}

/// Like do_everywhere_in_tree_dfs, but read-only.
/// Takes `&Tree<T>` instead of `&mut Tree<T>`,
/// and provides `NodeRef<T>` instead of `NodeMut<T>`.
pub fn do_everywhere_in_tree_dfs_readonly<T, F>(
  tree          : &Tree<T>,
  start_treeid  : NodeId,
  preorder      : bool,
  f             : &mut F
) -> Result<(), String>
where F: FnMut(NodeRef<T>) -> Result<(), String> {
  let node_ref : NodeRef<T> =
    tree . get (start_treeid) . ok_or (
      "do_everywhere_in_tree_dfs_readonly: start node not found" ) ?;
  if preorder { f (node_ref) ?; }
  for child in node_ref . children() {
    do_everywhere_in_tree_dfs_readonly (
      tree, child . id(), preorder, f ) ?; }
  if !preorder {
    let node_ref : NodeRef<T> =
      tree . get (start_treeid) . ok_or (
        "do_everywhere_in_tree_dfs_readonly: start node not found" ) ?;
    f (node_ref) ?; }
  Ok (( ))}

/// Like do_everywhere_in_tree_dfs, but the lambda ("f") returns:
/// - Ok(true) to continue recursing into children, or
/// - Ok(false) to stop (prune) at this node.
pub fn do_everywhere_in_tree_dfs_prunable<T, F>(
  tree          : &mut Tree<T>,
  start_treeid  : NodeId,
  f             : &mut F
) -> Result<(), String>
where F: FnMut(NodeMut<T>
              ) -> Result<bool, String> {
  let child_gnids : Vec<NodeId> = {
    let node_ref : NodeRef<T> =
      tree . get (start_treeid) . ok_or (
        "do_everywhere_in_tree_dfs_prunable: start node not found" )?;
    node_ref . children ()
      . map ( |c| c . id() )
      . collect () };
  let should_continue : bool = {
    let node_mut : NodeMut<T> =
      tree . get_mut (start_treeid) . ok_or (
        "do_everywhere_in_tree_dfs_prunable: node not found" )?;
    f (node_mut)? };
  if should_continue {
    for child_gnid in child_gnids {
      do_everywhere_in_tree_dfs_prunable (
        tree, child_gnid, f )?; }}
  Ok (( )) }

/// Compare two trees for structural and value equality.
/// Returns true if both trees have the same structure and all node values are equal.
pub fn eq_trees<T: PartialEq>(
  node1: ego_tree::NodeRef<T>,
  node2: ego_tree::NodeRef<T>
) -> bool {
  if node1 . value() != node2 . value() {
    return false;
  }
  let children1: Vec<_> = node1 . children() . collect();
  let children2: Vec<_> = node2 . children() . collect();
  if children1 . len() != children2 . len() {
    return false;
  }
  children1 . iter() . zip(children2 . iter())
    . all(|(c1, c2)| eq_trees(*c1, *c2))
}
