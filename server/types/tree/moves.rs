//! Moving a node that may already be in the tree.
//! PITFALL: ego_tree's 'prepend_id', 'append_id', 'insert_id_before'
//! and 'insert_id_after' read the neighbor they link to before
//! detaching the node they move, so moving a node next to itself
//! links it to itself. Each helper here detaches first, and does
//! nothing when the node is already in place.

use ego_tree::{NodeId, NodeRef, Tree};

/// Make 'node' the first child of 'parent'.
pub fn move_to_first_child<T> (
  tree   : &mut Tree<T>,
  parent : NodeId,
  node   : NodeId,
) -> Result<(), String> {
  let first : Option<NodeId> =
    node_ref (tree, parent, "move_to_first_child") ?
    . first_child () . map ( |child| child . id () );
  if first == Some (node) { return Ok (( )); }
  detach (tree, node, "move_to_first_child") ?;
  tree . get_mut (parent) . ok_or ("move_to_first_child: parent not found") ?
    . prepend_id (node);
  Ok (( )) }

/// Make 'node' the last child of 'parent'.
pub fn move_to_last_child<T> (
  tree   : &mut Tree<T>,
  parent : NodeId,
  node   : NodeId,
) -> Result<(), String> {
  let last : Option<NodeId> =
    node_ref (tree, parent, "move_to_last_child") ?
    . last_child () . map ( |child| child . id () );
  if last == Some (node) { return Ok (( )); }
  detach (tree, node, "move_to_last_child") ?;
  tree . get_mut (parent) . ok_or ("move_to_last_child: parent not found") ?
    . append_id (node);
  Ok (( )) }

/// Make 'node' the sibling immediately before 'anchor'.
pub fn move_before<T> (
  tree   : &mut Tree<T>,
  anchor : NodeId,
  node   : NodeId,
) -> Result<(), String> {
  if anchor == node { return Ok (( )); }
  let previous : Option<NodeId> =
    node_ref (tree, anchor, "move_before") ?
    . prev_sibling () . map ( |sibling| sibling . id () );
  if previous == Some (node) { return Ok (( )); }
  detach (tree, node, "move_before") ?;
  tree . get_mut (anchor) . ok_or ("move_before: anchor not found") ?
    . insert_id_before (node);
  Ok (( )) }

/// Make 'node' the sibling immediately after 'anchor'.
pub fn move_after<T> (
  tree   : &mut Tree<T>,
  anchor : NodeId,
  node   : NodeId,
) -> Result<(), String> {
  if anchor == node { return Ok (( )); }
  let next : Option<NodeId> =
    node_ref (tree, anchor, "move_after") ?
    . next_sibling () . map ( |sibling| sibling . id () );
  if next == Some (node) { return Ok (( )); }
  detach (tree, node, "move_after") ?;
  tree . get_mut (anchor) . ok_or ("move_after: anchor not found") ?
    . insert_id_after (node);
  Ok (( )) }

/// Make 'ordered' the first children of 'parent', in that order.
pub fn arrange_first_children<T> (
  tree    : &mut Tree<T>,
  parent  : NodeId,
  ordered : &[NodeId],
) -> Result<(), String> {
  let mut previous : Option<NodeId> = None;
  for node in ordered {
    match previous {
      None => move_to_first_child (tree, parent, *node) ?,
      Some (previous) => move_after (tree, previous, *node) ?, }
    previous = Some (*node); }
  Ok (( )) }

/// Make 'ordered' the last children of 'parent', in that order.
pub fn arrange_last_children<T> (
  tree    : &mut Tree<T>,
  parent  : NodeId,
  ordered : &[NodeId],
) -> Result<(), String> {
  let mut next : Option<NodeId> = None;
  for node in ordered . iter () . rev () {
    match next {
      None => move_to_last_child (tree, parent, *node) ?,
      Some (next) => move_before (tree, next, *node) ?, }
    next = Some (*node); }
  Ok (( )) }

fn node_ref<'a, T> (
  tree   : &'a Tree<T>,
  treeid : NodeId,
  caller : &str,
) -> Result<NodeRef<'a, T>, String> {
  tree . get (treeid)
    . ok_or_else ( || format! ("{}: node not found", caller) ) }

fn detach<T> (
  tree   : &mut Tree<T>,
  treeid : NodeId,
  caller : &str,
) -> Result<(), String> {
  tree . get_mut (treeid)
    . ok_or_else ( || format! ("{}: node not found", caller) ) ?
    . detach ();
  Ok (( )) }

#[cfg(test)]
#[path = "../../../tests/unit/tree_moves.rs"]
mod tests;
