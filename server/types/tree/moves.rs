//! Moving a node that may already be in the tree.
//! PITFALL: ego_tree's 'insert_id_before' and 'insert_id_after' read
//! the neighbor they link to before detaching the node they move, so
//! moving a node next to itself links it to itself. (Its 'prepend_id'
//! and 'append_id' check for a node already in place.) Each helper
//! here detaches first, and does nothing when the node is already in
//! place.

use ego_tree::{NodeId, NodeRef, Tree};
use std::collections::HashSet;

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

/// Make 'node' the sibling immediately before 'neighbor'.
pub fn move_before<T> (
  tree     : &mut Tree<T>,
  neighbor : NodeId,
  node     : NodeId,
) -> Result<(), String> {
  if neighbor == node { return Ok (( )); }
  let previous : Option<NodeId> =
    node_ref (tree, neighbor, "move_before") ?
    . prev_sibling () . map ( |sibling| sibling . id () );
  if previous == Some (node) { return Ok (( )); }
  detach (tree, node, "move_before") ?;
  tree . get_mut (neighbor) . ok_or ("move_before: neighbor not found") ?
    . insert_id_before (node);
  Ok (( )) }

/// Make 'node' the sibling immediately after 'neighbor'.
pub fn move_after<T> (
  tree     : &mut Tree<T>,
  neighbor : NodeId,
  node     : NodeId,
) -> Result<(), String> {
  if neighbor == node { return Ok (( )); }
  let next : Option<NodeId> =
    node_ref (tree, neighbor, "move_after") ?
    . next_sibling () . map ( |sibling| sibling . id () );
  if next == Some (node) { return Ok (( )); }
  detach (tree, node, "move_after") ?;
  tree . get_mut (neighbor) . ok_or ("move_after: neighbor not found") ?
    . insert_id_after (node);
  Ok (( )) }

/// Place each of 'created' that 'in_model_order' lists right after
/// the child listed before it, or first among the children of
/// 'parent' when it is listed first; every other child stays where
/// it stands. This is how a reconciler places what it creates without
/// moving what it keeps ('docs/diff-view.org', "What a reconciler
/// keeps"): 'in_model_order' is its kept and created children in the
/// order of its model.
pub fn place_created_children_after_their_model_predecessors<T> (
  tree           : &mut Tree<T>,
  parent         : NodeId,
  in_model_order : &[NodeId],
  created        : &HashSet<NodeId>,
) -> Result<(), String> {
  let mut previous : Option<NodeId> = None;
  for node in in_model_order {
    if created . contains (node) {
      match previous {
        None => move_to_first_child (tree, parent, *node) ?,
        Some (previous) => move_after (tree, previous, *node) ?, }}
    previous = Some (*node); }
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
