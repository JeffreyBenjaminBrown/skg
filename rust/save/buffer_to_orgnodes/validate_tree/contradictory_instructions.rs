use crate::types::{OrgNode, ID};

use ego_tree::{Tree,NodeRef};
use std::collections::{HashMap, HashSet};

/// Enum to track whether a node should be deleted or not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WhetherToDelete {
  Delete,
  DoNotDelete,
}

/* PURPOSE:
Find contradictory instructions in a buffer the user has tried to save.
.
If there's a node with 'toDelete' true,
but another node with the same ID that has 'toDelete' false,
that's a problem.
.
And if two nodes with the same ID 'define their contents'
(identifiable if repeated=false and mightContainMore=false),
that's a problem, too.
.
STRATEGY:
Builds a map from IDs to sets of WhetherToDelete.
After traversing the tree, reports every key (ID)
for which the associated value (set) has size 2.
Also builds a map from IDs to count of defining containers.
.
TODO:
Maybe this needs to be more complicated,
because one could delete an extra_id. */
pub fn find_inconsistent_instructions(
  trees: &[Tree<OrgNode>]
) -> (Vec<ID>, // IDs with inconsistent deletions across nodes
      Vec<ID>) { // IDs with multiple defining nodes
  let (id_toDelete_instructions, id_to_definer_count) =
    // Find the problems
    traverse_forest_and_collect(trees);
  let mut inconsistent_deletion_ids: Vec<ID> = Vec::new();
  let mut problematic_defining_ids: Vec<ID> = Vec::new();
  { // Collect inconsistent deletion instructions
    for (id, delete_set) in id_toDelete_instructions {
      if delete_set.len() == 2 {
        // Size 2 means both Delete and DoNotDelete are present.
        inconsistent_deletion_ids.push(id); }} }
  { // Collect multiple defining containers
    for (id, count) in id_to_definer_count {
      if count > 1 {
        // Multiple defining containers for this ID
        problematic_defining_ids.push(id); }} }
  ( inconsistent_deletion_ids,
    problematic_defining_ids ) }

/// Traverse all trees to collect delete instructions and defining containers
fn traverse_forest_and_collect(
  trees: &[Tree<OrgNode>]
) -> (HashMap<ID, HashSet<WhetherToDelete>>, // contradictory deletes
      HashMap<ID, usize>) { // multiple defining containers
  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  let mut id_to_definer_count
    : HashMap<ID, usize>
    = HashMap::new();
  for tree in trees {
    traverse_node_recursively_and_collect(
      tree.root(),
      &mut id_toDelete_instructions,
      &mut id_to_definer_count); }
  (id_toDelete_instructions, id_to_definer_count) }

/// Traverse a single node and its children to collect delete instructions and defining containers
fn traverse_node_recursively_and_collect(
  node_ref: NodeRef<OrgNode>,
  id_toDelete_instructions: &mut
    HashMap<ID, HashSet<WhetherToDelete>>,
  id_defining_count: &mut
    HashMap<ID, usize>
) {
  let node: &OrgNode = node_ref.value();
  if let Some(id) = &node.metadata.id {
    // Handle delete instructions
    let delete_instruction =
      if node.metadata.toDelete {
        WhetherToDelete::Delete
      } else { WhetherToDelete::DoNotDelete };
    id_toDelete_instructions // record delete_instruction
      . entry(id.clone())
      . or_insert_with(HashSet::new)
      . insert(delete_instruction);

    // Handle defining containers
    if !node.metadata.repeat && !node.metadata.mightContainMore {
      // Increment the count for this defining container
      *id_defining_count.entry(id.clone()).or_insert(0) += 1; }}
  for child in node_ref.children() { // recurse
    traverse_node_recursively_and_collect(
      child,
      id_toDelete_instructions,
      id_defining_count); }}
