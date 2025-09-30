use crate::types::{OrgNode2, ID};

use ego_tree::Tree;
use std::collections::{HashMap, HashSet};

/// Enum to track whether a node should be deleted or not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WhetherToDelete {
  Delete,
  DoNotDelete,
}

/* PURPOSE:
If there's a node with 'toDelete' true,
but another node with the same ID that has 'toDelete' false,
its ID is included in the output.
.
STRATEGY:
Builds a map from IDs to sets of WhetherToDelete.
After traversing the tree, reports every key (ID)
for which the associated value (set) has size 2.
.
TODO:
Maybe this needs to be more complicated,
because one could delete an extra_id. */
pub fn find_inconsistent_toDelete_instructions(
  trees: &[Tree<OrgNode2>]
) -> Vec<ID> {
  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  for tree in trees {
    traverse_node_and_collect(
      tree.root(), // traversal starts at root
      &mut id_toDelete_instructions); }
  let mut inconsistent_ids: Vec<ID> = Vec::new();
  for (id, delete_set) in id_toDelete_instructions {
    if delete_set.len() == 2 {
      // Size 2 means both Delete and DoNotDelete are present.
      inconsistent_ids.push(id); }}
  inconsistent_ids }

/// Traverse a node and its children to collect delete instructions
fn traverse_node_and_collect(
  node_ref: ego_tree::NodeRef<OrgNode2>,
  id_toDelete_instructions : &mut
    HashMap<ID, HashSet<WhetherToDelete>>
) {
  let node: &OrgNode2 = node_ref.value();

  if let Some(id) = &node.metadata.id {
    // This skips  nodes without IDs, which have nothing to contradict.
    let delete_instruction = if node.metadata.toDelete {
      WhetherToDelete::Delete
    } else { WhetherToDelete::DoNotDelete };
    { // record delete_instruction
      id_toDelete_instructions
        . entry(id.clone())
        . or_insert_with(HashSet::new)
        . insert(delete_instruction); }
  }
  for child in node_ref.children() { // recurse
    traverse_node_and_collect( child,
                               id_toDelete_instructions);
  }
}
