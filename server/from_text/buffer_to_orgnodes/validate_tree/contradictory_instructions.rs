use crate::types::orgnode::EditRequest;
use crate::types::orgnode_new::OrgNode;
use crate::types::misc::{ID, SourceNickname};

use ego_tree::{Tree,NodeRef};
use std::collections::{HashMap, HashSet};

/// Enum to track whether a node should be deleted or not
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum WhetherToDelete {
  Delete,
  DoNotDelete,
}

/// ASSUMES:
/// everything 'find_buffer_errors_for_saving' assumes.
/// .
/// PURPOSE:
/// Find contradictory instructions in a buffer the user asked to save.
/// The specific problems it finds are:
/// - There's a node with 'toDelete' true,
///   but another node with the same ID that has 'toDelete' false.
/// - Two nodes with the same ID 'define their contents'
///   (i.e. their 'indefinitive' fields are both false).
/// - Two nodes with the same ID have different sources,
///   even if some are indefinitive.
/// .
/// STRATEGY:
/// Builds a map from IDs to sets of WhetherToDelete.
/// After traversing the tree, reports every key (ID)
/// for which the associated value (set) has size 2.
/// Also builds a map from IDs to count of defining containers,
/// and a map from IDs to sets of sources. */
pub fn find_inconsistent_instructions(
  forest: &Tree<OrgNode>
) -> (Vec<ID>, // IDs with inconsistent deletions across nodes
      Vec<ID>, // IDs with multiple defining nodes
      Vec<(ID, HashSet<SourceNickname>)>) { // IDs with inconsistent sources
  let (id_toDelete_instructions, id_to_definer_count, id_to_sources) =
    // Find the problems
    traverse_forest_and_collect(forest);
  let mut inconsistent_deletion_ids: Vec<ID> = Vec::new();
  let mut problematic_defining_ids: Vec<ID> = Vec::new();
  let mut inconsistent_source_ids: Vec<(ID, HashSet<SourceNickname>)> =
    Vec::new();
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
  { // Collect inconsistent sources
    for (id, sources) in id_to_sources {
      if sources.len() > 1 {
        // Multiple different sources for this ID
        inconsistent_source_ids.push((id, sources)); }} }
  ( inconsistent_deletion_ids,
    problematic_defining_ids,
    inconsistent_source_ids ) }

/// Collect delete instructions, defining containers, and sources.
fn traverse_forest_and_collect(
  forest: &Tree<OrgNode> // "forest" = tree with ForestRoot
) -> (HashMap<ID, HashSet<WhetherToDelete>>, // contradictory deletes
      HashMap<ID, usize>, // multiple defining containers
      HashMap<ID, HashSet<SourceNickname>>) { // inconsistent sources
  let mut id_toDelete_instructions
    : HashMap<ID, HashSet<WhetherToDelete>>
    = HashMap::new();
  let mut id_to_definer_count
    : HashMap<ID, usize>
    = HashMap::new();
  let mut id_to_sources
    : HashMap<ID, HashSet<SourceNickname>>
    = HashMap::new();
  traverse_node_recursively_and_collect(
    forest.root(),
    &mut id_toDelete_instructions,
    &mut id_to_definer_count,
    &mut id_to_sources);
  (id_toDelete_instructions, id_to_definer_count, id_to_sources) }

/// Traverse a single node and its children to collect delete instructions, defining containers, and sources
fn traverse_node_recursively_and_collect(
  node_ref: NodeRef<OrgNode>,
  id_toDelete_instructions: &mut
    HashMap<ID, HashSet<WhetherToDelete>>,
  id_defining_count: &mut
    HashMap<ID, usize>,
  id_to_sources: &mut
    HashMap<ID, HashSet<SourceNickname>>
) {
  let orgnode: &OrgNode = node_ref.value();
  if let Some(id) = orgnode.id() {
    let delete_instruction: WhetherToDelete =
      if matches!(orgnode.edit_request(),
                  Some(EditRequest::Delete)) {
        WhetherToDelete::Delete
      } else { WhetherToDelete::DoNotDelete };
    id_toDelete_instructions // record delete_instruction
      . entry(id.clone())
      . or_insert_with(HashSet::new)
      . insert(delete_instruction);
    if !orgnode.is_indefinitive() {
      // Increment the count for this defining container
      *id_defining_count . entry (id.clone())
        . or_insert(0) += 1; }
    if let Some(source_str) = orgnode.source() {
      // Collect source for this ID
      let source : SourceNickname =
        SourceNickname::from(source_str.as_str());
      id_to_sources
        . entry(id.clone())
        . or_insert_with(HashSet::new)
        . insert(source); }}
  for child in node_ref.children() { // recurse
    traverse_node_recursively_and_collect(
      child,
      id_toDelete_instructions,
      id_defining_count,
      id_to_sources); }}
