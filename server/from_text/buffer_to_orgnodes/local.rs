/// Local validation functions for OrgNode trees.
/// These check structural properties of individual nodes
/// without requiring global context.

use crate::types::orgnode::{OrgNode, OrgNodeKind, TrueNode};
use crate::types::misc::{ID, SkgConfig};
use ego_tree::{Tree, NodeId};
use std::collections::HashSet;

/// Check that all non-ignored TrueNode children have distinct IDs.
/// "Non-ignored" means parent_ignores == false.
/// Returns true if all such children have distinct IDs,
/// or if there are no such children.
pub fn nonignored_children_have_distinct_ids (
  tree    : &Tree<OrgNode>,
  node_id : NodeId,
) -> bool {
  let Some(node_ref) = tree.get(node_id)
    else { return true; };
  let mut seen : HashSet<ID> = HashSet::new();
  for child in node_ref.children() {
    if let OrgNodeKind::True(t) = &child.value().kind {
      if !t.parent_ignores {
        if let Some(id) = &t.id_opt {
          if !seen.insert(id.clone()) {
            return false; }}}}}
  true }

/// Check if a TrueNode has a source that exists in the config.
pub fn has_valid_source (
  t      : &TrueNode,
  config : &SkgConfig,
) -> bool {
  t.source_opt.as_ref()
    .is_some_and( |s| config.sources.contains_key(s) ) }

/// Check if a TrueNode has an ID.
pub fn has_id ( t : &TrueNode ) -> bool {
  t.id_opt.is_some() }
