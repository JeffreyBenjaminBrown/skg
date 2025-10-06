use crate::save::{headline_to_triple, HeadlineInfo};
use crate::types::{OrgNode, OrgnodeMetadata};
use ego_tree::Tree;

/// Compare two org-mode headlines ignoring ID differences.
/// Converts each headline to HeadlineInfo and strips ID from metadata.
pub fn compare_headlines_modulo_id(
  headline1: &str,
  headline2: &str
) -> bool {
  let info1: Option<HeadlineInfo> = headline_to_triple(headline1);
  let info2: Option<HeadlineInfo> = headline_to_triple(headline2);

  match (info1, info2) {
    (Some((level1, metadata1, title1)),
     Some((level2, metadata2, title2))) => {
      let has_id1: bool = metadata1.as_ref().map_or(false, |m| m.id.is_some());
      let has_id2: bool = metadata2.as_ref().map_or(false, |m| m.id.is_some());
      if has_id1 != has_id2 {
        return false; } // One has ID, other doesn't, so they are unequal.
      // Strip IDs from both (no-op if no ID present) and compare
      let stripped_metadata1: Option<OrgnodeMetadata> = strip_id_from_metadata_struct(metadata1);
      let stripped_metadata2: Option<OrgnodeMetadata> = strip_id_from_metadata_struct(metadata2);
      (level1, stripped_metadata1, title1) == (level2, stripped_metadata2, title2)
    },
    (None, None) => headline1 == headline2,  // Both are not headlines, compare directly
    _ => false,  // One is headline, other is not
  }
}

/// Compare two tree forests modulo ID differences.
/// Trees are considered the same if their structure and content match,
/// ignoring ID values (but not ID presence/absence).
pub fn compare_trees_modulo_id(
  trees1: &[Tree<OrgNode>],
  trees2: &[Tree<OrgNode>]
) -> bool {
  if trees1.len() != trees2.len() {
    return false;
  }

  for (tree1, tree2) in trees1.iter().zip(trees2.iter()) {
    if !compare_nodes_modulo_id(tree1.root(), tree2.root()) {
      return false;
    }
  }

  true
}

/// Compare two nodes and their subtrees modulo ID differences.
fn compare_nodes_modulo_id(
  node1: ego_tree::NodeRef<OrgNode>,
  node2: ego_tree::NodeRef<OrgNode>
) -> bool {
  let n1 = node1.value();
  let n2 = node2.value();

  // Compare structure and content
  if n1.title != n2.title ||
     n1.body != n2.body ||
     n1.metadata.relToOrgParent != n2.metadata.relToOrgParent ||
     n1.metadata.cycle != n2.metadata.cycle ||
     n1.metadata.focused != n2.metadata.focused ||
     n1.metadata.folded != n2.metadata.folded ||
     n1.metadata.mightContainMore != n2.metadata.mightContainMore ||
     n1.metadata.repeat != n2.metadata.repeat ||
     n1.metadata.toDelete != n2.metadata.toDelete {
    return false;
  }

  // Compare ID presence/absence (but not values)
  match (&n1.metadata.id, &n2.metadata.id) {
    (Some(_), Some(_)) => {}, // Both have IDs, values don't matter
    (None, None) => {},       // Both lack IDs
    _ => return false,        // One has ID, other doesn't
  }

  // Compare children
  let children1: Vec<_> = node1.children().collect();
  let children2: Vec<_> = node2.children().collect();

  if children1.len() != children2.len() {
    return false;
  }

  for (child1, child2) in children1.iter().zip(children2.iter()) {
    if !compare_nodes_modulo_id(*child1, *child2) {
      return false;
    }
  }

  true
}

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<OrgnodeMetadata>
) -> Option<OrgnodeMetadata> {
  metadata.map(|mut meta| {
    meta.id = None;
    meta
  })
}
