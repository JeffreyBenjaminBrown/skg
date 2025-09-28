// Local types for parsing org files.

use crate::types::{ID, RelToOrgParent, MetadataItem};

/// Parsed metadata from an org-mode headline
#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMetadata {
  pub id: Option<ID>,
  pub repeated: bool,
  pub folded: bool,
  pub focused: bool,
  pub rel_to_parent: RelToOrgParent,
  pub metadata: Vec<MetadataItem>,
}
