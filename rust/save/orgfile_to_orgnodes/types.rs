// Local types for parsing org files.

use crate::types::{ID, OrgNodeInterpEnum};

/// Parsed metadata from an org-mode headline
#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMetadata {
  pub id: Option<ID>,
  pub repeated: bool,
  pub folded: bool,
  pub focused: bool,
  pub title: String, // TODO: This is not metadata! Functions using it should pass it alongside an OrgNodeMetadata, rather than within one.
  pub node_type: OrgNodeInterpEnum,
}
