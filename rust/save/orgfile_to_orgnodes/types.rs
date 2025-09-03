// Local types for parsing org files.

use crate::types::{ID, OrgNodeInterpEnum};

/// Parsed metadata from an org-mode headline
#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMetadata {
  pub id: Option<ID>,
  pub repeated: bool,
  pub folded: bool,
  pub focused: bool,
  pub node_type: OrgNodeInterpEnum,
}
