// Local types for parsing org files.

use crate::types::ID;

/// Parsed metadata from an org-mode headline
#[derive(Debug, Clone, PartialEq)]
pub struct OrgNodeMetadata {
  pub id: Option<ID>,
  pub repeated: bool,
  pub folded: bool,
  pub focused: bool,
  pub title: String,
  pub node_type: Option<String>, // TODO: This should not be an Option, but instead default to 'ContentNode'.
}
