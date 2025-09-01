use crate::types::{ID, OrgNode};
use uuid::Uuid;

/// Returns something almost identical,
/// but replacing each `None`-valued ID with a random UUID.
pub fn assign_ids_recursive (
  orgnode : &OrgNode )
  -> OrgNode {

  fn recurse_in_branches (
    branches: &[OrgNode]
  ) -> Vec<OrgNode> { branches
                      . iter ()
                      . map (assign_ids_recursive)
                      . collect () }

  match orgnode {
    OrgNode::Content (content_node) => {
      let mut new_content =
        content_node.clone();
      new_content.id = Some(
        content_node . id . clone() . unwrap_or_else (
          || ID::new (
            Uuid::new_v4() . to_string() )) );
      new_content.branches =
        recurse_in_branches ( &content_node.branches );
      OrgNode::Content (new_content)
    },
    OrgNode::Aliases (alias_node) => {
      let mut new_aliases = alias_node.clone();
      new_aliases.branches =
        recurse_in_branches ( &alias_node.branches );
      OrgNode::Aliases (new_aliases)
    }} }
