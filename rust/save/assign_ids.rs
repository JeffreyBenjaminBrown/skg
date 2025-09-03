use crate::types::{ID, OrgNodeInterp};
use uuid::Uuid;

/// Returns something almost identical,
/// but replacing each `None`-valued ID with a random UUID.
pub fn assign_ids_recursive (
  orgnode : &OrgNodeInterp )
  -> OrgNodeInterp {

  fn recurse_in_branches (
    branches: &[OrgNodeInterp]
  ) -> Vec<OrgNodeInterp> { branches
                      . iter ()
                      . map (assign_ids_recursive)
                      . collect () }

  match orgnode {
    OrgNodeInterp::Content (content_node) => {
      let mut new_content =
        content_node.clone();
      new_content.id = Some(
        content_node . id . clone() . unwrap_or_else (
          || ID::new (
            Uuid::new_v4() . to_string() )) );
      new_content.branches =
        recurse_in_branches ( &content_node.branches );
      OrgNodeInterp::Content (new_content)
    },
    OrgNodeInterp::Aliases (alias_list) => {
      OrgNodeInterp::Aliases (alias_list.clone())
    }} }
