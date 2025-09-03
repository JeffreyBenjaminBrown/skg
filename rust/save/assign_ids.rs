use crate::types::{ID, OrgNodeInterpretation};
use uuid::Uuid;

/// Returns something almost identical,
/// but replacing each `None`-valued ID with a random UUID.
pub fn assign_ids_recursive (
  orgnode : &OrgNodeInterpretation )
  -> OrgNodeInterpretation {

  fn recurse_in_branches (
    branches: &[OrgNodeInterpretation]
  ) -> Vec<OrgNodeInterpretation> { branches
                      . iter ()
                      . map (assign_ids_recursive)
                      . collect () }

  match orgnode {
    OrgNodeInterpretation::Content (content_node) => {
      let mut new_content =
        content_node.clone();
      new_content.id = Some(
        content_node . id . clone() . unwrap_or_else (
          || ID::new (
            Uuid::new_v4() . to_string() )) );
      new_content.branches =
        recurse_in_branches ( &content_node.branches );
      OrgNodeInterpretation::Content (new_content)
    },
    OrgNodeInterpretation::Aliases (alias_list) => {
      OrgNodeInterpretation::Aliases (alias_list.clone())
    }} }
