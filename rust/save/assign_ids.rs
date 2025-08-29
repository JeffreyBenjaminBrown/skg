use crate::types::{ID, OrgNode};

use uuid::Uuid;

pub fn assign_ids_recursive (
  orgnode : &OrgNode )
  -> OrgNode {
  // Returns something almost identical,
  // but replacing each `None`-valued ID with a random UUID.

  OrgNode {
    id : Some (
      orgnode . id . clone () . unwrap_or_else (
        || ID::new ( Uuid::new_v4 ()
                     . to_string () )) ),
    heading  : orgnode.heading.clone(),
    body     : orgnode.body.clone(),
    folded   : orgnode.folded,
    focused  : orgnode.focused,
    repeated : orgnode.repeated,
    branches : orgnode.branches
      . iter ()
      . map ( assign_ids_recursive )
      . collect ()
  } }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::{ID, OrgNode};

  #[test]
  fn test_assign_ids_recursive() {
    let node_c = OrgNode {
      // Innermost node, with no ID
      id: None,
      heading: "c".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![],
    };
    let node_b = OrgNode {
      // Intermediate node, with ID
      id: Some(ID::from("b")),
      heading: "b".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![node_c],
    };
    let node_a = OrgNode {
      // Outermost node, with no ID
      id: None,
      heading: "a".to_string(),
      body: None,
      folded: false,
      focused: false,
      repeated: false,
      branches: vec![node_b],
    };
    let result =
      assign_ids_recursive(
        &node_a);
    assert!(result.id.is_some(),
            "Node A should have an ID after processing");
    assert_eq!(result.branches.len(), 1,
               "Node A should have one child");
    let result_b = &result.branches[0];
    assert_eq!(result_b.id, Some(ID::from("b")),
               "Node B should keep its original ID 'b'");
    assert_eq!(result_b.branches.len(), 1,
               "Node B should have one child");
    let result_c = &result_b.branches[0];
    assert!(result_c.id.is_some(),
            "Node C should have an ID after processing");
    assert_eq!(result.heading, "a");
    assert_eq!(result_b.heading, "b");
    assert_eq!(result_c.heading, "c");
  } }
