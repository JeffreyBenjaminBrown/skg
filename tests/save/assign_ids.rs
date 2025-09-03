use skg::save::assign_ids::assign_ids_recursive;
use skg::types::{ID, ContentNode, OrgNode};

#[test]
fn test_assign_ids_recursive() {
  // Build from the leaves upward, wrapping each ContentNode as OrgNode::Content
  let c_content = ContentNode {
    id: None,
    headline: "c".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![], // leaf
  };
  let c = OrgNode::content(c_content);

  let b_content = ContentNode {
    id: Some(ID::from("b")),
    headline: "b".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![c], // Vec<OrgNode>
  };
  let b = OrgNode::content(b_content);

  let a = OrgNode::content(ContentNode {
    id: None,
    headline: "a".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![b], // Vec<OrgNode>
  });

  let result = assign_ids_recursive(&a);

  // Unpack OrgNode -> ContentNode to assert on fields
  let result = match result {
    OrgNode::Content(cn) => cn,
    _ => panic!("expected Content node at root"),
  };
  assert!(result.id.is_some(),
          "Node A should have an ID after processing");
  assert_eq!(result.branches.len(), 1,
             "Node A should have one child");

  let result_b = match &result.branches[0] {
    OrgNode::Content(cn) => cn,
    _ => panic!("expected Content node at B"),
  };
  assert_eq!(result_b.id, Some(ID::from("b")),
             "Node B should keep its original ID 'b'");
  assert_eq!(result_b.branches.len(), 1,
             "Node B should have one child");

  let result_c = match &result_b.branches[0] {
    OrgNode::Content(cn) => cn,
    _ => panic!("expected Content node at C"),
  };
  assert!(result_c.id.is_some(),
          "Node C should have an ID after processing");

  assert_eq!(result.headline, "a");
  assert_eq!(result_b.headline, "b");
  assert_eq!(result_c.headline, "c");
}
