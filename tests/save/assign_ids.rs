// cargo test assign_ids

use skg::save::assign_ids::assign_ids_recursive;
use skg::types::{ID, NodeWithEphem, OrgNodeInterp};

#[test]
fn test_assign_ids_recursive() {
  // Build from the leaves upward, wrapping each NodeWithEphem as OrgNodeInterp::Content
  let c_content = NodeWithEphem {
    id: None,
    title: "c".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![], // leaf
  };
  let c = OrgNodeInterp::Content(c_content);

  let b_content = NodeWithEphem {
    id: Some(ID::from("b")),
    title: "b".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![c], // Vec<OrgNodeInterp>
  };
  let b = OrgNodeInterp::Content(b_content);

  let a = OrgNodeInterp::Content(NodeWithEphem {
    id: None,
    title: "a".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![b], // Vec<OrgNodeInterp>
  });

  let result = assign_ids_recursive(&a);

  // Unpack OrgNodeInterp -> NodeWithEphem to assert on fields
  let result = match result {
    OrgNodeInterp::Content(cn) => cn,
    _ => panic!("expected Content node at root"),
  };
  assert!(result.id.is_some(),
          "Node A should have an ID after processing");
  assert_eq!(result.branches.len(), 1,
             "Node A should have one child");

  let result_b = match &result.branches[0] {
    OrgNodeInterp::Content(cn) => cn,
    _ => panic!("expected Content node at B"),
  };
  assert_eq!(result_b.id, Some(ID::from("b")),
             "Node B should keep its original ID 'b'");
  assert_eq!(result_b.branches.len(), 1,
             "Node B should have one child");

  let result_c = match &result_b.branches[0] {
    OrgNodeInterp::Content(cn) => cn,
    _ => panic!("expected Content node at C"),
  };
  assert!(result_c.id.is_some(),
          "Node C should have an ID after processing");

  assert_eq!(result.title, "a");
  assert_eq!(result_b.title, "b");
  assert_eq!(result_c.title, "c");
}
