// cargo test --test orgnode_to_node -- --nocapture

use std::collections::HashSet;

use skg::hyperlinks::hyperlinks_from_node;
use skg::save::orgnode_to_node::orgNodeInterpretation_to_nodes;
use skg::types::{ID, OrgNodeInterp, NodeWithEphem};

#[test]
fn test_convert_orgnode_to_node() {
  let org_node: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("1")),
    title: "a title".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![],
  });
  let (file_nodes, focused_id, folded_ids) =
    orgNodeInterpretation_to_nodes(
      &org_node);

  assert_eq!(file_nodes.len(), 1,
             "Expected exactly one Node");
  let file_node = file_nodes.iter().next()
    .expect("Expected one Node in set");
  assert_eq!(file_node.title, "a title");
  assert_eq!(file_node.ids.len(), 1);
  assert_eq!(file_node.ids[0].as_str(), "1");
  assert_eq!(file_node.body, None);
  assert!(file_node.contains.is_empty());
  assert!(file_node.subscribes_to.is_empty());
  assert!(file_node.hides_from_its_subscriptions.is_empty());
  assert!(file_node.overrides_view_of.is_empty());
  assert!( hyperlinks_from_node ( &file_node )
           . is_empty() );
  assert_eq!(focused_id, None,
             "Expected no focused node");
  assert!(folded_ids.is_empty(),
          "Expected no folded nodes");
}

#[test]
fn test_convert_circular_orgnode_to_node() {
  // Build a cycle:
  //
  // 1
  // └── 2 (focused)
  //     └── 1 (nested; headline "irrelevant")
  //         └── 3 ("also irrelevant")
  let nested_3: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("3")),
    title: "also irrelevant".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![],
  });
  let nested_dup_1: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("1")),
    title: "irrelevant".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false, // not marked repeated; dedup happens in converter
    branches: vec![nested_3],
  });
  let child_2: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("2")),
    title: "2".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: true, // focused node
    repeated: false,
    branches: vec![nested_dup_1],
  });
  let root_1: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("1")),
    title: "1".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![child_2.clone()],
  });

  // Sanity checks on the constructed org tree
  let root_content = match &root_1 {
    OrgNodeInterp::Content(content) => content,
    OrgNodeInterp::Aliases(_) =>
      panic!("Expected NodeWithEphem, found Aliases"),
    OrgNodeInterp::Ignored =>
      panic!("Expected NodeWithEphem, found Ignored"),
  };
  assert_eq!(root_content.id.as_ref().map(|id| id.as_str()),
             Some("1"));
  assert_eq!(root_content.title, "1");
  assert_eq!(root_content.branches.len(), 1);

  let child_2_content = match &root_content.branches[0] {
    OrgNodeInterp::Content(content) => content,
    OrgNodeInterp::Aliases(_) =>
      panic!("Expected NodeWithEphem, found Aliases"),
    OrgNodeInterp::Ignored =>
      panic!("Expected NodeWithEphem, found Ignored"), };
  assert_eq!(child_2_content.id.as_ref().map(|id| id.as_str()),
             Some("2"));
  assert_eq!(child_2_content.branches.len(), 1);

  let nested_content = match &child_2_content.branches[0] {
    OrgNodeInterp::Content(content) => content,
    OrgNodeInterp::Aliases(_) =>
      panic!("Expected NodeWithEphem, found Aliases"),
    OrgNodeInterp::Ignored =>
      panic!("Expected NodeWithEphem, found Ignored"),
  };
  assert_eq!(nested_content.id.as_deref().map(String::as_str),
             Some("1"));

  let (file_nodes, focused_id, folded_ids) =
    orgNodeInterpretation_to_nodes(&root_1);
  assert_eq!(file_nodes.len(), 2,
             "Expected exactly two Nodes");

  let node1 = file_nodes . iter() . find (
    |n| n.ids[0].as_str() == "1")
    .expect("Node with ID '1' not found");
  let node2 = file_nodes.iter().find(
    |n| n.ids[0].as_str() == "2")
    .expect("Node with ID '2' not found");

  assert_eq!(node1.title, "1");
  assert_eq!(node1.contains.len(), 1,
             "Node 1 should contain node 2");
  assert_eq!(node1.contains[0].as_str(), "2");

  assert_eq!(node2.title, "2");
  assert_eq!(node2.contains.len(), 1,
             "Node 2 should contain node 1");
  assert_eq!(node2.contains[0].as_str(), "1");

  assert_eq!(focused_id, Some(ID::from("2")),
             "Node 2 should be focused.");
  assert!(folded_ids.is_empty(),
          "Expected no folded nodes");
}

#[test]
fn test_focused_node_extraction() {
  // root(1)
  // ├── 2
  // ├── 3 [focused]
  // └── 4
  let child2: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("2")),
    title: "child 1".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![],
  });
  let child3: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("3")),
    title: "child 2".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: true, // focused
    repeated: false,
    branches: vec![],
  });
  let child4: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("4")),
    title: "child 3".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![],
  });
  let root: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("1")),
    title: "root".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: false,
    repeated: false,
    branches: vec![child2, child3, child4],
  });

  let (file_nodes, focused_id, folded_ids) =
    orgNodeInterpretation_to_nodes(&root);

  assert_eq!(file_nodes.len(), 4,
             "Expected exactly four Nodes");

  assert_eq!( focused_id, Some(ID::from("3")),
              "Expected node 3 to be focused" );

  // Verify the focused node exists in file_nodes
  let focused_node = file_nodes .iter() .find(
    |n| n.ids[0].as_str() == "3")
    .expect("Focused node with ID '3' not found");
  assert_eq!(focused_node.title, "child 2");

  assert!(folded_ids.is_empty(),
          "Expected no folded nodes");
}

#[test]
fn test_multiple_focused_nodes_last_wins() {
  // root(1) [focused]
  // ├── 2 [focused]
  // └── 3 [folded]
  let child2: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("2")),
    title: "child 1".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: true, // later focused
    repeated: false,
    branches: vec![],
  });
  let child3: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("3")),
    title: "child 2".to_string(),
    aliases: None,
    body: None,
    folded: true, // folded
    focused: false,
    repeated: false,
    branches: vec![],
  });
  let root: OrgNodeInterp = OrgNodeInterp::Content(NodeWithEphem {
    id: Some(ID::from("1")),
    title: "root".to_string(),
    aliases: None,
    body: None,
    folded: false,
    focused: true, // earlier focused; should be overridden by child2
    repeated: false,
    branches: vec![child2, child3],
  });

  let (file_nodes, focused_id, folded_ids) =
    orgNodeInterpretation_to_nodes(&root);

  assert_eq!(file_nodes.len(), 3,
             "Expected exactly three Nodes");

  // The last focused node encountered should win (node 2, processed after node 1).
  assert_eq!(
    focused_id,
    Some(ID::from("2")),
    "Expected node 2 to be focused (last one wins)"
  );

  assert_eq!( folded_ids,
              HashSet::from([
                ID::from("3")]),
              "Node 3 (and no others) should be folded." );
}
