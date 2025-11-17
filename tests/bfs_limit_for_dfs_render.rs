use std::error::Error;
use skg::bfs_limit_for_dfs_render::{parse_sexp_to_tree, NodeType};

/// Helper function to count nodes in a tree
fn count_nodes(tree: &ego_tree::Tree<NodeType>) -> usize {
  tree.root().traverse().filter(|edge| matches!(edge, ego_tree::iter::Edge::Open(_))).count()
}

/// Helper function to count direct children of root
fn count_root_children(tree: &ego_tree::Tree<NodeType>) -> usize {
  tree.root().children().count()
}

#[test]
fn test_single_content_node() -> Result<(), Box<dyn Error>> {
  let tree = parse_sexp_to_tree("(Content)")?;

  assert_eq!(tree.root().value(), &NodeType::Content);
  assert_eq!(count_nodes(&tree), 1, "Should have exactly 1 node");
  assert_eq!(count_root_children(&tree), 0, "Root should have no children");

  Ok(())
}

#[test]
fn test_single_leaf_node() -> Result<(), Box<dyn Error>> {
  let tree = parse_sexp_to_tree("(Leaf)")?;

  assert_eq!(tree.root().value(), &NodeType::Leaf);
  assert_eq!(count_nodes(&tree), 1, "Should have exactly 1 node");
  assert_eq!(count_root_children(&tree), 0, "Root should have no children");

  Ok(())
}

#[test]
fn test_content_with_two_leaf_children() -> Result<(), Box<dyn Error>> {
  let tree = parse_sexp_to_tree("(Content Leaf Leaf)")?;

  assert_eq!(tree.root().value(), &NodeType::Content);
  assert_eq!(count_nodes(&tree), 3, "Should have 3 nodes total");
  assert_eq!(count_root_children(&tree), 2, "Root should have 2 children");

  let children: Vec<&NodeType> = tree.root().children().map(|n| n.value()).collect();
  assert_eq!(children.len(), 2);
  assert_eq!(children[0], &NodeType::Leaf);
  assert_eq!(children[1], &NodeType::Leaf);

  Ok(())
}

#[test]
fn test_content_with_two_content_children() -> Result<(), Box<dyn Error>> {
  let tree = parse_sexp_to_tree("(Content Content Content)")?;

  assert_eq!(tree.root().value(), &NodeType::Content);
  assert_eq!(count_nodes(&tree), 3, "Should have 3 nodes total");
  assert_eq!(count_root_children(&tree), 2, "Root should have 2 children");

  let children: Vec<&NodeType> = tree.root().children().map(|n| n.value()).collect();
  assert_eq!(children.len(), 2);
  assert_eq!(children[0], &NodeType::Content);
  assert_eq!(children[1], &NodeType::Content);

  Ok(())
}

#[test]
fn test_content_with_leaf_and_content_children_wrapped() -> Result<(), Box<dyn Error>> {
  let tree = parse_sexp_to_tree("(Content (Leaf) (Content))")?;

  assert_eq!(tree.root().value(), &NodeType::Content);
  assert_eq!(count_nodes(&tree), 3, "Should have 3 nodes total");
  assert_eq!(count_root_children(&tree), 2, "Root should have 2 children");

  let children: Vec<&NodeType> = tree.root().children().map(|n| n.value()).collect();
  assert_eq!(children.len(), 2);
  assert_eq!(children[0], &NodeType::Leaf);
  assert_eq!(children[1], &NodeType::Content);

  Ok(())
}

#[test]
fn test_complex_tree() -> Result<(), Box<dyn Error>> {
  // (Content Leaf (Content Leaf Leaf Content))
  // Root is Content with 2 children:
  //   - First child: Leaf (no children)
  //   - Second child: Content with 3 children (Leaf, Leaf, Content)
  let tree = parse_sexp_to_tree("(Content Leaf (Content Leaf Leaf Content))")?;

  assert_eq!(tree.root().value(), &NodeType::Content);
  assert_eq!(count_root_children(&tree), 2, "Root should have 2 children");

  let root_children: Vec<_> = tree.root().children().collect();

  // First child is a Leaf
  assert_eq!(root_children[0].value(), &NodeType::Leaf);
  assert_eq!(root_children[0].children().count(), 0, "First child should have no children");

  // Second child is a Content with 3 children
  assert_eq!(root_children[1].value(), &NodeType::Content);
  assert_eq!(root_children[1].children().count(), 3, "Second child should have 3 children");

  let grandchildren: Vec<&NodeType> = root_children[1].children().map(|n| n.value()).collect();
  assert_eq!(grandchildren[0], &NodeType::Leaf);
  assert_eq!(grandchildren[1], &NodeType::Leaf);
  assert_eq!(grandchildren[2], &NodeType::Content);

  Ok(())
}

#[test]
fn test_leaf_cannot_have_children() {
  let result = parse_sexp_to_tree("(Leaf Content)");
  assert!(result.is_err(), "Leaf nodes should not be allowed to have children");

  let error_msg = result.unwrap_err().to_string();
  assert!(error_msg.contains("Leaf nodes cannot have children"));
}

#[test]
fn test_invalid_node_type() {
  let result = parse_sexp_to_tree("(Invalid)");
  assert!(result.is_err(), "Invalid node types should be rejected");

  let error_msg = result.unwrap_err().to_string();
  assert!(error_msg.contains("Unknown node type"));
}

#[test]
fn test_empty_list_error() {
  let result = parse_sexp_to_tree("()");
  assert!(result.is_err(), "Empty lists should be rejected");
}
