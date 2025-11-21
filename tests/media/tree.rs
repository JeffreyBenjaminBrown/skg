use ego_tree::Tree;
use skg::media::tree::{
  first_in_generation,
  first_in_generation_in_forest,
  next_in_generation,
  next_in_generation_in_forest,
  collect_generation_ids,
  collect_generation_ids_in_forest
};
use std::error::Error;

#[test]
fn test_first_in_generation_root() -> Result<(), Box<dyn Error>> {
  let tree: Tree<String> =
    build_binary_tree(1);
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 1)?;
  assert!(result.is_some(), "Generation 1 should exist (root)");
  assert_eq!(result.unwrap().value(), "1");
  Ok(())
}

#[test]
fn test_first_in_generation_first_child(
) -> Result<(), Box<dyn Error>> {
  let tree: Tree<String> =
    build_binary_tree(2);
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 2)?;
  assert!(result.is_some(),
          "Generation 2 should exist (first child)");
  assert_eq!(result.unwrap().value(), "11",
             "Should return first child");
  Ok(()) }

#[test]
fn test_first_in_generation_grandchild(
) -> Result<(), Box<dyn Error>> {
  let tree: Tree<String> =
    build_binary_tree(3);
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 3)?;
  assert!(result.is_some(),
          "Generation 3 should exist (grandchild)");
  assert_eq!(result.unwrap().value(), "111",
             "Should return first grandchild");
  Ok(()) }

#[test]
fn test_first_in_generation_deep_tree(
) -> Result<(), Box<dyn Error>> {
  let tree: Tree<String> =
    build_binary_tree(5);
  for (generation_num, expected_value) in [
    (1, "1"),
    (2, "11"),
    (3, "111"),
    (4, "1111"),
    (5, "11111")
  ] {
    let result: Option<ego_tree::NodeRef<String>> =
      first_in_generation(&tree, generation_num)?;
    assert!(result.is_some(),
            "Generation {} should exist", generation_num);
    assert_eq!(result.unwrap().value(), expected_value); }
  Ok(()) }

#[test]
fn test_first_in_generation_nonexistent(
) -> Result<(), Box<dyn Error>> {
  let tree: Tree<String> =
    build_binary_tree(1);
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 2)?;
  assert!(result.is_none(), "Generation 2 should not exist");
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 10)?;
  assert!(result.is_none(), "Generation 10 should not exist");
  Ok(()) }

#[test]
fn test_first_in_generation_invalid_input() {
  let tree: Tree<String> =
    build_binary_tree(1);
  let result: Result<Option<ego_tree::NodeRef<String>>, Box<dyn Error>>
    = first_in_generation(&tree, 0);
  assert!(result.is_err(), "Generation 0 should return an error");
  assert!(result.unwrap_err().to_string().contains("must be >= 1")); }

#[test]
fn test_first_in_generation_skips_childless_nodes(
) -> Result<(), Box<dyn Error>> {
  // Tree where first child has no children, but second child does
  //     1
  //    / \
  //   2   3
  //       |
  //       4
  let mut tree: Tree<i32> =
    Tree::new(1);
  tree.root_mut().append(2);
  let child2_id: ego_tree::NodeId =
    tree.root_mut().append(3).id();
  tree.get_mut(child2_id).unwrap().append(4);

  // Generation 3 exists!
  // Algorithm should skip node 2 (no children) and find 4 via node 3
  let result: Option<ego_tree::NodeRef<i32>> =
    first_in_generation(&tree, 3)?;
  assert!(result.is_some(), "Generation 3 should exist");
  assert_eq!(result.unwrap().value(), &4,
             "Should find node 4 by searching through generation 2");
  Ok(()) }

#[test]
fn test_first_in_generation_with_deleted_subtrees(
) -> Result<(), Box<dyn Error>> {
  // Build a 5-generation tree,
  // and delete children of nodes "11" and "121".
  let mut tree: Tree<String> =
    build_binary_tree(5);

  { // Find and delete all children of node "11"
    let node_11_id: ego_tree::NodeId =
      find_node_by_label(&tree, "11").unwrap().id();
    let mut node_11_mut: ego_tree::NodeMut<String> =
      tree.get_mut(node_11_id).unwrap();
    while node_11_mut.first_child().is_some() {
      node_11_mut.first_child().unwrap().detach(); }}

  { // Find and delete all children of node "121"
    let node_121_id: ego_tree::NodeId =
      find_node_by_label(&tree, "121").unwrap().id();
    let mut node_121_mut: ego_tree::NodeMut<String> =
      tree.get_mut(node_121_id).unwrap();
    while node_121_mut.first_child().is_some() {
      node_121_mut.first_child().unwrap().detach(); }}

  let gen2: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 2)?;
  assert_eq!(gen2.unwrap().value(), "11",
    "First in generation 2 should be 11");

  let gen3: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 3)?;
  assert_eq!(gen3.unwrap().value(), "121",
    "First in generation 3 should be 121 (11 has no children)");

  let gen4: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 4)?;
  assert_eq!(gen4.unwrap().value(), "1221",
    "First in generation 4 should be 1221 (121 has no children)");

  let gen5: Option<ego_tree::NodeRef<String>> =
    first_in_generation(&tree, 5)?;
  assert_eq!(gen5.unwrap().value(), "12211",
    "First in generation 5 should be 12211");

  Ok(()) }

#[test]
fn test_next_in_generation_from_root() {
  let tree: Tree<String> =
    build_binary_tree(1);
  let root: ego_tree::NodeRef<String> =
    tree.root();
  let next: Option<ego_tree::NodeRef<String>> =
    next_in_generation(root);
  assert!(next.is_none(), "Root has no next in generation"); }

#[test]
fn test_next_in_generation_second() {
  let tree: Tree<String> =
    build_binary_tree(2);

  // Generation 2: test first and last positions
  let node_11: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "11").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_11).unwrap();
  assert_eq!(next.value(), "12", "Next after 11 should be 12");

  let node_12: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "12").unwrap();
  let next: Option<ego_tree::NodeRef<String>> =
    next_in_generation(node_12);
  assert!(next.is_none(), "12 is last in its generation"); }

#[test]
fn test_next_in_generation_third() {
  let tree: Tree<String> =
    build_binary_tree(3);

  // Generation 3: test first, second, and last positions
  let node_111: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "111").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_111).unwrap();
  assert_eq!(next.value(), "112", "Next after 111 should be 112");

  let node_112: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "112").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_112).unwrap();
  assert_eq!(next.value(), "121", "Next after 112 should be 121");

  let node_122: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "122").unwrap();
  let next: Option<ego_tree::NodeRef<String>> =
    next_in_generation(node_122);
  assert!(next.is_none(), "122 is last in its generation"); }

#[test]
fn test_next_in_generation_fourth() {
  let tree: Tree<String> =
    build_binary_tree(4);

  let node_1111: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "1111").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_1111).unwrap();
  assert_eq!(next.value(), "1112", "Next after 1111 should be 1112");

  let node_1112: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "1112").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_1112).unwrap();
  assert_eq!(next.value(), "1121", "Next after 1112 should be 1121");

  let node_1222: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "1222").unwrap();
  let next: Option<ego_tree::NodeRef<String>> =
    next_in_generation(node_1222);
  assert!(next.is_none(), "1222 is last in its generation");
}

#[test]
fn test_next_in_generation_1122_to_1211() {
  let tree: Tree<String> =
    build_binary_tree(4);

  // Special test: from 1122, should climb 2 generations to reach 1211
  let node_1122: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "1122").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_1122).unwrap();
  assert_eq!(
    next.value(), "1211",
    "Next after 1122 should be 1211 (climbing 2 generations)"); }

#[test]
fn test_next_in_generation_with_deleted_node() {
  // Build a 5-generation tree and delete node "1122"
  let mut tree: Tree<String> =
    build_binary_tree(5);

  { // Find and delete node "1122" (and all its descendants)
    let node_1122_id: ego_tree::NodeId =
      find_node_by_label(&tree, "1122").unwrap().id();
    tree.get_mut(node_1122_id).unwrap().detach(); }

  // Test from 1121: next should be 1211 (skipping deleted 1122)
  let node_1121: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "1121").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_1121).unwrap();
  assert_eq!(next.value(), "1211",
    "Next after 1121 should be 1211 (1122 is deleted)");

  // from 11212: next should be 12111 (skipping deleted 1122's would-be descendants)
  let node_11212: ego_tree::NodeRef<String> =
    find_node_by_label(&tree, "11212").unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation(node_11212).unwrap();
  assert_eq!(next.value(), "12111",
    "Next after 11212 should be 12111 (1122 and its children are deleted)");
}

#[test]
fn test_next_in_generation_in_forest() {
  // Create a forest: Vec of 3 trees
  // Tree A (depth 2), Tree B (depth 3), Tree C (depth 2)
  let mut tree_a: Tree<String> =
    Tree::new("A".to_string());
  tree_a.root_mut().append("A1".to_string());
  tree_a.root_mut().append("A2".to_string());

  let mut tree_b: Tree<String> =
    Tree::new("B".to_string());
  let b1_id: ego_tree::NodeId =
    tree_b.root_mut().append("B1".to_string()).id();
  let b2_id: ego_tree::NodeId =
    tree_b.root_mut().append("B2".to_string()).id();
  tree_b.get_mut(b1_id).unwrap().append("B11".to_string());
  tree_b.get_mut(b2_id).unwrap().append("B21".to_string());

  let mut tree_c: Tree<String> =
    Tree::new("C".to_string());
  tree_c.root_mut().append("C1".to_string());
  tree_c.root_mut().append("C2".to_string());

  let forest: Vec<Tree<String>> =
    vec![tree_a, tree_b, tree_c];

  // Test: from A2 (last in Tree A's generation 2), next should be B1 (first in Tree B's generation 2)
  let node_a2: ego_tree::NodeRef<String> =
    forest[0].root().children().nth(1).unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation_in_forest(&forest, 0, node_a2).unwrap();
  assert_eq!(next.value(), "B1", "Next after A2 should be B1 (crossing to next tree)");

  // Test: from B2, next should be C1 (skipping to Tree C)
  let node_b2: ego_tree::NodeRef<String> =
    forest[1].root().children().nth(1).unwrap();
  let next: ego_tree::NodeRef<String> =
    next_in_generation_in_forest(&forest, 1, node_b2).unwrap();
  assert_eq!(next.value(), "C1", "Next after B2 should be C1");

  // Test: from C2 (last node in last tree), next should be None
  let node_c2: ego_tree::NodeRef<String> =
    forest[2].root().children().nth(1).unwrap();
  let next: Option<ego_tree::NodeRef<String>> =
    next_in_generation_in_forest(&forest, 2, node_c2);
  assert!(next.is_none(), "C2 is last in the forest's generation 2");
}

#[test]
fn test_first_in_generation_in_forest() -> Result<(), Box<dyn Error>> {
  // Create a forest with 3 trees
  // Tree A: only has depth 2 (A -> A1)
  // Tree B: has depth 4 (B -> B1 -> B11 -> B111)
  // Tree C: has depth 3 (C -> C1 -> C11)
  let mut tree_a: Tree<String> =
    Tree::new("A".to_string());
  tree_a.root_mut().append("A1".to_string());

  let mut tree_b: Tree<String> =
    Tree::new("B".to_string());
  let b1_id: ego_tree::NodeId =
    tree_b.root_mut().append("B1".to_string()).id();
  let b11_id: ego_tree::NodeId =
    tree_b.get_mut(b1_id).unwrap().append("B11".to_string()).id();
  tree_b.get_mut(b11_id).unwrap().append("B111".to_string());

  let mut tree_c: Tree<String> =
    Tree::new("C".to_string());
  let c1_id: ego_tree::NodeId =
    tree_c.root_mut().append("C1".to_string()).id();
  tree_c.get_mut(c1_id).unwrap().append("C11".to_string());

  let forest: Vec<Tree<String>> =
    vec![tree_a, tree_b, tree_c];

  // Search for generation 1 - should find Tree A's root
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation_in_forest(&forest, 1)?;
  assert_eq!(result.unwrap().value(), "A",
    "Generation 1 should be first tree's root");

  // Search for generation 2 - should find A1 in Tree A
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation_in_forest(&forest, 2)?;
  assert_eq!(result.unwrap().value(), "A1",
    "Generation 2 should be A1");

  // Search for generation 3 - Tree A doesn't have it, should find B11 in Tree B
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation_in_forest(&forest, 3)?;
  assert_eq!(result.unwrap().value(), "B11",
    "Generation 3 should be B11 from Tree B");

  // Search for generation 4 - only Tree B has it (B111)
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation_in_forest(&forest, 4)?;
  assert_eq!(result.unwrap().value(), "B111",
    "Generation 4 should be B111 from Tree B");

  // Search for generation 5 - no tree has it
  let result: Option<ego_tree::NodeRef<String>> =
    first_in_generation_in_forest(&forest, 5)?;
  assert!(result.is_none(),
    "Generation 5 should not exist in any tree");

  Ok(()) }

#[test]
fn test_collect_generation_ids_error_on_zero() {
  let tree: Tree<i32> = Tree::new(1);
  let result = collect_generation_ids(&tree, 0);
  assert!(result.is_err());
  assert_eq!(result.unwrap_err().to_string(), "Generation must be >= 1");
}

#[test]
fn test_collect_generation_ids_in_forest_error_on_zero() {
  let tree1: Tree<i32> = Tree::new(1);
  let tree2: Tree<i32> = Tree::new(2);
  let forest = vec![tree1, tree2];
  let result = collect_generation_ids_in_forest(&forest, 0);
  assert!(result.is_err());
  assert_eq!(result.unwrap_err().to_string(), "Generation must be >= 1");
}

#[test]
fn test_collect_generation_ids_success() {
  let tree: Tree<i32> = Tree::new(1);
  let result = collect_generation_ids(&tree, 1);
  assert!(result.is_ok());
  let ids = result.unwrap();
  assert_eq!(ids.len(), 1);
}

#[test]
fn test_collect_generation_ids_in_forest_success() {
  let tree1: Tree<i32> = Tree::new(1);
  let tree2: Tree<i32> = Tree::new(2);
  let forest = vec![tree1, tree2];
  let result = collect_generation_ids_in_forest(&forest, 1);
  assert!(result.is_ok());
  let grouped_ids = result.unwrap();
  // Should have 2 groups, one per tree
  assert_eq!(grouped_ids.len(), 2);
  // Each group should have 1 node (the root)
  assert_eq!(grouped_ids[0].1.len(), 1);
  assert_eq!(grouped_ids[1].1.len(), 1);
}

// Helper function to find a node by its label
fn find_node_by_label<'a>(
  tree: &'a Tree<String>, label: &str
) -> Option<ego_tree::NodeRef<'a, String>> {
  for edge in tree.root().traverse() {
    if let ego_tree::iter::Edge::Open(node) = edge {
      if node.value() == label {
        return Some(node); }} }
  None }

/// Helper function to build a binary tree with string labels.
/// Root is "1", children are "11" and "12", grandchildren are "111", "112", "121", "122", etc.
/// If the string associated with node N is a prefix
/// of that associated with node M, then N is M's ancestor.
fn build_binary_tree(
  generations: usize
) -> Tree<String> {
  fn build_subtree(
    tree: &mut Tree<String>,
    parent_id: ego_tree::NodeId,
    label: String,
    current_generation: usize,
    max_generations: usize
  ) {
    if current_generation >= max_generations {
      return; }
    let left_label: String =
      format!("{}1", label);
    let right_label: String =
      format!("{}2", label);
    let left_id: ego_tree::NodeId =
      tree . get_mut (parent_id) . unwrap()
      . append (left_label.clone()) . id();
    let right_id: ego_tree::NodeId =
      tree.get_mut (parent_id) . unwrap()
      . append (right_label.clone()) . id();
    build_subtree ( tree, left_id, left_label,
                    current_generation + 1, max_generations);
    build_subtree ( tree, right_id, right_label,
                    current_generation + 1, max_generations); }

  let mut tree: Tree<String> =
    Tree::new("1".to_string());
  if generations > 1 {
    let root_id: ego_tree::NodeId =
      tree.root().id();
    build_subtree ( &mut tree, root_id, "1".to_string(),
                     1, generations); }
  tree }
