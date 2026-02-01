// cargo test --test rebuild -- integrate_backpath

use indoc::indoc;
use skg::to_org::expand::backpath::integrate_path_that_might_fork_or_cycle;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::unchecked_orgnode::unchecked_to_checked_tree;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig};
use skg::types::orgnode::{OrgNode, OrgNodeKind};
use skg::types::skgnodemap::SkgNodeMap;
use skg::org_to_text::orgnode_forest_to_string;

use ego_tree::{NodeId,Tree};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn test_path_with_cycle() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-integrate-backpath-cycle",
    "tests/rebuild/fixtures",
    "/tmp/tantivy-test-integrate-backpath-cycle",
    |config, driver, _tantivy| Box::pin(async move {
      test_path_with_cycle_impl(config, driver).await
    })
  )
}

async fn test_path_with_cycle_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Create the initial tree
  let input: &str = indoc! {"
    * (skg (node (id 1) (source main))) 1
    ** (skg folded (node (id 2) (source main))) 2
    *** (skg (node (id off-path) (source main))) off-path
  "};

  let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
  let mut forest: Tree<OrgNode> = unchecked_to_checked_tree(unchecked_forest)?;
  assert_eq!(forest.root().children().count(), 1,
             "Should have exactly 1 tree");

  let tree_root_id =
    forest . root () . first_child () . unwrap () . id ();
  let root_id = tree_root_id;

  // Setup backpath data
  let path = vec![
    ID::from("1"),
    ID::from("2"),
    ID::from("3"),
    ID::from("4"),
  ];
  let branches = HashSet::new();
  let cycle_node = Some(ID::from("1"));

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, root_id, path, branches,
    cycle_node, &config, driver, ).await?;

  let expected: &str = indoc! {"
    * (skg (node (id 1) (source main))) 1
    ** (skg folded (node (id 2) (source main))) 2
    *** (skg (node (id 3) (source main) parentIgnores indefinitive)) 3
    **** (skg (node (id 4) (source main) parentIgnores indefinitive)) 4
    ***** (skg (node (id 1) (source main) parentIgnores indefinitive)) 1
    *** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes(expected)?.0;
  let expected_trees: Tree<OrgNode> = unchecked_to_checked_tree(expected_unchecked)?;

  let actual_str = orgnode_forest_to_string(&forest)?;
  let expected_str = orgnode_forest_to_string(&expected_trees)?;
  assert_eq!(
    actual_str, expected_str,
    "Tree structure after integrating path with cycle should match expected"
  );

  Ok(())
}

#[test]
fn test_path_with_branches_no_cycle(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-integrate-backpath-branches-no-cycle",
    "tests/rebuild/fixtures",
    "/tmp/tantivy-test-integrate-backpath-branches-no-cycle",
    |config, driver, _tantivy| Box::pin(async move {
      test_path_with_branches_no_cycle_impl(config, driver).await
    } )) }

async fn test_path_with_branches_no_cycle_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Create the initial tree
  let input: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
  let mut forest: Tree<OrgNode> = unchecked_to_checked_tree(unchecked_forest)?;
  assert_eq!(forest.root().children().count(), 1,
             "Should have exactly 1 tree");

  // Find node with id "1" (second node in the tree)
  let mut node_1_id : Option<NodeId> = None;
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let OrgNodeKind::True(t) = &node_ref.value().kind {
        if t.id.0 == "1" {
          node_1_id = Some(node_ref.id());
          break; }}}}
  let node_1_id : NodeId =
    node_1_id.expect("Should find node with id 1");

  // Setup backpath data
  let path = vec![
    ID::from("1"),
    ID::from("2"),
    ID::from("3"),
  ];
  let mut branches = HashSet::new();
  branches.insert(ID::from("1"));
  branches.insert(ID::from("2"));
  branches.insert(ID::from("3"));
  let cycle_node = None;

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id 3) (source main) parentIgnores indefinitive)) 3
    ***** (skg (node (id 1) (source main) parentIgnores indefinitive)) 1
    ***** (skg (node (id 2) (source main) parentIgnores indefinitive)) 2
    ***** (skg (node (id 3) (source main) parentIgnores indefinitive)) 3
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes(expected)?.0;
  let expected_trees: Tree<OrgNode> = unchecked_to_checked_tree(expected_unchecked)?;

  let actual_str = orgnode_forest_to_string(&forest)?;
  let expected_str = orgnode_forest_to_string(&expected_trees)?;
  assert_eq!(
    actual_str, expected_str,
    "Tree structure after integrating path with branches (no cycle) should match expected"
  );

  Ok(())
}

#[test]
fn test_path_with_branches_with_cycle(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-integrate-backpath-branches-with-cycle",
    "tests/rebuild/fixtures",
    "/tmp/tantivy-test-integrate-backpath-branches-with-cycle",
    |config, driver, _tantivy| Box::pin(async move {
      test_path_with_branches_with_cycle_impl(config, driver).await
    } )) }

async fn test_path_with_branches_with_cycle_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Create the initial tree
  let input: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
  let mut forest: Tree<OrgNode> = unchecked_to_checked_tree(unchecked_forest)?;
  assert_eq!(forest.root().children().count(), 1,
             "Should have exactly 1 tree");

  // Find node with id "1" (second node in the tree)
  let mut node_1_id : Option<NodeId> =
    None;
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let OrgNodeKind::True(t) = &node_ref.value().kind {
        if t.id.0 == "1" {
          node_1_id = Some(node_ref.id());
          break; }}}}
  let node_1_id : NodeId =
    node_1_id.expect("Should find node with id 1");

  // Setup backpath data
  let path = vec![
    ID::from("1"),
    ID::from("2"),
    ID::from("3"),
  ];
  let mut branches = HashSet::new();
  branches.insert(ID::from("1"));
  branches.insert(ID::from("2"));
  branches.insert(ID::from("3"));
  let cycle_node = Some(ID::from("1"));

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id 3) (source main) parentIgnores indefinitive)) 3
    ***** (skg (node (id 1) (source main) parentIgnores indefinitive)) 1
    ***** (skg (node (id 2) (source main) parentIgnores indefinitive)) 2
    ***** (skg (node (id 3) (source main) parentIgnores indefinitive)) 3
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes(expected)?.0;
  let expected_trees: Tree<OrgNode> = unchecked_to_checked_tree(expected_unchecked)?;

  let actual_str = orgnode_forest_to_string(&forest)?;
  let expected_str = orgnode_forest_to_string(&expected_trees)?;
  assert_eq!(
    actual_str, expected_str,
    "Tree structure after integrating path with branches (with cycle) should match expected"
  );

  Ok(())
}
