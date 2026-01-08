// cargo test --test rebuild -- integrate_backpath

use indoc::indoc;
use skg::to_org::expand::backpath::integrate_path_that_might_fork_or_cycle;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::test_utils::{compare_orgnode_portions_of_pairforest_and_orgnodeforest, run_with_test_db, orgnode_forest_to_paired};
use skg::types::misc::{ID, SkgConfig};
use skg::types::orgnode_new::OrgNode;
use skg::types::tree::PairTree;

use ego_tree::Tree;
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
    * (skg (id 1)) 1
    ** (skg (id 2) (view folded)) 2
    *** (skg (id off-path)) off-path
  "};

  let orgnode_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.root().children().count(), 1,
             "Should have exactly 1 tree");

  let mut forest: PairTree =
    orgnode_forest_to_paired(orgnode_trees);
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
  integrate_path_that_might_fork_or_cycle(
    &mut forest, root_id, path, branches,
    cycle_node, &config, driver, ).await?;

  let expected: &str = indoc! {"
    * (skg (id 1)) 1
    ** (skg (id 2) (view folded)) 2
    *** (skg (id 3) (source main) (code indefinitive (interp parentIgnores))) 3
    **** (skg (id 4) (source main) (code indefinitive (interp parentIgnores))) 4
    ***** (skg (id 1) (source main) (code indefinitive (interp parentIgnores))) 1
    *** (skg (id off-path)) off-path
  "};

  let expected_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(expected)?;

  assert!(
    compare_orgnode_portions_of_pairforest_and_orgnodeforest(
      &forest, &expected_trees),
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
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id off-path)) off-path
  "};

  let orgnode_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.root().children().count(), 1,
             "Should have exactly 1 tree");

  let mut forest: PairTree =
    orgnode_forest_to_paired(orgnode_trees);

  // Find node with id "1" (second node in the tree)
  let mut node_1_id = None;
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let Some(id) = node_ref.value().orgnode_new().id() {
        if id.0 == "1" {
          node_1_id = Some(node_ref.id());
          break;
        }
      }
    }
  }
  let node_1_id = node_1_id.expect("Should find node with id 1");

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
  integrate_path_that_might_fork_or_cycle(
    &mut forest, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id 3) (source main) (code indefinitive (interp parentIgnores))) 3
    ***** (skg (id 1) (source main) (code indefinitive (interp parentIgnores))) 1
    ***** (skg (id 2) (source main) (code indefinitive (interp parentIgnores))) 2
    ***** (skg (id 3) (source main) (code indefinitive (interp parentIgnores))) 3
    **** (skg (id off-path)) off-path
  "};

  let expected_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(expected)?;

  assert!(
    compare_orgnode_portions_of_pairforest_and_orgnodeforest(
      &forest, &expected_trees),
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
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id off-path)) off-path
  "};

  let orgnode_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.root().children().count(), 1,
             "Should have exactly 1 tree");

  let mut forest: PairTree =
    orgnode_forest_to_paired(orgnode_trees);

  // Find node with id "1" (second node in the tree)
  let mut node_1_id = None;
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let Some(id) = node_ref.value().orgnode_new().id() {
        if id.0 == "1" {
          node_1_id = Some(node_ref.id());
          break;
        }
      }
    }
  }
  let node_1_id = node_1_id.expect("Should find node with id 1");

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
  integrate_path_that_might_fork_or_cycle(
    &mut forest, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id 3) (source main) (code indefinitive (interp parentIgnores))) 3
    ***** (skg (id 1) (source main) (code indefinitive (interp parentIgnores))) 1
    ***** (skg (id 2) (source main) (code indefinitive (interp parentIgnores))) 2
    ***** (skg (id 3) (source main) (code indefinitive (interp parentIgnores))) 3
    **** (skg (id off-path)) off-path
  "};

  let expected_trees: Tree<OrgNode> =
    org_to_uninterpreted_nodes(expected)?;

  assert!(
    compare_orgnode_portions_of_pairforest_and_orgnodeforest(
      &forest, &expected_trees),
    "Tree structure after integrating path with branches (with cycle) should match expected"
  );

  Ok(())
}
