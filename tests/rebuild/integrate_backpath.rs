// cargo test --test rebuild -- integrate_backpath

use indoc::indoc;
use skg::to_org::integrate_path_that_might_fork_or_cycle;
use skg::from_text::org_to_uninterpreted_nodes;
use skg::test_utils::{compare_orgnode_forests, run_with_test_db, orgnode_forest_to_paired};
use skg::types::{ID, OrgNode, SkgConfig};
use skg::types::trees::PairTree;
use skg::media::tree::map_snd_over_forest;

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
    |config, driver| Box::pin(async move {
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

  let orgnode_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.len(), 1, "Should have exactly 1 tree");

  let mut trees: Vec<PairTree> =
    orgnode_forest_to_paired(orgnode_trees);
  let tree = &mut trees[0];
  let root_id = tree.root().id();

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
    tree, root_id, path, branches,
    cycle_node, &config, driver, ).await?;

  let expected: &str = indoc! {"
    * (skg (id 1)) 1
    ** (skg (id 2) (view folded)) 2
    *** (skg (id 3) (source main) (code indefinitive (relToParent parentIgnores))) 3
    **** (skg (id 4) (source main) (code indefinitive (relToParent parentIgnores))) 4
    ***** (skg (id 1) (source main) (code indefinitive (relToParent parentIgnores))) 1
    *** (skg (id off-path)) off-path
  "};

  let expected_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(expected)?;

  // Convert paired trees back to OrgNode trees for comparison
  let result_trees: Vec<Tree<OrgNode>> =
    map_snd_over_forest(trees);

  assert!(
    compare_orgnode_forests(&result_trees, &expected_trees),
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
    |config, driver| Box::pin(async move {
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

  let orgnode_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.len(), 1, "Should have exactly 1 tree");

  let mut trees: Vec<PairTree> =
    orgnode_forest_to_paired(orgnode_trees);
  let tree = &mut trees[0];

  // Find node with id "1" (second node in the tree)
  let mut node_1_id = None;
  for edge in tree.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let Some(ref id) = node_ref.value().1.metadata.id {
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
    tree, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id 3) (source main) (code indefinitive (relToParent parentIgnores))) 3
    ***** (skg (id 1) (source main) (code indefinitive (relToParent parentIgnores))) 1
    ***** (skg (id 2) (source main) (code indefinitive (relToParent parentIgnores))) 2
    ***** (skg (id 3) (source main) (code indefinitive (relToParent parentIgnores))) 3
    **** (skg (id off-path)) off-path
  "};

  let expected_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(expected)?;

  // Convert paired trees back to OrgNode trees for comparison
  let result_trees: Vec<Tree<OrgNode>> =
    map_snd_over_forest(trees);

  assert!(
    compare_orgnode_forests(&result_trees, &expected_trees),
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
    |config, driver| Box::pin(async move {
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

  let orgnode_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input)?;
  assert_eq!(orgnode_trees.len(), 1, "Should have exactly 1 tree");

  let mut trees: Vec<PairTree> =
    orgnode_forest_to_paired(orgnode_trees);
  let tree = &mut trees[0];

  // Find node with id "1" (second node in the tree)
  let mut node_1_id = None;
  for edge in tree.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      if let Some(ref id) = node_ref.value().1.metadata.id {
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
    tree, node_1_id, path, branches,
    cycle_node, &config, driver ).await?;

  let expected: &str = indoc! {"
    * (skg (id 0)) 0
    ** (skg (id 1)) 1
    *** (skg (id 2) (view folded)) 2
    **** (skg (id 3) (source main) (code indefinitive (relToParent parentIgnores))) 3
    ***** (skg (id 1) (source main) (code indefinitive (relToParent parentIgnores))) 1
    ***** (skg (id 2) (source main) (code indefinitive (relToParent parentIgnores))) 2
    ***** (skg (id 3) (source main) (code indefinitive (relToParent parentIgnores))) 3
    **** (skg (id off-path)) off-path
  "};

  let expected_trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(expected)?;

  // Convert paired trees back to OrgNode trees for comparison
  let result_trees: Vec<Tree<OrgNode>> =
    map_snd_over_forest(trees);

  assert!(
    compare_orgnode_forests(&result_trees, &expected_trees),
    "Tree structure after integrating path with branches (with cycle) should match expected"
  );

  Ok(())
}
