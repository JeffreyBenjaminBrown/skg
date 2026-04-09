// cargo test --test rebuild -- integrate_backpath

use indoc::indoc;
use skg::to_org::expand::backpath::{
  integrate_path_that_might_fork_or_cycle,
  build_and_integrate_containerward_path};
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::unchecked_viewnode::unchecked_to_checked_tree;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig};
use skg::types::viewnode::{ViewNode, ViewNodeKind, Birth};
use skg::types::memory::SkgNodeMap;
use skg::org_to_text::viewnode_forest_to_string;

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
      test_path_with_cycle_impl(config, driver) . await
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

  let unchecked_forest = org_to_uninterpreted_nodes (input)?. 0;
  let mut forest: Tree<ViewNode> = unchecked_to_checked_tree (unchecked_forest)?;
  assert_eq!(forest . root() . children() . count(), 1,
             "Should have exactly 1 tree");

  let tree_root_id =
    forest . root () . first_child () . unwrap () . id ();
  let root_id = tree_root_id;

  // Setup backpath data (origin-free: origin "1" is stripped)
  let path = vec![
    ID::from ("2"),
    ID::from ("3"),
    ID::from ("4"),
  ];
  let branches = HashSet::new();
  let cycle_nodes = HashSet::from([ID::from ("1")]);

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, root_id, path, branches,
    cycle_nodes, &config, driver, Birth::ContainerOf
  ). await?;

  let expected: &str = indoc! {"
    * (skg (node (id 1) (source main))) 1
    ** (skg folded (node (id 2) (source main))) 2
    *** (skg (node (id 3) (source main) (birth containerOf) indefinitive)) 3
    **** (skg (node (id 4) (source main) (birth containerOf) indefinitive)) 4
    ***** (skg (node (id 1) (source main) (birth containerOf) indefinitive)) 1
    *** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> = unchecked_to_checked_tree (expected_unchecked)?;

  let actual_str = viewnode_forest_to_string (&forest, config)?;
  let expected_str = viewnode_forest_to_string (&expected_trees, config)?;
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
      test_path_with_branches_no_cycle_impl(config, driver) . await
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

  let unchecked_forest = org_to_uninterpreted_nodes (input)?. 0;
  let mut forest: Tree<ViewNode> = unchecked_to_checked_tree (unchecked_forest)?;
  assert_eq!(forest . root() . children() . count(), 1,
             "Should have exactly 1 tree");

  // Find node with id "1" (second node in the tree)
  let mut node_1_id : Option<NodeId> = None;
  for edge in forest . root() . traverse() {
    if let ego_tree::iter::Edge::Open (node_ref) = edge {
      if let ViewNodeKind::True (t) = &node_ref . value() . kind {
        if t . id . 0 == "1" {
          node_1_id = Some(node_ref . id());
          break; }}}}
  let node_1_id : NodeId =
    node_1_id . expect ("Should find node with id 1");

  // Setup backpath data (origin-free: origin "1" is stripped)
  let path = vec![
    ID::from ("2"),
    ID::from ("3"),
  ];
  let mut branches = HashSet::new();
  branches . insert(ID::from ("1"));
  branches . insert(ID::from ("2"));
  branches . insert(ID::from ("3"));
  let cycle_nodes = HashSet::new();

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, node_1_id, path, branches,
    cycle_nodes, &config, driver, Birth::ContainerOf
  ). await?;

  let expected: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id 3) (source main) (birth containerOf) indefinitive)) 3
    ***** (skg (node (id 3) (source main) (birth containerOf) indefinitive)) 3
    ***** (skg (node (id 2) (source main) (birth containerOf) indefinitive)) 2
    ***** (skg (node (id 1) (source main) (birth containerOf) indefinitive)) 1
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> = unchecked_to_checked_tree (expected_unchecked)?;

  let actual_str = viewnode_forest_to_string (&forest, config)?;
  let expected_str = viewnode_forest_to_string (&expected_trees, config)?;
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
      test_path_with_branches_with_cycle_impl(config, driver) . await
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

  let unchecked_forest = org_to_uninterpreted_nodes (input)?. 0;
  let mut forest: Tree<ViewNode> = unchecked_to_checked_tree (unchecked_forest)?;
  assert_eq!(forest . root() . children() . count(), 1,
             "Should have exactly 1 tree");

  // Find node with id "1" (second node in the tree)
  let mut node_1_id : Option<NodeId> =
    None;
  for edge in forest . root() . traverse() {
    if let ego_tree::iter::Edge::Open (node_ref) = edge {
      if let ViewNodeKind::True (t) = &node_ref . value() . kind {
        if t . id . 0 == "1" {
          node_1_id = Some(node_ref . id());
          break; }}}}
  let node_1_id : NodeId =
    node_1_id . expect ("Should find node with id 1");

  // Setup backpath data (origin-free: origin "1" is stripped)
  let path = vec![
    ID::from ("2"),
    ID::from ("3"),
  ];
  let mut branches = HashSet::new();
  branches . insert(ID::from ("1"));
  branches . insert(ID::from ("2"));
  branches . insert(ID::from ("3"));
  let cycle_nodes = HashSet::from([ID::from ("1")]);

  // Integrate the path
  let mut map : SkgNodeMap = SkgNodeMap::new();
  integrate_path_that_might_fork_or_cycle(
    &mut forest, &mut map, node_1_id, path, branches,
    cycle_nodes, &config, driver, Birth::ContainerOf
  ). await?;

  let expected: &str = indoc! {"
    * (skg (node (id 0) (source main))) 0
    ** (skg (node (id 1) (source main))) 1
    *** (skg folded (node (id 2) (source main))) 2
    **** (skg (node (id 3) (source main) (birth containerOf) indefinitive)) 3
    ***** (skg (node (id 3) (source main) (birth containerOf) indefinitive)) 3
    ***** (skg (node (id 2) (source main) (birth containerOf) indefinitive)) 2
    ***** (skg (node (id 1) (source main) (birth containerOf) indefinitive)) 1
    **** (skg (node (id off-path) (source main))) off-path
  "};

  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> = unchecked_to_checked_tree (expected_unchecked)?;

  let actual_str = viewnode_forest_to_string (&forest, config)?;
  let expected_str = viewnode_forest_to_string (&expected_trees, config)?;
  assert_eq!(
    actual_str, expected_str,
    "Tree structure after integrating path with branches (with cycle) should match expected"
  );

  Ok(())
}

#[test]
fn test_fork_expansion_at_origin(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-integrate-backpath-fork-expansion",
    "tests/rebuild/fixtures-fork-expansion",
    "/tmp/tantivy-test-integrate-backpath-fork-expansion",
    |config, driver, _tantivy| Box::pin(async move {
      test_fork_expansion_at_origin_impl(config, driver) . await
    } )) }

async fn test_fork_expansion_at_origin_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // a11 has containers {a1, a2} — immediate fork.
  // a1 has containers {a, a1} — fork+cycle.
  // a2 has containers {a, b} — fork.
  let input: &str = indoc! {"
    * (skg (node (id a11) (source main))) a11
  "};
  let unchecked_forest = org_to_uninterpreted_nodes (input)?. 0;
  let mut forest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_forest)?;
  let node_a11_id : NodeId =
    forest . root () . first_child () . unwrap () . id ();
  let mut map : SkgNodeMap = SkgNodeMap::new();
  build_and_integrate_containerward_path (
    &mut forest, &mut map, node_a11_id, &config, driver
  ) . await ?;
  // Expected: a11 gets two children (a1 and a2).
  // Under a1: branches {a, a1} (sorted: a first, then a1).
  // Under a2: branches {a, b} (sorted: a first, then b).
  // Prepending reverses sorted order:
  // top-level branches [a1,a2] prepend as a2 then a1;
  // sub-branches similarly reversed.
  let expected: &str = indoc! {"
    * (skg (node (id a11) (source main))) a11
    ** (skg (node (id a2) (source main) (birth containerOf) indefinitive)) a2
    *** (skg (node (id b) (source main) (birth containerOf) indefinitive)) b
    *** (skg (node (id a) (source main) (birth containerOf) indefinitive)) a
    ** (skg (node (id a1) (source main) (birth containerOf) indefinitive)) a1
    *** (skg (node (id a1) (source main) (birth containerOf) indefinitive)) a1
    *** (skg (node (id a) (source main) (birth containerOf) indefinitive)) a
  "};
  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> =
    unchecked_to_checked_tree (expected_unchecked)?;
  let actual_str : String =
    viewnode_forest_to_string (&forest, config)?;
  let expected_str : String =
    viewnode_forest_to_string (&expected_trees, config)?;
  assert_eq!(
    actual_str, expected_str,
    "Fork expansion at origin should expand immediate branches" );
  Ok(()) }
