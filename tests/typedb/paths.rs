// cargo test --test typedb -- typedb::paths
//
// To understand the graph, see
// tests/typedb/paths/fixtures/README.org

use skg::dbs::typedb::paths::{
  path_containerward_to_first_nonlinearity,
  PathToFirstNonlinearity};
use skg::to_org::expand::backpath::build_and_integrate_containerward_path;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::unchecked_viewnode::unchecked_to_checked_tree;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig};
use skg::types::viewnode::ViewNode;
use skg::types::memory::SkgNodeMap;
use skg::org_to_text::viewnode_forest_to_string;

use indoc::indoc;
use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn test_multi_cycle_fork(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-paths-multi-cycle-fork",
    "tests/typedb/paths/fixtures",
    "/tmp/tantivy-test-paths-multi-cycle-fork",
    |config, driver, _tantivy| Box::pin(async move {
      test_multi_cycle_fork_impl(config, driver) . await
    } )) }

async fn test_multi_cycle_fork_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Graph: b→a, b→e, c→b, d→e, d→c, e→d, f→e.
  // Containerward from a: a → b → c → d → e → fork {b, d, f}.
  // b and d are cycles (already in path). f is not.

  // Part 1: assert on PathToFirstNonlinearity struct.
  let result : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      &config.db_name, driver, &ID::from("a")
    ) . await ?;
  assert_eq!(
    result.path,
    vec![ ID::from("b"), ID::from("c"),
          ID::from("d"), ID::from("e") ] );
  assert_eq!(
    result.cycle_nodes,
    HashSet::from([ ID::from("b"), ID::from("d") ]) );
  assert_eq!(
    result.branches,
    HashSet::from([ ID::from("b"), ID::from("d"),
                    ID::from("f") ]) );

  // Part 2: assert on the rendered tree.
  let input: &str = indoc! {"
    * (skg (node (id a) (source main))) a
  "};
  let unchecked_forest = org_to_uninterpreted_nodes (input)?. 0;
  let mut forest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_forest)?;
  let node_a_id =
    forest . root () . first_child () . unwrap () . id ();
  let mut map : SkgNodeMap = SkgNodeMap::new();
  build_and_integrate_containerward_path (
    &mut forest, &mut map, node_a_id, &config, driver
  ) . await ?;
  // Sorted branches [b, d, f] are prepended in order,
  // so visual order is reversed: f, d, b.
  let expected: &str = indoc! {"
    * (skg (node (id a) (source main))) a
    ** (skg (node (id b) (source main) parentIgnores indefinitive)) b
    *** (skg (node (id c) (source main) parentIgnores indefinitive)) c
    **** (skg (node (id d) (source main) parentIgnores indefinitive)) d
    ***** (skg (node (id e) (source main) parentIgnores indefinitive)) e
    ****** (skg (node (id f) (source main) parentIgnores indefinitive)) f
    ****** (skg (node (id d) (source main) parentIgnores indefinitive)) d
    ****** (skg (node (id b) (source main) parentIgnores indefinitive)) b
  "};
  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> =
    unchecked_to_checked_tree (expected_unchecked)?;
  let actual_str : String =
    viewnode_forest_to_string (&forest)?;
  let expected_str : String =
    viewnode_forest_to_string (&expected_trees)?;
  assert_eq!(
    actual_str, expected_str,
    "Multi-cycle fork: tree structure should match" );
  Ok(()) }
