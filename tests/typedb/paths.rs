// cargo test --test typedb -- typedb::paths
//
// To understand the graph, see
// tests/typedb/paths/fixtures/README.org

use skg::dbs::typedb::paths::{
  path_containerward_to_first_nonlinearity,
  PathToFirstNonlinearity};
use skg::to_org::expand::backpath::build_and_integrate_containerward_path;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SkgConfig};
use skg::types::viewnode::ViewNode;

use skg::org_to_text::viewforest_to_string;

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
  let unchecked_viewforest = org_to_uninterpreted_nodes (input)?. 0;
  let mut viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (unchecked_viewforest)?;
  let node_a_id =
    viewforest . root () . first_child () . unwrap () . id ();

  build_and_integrate_containerward_path (
    &mut viewforest, node_a_id, &config, driver
  ) . await ?;
  // Sorted branches [b, d, f] are prepended in order,
  // so visual order is reversed: f, d, b.
  let expected: &str = indoc! {"
    * (skg (node (id a) (source main))) a
    ** (skg (node (id b) (source main) (parentIs independent) (birth containsParent) indef)) b
    *** (skg (node (id c) (source main) (parentIs independent) (birth containsParent) indef)) c
    **** (skg (node (id d) (source main) (parentIs independent) (birth containsParent) indef)) d
    ***** (skg (node (id e) (source main) (parentIs independent) (birth containsParent) indef)) e
    ****** (skg (node (id f) (source main) (parentIs independent) (birth containsParent) indef)) f
    ****** (skg (node (id d) (source main) (parentIs independent) (birth containsParent) indef)) d
    ****** (skg (node (id b) (source main) (parentIs independent) (birth containsParent) indef)) b
  "};
  let expected_unchecked = org_to_uninterpreted_nodes (expected)?. 0;
  let expected_trees: Tree<ViewNode> =
    maybePlaced_to_placed_tree (expected_unchecked)?;
  let actual_str : String =
    viewforest_to_string (&viewforest, config)?;
  let expected_str : String =
    viewforest_to_string (&expected_trees, config)?;
  assert_eq!(
    actual_str, expected_str,
    "Multi-cycle fork: tree structure should match" );
  Ok(()) }
