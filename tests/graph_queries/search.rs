#[path = "search/contains_from_pids.rs"]
pub mod contains_from_pids;

use skg::dbs::graph_queries::paths::{
  PathToFirstNonlinearity, path_containerward_to_first_nonlinearity};
use skg::dbs::graph_queries::relations::find_container_ids_of_pid;
use skg::test_utils::run_with_test_graph;
use skg::types::misc::ID;

use std::collections::HashSet;
use std::error::Error;

fn id (value : &str) -> ID { ID::from (value) }
fn ids (values : &[&str]) -> HashSet<ID> {
  values . iter () . map (|value| id (value)) . collect () }

#[test]
fn relation_lookup_and_containerward_paths_use_the_fixture_graph (
) -> Result<(), Box<dyn Error>> {
  run_with_test_graph (
    "skg-test-graph-search-robust",
    "tests/graph_queries/search/robust/fixtures",
    "/tmp/tantivy-test-graph-search-robust",
    |_config, graph, _tantivy| Box::pin (async move {
      let snapshot = graph . load_full ();
      let graph = &snapshot . graph;

      for (node, expected) in [
        ("1", &[][..]),
        ("2", &["211"][..]),
        ("11", &["1"][..]),
        ("21", &["2"][..]),
        ("211", &["21"][..]),
        ("shared_1", &["shared"][..]),
        ("shared_2", &["shared"][..]),
        ("shared", &["1", "2"][..]),
      ] {
        assert_eq! (find_container_ids_of_pid (graph, &id (node), None),
                    ids (expected), "containers of {node}");
      }
      assert_eq! (
        find_container_ids_of_pid (graph, &id ("11-extra-id"), None),
        find_container_ids_of_pid (graph, &id ("11"), None));

      let path_cases : &[(&str, &[&str], &[&str], &[&str])] = &[
        ("11", &["1"], &[], &[]),
        ("11-extra-id", &["1"], &[], &[]),
        ("111", &["11", "1"], &[], &[]),
        ("211", &["21", "2"], &["211"], &[]),
        ("21", &["2", "211"], &["21"], &[]),
        ("shared", &[], &[], &["1", "2"]),
        ("shared_1", &["shared"], &[], &["1", "2"]),
      ];
      for (node, path, cycles, branches) in path_cases {
        let actual : PathToFirstNonlinearity =
          path_containerward_to_first_nonlinearity (
            graph, &id (node), None);
        assert_eq! (actual . path,
                    path . iter () . map (|value| id (value)) . collect::<Vec<_>> (),
                    "path from {node}");
        assert_eq! (actual . cycle_nodes, ids (cycles), "cycles from {node}");
        assert_eq! (actual . branches, ids (branches), "branches from {node}");
      }
      Ok (( ))
    }))
}
