use skg::dbs::graph_queries::{
  nodes::pid_and_source_from_id,
  pids_from_ids::pids_from_ids};
use skg::test_utils::run_with_test_graph;
use skg::types::misc::{ID, SourceName};

use std::error::Error;

#[test]
fn primary_and_extra_ids_resolve_from_one_graph_snapshot (
) -> Result<(), Box<dyn Error>> {
  run_with_test_graph (
    "skg-test-graph-id-resolution",
    "tests/graph_queries/search/util/fixtures",
    "/tmp/tantivy-test-graph-id-resolution",
    |_config, graph, _tantivy| Box::pin (async move {
      let snapshot = graph . load_full ();
      let graph = &snapshot . graph;
      assert_eq! (pid_and_source_from_id (graph, &ID::from ("4")),
                  Some ((ID::from ("4"), SourceName::from ("main"))));
      assert_eq! (pid_and_source_from_id (graph, &ID::from ("44")),
                  Some ((ID::from ("4"), SourceName::from ("main"))));

      let input = vec![ID::from ("4"), ID::from ("44"),
                       ID::from ("nonexistent"), ID::from ("5")];
      let resolved = pids_from_ids (graph, &input);
      assert_eq! (resolved . get (&ID::from ("4")), Some (&Some (ID::from ("4"))));
      assert_eq! (resolved . get (&ID::from ("44")), Some (&Some (ID::from ("4"))));
      assert_eq! (resolved . get (&ID::from ("nonexistent")), Some (&None));
      assert_eq! (resolved . get (&ID::from ("5")), Some (&Some (ID::from ("5"))));
      assert! (pids_from_ids (graph, &[]) . is_empty ());
      Ok (( ))
    }))
}
