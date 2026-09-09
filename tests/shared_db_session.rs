use skg::dbs::in_rust_graph::{InRustGraph, new_handle};
use skg::test_utils::{nodecomplete_example, run_with_shared_test_graph};
use skg::types::misc::ID;
use skg::types::nodes::complete::NodeComplete;

use std::error::Error;

#[test]
fn reset_replaces_the_complete_fixture_graph (
) -> Result<(), Box<dyn Error>> {
  run_with_shared_test_graph (
    "skg-test-shared-graph-session",
    |session| Box::pin (async move {
      session . reset (
        "first fixture", "tests/overridden_as_such/fixtures") . await?;
      assert! (session . graph . load_full () . graph . nodes
               . contains_key (&ID::from ("R")));
      session . reset (
        "second fixture", "tests/contexts/fixtures") . await?;
      let graph = session . graph . load_full ();
      assert! (graph . graph . nodes . contains_key (&ID::from ("link-source")));
      assert! (!graph . graph . nodes . contains_key (&ID::from ("R")),
               "a pid from the previous fixture leaked through reset");
      Ok (( ))
    }))
}

#[test]
fn independent_handles_do_not_share_publications () {
  let graph_1 : InRustGraph = {
    let mut node : NodeComplete = nodecomplete_example ();
    node . pid = ID::from ("first-graph-node");
    InRustGraph::from_nodecompletes (&[node]) };
  let graph_2 : InRustGraph = {
    let mut node : NodeComplete = nodecomplete_example ();
    node . pid = ID::from ("second-graph-node");
    InRustGraph::from_nodecompletes (&[node]) };
  let handle_1 = new_handle (graph_1);
  let handle_2 = new_handle (graph_2);
  assert! (handle_1 . load_full () . graph . nodes
           . contains_key (&ID::from ("first-graph-node")));
  assert! (!handle_1 . load_full () . graph . nodes
           . contains_key (&ID::from ("second-graph-node")));
  assert! (handle_2 . load_full () . graph . nodes
           . contains_key (&ID::from ("second-graph-node")));
}
