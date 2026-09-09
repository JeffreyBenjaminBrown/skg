use skg::dbs::graph_queries::ancestry::{
  AncestryTree, full_containerward_ancestry};
use skg::test_utils::run_with_shared_test_graph;
use skg::types::misc::ID;

use std::error::Error;

fn id (value : &str) -> ID { ID::from (value) }

fn root (value : &str) -> AncestryTree { AncestryTree::Root (id (value)) }
fn repeated (value : &str) -> AncestryTree {
  AncestryTree::Repeated (id (value)) }
fn inner (value : &str, children : Vec<AncestryTree>) -> AncestryTree {
  AncestryTree::Inner (id (value), children) }

#[test]
fn ancestry_shapes_cycles_diamonds_and_depth_limits (
) -> Result<(), Box<dyn Error>> {
  run_with_shared_test_graph (
    "skg-test-graph-ancestry",
    |session| Box::pin (async move {
      let cases : &[(&str, &str, &str, usize, AncestryTree)] = &[
        ("island", "tests/graph_queries/ancestry/fixtures-island", "a", 20,
         root ("a")),
        ("fork", "tests/graph_queries/ancestry/fixtures-fork", "x", 20,
         inner ("x", vec![root ("a"), inner ("b", vec![root ("c")])])),
        ("one-cycle", "tests/graph_queries/ancestry/fixtures-1cycle", "a", 20,
         inner ("a", vec![repeated ("a")])),
        ("two-cycle", "tests/graph_queries/ancestry/fixtures-2cycle", "a", 20,
         inner ("a", vec![inner ("b", vec![repeated ("a")])])),
        ("three-cycle", "tests/graph_queries/ancestry/fixtures-3cycle", "a", 20,
         inner ("a", vec![inner ("c", vec![inner (
           "b", vec![repeated ("a")])])])),
        ("diamond", "tests/graph_queries/ancestry/fixtures-diamond", "x", 20,
         inner ("x", vec![inner ("b", vec![root ("a")]),
                           inner ("c", vec![repeated ("a")])])),
        ("fork-diamond-cycle",
         "tests/graph_queries/ancestry/fixtures-fork-diamond-cycle", "origin", 20,
         inner ("origin", vec![
           inner ("aa", vec![inner ("a", vec![
             repeated ("origin"), repeated ("root")])]),
           inner ("b", vec![root ("root")])])),
        ("depth-limit", "tests/graph_queries/ancestry/fixtures-depth-limit", "e", 3,
         inner ("e", vec![inner ("d", vec![
           AncestryTree::DepthTruncated (id ("c"))])])),
      ];
      for (name, fixtures, origin, depth, expected) in cases {
        session . reset (name, fixtures) . await?;
        let snapshot = session . graph . load_full ();
        let actual : AncestryTree = full_containerward_ancestry (
          &snapshot . graph, &id (origin), *depth, None);
        assert_eq! (&actual, expected, "ancestry case {name}");
      }
      Ok (( ))
    }))
}
