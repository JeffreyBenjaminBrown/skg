/// PURPOSE: Pin the hides counts feeding the "H" herald token: a node
/// that hides another counts its outbound 'hides'; the node it hides
/// counts its inbound 'hiders'; a bystander has neither.
///
/// Statistics are graph-native after the uniform-heralds rewrite, so
/// only the in-Rust graph path is exercised here.

use skg::dbs::in_rust_graph::stats::{
  AllGraphNodeStats, fetch_all_graphnodestats, graphnodestats_for_pid};
use skg::test_utils::{graph_handle_from_config, run_with_test_stores};
use skg::types::misc::ID;
use skg::types::viewnode::RelationCounts;

use std::error::Error;

fn rels_for (
  pid   : &str,
  stats : &AllGraphNodeStats,
) -> RelationCounts {
  graphnodestats_for_pid ( &ID::from (pid), stats, None )
    . rels
    . unwrap_or_else ( || panic! ("no rels for {}", pid) ) }

fn assert_hiding_stats (
  stats : &AllGraphNodeStats,
) {
  assert! ( rels_for ("hider", stats) . hides >= 1,
            "the hider should hide something (outbound H)" );
  assert! ( rels_for ("hidden", stats) . hiders >= 1,
            "the hidden node should be hidden by something (inbound H)" );
  let bystander : RelationCounts = rels_for ("bystander", stats);
  assert_eq! ( bystander . hides, 0,
               "the bystander should hide nothing" );
  assert_eq! ( bystander . hiders, 0,
               "the bystander should be hidden by nothing" ); }

#[test]
fn in_rust_path_computes_hiding (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-graphnodestats-hiding-inrust",
    "tests/graphnodestats_hiding/fixtures",
    "/tmp/tantivy-test-graphnodestats-hiding-inrust",
    |config, _tantivy| Box::pin ( async move {
      let graph = graph_handle_from_config (config) ? . load_full ();
      let stats : AllGraphNodeStats =
        fetch_all_graphnodestats (
          &graph,
          & [ ID::from ("hider"), ID::from ("hidden"),
              ID::from ("bystander") ] ) ?;
      assert_hiding_stats (&stats);
      Ok (( )) } ) ) }
