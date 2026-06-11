// cargo nextest run --test override_chain_notice
//
// A FRESH view that traverses a legacy compound override chain
// surfaces the "Compound overrides relationship traversed..."
// notice through the render's warning channel (Jeff, 2026-06-11).
//
// Owned chains cannot exist on disk (save and rebuild reject them),
// so the chain lives only in graphs constructed here: the fixture
// files hold P contains N, R overrides N, and an edgeless X; the
// in-test graph adds X overrides R. Resolution then maps N -> X
// with path length 2.
//
// PITFALL: this target installs that chain-bearing graph as the
// PROCESS-GLOBAL handle (content fetches and stats read it). It
// must stay in its own test target: under plain 'cargo test',
// sharing a process with tests that expect the disk graph would
// poison their tamper checks.

use std::error::Error;
use std::sync::Arc;

use skg::dbs::in_rust_graph::{InRustGraph, new_handle};
use skg::test_utils::run_with_test_db;
use skg::to_org::render::content_view::multi_root_view_via_env;
use skg::types::env::SkgEnv;
use skg::types::misc::{ID, MSV, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

fn node (
  pid       : &str,
  contains  : &[&str],
  overrides : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from ("main");
  node . contains =
    contains . iter () . map ( |id| ID::from (*id) ) . collect ();
  node . overrides_view_of =
    if overrides . is_empty () {
      MSV::Unspecified
    } else {
      MSV::Specified (
        overrides . iter ()
        . map ( |id| ID::from (*id) )
        . collect () )
    };
  node }

#[test]
fn a_fresh_view_traversing_a_chain_warns
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-override-chain-notice",
    "tests/override_chain_notice/fixtures",
    "/tmp/tantivy-test-override-chain-notice",
    |config, driver, tantivy| Box::pin ( async move {
      let chain_graph : InRustGraph =
        // The fixtures plus the in-graph-only chain edge X -> R.
        InRustGraph::from_nodecompletes ( &[
          node ("P", &["N"], &[]),
          node ("N", &[],    &[]),
          node ("R", &[],    &["N"]),
          node ("X", &[],    &["R"]),
        ] );
      skg::dbs::in_rust_graph::try_init_global_handle (
        new_handle ( chain_graph . clone () ) );
      let env : SkgEnv = SkgEnv {
        config        : config . clone (),
        in_rust_graph : new_handle (chain_graph),
        tantivy_index : tantivy . clone (),
        driver        : Arc::clone (driver), };
      let mut warnings : Vec<String> = Vec::new ();
      let (view, _pids, _tree) =
        multi_root_view_via_env (
          &env, &[ ID::from ("P") ], false, None,
          &mut warnings ) . await ?;
      assert! ( view . lines () . any (
                  |l| l . contains ("(id X)")
                      && l . contains ("(overridesHere N)") ),
        "the chain's end, X, is drawn in place of N:\n{}", view );
      assert! ( warnings . iter () . any (
                  |w| w . contains (
                    "Compound overrides relationship traversed" )),
        "the fresh view surfaces the compound notice; got: {:?}",
        warnings );
      Ok (( )) } )) }
