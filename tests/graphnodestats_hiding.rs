/// PURPOSE: Pin the 'hiding' graphstat ("H" herald) in both stats
/// paths: a node that hides another, and the node it hides, both
/// count as hiding; a bystander does not.
///
/// PITFALL: which path runs depends on the process-global in-Rust
/// graph handle. The TypeDB-path test relies on the handle being
/// uninitialized, which nextest's process-per-test model guarantees.
/// (Under plain 'cargo test' the in-Rust test's handle could leak
/// into the TypeDB-path test; both paths must agree here anyway, and
/// the fixture graphs match, so the assertions still hold.)

use skg::dbs::typedb::search::all_graphnodestats::{
  AllGraphNodeStats, fetch_all_graphnodestats, graphnodestats_for_pid};
use skg::test_utils::{graph_handle_from_config, run_with_test_db};
use skg::types::misc::ID;

use std::error::Error;

fn assert_hiding_stats (
  stats : &AllGraphNodeStats,
) {
  assert! ( stats . has_hides . contains ( &ID::from ("hider") ),
            "the hider should count as hiding" );
  assert! ( stats . has_hides . contains ( &ID::from ("hidden") ),
            "the hidden node should count as hiding" );
  assert! ( ! stats . has_hides . contains ( &ID::from ("bystander") ),
            "the bystander should not count as hiding" );
  assert! ( graphnodestats_for_pid (
              &ID::from ("hider"), stats, None ) . hiding );
  assert! ( ! graphnodestats_for_pid (
              &ID::from ("bystander"), stats, None ) . hiding ); }

#[test]
fn typedb_path_computes_hiding (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-graphnodestats-hiding-typedb",
    "tests/graphnodestats_hiding/fixtures",
    "/tmp/tantivy-test-graphnodestats-hiding-typedb",
    |config, driver, _tantivy| Box::pin ( async move {
      let stats : AllGraphNodeStats =
        fetch_all_graphnodestats (
          & config . db_name, driver,
          & [ ID::from ("hider"), ID::from ("hidden"),
              ID::from ("bystander") ] ) . await ?;
      assert_hiding_stats (&stats);
      Ok (( )) } ) ) }

#[test]
fn in_rust_path_computes_hiding (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-graphnodestats-hiding-inrust",
    "tests/graphnodestats_hiding/fixtures",
    "/tmp/tantivy-test-graphnodestats-hiding-inrust",
    |config, driver, _tantivy| Box::pin ( async move {
      skg::dbs::in_rust_graph::try_init_global_handle (
        graph_handle_from_config (config) ? );
      let stats : AllGraphNodeStats =
        // With the global handle installed, the dispatcher takes the
        // in-Rust path; TypeDB is not consulted.
        fetch_all_graphnodestats (
          & config . db_name, driver,
          & [ ID::from ("hider"), ID::from ("hidden"),
              ID::from ("bystander") ] ) . await ?;
      assert_hiding_stats (&stats);
      Ok (( )) } ) ) }
