// cargo nextest run --test shared_db_session
//
// Pins the shared-db-session machinery (server/test_utils.rs) that
// lets a group of sub-tests reuse one TypeDB database, wiping the
// data in place between sub-tests instead of deleting and
// recreating the database. See TODO/faster-tests.org.

use skg::dbs::in_rust_graph::{InRustGraph, install_or_swap_global_handle, new_handle, snapshot_global, InRustGraphHandle};
use skg::test_utils::{
  ALL_SCHEMA_INSTANCE_TYPES,
  all_pids_from_typedb,
  count_instances_of_type,
  nodecomplete_example,
  run_with_shared_test_db,
  wipe_all_data };
use skg::types::misc::ID;
use skg::types::nodes::complete::NodeComplete;

use std::collections::HashSet;
use std::error::Error;
use std::sync::Arc;

/// Assert that the named types currently have instances and that,
/// after a wipe, EVERY schema type has zero instances. The zero
/// check is what protects shared sessions from cross-sub-test
/// leakage; the nonzero check keeps it from passing vacuously.
async fn assert_wipe_completeness (
  db_name              : &str,
  driver               : &Arc<typedb_driver::TypeDBDriver>,
  types_expected_nonzero : &[&str],
) -> Result<(), Box<dyn Error>> {
  for type_name in types_expected_nonzero {
    let count : usize = count_instances_of_type (
      db_name, driver, type_name ) . await ?;
    assert! ( count > 0,
              "expected instances of '{}' before the wipe", type_name ); }
  wipe_all_data (db_name, driver) . await ?;
  for type_name in ALL_SCHEMA_INSTANCE_TYPES {
    let count : usize = count_instances_of_type (
      db_name, driver, type_name ) . await ?;
    assert_eq! ( count, 0,
                 "instances of '{}' survived the wipe", type_name ); }
  Ok (( )) }

#[test]
fn test_shared_session_wipe_and_reset
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-shared-session",
    |session| Box::pin ( async move {
      { // Across the four fixture sets below, every relation type in
        // schema.tql gets populated at least once, so the wipe
        // assertions cover the whole schema.
        session . reset (
          "wipe after contains+subscribes+hides",
          "tests/hidden_from_subscriptions/fixtures-every-kind-of-col"
        ) . await ?;
        assert_wipe_completeness (
          & session . db_name, & session . driver,
          &[ "node", "source", "has_source",
             "contains", "subscribes",
             "hides_from_its_subscriptions" ] ) . await ?;
        session . reset (
          "wipe after overrides",
          "tests/overridden_as_such/fixtures" ) . await ?;
        assert_wipe_completeness (
          & session . db_name, & session . driver,
          &[ "overrides_view_of" ] ) . await ?;
        session . reset (
          "wipe after extra_ids",
          "tests/delete_strips_references_from_neighbors/fixtures-extra-ids"
        ) . await ?;
        assert_wipe_completeness (
          & session . db_name, & session . driver,
          &[ "extra_id", "has_extra_id" ] ) . await ?;
        session . reset (
          "wipe after textlinks",
          "tests/contexts/fixtures" ) . await ?;
        assert_wipe_completeness (
          & session . db_name, & session . driver,
          &[ "textlinks_to" ] ) . await ?; }
      { // A reset after mutations must leave EXACTLY the new fixture
        // set's pids -- nothing from the previous sub-test.
        session . reset (
          "no leakage across resets",
          "tests/overridden_as_such/fixtures" ) . await ?;
        session . reset (
          "no leakage across resets (2)",
          "tests/contexts/fixtures" ) . await ?;
        let pids : HashSet<ID> = all_pids_from_typedb (
          & session . db_name, & session . driver ) . await ?;
        assert! ( pids . contains ( &ID::new ("link-source") ));
        assert! ( ! pids . contains ( &ID::new ("R") ),
                  "pid from the previous fixture set leaked through reset" ); }
      Ok (( )) } )) }

#[test]
fn test_install_or_swap_global_handle
  () {
  let graph_1 : InRustGraph = {
    let mut node : NodeComplete = nodecomplete_example ();
    node . pid = ID::new ("first-graph-node");
    InRustGraph::from_nodecompletes ( & [node] ) };
  let graph_2 : InRustGraph = {
    let mut node : NodeComplete = nodecomplete_example ();
    node . pid = ID::new ("second-graph-node");
    InRustGraph::from_nodecompletes ( & [node] ) };
  let handle_1 : InRustGraphHandle =
    install_or_swap_global_handle ( new_handle (graph_1) );
  assert! ( snapshot_global () . unwrap () . nodes
            . contains_key ( &ID::new ("first-graph-node") ));
  let handle_2 : InRustGraphHandle =
    install_or_swap_global_handle ( new_handle (graph_2) );
  assert! ( snapshot_global () . unwrap () . nodes
            . contains_key ( &ID::new ("second-graph-node") ),
            "second install should have swapped the global contents" );
  assert! ( ! snapshot_global () . unwrap () . nodes
            . contains_key ( &ID::new ("first-graph-node") ));
  { // Both returned handles alias the global: a store through either
    // is globally visible, so saves through them stay consistent.
    // (handle_1 aliases it too, so its old graph_1 contents are
    // already gone -- rebuild graph_1 to have something to store.)
    let graph_1_again : InRustGraph = {
      let mut node : NodeComplete = nodecomplete_example ();
      node . pid = ID::new ("first-graph-node");
      InRustGraph::from_nodecompletes ( & [node] ) };
    handle_2 . store ( std::sync::Arc::new (graph_1_again) );
    assert! ( snapshot_global () . unwrap () . nodes
              . contains_key ( &ID::new ("first-graph-node") ));
    assert! ( handle_1 . load_full () . nodes
              . contains_key ( &ID::new ("first-graph-node") ),
              "handle_1 should alias the global too" ); }}
