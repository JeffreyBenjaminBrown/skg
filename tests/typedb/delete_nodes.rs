
// cargo test test_delete_container_node -- --nocapture
// cargo test test_delete_contained_node -- --nocapture

use skg::test_utils::{setup_test_db, cleanup_test_db};
use skg::typedb::nodes::{delete_nodes_from_pids, which_ids_exist};
use skg::typedb::search::find_related_nodes;
use skg::typedb::util::extract_payload_from_typedb_string_rep;
use skg::types::{ID, SkgConfig};

use futures::StreamExt;
use futures::executor::block_on;
use std::collections::{HashSet, BTreeSet};
use std::error::Error;
use typedb_driver::{
  answer::{ConceptRow, QueryAnswer},
  Transaction,
  TransactionType,
  TypeDBDriver,
};

#[test]
fn test_delete_container_node (
) -> Result<(), Box<dyn Error>> {
  // Relevant aspects of the fixtures for this test:
  //   1 contains [2, 3]
  //   2 has extra_id 22
  block_on(async {
    let db_name : &str =
      "skg-test-delete-container";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_db (
        db_name,
        "tests/typedb/fixtures",
        "/tmp/tantivy-test-delete-container"
      ) . await ?;

    // Verify nodes 1, 2 and 3 exist
    let initial_ids : BTreeSet<String> =
      ["1", "2", "3"].iter().map(|s| s.to_string()).collect();
    let existing_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & initial_ids
    ) . await ?;
    assert!(existing_ids.contains("1"),
            "Node 1 should exist initially");
    assert!(existing_ids.contains("2"),
            "Node 2 should exist initially");
    assert!(existing_ids.contains("3"),
            "Node 3 should exist initially");

    // Verify node 1 contains 2 and 3
    let initially_contained : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & ID("1".to_string()),
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!(initially_contained.contains(&ID("2".to_string())),
            "Node 1 should initially contain node 2");
    assert!(initially_contained.contains(&ID("3".to_string())),
            "Node 1 should initially contain node 3");

    delete_nodes_from_pids ( // Delete node 1
      & config . db_name,
      & driver,
      & vec![ ID("1".to_string()) ],
    ) . await ?;

    // Verify node 1 is deleted, but nodes 2 and 3 still exist
    let after_delete_ids : BTreeSet<String> =
      ["1", "2", "3"].iter().map(|s| s.to_string()).collect();
    let remaining_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & after_delete_ids
    ) . await ?;
    assert!(!remaining_ids.contains("1"), "Node 1 should be deleted");
    assert!(remaining_ids.contains("2"), "Node 2 should still exist");
    assert!(remaining_ids.contains("3"), "Node 3 should still exist");

    // Check for any remaining contains relationships where 2 is contained
    { let tx : Transaction = driver . transaction (
        & config . db_name,
        TransactionType::Read
      ) . await ?;
      let query : &str = r#"
        match
          $container isa node, has id $container_id;
          $contained isa node, has id "2";
          $rel isa contains (container: $container, contained: $contained);
        select $container_id;
      "#;
      let answer : QueryAnswer = tx . query ( query ) . await ?;
      let mut rows = answer . into_rows ();
      println!("Checking for containers of node 2 after deleting node 1...");
      let mut container_count : usize = 0;
      while let Some ( row_res ) = rows . next () . await {
        let row : ConceptRow = row_res ?;
        if let Some ( concept ) = row . get ( "container_id" ) ? {
          let container_id : String =
            extract_payload_from_typedb_string_rep (
              & concept . to_string () );
          println!("Found container of node 2: {}", container_id);
          container_count += 1; }}
      assert_eq!(container_count, 0, "No containers should be found for node 2 after deleting node 1 (cascading delete should work)");
    } // Drop transaction here

    cleanup_test_db (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (( )) } ) }

#[test]
fn test_delete_contained_node (
) -> Result<(), Box<dyn Error>> {
  // Relevant aspects of the fixtures for this test:
  //   1 contains [2, 3]
  //   2 has extra_id 22
  block_on(async {
    let db_name : &str =
      "skg-test-delete-contained";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_db (
        db_name,
        "tests/typedb/fixtures",
        "/tmp/tantivy-test-delete-contained"
      ) . await ?;

    // Verify nodes 1, 2, 3 exist
    let initial_ids : BTreeSet<String> =
      ["1", "2", "3"].iter().map(|s| s.to_string()).collect();
    let existing_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & initial_ids
    ) . await ?;
    assert!(existing_ids.contains("1"), "Node 1 should exist initially");
    assert!(existing_ids.contains("2"), "Node 2 should exist initially");
    assert!(existing_ids.contains("3"), "Node 3 should exist initially");

    // Verify node 1 contains both 2 and 3
    let initially_contained : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & ID("1".to_string()),
      "contains",
      "container",
      "contained"
    ) . await ?;
    assert!(initially_contained.contains(&ID("2".to_string())),
            "Node 1 should initially contain node 2");
    assert!(initially_contained.contains(&ID("3".to_string())),
            "Node 1 should initially contain node 3");

    // Delete node 2 (contained) using its PID 2
    delete_nodes_from_pids (
      & config . db_name,
      & driver,
      & vec![ ID("2".to_string()) ]
    ) . await ?;

    // Verify nodes 1 and 3 exist, but 2 does not
    let after_delete_ids : BTreeSet<String> =
      ["1", "2", "3"].iter().map(|s| s.to_string()).collect();
    let remaining_ids : HashSet<String> = which_ids_exist (
      & config . db_name,
      & driver,
      & after_delete_ids
    ) . await ?;
    assert!(remaining_ids.contains("1"), "Node 1 should still exist");
    assert!(!remaining_ids.contains("2"), "Node 2 should be deleted");
    assert!(remaining_ids.contains("3"), "Node 3 should still exist");

    // Verify extra_id 22 is also gone
    let extra_id_check : BTreeSet<String> =
      ["22"].iter().map(|s| s.to_string()).collect();
    let remaining_extra_ids : HashSet<String> =
      which_ids_exist (
        & config . db_name,
        & driver,
        & extra_id_check
      ) . await ?;
    assert!(!remaining_extra_ids.contains("22"),
            "Extra ID 22 should be deleted");

    // Check what node 1 still contains after deleting node 2
    let finally_contained : HashSet<ID> = find_related_nodes (
      & config . db_name,
      & driver,
      & ID("1".to_string()),
      "contains",
      "container",
      "contained"
    ) . await ?;
    println!("After deleting node 2, node 1 contains:");
    for contained_id in & finally_contained {
      println!("  - {}", contained_id.0); }

    assert!(!finally_contained.contains(&ID("2".to_string())),
            "Node 1 should no longer contain deleted node 2 (cascading delete should work)");
    assert!(finally_contained.contains(&ID("3".to_string())),
            "Node 1 should still contain node 3");

    cleanup_test_db (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (( )) } ) }
