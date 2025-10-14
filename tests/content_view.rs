// cargo test --test content_view -- --nocapture

use indoc::indoc;
use futures::executor::block_on;
use std::error::Error;
use typedb_driver::TypeDBDriver;

use skg::mk_org_text::{multi_root_view};
use skg::test_utils::{setup_test_db, cleanup_test_db};
use skg::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  path_containerward_to_end_cycle_and_or_branches, };
use skg::types::{ID, SkgConfig};

#[test]
fn test_a_mess_of_stuff
  () -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let db_name : &str =
      "skg-test-content-view";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_db (
        db_name,
        "tests/content_view/fixtures",
        "/tmp/tantivy-test-content-view"
      ) . await ?;

    // Test the path from node "4" to the root container
    match path_containerward_to_end_cycle_and_or_branches (
      & config . db_name,
      & driver,
      & ID("4".to_string() )
    ).await {
      Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
        path,
        vec![
          ID ( "4".to_string() ),
          ID ( "2".to_string() ),
          ID ( "1".to_string() ) ],
        "Unexpected path to root container from node '4'.");
      }, Err(e) => {
        panic!("Error finding path to root container: {}", e); } }

    match climb_containerward_and_fetch_rootish_context (
      & config . db_name,
      & driver,
      & ID("4".to_string() )
    ).await {
      Ok(root) => { assert_eq!(
        root,
        ID ( "1".to_string() ),
        "Root of node '4' should be 1." ) },
      Err(e) => { panic!(
        "Error finding root container from id {}", e); } }

    // Test the path "to root" from node "cycle-3".
    // (1 contains 2 contains 3 contains 1.)
    match path_containerward_to_end_cycle_and_or_branches (
      & config . db_name,
      & driver,
      & ID("cycle-3".to_string() )
    ).await {
      Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
        path,
        vec![
          ID ( "cycle-3".to_string() ),
          ID ( "cycle-2".to_string() ),
          ID ( "cycle-1".to_string() ) ],
        "Unexpected path to \"root container\" from node 'cycle-3'.");
      }, Err(e) => {
        panic!("Error finding path to root container: {}", e); } }

    // Test the path "to root" from node "cycle-1".
    // (1 contains 2 contains 3 contains 1.)
    match path_containerward_to_end_cycle_and_or_branches (
      & config . db_name,
      & driver,
      & ID("cycle-1".to_string() )
    ).await {
      Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
        path,
        vec![
          ID ( "cycle-1".to_string() ),
          ID ( "cycle-3".to_string() ),
          ID ( "cycle-2".to_string() ),
        ],
        "Unexpected path to \"root container\" from node 'cycle-1'.");
      }, Err(e) => {
        panic!("Error finding path to root container: {}", e); } }

    cleanup_test_db (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (()) } ) }

#[test]
fn test_multi_root_view
  () -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name : &str =
      "skg-test-multi-root-view";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_db (
        db_name,
        "tests/content_view/fixtures-2",
        "/tmp/tantivy-test-multi-root-view"
      ) . await ?;

    let focii : Vec<ID> = vec![
      ID("1".to_string()),
      ID("2".to_string()),
      ID("1".to_string())
    ];
    let result : String = multi_root_view (
      & driver,
      & config,
      & focii
    ) . await ?;

    println!("Multi-root view result:\n{}", result);

    let expected = indoc! {"* (skg (id 1) (numContainers 0) (numContents 0) (numLinksIn 0)) 1
                            1 has a body
                            * (skg (id 2) (numContainers 0) (numContents 0) (numLinksIn 0)) 2
                            * (skg (id 1) repeated (numContainers 0) (numContents 0) (numLinksIn 0)) 1
                            Repeated, probably above. Edit there, not here.
                            "};
    assert_eq!(result, expected,
               "Multi-root view should produce exact expected output");

    cleanup_test_db (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (()) } ) }
