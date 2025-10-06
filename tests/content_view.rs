// cargo test --test content_view -- --nocapture

use indoc::indoc;
use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  TypeDBDriver,
  Credentials,
  DriverOptions,
};

use skg::render::{single_root_view, multi_root_view};
use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  path_containerward_to_end_cycle_and_or_branches, };
use skg::types::{ID, SkgConfig};

#[test]
fn test_a_mess_of_stuff
  () -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let config = SkgConfig {
      db_name        : "skg-test-content-view" . into(),
      skg_folder     : "tests/content_view/fixtures".into(),
      tantivy_folder : "irrelevant".into(),
      port           : 1730, };
    let skg_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");
    populate_test_db_from_fixtures (
      skg_folder,
      & config . db_name,
      & driver
    ) . await?;

    { // Print org views.
      // TODO ? Automate these "manual eyballing" tests.
      { // from the root of node "1".
        println!("Building org view from ID 2...");
        let view = single_root_view (
          & driver, & config,
          & ID ( "2" . to_string () )
        ) . await?;
        println!("{}", view); }
      { // From the root of node "5".
        println!("Building org view from ID 5...");
        let view = single_root_view (
          & driver, & config,
          & ID ( "5" . to_string () )
        ) . await?;
        println!("{}", view); }
      { // From the root of node "cycle-1".
        println!("Building org view from ID cycle-1...");
        let view = single_root_view (
          & driver, & config,
          & ID ( "cycle-1" . to_string () )
        ) . await?;
        println!("{}", view); } }

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

    Ok (()) } ) }

#[test]
fn test_multi_root_view
  () -> Result<(), Box<dyn Error>> {
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let config = SkgConfig {
      db_name        : "skg-test-multi-root-view" . into(),
      skg_folder     : "tests/content_view/fixtures-2".into(),
      tantivy_folder : "irrelevant".into(),
      port           : 1730, };
    let skg_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");
    populate_test_db_from_fixtures (
      skg_folder,
      & config . db_name,
      & driver
    ) . await?;

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

    let expected = indoc! {"* <skg<id:1>> 1
                            1 has a body
                            * <skg<id:2>> 2
                            * <skg<id:1,repeated>> 1
                            Repeated, probably above. Edit there, not here.
                            "};
    assert_eq!(result, expected,
               "Multi-root view should produce exact expected output");

    Ok (()) } ) }
