// cargo test --test content_view -- --nocapture

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  TypeDBDriver,
  Credentials,
  DriverOptions,
};

use skg::render::org::single_document_org_view;
use skg::typedb::create::overwrite_and_populate_new_db;
use skg::typedb::search::{
  find_rootish_container,
  path_to_rootish_container, };
use skg::types::ID;

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
    let db_name = "skg-test";
    overwrite_and_populate_new_db(
      "tests/content_view/fixtures", db_name, &driver ) . await?;

    { // Print org views.
      // TODO: Automate these "manual eyballing" tests.
      { // from the root of node "1".
        println!("Building org view from ID 2...");
        let view = single_document_org_view (
          db_name, &driver, &ID("2".to_string() )
        ) . await?;
        println!("{}", view); }
      { // From the root of node "5".
          println!("Building org view from ID 5...");
          let view = single_document_org_view (
            db_name, &driver, &ID("5".to_string() )
          ) . await?;
          println!("{}", view); }
      { // From the root of node "cycle-1".
          println!("Building org view from ID cycle-1...");
          let view = single_document_org_view (
            db_name, &driver, &ID("cycle-1".to_string() )
          ) . await?;
          println!("{}", view); } }

    // Test the path from node "4" to the root container
    match path_to_rootish_container (
      db_name, &driver, &ID("4".to_string() )
    ).await {
      Ok(path) => { assert_eq!(
        path,
        vec![
          ID ( "4".to_string() ),
          ID ( "2".to_string() ),
          ID ( "1".to_string() ) ],
        "Unexpected path to root container from node '4'.");
      }, Err(e) => {
        panic!("Error finding path to root container: {}", e); } }

    match find_rootish_container (
      db_name, &driver, &ID("4".to_string() )
    ).await {
      Ok(root) => { assert_eq!(
        root,
        ID ( "1".to_string() ),
        "Root of node '4' should be 1." ) },
      Err(e) => { panic!(
        "Error finding root container from id {}", e); } }

    // Test the path "to root" from node "cycle-3".
    // (1 contains 2 contains 3 contains 1.)
    match path_to_rootish_container (
      db_name, &driver, &ID("cycle-3".to_string() )
    ).await {
      Ok(path) => { assert_eq!(
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
    match path_to_rootish_container (
      db_name, &driver, &ID("cycle-1".to_string() )
    ).await {
      Ok(path) => { assert_eq!(
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
