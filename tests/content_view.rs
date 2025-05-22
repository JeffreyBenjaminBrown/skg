// cargo test --test content_view -- --nocapture

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  TypeDBDriver,
  Credentials,
  DriverOptions,
};

use skg::typedb::create::make_db_destroying_earlier_one;
use skg::typedb::search::{
  single_document_view,
  path_to_root_container };
use skg::types::ID;

#[test]
fn test_single_document_view
  () -> Result<(), Box<dyn Error>> {
  // Use block_on to run async code in a synchronous test
  block_on(async {
    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
    let db_name = "skg-test";
    make_db_destroying_earlier_one(
      "tests/content_view/fixtures", db_name, &driver ) . await?;

    // Print the view from node "1".
    // TODO: Automate.
    println!("Building view from node with ID 2...");
    let view = single_document_view (
      db_name, &driver, &ID("2".to_string() )
      ) . await?;
    println!("Document View Output:");
    println!("{}", view);

    // Print the view from node "5".
    // TODO: Automate.
    println!("Building view from node with ID 5...");
    let view = single_document_view(
      db_name, &driver, &ID("5".to_string() )
    ) . await?;
    println!("Document View Output:");
    println!("{}", view);

    // Test the path from node "4" to the root container
    match path_to_root_container(
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

    Ok (()) } ) }
