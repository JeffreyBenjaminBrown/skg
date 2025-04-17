// cargo test --test content_view -- --nocapture

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  TypeDBDriver,
  Credentials,
  DriverOptions,
};

use skg::typedb::create::make_db_destroying_earlier_one;
use skg::typedb::search::recursive_s_expression_from_node;

#[test]
fn test_recursive_s_expression_from_node
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
    println!("Building view from node with ID 1...");
    let view = recursive_s_expression_from_node(
      db_name, &driver, "1").await?;
    println!("Document View Output:");
    println!("{}", view);
    Ok (()) } ) }
