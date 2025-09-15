use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::search::fragile::find_container_of;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn test_find_container_of (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let config = SkgConfig {
      db_name        : "skg-test-typedb-search-fragile"       . into(),
      skg_folder     : "tests/typedb/search/fragile/fixtures" . into(),
      tantivy_folder : "irrelevant"                            . into() };
    let index_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");

    let driver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;

    populate_test_db_from_fixtures (
      index_folder,
      & config . db_name,
      & driver
    ) . await ?;

    let container_node = find_container_of(
      & config . db_name,
      & driver,
      & ID("2".to_string() )
    ).await?;
    assert_eq!(container_node, ID("1".to_string() ) );

    Ok (( )) } ) }
