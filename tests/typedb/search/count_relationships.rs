// cargo test typedb::search::count_relationships

use skg::test_utils::populate_test_db_from_fixtures;
use skg::typedb::search::count_containers;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::collections::HashMap;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_database () . await ?;
    test_count_containers (
      &config, &driver ) . await ?;
    Ok (( )) } ) }

async fn setup_test_database (
) -> Result < ( SkgConfig, TypeDBDriver ), Box<dyn Error> > {
  let config : SkgConfig =
    SkgConfig {
      db_name        : "skg-test-typedb-search-count-relationships" . into(),
      skg_folder     : "tests/typedb/search/contains_from_pids/fixtures" . into(),
      tantivy_folder : "irrelevant"                                . into(),
      port           : 1730 };
  let index_folder : &str =
    config . skg_folder . to_str ()
    . expect ("Invalid UTF-8 in skg folder path");
  let driver : TypeDBDriver =
    TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;
  populate_test_db_from_fixtures (
    index_folder,
    & config . db_name,
    & driver ). await ?;
  Ok (( config, driver )) }

async fn test_count_containers (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  let input_ids : Vec<ID> =
    vec! [ ID ( "1"  . to_string () ),
           ID ( "2"  . to_string () ),
           ID ( "3"  . to_string () ),
           ID ( "10" . to_string () ),
           ID ( "11" . to_string () ) ];

  let counts : HashMap < ID, usize > =
    count_containers (
      & config . db_name,
      & driver,
      & input_ids ) . await ?;

  // Expected counts:
  // 1 is contained by 3 (count = 1)
  // 2 is contained by 1 (count = 1)
  // 3 is contained by 1 (count = 1)
  // 10 is not contained by anyone (count = 0)
  // 11 is contained by 10 (count = 1)
  let mut expected_counts : HashMap < ID, usize > =
    HashMap::new ();
  expected_counts . insert ( ID ( "1" . to_string () ), 1 );
  expected_counts . insert ( ID ( "2" . to_string () ), 1 );
  expected_counts . insert ( ID ( "3" . to_string () ), 1 );
  expected_counts . insert ( ID ( "10" . to_string () ), 0 );
  expected_counts . insert ( ID ( "11" . to_string () ), 1 );

  assert_eq! ( counts, expected_counts );

  Ok (( )) }
