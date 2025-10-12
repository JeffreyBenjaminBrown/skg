// cargo test typedb::search::contains_from_pids

use skg::test_utils::populate_test_db_from_fixtures;
use skg::typedb::search::contains_from_pids;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
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
    test_contains_from_pids (
      &config, &driver ) . await ?;
    Ok (( )) } ) }

async fn setup_test_database (
) -> Result < ( SkgConfig, TypeDBDriver ), Box<dyn Error> > {
  let config : SkgConfig =
    SkgConfig {
      db_name        : "skg-test-typedb-search-contains-from-pids" . into(),
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

async fn test_contains_from_pids (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  let input_pids : Vec<ID> =
    vec! [ ID ( "1"  . to_string () ),
           ID ( "2"  . to_string () ),
           ID ( "3"  . to_string () ),
           ID ( "11" . to_string () ) ];

  let ( container_to_contents, content_to_containers )
    : ( HashMap < ID, HashSet < ID > >, HashMap < ID, HashSet < ID > > ) =
    contains_from_pids (
      & config . db_name,
      & driver,
      & input_pids ) . await ?;

  // Expected container_to_contents:
  //   1 -> {2, 3}
  //   3 -> {1}
  let mut expected_container_to_contents : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  expected_container_to_contents . insert (
    ID ( "1" . to_string () ),
    HashSet::from ( [ ID ( "2" . to_string () ),
                      ID ( "3" . to_string () ) ] ) );
  expected_container_to_contents . insert (
    ID ( "3" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );

  // Expected content_to_containers:
  //   2 -> {1}
  //   3 -> {1}
  //   1 -> {3}
  let mut expected_content_to_containers : HashMap < ID, HashSet < ID > > =
    HashMap::new ();
  expected_content_to_containers . insert (
    ID ( "2" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );
  expected_content_to_containers . insert (
    ID ( "3" . to_string () ),
    HashSet::from ( [ ID ( "1" . to_string () ) ] ) );
  expected_content_to_containers . insert (
    ID ( "1" . to_string () ),
    HashSet::from ( [ ID ( "3" . to_string () ) ] ) );

  assert_eq! ( container_to_contents, expected_container_to_contents );
  assert_eq! ( content_to_containers, expected_content_to_containers );

  Ok (( )) }
