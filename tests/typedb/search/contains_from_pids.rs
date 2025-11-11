// cargo test typedb::search::contains_from_pids

use skg::test_utils::run_with_test_db;
use skg::media::typedb::search::contains_from_pids;
use skg::types::{ID, SkgConfig};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-typedb-search-contains-from-pids",
    "tests/typedb/search/contains_from_pids/fixtures",
    "/tmp/tantivy-test-typedb-search-contains-from-pids",
    | config, driver | Box::pin ( async move {
      test_contains_from_pids (
        config, driver ) . await ?;
      Ok (( )) } )
  ) }

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
