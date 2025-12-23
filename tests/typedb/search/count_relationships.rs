// cargo test typedb::search::count_relationships

use skg::test_utils::run_with_test_db;
use skg::media::typedb::search::{count_containers, count_contents, count_link_sources};
use skg::types::{ID, SkgConfig};

use std::collections::HashMap;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-typedb-search-count-relationships",
    "tests/typedb/search/contains_from_pids/fixtures",
    "/tmp/tantivy-test-typedb-search-count-relationships",
    |config, driver, _tantivy| Box::pin ( async move {
      test_count_containers (
        config, driver ) . await ?;
      test_count_contents (
        config, driver ) . await ?;
      test_count_link_sources (
        config, driver ) . await ?;
      Ok (( )) } )
  ) }

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

async fn test_count_contents (
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
    count_contents (
      & config . db_name,
      & driver,
      & input_ids ) . await ?;

  // Expected counts:
  // 1 contains 2 and 3 (count = 2)
  // 2 contains nothing (count = 0)
  // 3 contains 1 (count = 1)
  // 10 contains 11 and 12 (count = 2)
  // 11 contains nothing (count = 0)
  let mut expected_counts : HashMap < ID, usize > =
    HashMap::new ();
  expected_counts . insert ( ID ( "1" . to_string () ), 2 );
  expected_counts . insert ( ID ( "2" . to_string () ), 0 );
  expected_counts . insert ( ID ( "3" . to_string () ), 1 );
  expected_counts . insert ( ID ( "10" . to_string () ), 2 );
  expected_counts . insert ( ID ( "11" . to_string () ), 0 );

  assert_eq! ( counts, expected_counts );

  Ok (( )) }

async fn test_count_link_sources (
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
    count_link_sources (
      & config . db_name,
      & driver,
      & input_ids ) . await ?;

  // Expected counts:
  // 1 is linked to by 3 (count = 1)
  // 2 is linked to by 3 and 10 (count = 2)
  // 3 is not linked to by anyone (count = 0)
  // 10 is not linked to by anyone (count = 0)
  // 11 is not linked to by anyone (count = 0)
  let mut expected_counts : HashMap < ID, usize > =
    HashMap::new ();
  expected_counts . insert ( ID ( "1" . to_string () ), 1 );
  expected_counts . insert ( ID ( "2" . to_string () ), 2 );
  expected_counts . insert ( ID ( "3" . to_string () ), 0 );
  expected_counts . insert ( ID ( "10" . to_string () ), 0 );
  expected_counts . insert ( ID ( "11" . to_string () ), 0 );

  assert_eq! ( counts, expected_counts );

  Ok (( )) }
