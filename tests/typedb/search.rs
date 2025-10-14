// cargo test typedb::search

pub mod contains_from_pids;
pub mod count_relationships;

use skg::test_utils::{setup_test_db, cleanup_test_db};
use skg::typedb::search::find_containers_of;
use skg::typedb::search::path_containerward_to_end_cycle_and_or_branches;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let db_name : &str =
      "skg-test-typedb-search-robust";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_db (
        db_name,
        "tests/typedb/search/robust/fixtures",
        "/tmp/tantivy-test-typedb-search-robust"
      ) . await ?;
    test_find_containers_of (
      &config, &driver ) . await ?;
    test_path_containerward_to_end_cycle_and_or_branches (
      &config, &driver ) . await ?;
    cleanup_test_db (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (( )) } ) }

async fn test_find_containers_of (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  let containers_of_1 : HashSet<ID> =
    find_containers_of ( // 1 has no containers
      & config . db_name, & driver,
      & ID("1".to_string() )) . await ?;
  assert_eq! ( containers_of_1,
               HashSet::new() );

  let containers_of_2 : HashSet<ID> =
    find_containers_of ( // 2 is in 211
      & config . db_name, & driver,
      & ID("2".to_string() )) . await ?;
  assert_eq! ( containers_of_2,
               HashSet::from([ID("211".to_string())]) );
  let containers_of_11 : HashSet<ID> =
    find_containers_of ( // 11 is in 1
      & config . db_name, & driver,
      & ID("11".to_string() )) . await ?;
  assert_eq! ( containers_of_11,
               HashSet::from([ID("1".to_string())]) );
  let containers_of_21 : HashSet<ID> =
    find_containers_of ( // 21 is in 1
      & config . db_name, & driver,
      & ID("21".to_string() )) . await ?;
  assert_eq! ( containers_of_21,
               HashSet::from([ID("2".to_string())]) );
  let containers_of_211 : HashSet<ID> =
    find_containers_of ( // 211 is in 21
      & config . db_name, & driver,
      & ID("211".to_string() )) . await ?;
  assert_eq! ( containers_of_211,
               HashSet::from([ID("21".to_string())]) );
  let containers_of_shared_1 : HashSet<ID> =
    find_containers_of ( // shared_1 is in shared
      & config . db_name, & driver,
      & ID("shared_1".to_string() )) . await ?;
  assert_eq! ( containers_of_shared_1,
               HashSet::from([ID("shared".to_string())]) );
  let containers_of_shared_2 : HashSet<ID> =
    find_containers_of ( // shared_2 is in shared
      & config . db_name, & driver,
      & ID("shared_2".to_string() )) . await ?;
  assert_eq! ( containers_of_shared_2,
               HashSet::from([ID("shared".to_string())]) );

  let containers_of_shared : HashSet<ID> =
    find_containers_of( // shared is in 1 *and* 2
      & config . db_name, & driver,
      & ID("shared".to_string() )) . await ?;
  assert_eq! ( containers_of_shared,
               HashSet::from ( [ ID("1".to_string() ),
                                 ID("2".to_string() )] ) );

  let containers_of_11_extra : HashSet<ID> =
    find_containers_of(
      // '11-extra-id' and '11' give the same result
      & config . db_name, & driver,
      & ID("11-extra-id".to_string() )) . await ?;
  assert_eq! ( containers_of_11_extra,
               containers_of_11 );

  Ok (( )) }

async fn test_path_containerward_to_end_cycle_and_or_branches (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  // The containerward paths from 11 and from 11_extra_id
  // (which are two distinct IDs for the same node)
  // are [11, 1] and [11-extra-id, 1] respectively,
  // with no option and no set.
  let result_11 : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("11".to_string() )) . await ?;
  assert_eq! ( result_11.0, vec![ID("11".to_string()),
                                 ID("1".to_string())] );
  assert_eq! ( result_11.1, None );
  assert_eq! ( result_11.2, HashSet::new() );
  let result_11_extra : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("11-extra-id".to_string() )) . await ?;
  assert_eq! ( result_11_extra.0, vec![ID("11-extra-id".to_string()),
                                       ID("1".to_string())] );
  assert_eq! ( result_11_extra.1, None );
  assert_eq! ( result_11_extra.2, HashSet::new() );

  // The containerward path from 111 is [111, 11, 1].
  // No option, no set.
  let result_111 : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("111".to_string() )) . await ?;
  assert_eq! ( result_111.0, vec![ID("111".to_string()),
                                  ID("11".to_string()),
                                  ID("1".to_string())] );
  assert_eq! ( result_111.1, None );
  assert_eq! ( result_111.2, HashSet::new() );

  // The result from 211 is Vec([211, 21, 2]), Some(211), {}.
  // That is, it comes back to 211.
  let result_211 : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("211".to_string() )) . await ?;
  assert_eq! ( result_211.0, vec![ID("211".to_string()),
                                  ID("21".to_string()),
                                  ID("2".to_string())] );
  assert_eq! ( result_211.1, Some(ID("211".to_string())) );
  assert_eq! ( result_211.2, HashSet::new() );

  // The result from 21 is Vec([21, 2, 211]), Some(21), {}.
  // That is, it comes back to 21.
  let result_21 : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("21".to_string() )) . await ?;
  assert_eq! ( result_21.0, vec![ID("21".to_string()),
                                 ID("2".to_string()),
                                 ID("211".to_string())] );
  assert_eq! ( result_21.1, Some(ID("21".to_string())) );
  assert_eq! ( result_21.2, HashSet::new() );

  // The result from shared is [shared], None, {1,2}.
  let result_shared : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("shared".to_string() )) . await ?;
  assert_eq! ( result_shared.0, vec![ID("shared".to_string())] );
  assert_eq! ( result_shared.1, None );
  assert_eq! ( result_shared.2, HashSet::from([ID("1".to_string()),
                                               ID("2".to_string())]) );

  // The result from shared_1 is [shared_1, shared], None, {1,2}.
  let result_shared_1 : ( Vec<ID>, Option<ID>, HashSet<ID> ) =
    path_containerward_to_end_cycle_and_or_branches (
      & config . db_name, & driver,
      & ID("shared_1".to_string() )) . await ?;
  assert_eq! ( result_shared_1.0, vec![ID("shared_1".to_string()),
                                       ID("shared".to_string())] );
  assert_eq! ( result_shared_1.1, None );
  assert_eq! ( result_shared_1.2,
               HashSet::from([ID("1".to_string()),
                              ID("2".to_string())]) );

  Ok (( )) }
