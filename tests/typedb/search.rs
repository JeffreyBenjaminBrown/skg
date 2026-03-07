// cargo test typedb::search

pub mod contains_from_pids;

use skg::test_utils::run_with_test_db;
use skg::dbs::typedb::search::find_related_nodes;
use skg::dbs::typedb::paths::{
  path_containerward_to_first_nonlinearity,
  PathToFirstNonlinearity};
use skg::types::misc::{ID, SkgConfig};

use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn the_tests (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-typedb-search-robust",
    "tests/typedb/search/robust/fixtures",
    "/tmp/tantivy-test-typedb-search-robust",
    |config, driver, _tantivy| Box::pin ( async move {
      test_find_containers_of (
        config, driver ) . await ?;
      test_path_containerward_to_first_nonlinearity (
        config, driver ) . await ?;
      Ok (( )) } ) ) }

async fn test_find_containers_of (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  let containers_of_1 : HashSet<ID> =
    find_related_nodes ( // 1 has no containers
      & config . db_name, & driver,
      &[ID("1" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_1,
               HashSet::new() );

  let containers_of_2 : HashSet<ID> =
    find_related_nodes ( // 2 is in 211
      & config . db_name, & driver,
      &[ID("2" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_2,
               HashSet::from([ID("211" . to_string())]) );
  let containers_of_11 : HashSet<ID> =
    find_related_nodes ( // 11 is in 1
      & config . db_name, & driver,
      &[ID("11" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_11,
               HashSet::from([ID("1" . to_string())]) );
  let containers_of_21 : HashSet<ID> =
    find_related_nodes ( // 21 is in 1
      & config . db_name, & driver,
      &[ID("21" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_21,
               HashSet::from([ID("2" . to_string())]) );
  let containers_of_211 : HashSet<ID> =
    find_related_nodes ( // 211 is in 21
      & config . db_name, & driver,
      &[ID("211" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_211,
               HashSet::from([ID("21" . to_string())]) );
  let containers_of_shared_1 : HashSet<ID> =
    find_related_nodes ( // shared_1 is in shared
      & config . db_name, & driver,
      &[ID("shared_1" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_shared_1,
               HashSet::from([ID("shared" . to_string())]) );
  let containers_of_shared_2 : HashSet<ID> =
    find_related_nodes ( // shared_2 is in shared
      & config . db_name, & driver,
      &[ID("shared_2" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_shared_2,
               HashSet::from([ID("shared" . to_string())]) );

  let containers_of_shared : HashSet<ID> =
    find_related_nodes ( // shared is in 1 *and* 2
      & config . db_name, & driver,
      &[ID("shared" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_shared,
               HashSet::from ( [ ID("1" . to_string() ),
                                 ID("2" . to_string() )] ) );

  let containers_of_11_extra : HashSet<ID> =
    find_related_nodes (
      // '11-extra-id' and '11' give the same result
      & config . db_name, & driver,
      &[ID("11-extra-id" . to_string() )],
      "contains", "contained", "container"
    ) . await ?;
  assert_eq! ( containers_of_11_extra,
               containers_of_11 );

  Ok (( )) }

async fn test_path_containerward_to_first_nonlinearity (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {

  // The containerward paths from 11 and from 11_extra_id
  // (which are two distinct IDs for the same node)
  // are [1] and [1] respectively (paths begin without the origin)
  // with no fork and no cycles.
  let result_11 : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("11" . to_string() )) . await ?;
  assert_eq! ( result_11.path, vec![ID("1" . to_string())] );
  assert_eq! ( result_11.cycle_nodes, HashSet::new() );
  assert_eq! ( result_11.branches, HashSet::new() );
  let result_11_extra : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("11-extra-id" . to_string() )) . await ?;
  assert_eq! ( result_11_extra.path, vec![ID("1" . to_string())] );
  assert_eq! ( result_11_extra.cycle_nodes, HashSet::new() );
  assert_eq! ( result_11_extra.branches, HashSet::new() );

  // The containerward path from 111 is [11, 1].
  // No fork and no cycles.
  let result_111 : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("111" . to_string() )) . await ?;
  assert_eq! ( result_111.path, vec![ID("11" . to_string()),
                                     ID("1" . to_string())] );
  assert_eq! ( result_111.cycle_nodes, HashSet::new() );
  assert_eq! ( result_111.branches, HashSet::new() );

  // The result from 211 is path=[21, 2], Some(211), {}.
  // That is, the path loops back at 211.
  let result_211 : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("211" . to_string() )) . await ?;
  assert_eq! ( result_211.path, vec![ID("21" . to_string()),
                                     ID("2" . to_string())] );
  assert_eq! ( result_211.cycle_nodes,
               HashSet::from([ID("211" . to_string())]) );
  assert_eq! ( result_211.branches, HashSet::new() );

  // The result from 21 is path=[2, 211], cycles = (21), fork = {}.
  // That is, it comes back to 21.
  let result_21 : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("21" . to_string() )) . await ?;
  assert_eq! ( result_21.path, vec![ID("2" . to_string()),
                                    ID("211" . to_string())] );
  assert_eq! ( result_21.cycle_nodes, HashSet::from([ID("21" . to_string())]) );
  assert_eq! ( result_21.branches, HashSet::new() );

  // The result from shared is path=[], cycles = empty, fork = {1,2}.
  let result_shared : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("shared" . to_string() )) . await ?;
  assert_eq! ( result_shared.path, vec![] );
  assert_eq! ( result_shared.cycle_nodes, HashSet::new() );
  assert_eq! ( result_shared.branches, HashSet::from([ID("1" . to_string()),
                                                      ID("2" . to_string())]) );

  // from shared_1 we get path=[shared], cycles = {}, fork = {1,2}
  let result_shared_1 : PathToFirstNonlinearity =
    path_containerward_to_first_nonlinearity (
      & config . db_name, & driver,
      & ID("shared_1" . to_string() )) . await ?;
  assert_eq! ( result_shared_1.path, vec![ID("shared" . to_string())] );
  assert_eq! ( result_shared_1.cycle_nodes, HashSet::new() );
  assert_eq! ( result_shared_1.branches,
               HashSet::from([ID("1" . to_string()),
                              ID("2" . to_string())]) );

  Ok (( )) }
