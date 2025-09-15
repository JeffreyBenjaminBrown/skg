use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::search::robust::find_containers_of;
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn test_find_containers_of (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let config : SkgConfig =
      SkgConfig {
        db_name        : "skg-test-typedb-search-robust"       . into(),
        skg_folder     : "tests/typedb/search/robust/fixtures" . into(),
        tantivy_folder : "irrelevant"                          . into() };
    let index_folder : &str =
      config . skg_folder . to_str ()
      . expect ("Invalid UTF-8 in tantivy index path");
    let driver : TypeDBDriver =
      TypeDBDriver::new(
        "127.0.0.1:1729",
        Credentials::new("admin", "password"),
        DriverOptions::new(false, None)?
      ).await?;
    populate_test_db_from_fixtures (
      index_folder,
      & config . db_name,
      & driver
    ) . await ?;

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

    Ok (( )) } ) }
