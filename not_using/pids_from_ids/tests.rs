// TODO: Are the two tests below identical?
// They certainly need deduplication.

use skg::typedb::init::populate_test_db_from_fixtures;
use skg::typedb::search::util::{pid_from_id, pids_from_ids, extract_payload_from_typedb_string_rep};
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn test_pids_from_ids (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let config = SkgConfig {
      db_name        : "skg-test-typedb-search-util-bulk"  . into(),
      skg_folder     : "tests/typedb/search/util/fixtures" . into(),
      tantivy_folder : "irrelevant"                        . into(),
      port           : 1730 };
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

    // Test bulk lookup of multiple IDs
    let input_ids = vec![
      ID("4".to_string()),
      ID("44".to_string()),
      ID("nonexistent".to_string()),
      ID("5".to_string())
    ];

    let results = pids_from_ids ( & config . db_name,
                                  & driver,
                                  &input_ids ). await ?;

    // Check results
    assert_eq!(results.len(), 4);
    assert_eq!(results.get(&ID("4".to_string())), Some(&Some(ID("4".to_string()))));
    assert_eq!(results.get(&ID("44".to_string())), Some(&Some(ID("4".to_string()))));
    assert_eq!(results.get(&ID("nonexistent".to_string())), Some(&None));
    assert_eq!(results.get(&ID("5".to_string())), Some(&Some(ID("5".to_string()))));

    // Test empty input
    let empty_results = pids_from_ids ( & config . db_name,
                                        & driver,
                                        &[] ). await ?;
    assert!(empty_results.is_empty());

    Ok (( )) } ) }


#[test]
fn test_pids_from_ids (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let config = SkgConfig {
      db_name        : "skg-test-typedb-pids-from-ids" . into(),
      skg_folder     : "tests/typedb/fixtures"          . into(),
      tantivy_folder : "irrelevant"                     . into(),
      port           : 1730 };
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

    // Test bulk lookup of multiple IDs
    let input_ids = vec![
      ID("a".to_string()),
      ID("b".to_string()),
      ID("nonexistent".to_string()),
      ID("c".to_string())
    ];

    let results = pids_from_ids ( & config . db_name,
                                  & driver,
                                  &input_ids ). await ?;

    // Check results
    assert_eq!(results.len(), 4);
    assert_eq!(results.get(&ID("a".to_string())), Some(&Some(ID("a".to_string()))));
    assert_eq!(results.get(&ID("b".to_string())), Some(&Some(ID("b".to_string()))));
    assert_eq!(results.get(&ID("nonexistent".to_string())), Some(&None));
    assert_eq!(results.get(&ID("c".to_string())), Some(&Some(ID("c".to_string()))));

    // Test empty input
    let empty_results = pids_from_ids ( & config . db_name,
                                        & driver,
                                        &[] ). await ?;
    assert!(empty_results.is_empty());

    Ok (( )) } ) }
