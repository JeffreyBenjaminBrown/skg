// cargo test typedb::search::util

use skg::test_utils::{setup_test_tantivy_and_typedb_dbs, cleanup_test_tantivy_and_typedb_dbs};
use skg::typedb::util::{pid_from_id, extract_payload_from_typedb_string_rep, pids_from_ids};
use skg::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn test_extract_payload_from_typedb_string_rep() {
  assert_eq!(
    extract_payload_from_typedb_string_rep("attribute \"value\" something"),
    "value".to_string()
  );
  assert_eq!(
    extract_payload_from_typedb_string_rep("\"hello world\" end"),
    "hello world".to_string()
  );
}

#[test]
fn test_pid_from_id (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name : &str =
      "skg-test-typedb-search-util";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_tantivy_and_typedb_dbs (
        db_name,
        "tests/typedb/search/util/fixtures",
        "/tmp/tantivy-test-typedb-search-util"
      ) . await ?;

    let path_to_4 = pid_from_id ( & config . db_name,
                                    & driver,
                                    & ID("4".to_string() ),
    ) . await ? . unwrap ();
    let path_to_44 = pid_from_id ( & config . db_name,
                                     & driver,
                                     & ID("44".to_string() )
    ) . await ? . unwrap ();
    assert_eq!(path_to_4,  ID("4" . to_string () ));
    assert_eq!(path_to_44, ID("4" . to_string () ));

    cleanup_test_tantivy_and_typedb_dbs (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (( )) } ) }


#[test]
fn test_pids_from_ids (
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let db_name : &str =
      "skg-test-typedb-pids-from-ids";
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_tantivy_and_typedb_dbs (
        db_name,
        "tests/typedb/search/util/fixtures",
        "/tmp/tantivy-test-typedb-pids-from-ids"
      ) . await ?;

    // Test bulk lookup of multiple IDs using nested subqueries
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

    cleanup_test_tantivy_and_typedb_dbs (
      db_name,
      &driver,
      Some ( config . tantivy_folder . as_path () )
    ) . await ?;
    Ok (( )) } ) }
