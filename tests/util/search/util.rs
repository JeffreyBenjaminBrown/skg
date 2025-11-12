// cargo test typedb::search::util

use skg::test_utils::run_with_test_db;
use skg::media::typedb::util::{pid_and_source_from_id, extract_payload_from_typedb_string_rep, pids_from_ids};
use skg::types::ID;

use std::error::Error;

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
  run_with_test_db (
    "skg-test-typedb-search-util",
    "tests/typedb/search/util/fixtures",
    "/tmp/tantivy-test-typedb-search-util",
    | config, driver | Box::pin ( async move {
      let (pid_for_4, source_for_4) = pid_and_source_from_id (
        & config . db_name,
        & driver,
        & ID("4".to_string() ),
      ) . await ? . unwrap ();
      let (pid_for_44, source_for_44) =
        pid_and_source_from_id ( & config . db_name,
                                   & driver,
                                   & ID("44".to_string() )
      ) . await ? . unwrap ();
      assert_eq!(pid_for_4,  ID("4" . to_string () ));
      assert_eq!(pid_for_44, ID("4" . to_string () ));
      assert_eq!(source_for_4,  "main".to_string() );
      assert_eq!(source_for_44, "main".to_string() );
      Ok (( )) } )) }

#[test]
fn test_pids_from_ids (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-typedb-pids-from-ids",
    "tests/typedb/search/util/fixtures",
    "/tmp/tantivy-test-typedb-pids-from-ids",
    | config, driver | Box::pin ( async move {
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

      Ok (( )) } )
  ) }
