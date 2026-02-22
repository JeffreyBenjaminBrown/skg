// cargo test neo4j::search::util

use skg::test_utils::run_with_test_db;
use skg::dbs::neo4j::search::pid_and_source_from_id;
use skg::dbs::neo4j::util::pids_from_ids::pids_from_ids;
use skg::types::misc::{ID, SourceName};

use std::error::Error;

#[test]
fn test_pid_from_id (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-search-util",
    "tests/typedb/search/util/fixtures",
    "/tmp/tantivy-test-neo4j-search-util",
    |config, graph, _tantivy| Box::pin ( async move {
      let (pid_for_4, source_for_4) = pid_and_source_from_id (
        graph,
        & ID("4".to_string() ),
      ) . await ? . unwrap ();
      let (pid_for_44, source_for_44) =
        pid_and_source_from_id ( graph,
                                   & ID("44".to_string() )
      ) . await ? . unwrap ();
      assert_eq!(pid_for_4,  ID("4" . to_string () ));
      assert_eq!(pid_for_44, ID("4" . to_string () ));
      assert_eq!(source_for_4,  SourceName::from("main") );
      assert_eq!(source_for_44, SourceName::from("main") );
      Ok (( )) } )) }

#[test]
fn test_pids_from_ids (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-neo4j-pids-from-ids",
    "tests/typedb/search/util/fixtures",
    "/tmp/tantivy-test-neo4j-pids-from-ids",
    |config, graph, _tantivy| Box::pin ( async move {
      // Test bulk lookup of multiple IDs
      let input_ids = vec![
        ID("4".to_string()),
        ID("44".to_string()),
        ID("nonexistent".to_string()),
        ID("5".to_string())
      ];

      let results = pids_from_ids ( graph,
                                    &input_ids ). await ?;

      // Check results
      assert_eq!(results.len(), 4);
      assert_eq!(results.get(&ID("4".to_string())), Some(&Some(ID("4".to_string()))));
      assert_eq!(results.get(&ID("44".to_string())), Some(&Some(ID("4".to_string()))));
      assert_eq!(results.get(&ID("nonexistent".to_string())), Some(&None));
      assert_eq!(results.get(&ID("5".to_string())), Some(&Some(ID("5".to_string()))));

      // Test empty input
      let empty_results = pids_from_ids ( graph,
                                          &[] ). await ?;
      assert!(empty_results.is_empty());

      Ok (( )) } )
  ) }
