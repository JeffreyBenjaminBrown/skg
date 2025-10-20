// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::save::{org_to_uninterpreted_nodes, add_missing_info_to_trees};
use skg::test_utils::{run_with_test_db, compare_two_forests_modulo_id};
use skg::types::{OrgNode, SkgConfig, ID};
use ego_tree::Tree;

use std::error::Error;
use typedb_driver::TypeDBDriver;

#[test]
fn test_add_missing_info_comprehensive(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-add-missing-info",
    "tests/new/buffer_to_orgnodes/add_missing_info/fixtures",
    "/tmp/tantivy-test-add-missing-info",
    | config, driver | Box::pin ( async move {
      test_add_missing_info_logic ( config, driver ) . await ?;
      Ok (( )) } )
  ) }

async fn test_add_missing_info_logic (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  // Applying 'add_missing_info_to_trees' should make
  // 'with_missing_info' equivalent to 'without_missing_info',
  // modulo the specific ID values added.
  let with_missing_info: &str =
    indoc! {"
            * (skg (id root)) root
            ** (skg (relToParent aliasCol)) aliases
            *** new alias
            *** (skg (relToParent alias)) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg (id root-pid)) root
            ** (skg (relToParent aliasCol)) aliases
            *** (skg (relToParent alias)) new alias
            *** (skg (relToParent alias)) preexisting alias
            ** (skg (id unpredictable)) no id
            *** (skg (id unpredictable)) also no id
        "};
  let mut after_adding_missing_info: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(
      with_missing_info).unwrap();
  add_missing_info_to_trees(
    &mut after_adding_missing_info,
    &config.db_name,
    driver ).await ?;
  let expected_forest: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(
      without_missing_info ). unwrap();
  assert_eq!(
    expected_forest.len(),
    1,
    "Expected exactly one tree in the expected forest" );
  assert!(
    compare_two_forests_modulo_id(
      &after_adding_missing_info,
      &expected_forest),
    "add_missing_info_to_trees: Forests not equivalent modulo ID." );

  { let actual_root : &OrgNode =
      after_adding_missing_info[0] . root() . value();
    let actual_root_id : &ID =
      actual_root . metadata . id.as_ref() . unwrap();
    assert_eq!(
      actual_root_id . 0,
      "root-pid",
      "Root ID 'root' should have changed to 'root-pid', based on the .skg file in fixtures/."
    ); }

  Ok (( )) }
