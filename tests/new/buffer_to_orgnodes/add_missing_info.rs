// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::save::{org_to_uninterpreted_nodes, add_missing_info_to_trees};
use skg::test_utils::populate_test_db_from_fixtures;
use skg::types::{OrgNode, SkgConfig, ID};
use ego_tree::Tree;

// Import test utilities
use skg::test_utils::compare_trees_modulo_id;

use futures::executor::block_on;
use std::error::Error;
use typedb_driver::{
  Credentials,
  DriverOptions,
  TypeDBDriver, };

#[test]
fn test_add_missing_info_comprehensive(
) -> Result<(), Box<dyn Error>> {
  block_on ( async {
    let ( config, driver ) : ( SkgConfig, TypeDBDriver ) =
      setup_test_database () . await ?;
    test_add_missing_info_logic ( &config, &driver ) . await ?;
    Ok (( )) } ) }

async fn setup_test_database (
) -> Result < ( SkgConfig, TypeDBDriver ), Box<dyn Error> > {
  let config : SkgConfig =
    SkgConfig {
      db_name        : "skg-test-add-missing-info"                           . into(),
      skg_folder     : "tests/new/buffer_to_orgnodes/add_missing_info/fixtures" . into(),
      tantivy_folder : "irrelevant"                                         . into(),
      port           : 1730 };
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
    & driver ). await ?;
  Ok (( config, driver )) }

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
            ** (skg (relToOrgParent aliasCol)) aliases
            *** new alias
            *** (skg (relToOrgParent alias)) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg (id root-pid)) root
            ** (skg (relToOrgParent aliasCol)) aliases
            *** (skg (relToOrgParent alias)) new alias
            *** (skg (relToOrgParent alias)) preexisting alias
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
    compare_trees_modulo_id(
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
