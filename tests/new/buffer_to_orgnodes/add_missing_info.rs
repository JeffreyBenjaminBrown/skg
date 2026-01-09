// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::buffer_to_orgnodes::add_missing_info::add_missing_info_to_forest;
use skg::test_utils::{run_with_test_db, compare_two_forests_modulo_id, compare_orgnode_forests};
use skg::types::orgnode::OrgNode;
use skg::types::misc::{SkgConfig, ID};
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
    |config, driver, _tantivy| Box::pin ( async move {
      test_add_missing_info_logic ( config, driver ) . await ?;
      Ok (( )) } )
  ) }

async fn test_add_missing_info_logic (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  // Applying 'add_missing_info_to_forest' should make
  // 'with_missing_info' equivalent to 'without_missing_info',
  // modulo the specific ID values added.
  // Also tests source inheritance from parent to children.
  let with_missing_info: &str =
    indoc! {"
            * (skg (id root) (source main)) root
            ** (skg (code (interp aliasCol))) aliases
            *** new alias
            *** (skg (code (interp alias))) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg (id root-pid) (source main)) root
            ** (skg (code (interp aliasCol))) aliases
            *** (skg (code (interp alias))) new alias
            *** (skg (code (interp alias))) preexisting alias
            ** (skg (id unpredictable) (source main)) no id
            *** (skg (id unpredictable) (source main)) also no id
        "};
  let mut after_adding_missing_info: Tree<OrgNode> =
    org_to_uninterpreted_nodes(
      with_missing_info).unwrap();
  add_missing_info_to_forest(
    &mut after_adding_missing_info,
    &config.db_name,
    driver ).await ?;
  let expected_forest: Tree<OrgNode> =
    org_to_uninterpreted_nodes(
      without_missing_info ). unwrap();
  assert_eq!(
    expected_forest.root().children().count(),
    1,
    "Expected exactly one tree in the expected forest" );
  assert!(
    compare_two_forests_modulo_id(
      &after_adding_missing_info,
      &expected_forest),
    "add_missing_info_to_forest: Forests not equivalent modulo ID." );

  { let actual_root : &OrgNode =
      after_adding_missing_info.root().first_child().unwrap().value();
    let actual_root_id : &ID =
      actual_root . id() . unwrap();
    assert_eq!(
      actual_root_id . 0,
      "root-pid",
      "Root ID 'root' should have changed to 'root-pid', based on the .skg file in fixtures/."
    ); }

  Ok (( )) }

#[test]
fn test_source_inheritance_multi_level(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-source-inheritance",
    "tests/new/buffer_to_orgnodes/add_missing_info/fixtures",
    "/tmp/tantivy-test-source-inheritance",
    |config, driver, _tantivy| Box::pin ( async move {
      test_source_inheritance_logic ( config, driver ) . await ?;
      Ok (( )) } )
  ) }

async fn test_source_inheritance_logic (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  // Tests source inheritance through multiple levels,
  // with explicit sources overriding inheritance at various depths.
  let input: &str =
    indoc! {"
            * (skg (id 1) (source main)) _
            ** (skg (id 11)) _
            *** (skg (id 111) (source alt)) _
            **** (skg (id 1111)) _
            *** (skg (id 112)) _
            * (skg (id 2)) _
            ** (skg (id 21) (source alt)) _
            ** (skg (id 22)) _
        "};

  let expected: &str =
    indoc! {"
            * (skg (id 1) (source main)) _
            ** (skg (id 11) (source main)) _
            *** (skg (id 111) (source alt)) _
            **** (skg (id 1111) (source alt)) _
            *** (skg (id 112) (source main)) _
            * (skg (id 2)) _
            ** (skg (id 21) (source alt)) _
            ** (skg (id 22)) _
        "};

  let mut actual_forest: Tree<OrgNode> =
    org_to_uninterpreted_nodes( input ).unwrap();
  add_missing_info_to_forest(
    &mut actual_forest,
    &config.db_name,
    driver ).await ?;
  let expected_forest: Tree<OrgNode> =
    org_to_uninterpreted_nodes( expected ).unwrap();

  assert!(
    compare_orgnode_forests(
      &actual_forest,
      &expected_forest),
    "Source inheritance: Forests not equivalent.\n\
     Expected sources to inherit from parent, with explicit sources overriding." );

  Ok (( )) }
