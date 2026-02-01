// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::buffer_to_orgnodes::add_missing_info::add_missing_info_to_forest;
use skg::test_utils::{run_with_test_db, compare_two_unchecked_forests_modulo_id, compare_unchecked_orgnode_forests};
use skg::types::unchecked_orgnode::UncheckedOrgNode;
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
            * (skg (node (id root) (source main))) root
            ** (skg aliasCol) aliases
            *** new alias
            *** (skg alias) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg (node (id root-pid) (source main))) root
            ** (skg aliasCol) aliases
            *** (skg alias) new alias
            *** (skg alias) preexisting alias
            ** (skg (node (id unpredictable) (source main))) no id
            *** (skg (node (id unpredictable) (source main))) also no id
        "};
  let mut after_adding_missing_info: Tree<UncheckedOrgNode> =
    org_to_uninterpreted_nodes(
      with_missing_info).unwrap().0;
  add_missing_info_to_forest(
    &mut after_adding_missing_info,
    &config.db_name,
    driver ).await ?;
  let expected_forest: Tree<UncheckedOrgNode> =
    org_to_uninterpreted_nodes(
      without_missing_info ). unwrap().0;
  assert_eq!(
    expected_forest.root().children().count(),
    1,
    "Expected exactly one tree in the expected forest" );
  assert!(
    compare_two_unchecked_forests_modulo_id(
      &after_adding_missing_info,
      &expected_forest),
    "add_missing_info_to_forest: Forests not equivalent modulo ID." );

  { let actual_root : &UncheckedOrgNode =
      after_adding_missing_info.root().first_child().unwrap().value();
    let actual_root_id : &ID =
      actual_root . id_opt() . unwrap();
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
            * (skg (node (id 1) (source main))) _
            ** (skg (node (id 11))) _
            *** (skg (node (id 111) (source alt))) _
            **** (skg (node (id 1111))) _
            *** (skg (node (id 112))) _
            * (skg (node (id 2))) _
            ** (skg (node (id 21) (source alt))) _
            ** (skg (node (id 22))) _
        "};

  let expected: &str =
    indoc! {"
            * (skg (node (id 1) (source main))) _
            ** (skg (node (id 11) (source main))) _
            *** (skg (node (id 111) (source alt))) _
            **** (skg (node (id 1111) (source alt))) _
            *** (skg (node (id 112) (source main))) _
            * (skg (node (id 2))) _
            ** (skg (node (id 21) (source alt))) _
            ** (skg (node (id 22))) _
        "};

  let mut actual_forest: Tree<UncheckedOrgNode> =
    org_to_uninterpreted_nodes( input ).unwrap().0;
  add_missing_info_to_forest(
    &mut actual_forest,
    &config.db_name,
    driver ).await ?;
  let expected_forest: Tree<UncheckedOrgNode> =
    org_to_uninterpreted_nodes( expected ).unwrap().0;

  assert!(
    compare_unchecked_orgnode_forests(
      &actual_forest,
      &expected_forest),
    "Source inheritance: Forests not equivalent.\n\
     Expected sources to inherit from parent, with explicit sources overriding." );

  Ok (( )) }
