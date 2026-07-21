// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_uninterpreted_nodes,
  org_to_uninterpreted_viewforest};
use skg::from_text::buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest,
  absent_parentIs_under_visible_parent_becomes_isContainer};
use skg::test_utils::{run_with_shared_test_db, compare_viewnode_trees_modulo_id, compare_viewnode_trees};
use skg::types::maybe_placed_viewnode::{
  MpViewnode, MpViewnodeKind, MpVognode};
use skg::types::misc::{SkgConfig, ID, SourceName, TantivyIndex};
use skg::types::tree::forest::{
  MpViewForest,
  tree_forest_root_ids};
use skg::types::viewnode::ParentIs;

use ego_tree::Tree;

use std::error::Error;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-new-add-missing-info",
    |s| Box::pin ( async move {
      s . reset ("test_add_missing_info_comprehensive",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") . await ?;
      test_add_missing_info_comprehensive (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_source_inheritance_multi_level",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") . await ?;
      test_source_inheritance_multi_level (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_sourceless_col_member_gets_graph_source",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") . await ?;
      test_sourceless_col_member_gets_graph_source (
        &s . config, &s . driver ) . await ?;
      Ok (( )) } )) }

/// Regression for TODO/BUG_reciprocal-subscribe.org: a subscribee
/// pasted from the link stack arrives as a bare id under a
/// 'subscribeeCol' scaffold, with no source. Its org-parent is a
/// scaffold, so 'inherit_parent_source_if_possible' cannot supply a
/// source; enrichment must resolve it from the graph by id instead.
/// Before the fix this node stayed sourceless and the save was refused
/// with "ActiveNode must have a source that exists in the config".
async fn test_sourceless_col_member_gets_graph_source (
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // 'root' is an extra_id of fixture node 'root-pid', whose source is
  // 'main'. The subscribee reference below carries neither source nor
  // the primary id, and is indefinitive -- exactly the bare-paste shape
  // (a link-stack paste yields an indefinitive node with only an id).
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg subscribeeCol)
            *** (skg (node (id root) indef)) subscribee reference
        "};
  let mut viewforest : MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;
  add_missing_info_to_viewforest (
    &mut viewforest, &config . db_name, driver ) . await ?;
  let owner = viewforest . root() . first_child() . unwrap();
  let col   = owner . first_child() . unwrap();
  let member = col . first_child() . unwrap();
  match &member . value() . kind {
    MpViewnodeKind::Vognode (MpVognode::Active (t)) => {
      assert_eq! (
        t . source, Some (SourceName::from ("main")),
        "Sourceless col member should inherit its source from the \
         graph (node root-pid lives in source 'main'), not stay \
         sourceless." );
      assert_eq! (
        t . id . as_ref() . unwrap() . 0, "root-pid",
        "The referenced id 'root' should have resolved to its pid \
         'root-pid'." ); }
    _ => panic! ("expected an ActiveNode subscribee member") }
  Ok (( )) }

async fn test_add_missing_info_comprehensive (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_add_missing_info_logic ( config, driver ) . await ?;
      Ok (( )) }

async fn test_add_missing_info_logic (
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  // Applying 'add_missing_info_to_viewforest' should make
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
  let mut after_adding_missing_info : MpViewForest =
    org_to_uninterpreted_viewforest (
      with_missing_info) . unwrap() . 0;
  add_missing_info_to_viewforest(
    &mut after_adding_missing_info,
    &config . db_name,
    driver ) . await ?;
  let expected_viewforest: Tree<MpViewnode> =
    org_to_uninterpreted_nodes(
      without_missing_info ) . unwrap() . 0;
  assert_eq!(
    tree_forest_root_ids (&expected_viewforest) . len(),
    1,
    "Expected exactly one tree in the expected viewforest" );
  assert!(
    compare_viewnode_trees_modulo_id(
      &after_adding_missing_info,
      &expected_viewforest),
    "add_missing_info_to_viewforest: Forests not equivalent modulo ID." );

  { let actual_root : &MpViewnode =
      after_adding_missing_info . root() . first_child() . unwrap() . value();
    let actual_root_id : &ID =
      actual_root . id_opt() . unwrap();
    assert_eq!(
      actual_root_id . 0,
      "root-pid",
      "Root ID 'root' should have changed to 'root-pid', based on the .skg file in fixtures/."
    ); }

  Ok (( )) }

#[test]
fn test_absent_parentIs_under_visible_parent_becomes_isContainer () {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main) (parentIs absent))) root
            ** (skg (node (id moved) (source main) (parentIs absent))) moved
        "};
  let mut viewforest : MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;

  absent_parentIs_under_visible_parent_becomes_isContainer (
    &mut viewforest );

  let root_node =
    viewforest . root() . first_child() . unwrap();
  let moved_node =
    root_node . first_child() . unwrap();

  match &root_node . value() . kind {
    MpViewnodeKind::Vognode (
      MpVognode::Active (t)) =>
      assert_eq! (t . parentIs, ParentIs::Absent),
    _ => panic! ("expected root ActiveNode") }
  match &moved_node . value() . kind {
    MpViewnodeKind::Vognode (
      MpVognode::Active (t)) =>
      assert_eq! (t . parentIs, ParentIs::Affected),
    _ => panic! ("expected moved ActiveNode") }
}

async fn test_source_inheritance_multi_level (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_source_inheritance_logic ( config, driver ) . await ?;
      Ok (( )) }

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

  let mut actual_viewforest: MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;
  add_missing_info_to_viewforest(
    &mut actual_viewforest,
    &config . db_name,
    driver ) . await ?;
  let expected_viewforest: Tree<MpViewnode> =
    org_to_uninterpreted_nodes (expected) . unwrap() . 0;

  assert!(
    compare_viewnode_trees(
      actual_viewforest . root(),
      expected_viewforest . root()),
    "Source inheritance: Forests not equivalent.\n\
     Expected sources to inherit from parent, with explicit sources overriding." );

  Ok (( )) }
