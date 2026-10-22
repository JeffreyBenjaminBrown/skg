// cargo test test_add_missing_info_comprehensive

use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_uninterpreted_nodes,
  org_to_uninterpreted_viewforest};
use skg::from_text::buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest,
  na_affectsParent_under_visible_parent_becomes_isContainer};
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::types::errors::BufferValidationError;
use skg::test_utils::{run_with_shared_test_stores, compare_viewnode_trees_modulo_skgid, compare_viewnode_trees};
use skg::types::maybe_placed_viewnode::{
  MpViewnode, MpViewnodeKind, MpVognode};
use skg::types::misc::{SkgConfig, ID, SkgrepoName, TantivyIndex};
use skg::types::tree::forest::{
  MpViewForest,
  tree_forest_root_skgids};
use skg::types::viewnode::AffectsParent;

use ego_tree::Tree;

use std::error::Error;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-new-add-missing-info",
    |s| Box::pin ( async move {
      s . reset ("test_add_missing_info_comprehensive",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_add_missing_info_comprehensive (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_repo_inheritance_multi_level",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_skgrepo_inheritance_multi_level (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_repoless_folder_member_gets_graph_repo",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_repoless_folder_member_gets_graph_skgrepo (
        &s . config ) . await ?;
      s . reset ("test_child_of_writeProtected_occurrence_inherits_its_skgid_skgrepo",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_child_of_writeProtected_occurrence_inherits_its_skgid_skgrepo (
        &s . config ) . await ?;
      s . reset ("test_new_headline_inherits_through_phantoms_and_folders",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_new_headline_inherits_through_phantoms_and_folders (
        &s . config ) . await ?;
      s . reset ("test_new_headline_at_top_level_is_refused",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_new_headline_at_top_level_is_refused (
        &s . config ) . await ?;
      Ok (( )) } )) }

/// The 'new note' headline of 'viewforest': its skgrepo and its
/// 'affectsParent'.
fn new_note_skgrepo_and_affectsParent (
  viewforest : &MpViewForest,
) -> (Option<SkgrepoName>, AffectsParent) {
  viewforest . nodes ()
    . find_map ( |node_ref| match &node_ref . value () . kind {
        MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
          if t . title == "new note"
          => Some (( t . home_skgrepo . clone (), t . affectsParent )),
        _ => None } )
    . expect ("the new note should be in the forest") }

/// A headline without '(repo NAME)' inherits the skgrepo of its
/// nearest ancestor that is an unrestricted vognode, through a
/// historical phantom or a partner folder. A non-member note in a
/// folder stays a non-member. Below a write-protected occurrence the
/// skgrepo is the one its ID will have after the save, as for a
/// child. (Fixture node 'root-pid' lives in 'main'; 'alt' is a skgrepo
/// no fixture node has, so only inheritance can supply it.)
async fn test_new_headline_inherits_through_phantoms_and_folders (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  for (input, expected_skgrepo, expected_affectsParent) in [
    ( // Under a historical phantom under an editable node.
      indoc! {"
        * (skg (node (id editable) (repo alt))) editable
        ** (skg (historical (id gone) (repo main) (diff (presence node- rel-)))) gone
        *** new note
      "}, "alt", AffectsParent::True ),
    ( // In a partner folder of an editable node, as a non-member.
      indoc! {"
        * (skg (node (id editable) (repo alt))) editable
        ** (skg containerFolder)
        *** (skg (node (affectsParent false))) new note
      "}, "alt", AffectsParent::False ),
    ( // In a partner folder under a write-protected occurrence whose
      // shown skgrepo is stale: the graph says 'main'.
      indoc! {"
        * (skg (node (id other) (repo alt))) other
        ** (skg (node (id root) (repo stale) writeProtected)) root again
        *** (skg containerFolder)
        **** (skg (node (affectsParent false))) new note
      "}, "main", AffectsParent::False ), ] {
    let mut viewforest : MpViewForest =
      org_to_uninterpreted_viewforest (input) . unwrap() . 0;
    add_missing_info_to_viewforest (
      &mut viewforest, &config) ?;
    assert_eq! ( new_note_skgrepo_and_affectsParent (&viewforest),
                 ( Some (SkgrepoName::from (expected_skgrepo)),
                   expected_affectsParent ),
                 "new note, in:\n{}", input ); }
  Ok (( )) }

/// The buffer root supplies no skgrepo: a headline without '(repo
/// NAME)' at top level stays repoless, and validation refuses it.
async fn test_new_headline_at_top_level_is_refused (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let mut viewforest : MpViewForest =
    org_to_uninterpreted_viewforest ("* new note\n") . unwrap() . 0;
  add_missing_info_to_viewforest (
    &mut viewforest, &config) ?;
  assert_eq! ( new_note_skgrepo_and_affectsParent (&viewforest) . 0,
               None );
  let errors : Vec<BufferValidationError> =
    find_buffer_errors_for_saving (&viewforest, config) ?;
  assert! ( errors . iter () . any ( |error| matches! (
              error, BufferValidationError::LocalStructureViolation (message, _)
              if message == "UnrestrictedVognode must have a repo that exists in the config." )),
            "{:?}", errors );
  Ok (( )) }

/// A new child of a write-protected occurrence inherits the skgrepo its
/// parent's ID will have after the save, not the (stale) one that
/// occurrence shows: its editable occurrence's, if the buffer has one,
/// else the graph's. (Fixture node 'root-pid' lives in 'main'.)
async fn test_child_of_writeProtected_occurrence_inherits_its_skgid_skgrepo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  for (input, expected_skgrepo) in [
    ( // The editable occurrence moves root-pid to 'moved'.
      indoc! {"
        * (skg (node (id root) (repo moved))) root
        * (skg (node (id other) (repo main))) other
        ** (skg (node (id root) (repo main) writeProtected)) root again
        *** new child
      "}, "moved" ),
    ( // No editable occurrence: the graph says 'main'.
      indoc! {"
        * (skg (node (id other) (repo main))) other
        ** (skg (node (id root) (repo stale) writeProtected)) root again
        *** new child
      "}, "main" ), ] {
    let mut viewforest : MpViewForest =
      org_to_uninterpreted_viewforest (input) . unwrap() . 0;
    add_missing_info_to_viewforest (
      &mut viewforest, &config) ?;
    let new_child_skgrepo : Option<SkgrepoName> =
      viewforest . nodes ()
      . find_map ( |node_ref| match &node_ref . value () . kind {
          MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))
            if t . title == "new child"
            => Some (t . home_skgrepo . clone ()),
          _ => None } )
      . expect ("the new child should be in the forest");
    assert_eq! ( new_child_skgrepo,
                 Some (SkgrepoName::from (expected_skgrepo)),
                 "new child of a write-protected occurrence, in:\n{}",
                 input ); }
  Ok (( )) }

/// Regression for TODO/DONE/BUG_reciprocal-subscribe.org: a subscribee
/// pasted from the link stack arrives as a bare id under a
/// 'subscribeeFolder' non-vognode, with no skgrepo.
/// 'inherit_skgrepo_from_nearest_unrestricted_ancestor_if_possible'
/// would give it the recorder's skgrepo; enrichment must resolve the
/// node's own from the graph by id instead.
/// Before the fix this node stayed repoless and the save was refused
/// with "UnrestrictedVognode must have a repo that exists in the config".
async fn test_repoless_folder_member_gets_graph_skgrepo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  // 'root' is an extra_id of fixture node 'root-pid', whose skgrepo is
  // 'main'. The subscribee reference below carries neither skgrepo nor
  // the primary id, and is write-protected -- exactly the bare-paste shape
  // (a link-stack paste yields a write-protected node with only an id).
  let input : &str =
    indoc! {"
            * (skg (node (id recorder) (repo main))) recorder
            ** (skg subscribeeFolder)
            *** (skg (node (id root) writeProtected)) subscribee reference
        "};
  let mut viewforest : MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;
  add_missing_info_to_viewforest (
    &mut viewforest, &config) ?;
  let recorder = viewforest . root() . first_child() . unwrap();
  let folder   = recorder . first_child() . unwrap();
  let member = folder . first_child() . unwrap();
  match &member . value() . kind {
    MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) => {
      assert_eq! (
        t . home_skgrepo, Some (SkgrepoName::from ("main")),
        "Repoless folder member should inherit its repo from the \
         graph (node root-pid lives in repo 'main'), not stay \
         repoless." );
      assert_eq! (
        t . skgid . as_ref() . unwrap() . 0, "root-pid",
        "The referenced id 'root' should have resolved to its pid \
         'root-pid'." ); }
    _ => panic! ("expected an UnrestrictedVognode subscribee member") }
  Ok (( )) }

async fn test_add_missing_info_comprehensive (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_add_missing_info_logic ( config ) . await ?;
      Ok (( )) }

async fn test_add_missing_info_logic (
  config : &SkgConfig,

) -> Result<(), Box<dyn Error>> {
  // Applying 'add_missing_info_to_viewforest' should make
  // 'with_missing_info' equivalent to 'without_missing_info',
  // modulo the specific ID values added.
  // Also tests skgrepo inheritance from parent to children.
  let with_missing_info: &str =
    indoc! {"
            * (skg (node (id root) (repo main))) root
            ** (skg aliasFolder) aliases
            *** new alias
            *** (skg alias) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg (node (id root-pid) (repo main))) root
            ** (skg aliasFolder) aliases
            *** (skg alias) new alias
            *** (skg alias) preexisting alias
            ** (skg (node (id unpredictable) (repo main))) no id
            *** (skg (node (id unpredictable) (repo main))) also no id
        "};
  let mut after_adding_missing_info : MpViewForest =
    org_to_uninterpreted_viewforest (
      with_missing_info) . unwrap() . 0;
  add_missing_info_to_viewforest(
    &mut after_adding_missing_info,
    &config) ?;
  let expected_viewforest: Tree<MpViewnode> =
    org_to_uninterpreted_nodes(
      without_missing_info ) . unwrap() . 0;
  assert_eq!(
    tree_forest_root_skgids (&expected_viewforest) . len(),
    1,
    "Expected exactly one tree in the expected viewforest" );
  assert!(
    compare_viewnode_trees_modulo_skgid(
      &after_adding_missing_info,
      &expected_viewforest),
    "add_missing_info_to_viewforest: Forests not equivalent modulo ID." );

  { let actual_root : &MpViewnode =
      after_adding_missing_info . root() . first_child() . unwrap() . value();
    let actual_root_skgid : &ID =
      actual_root . skgid_opt() . unwrap();
    assert_eq!(
      actual_root_skgid . 0,
      "root-pid",
      "Root ID 'root' should have changed to 'root-pid', based on the .skg file in fixtures/."
    ); }

  Ok (( )) }

#[test]
fn test_na_affectsParent_under_visible_parent_becomes_isContainer () {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (repo main) (affectsParent na))) root
            ** (skg (node (id moved) (repo main) (affectsParent na))) moved
        "};
  let mut viewforest : MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;

  na_affectsParent_under_visible_parent_becomes_isContainer (
    &mut viewforest );

  let root_node =
    viewforest . root() . first_child() . unwrap();
  let moved_node =
    root_node . first_child() . unwrap();

  match &root_node . value() . kind {
    MpViewnodeKind::Vognode (
      MpVognode::Unrestricted (t)) =>
      assert_eq! (t . affectsParent, AffectsParent::NA),
    _ => panic! ("expected root UnrestrictedVognode") }
  match &moved_node . value() . kind {
    MpViewnodeKind::Vognode (
      MpVognode::Unrestricted (t)) =>
      assert_eq! (t . affectsParent, AffectsParent::True),
    _ => panic! ("expected moved UnrestrictedVognode") }
}

async fn test_skgrepo_inheritance_multi_level (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_skgrepo_inheritance_logic ( config ) . await ?;
      Ok (( )) }

async fn test_skgrepo_inheritance_logic (
  config : &SkgConfig,

) -> Result<(), Box<dyn Error>> {
  // Tests skgrepo inheritance through multiple levels,
  // with explicit skgrepos overriding inheritance at various depths.
  let input: &str =
    indoc! {"
            * (skg (node (id 1) (repo main))) _
            ** (skg (node (id 11))) _
            *** (skg (node (id 111) (repo alt))) _
            **** (skg (node (id 1111))) _
            *** (skg (node (id 112))) _
            * (skg (node (id 2))) _
            ** (skg (node (id 21) (repo alt))) _
            ** (skg (node (id 22))) _
        "};

  let expected: &str =
    indoc! {"
            * (skg (node (id 1) (repo main))) _
            ** (skg (node (id 11) (repo main))) _
            *** (skg (node (id 111) (repo alt))) _
            **** (skg (node (id 1111) (repo alt))) _
            *** (skg (node (id 112) (repo main))) _
            * (skg (node (id 2))) _
            ** (skg (node (id 21) (repo alt))) _
            ** (skg (node (id 22))) _
        "};

  let mut actual_viewforest: MpViewForest =
    org_to_uninterpreted_viewforest (input) . unwrap() . 0;
  add_missing_info_to_viewforest(
    &mut actual_viewforest,
    &config) ?;
  let expected_viewforest: Tree<MpViewnode> =
    org_to_uninterpreted_nodes (expected) . unwrap() . 0;

  assert!(
    compare_viewnode_trees(
      actual_viewforest . root(),
      expected_viewforest . root()),
    "Repo inheritance: Forests not equivalent.\n\
     Expected repos to inherit from parent, with explicit repos overriding." );

  Ok (( )) }
