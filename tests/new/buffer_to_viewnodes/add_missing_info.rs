// cargo test test_add_missing_info_comprehensive

use skg::dbs::in_rust_graph::InRustGraph;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_viewnode_tree,
  org_to_viewforest};
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::types::errors::BufferValidationError;
use skg::test_utils::{run_with_shared_test_stores, compare_viewnode_trees_modulo_gnid, compare_viewnode_trees, graph_from_config};
use skg::types::viewnode::{
  PropertyMember, Viewnode, ViewnodeKind, Vognode};
use skg::from_text::buffer_to_viewnodes::add_missing_info::EnrichmentProvenance;
use skg::types::misc::{SkgConfig, ID, SkgrepoName, TantivyIndex};
use skg::types::tree::forest::{
  ViewForest,
  tree_forest_root_gnids};

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
      s . reset ("test_child_of_writeProtected_occurrence_inherits_its_gnid_skgrepo",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_child_of_writeProtected_occurrence_inherits_its_gnid_skgrepo (
        &s . config ) . await ?;
      s . reset ("test_new_headline_inherits_through_phantoms_and_folders",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_new_headline_inherits_through_phantoms_and_folders (
        &s . config ) . await ?;
      s . reset ("test_new_headline_at_top_level_is_refused",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_new_headline_at_top_level_is_refused (
        &s . config ) . await ?;
      s . reset ("test_provenance_records_what_the_parse_invented",
                 "tests/new/buffer_to_viewnodes/add_missing_info/fixtures") ?;
      test_provenance_records_what_the_parse_invented (
        &s . config ) . await ?;
      Ok (( )) } )) }

/// The provenance keeps apart what the parse invented: a new headline
/// that inherits its skgrepo is in both sets (a rider on its
/// ancestor's fork), a new headline naming its skgrepo only in
/// 'new_nodes' (the explicitly placed new node), an existing node
/// without a skgrepo only in 'inherited_skgrepo_nodes'. A
/// write-protected reference the graph places, and an alias, are in
/// neither. (Fixture node 'root-pid' lives in 'main'.)
async fn test_provenance_records_what_the_parse_invented (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let input : &str = indoc! {"
    * (skg node (gnid recorder) (skgRepos (nodeRepo alt))) recorder
    ** inheriting new
    ** (skg node (skgRepos (nodeRepo main))) explicit new
    ** (skg node (gnid existing)) existing without a repo
    ** (skg node (gnid root) writeProtected) placed by the graph
    ** (skg aliasFolder)
    *** typed alias
  "};
  let (viewforest, errors, _warnings, provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>,
       EnrichmentProvenance)
    = org_to_viewforest (input, &graph_from_config (config) ?) ?;
  assert! (errors . is_empty (), "{:?}", errors);
  let gnid_of = |title : &str| -> ID {
    viewforest . nodes ()
    . find_map ( |node_ref| match &node_ref . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . title == title => Some (t . gnid . clone ()),
        _ => None } )
    . expect (title) };
  let inheriting : ID = gnid_of ("inheriting new");
  let explicit   : ID = gnid_of ("explicit new");
  let existing   : ID = gnid_of ("existing without a repo");
  assert_eq! ( provenance . new_nodes,
               [ inheriting . clone (), explicit ] . into_iter () . collect () );
  assert_eq! ( provenance . inherited_skgrepo_nodes,
               [ inheriting, existing ] . into_iter () . collect () );
  assert_eq! ( gnid_of ("placed by the graph"), ID::from ("root-pid") );
  assert! ( viewforest . nodes () . any ( |node_ref| matches! (
              &node_ref . value () . kind,
              ViewnodeKind::PropertyMember (PropertyMember::Alias { text, .. })
                if text == "typed alias" )),
            "an occurrence typed under an alias folder is an alias" );
  Ok (( )) }

/// The 'new note' headline of 'viewforest': its skgrepo and its
/// membership.
fn new_note_skgrepo_and_membership (
  viewforest : &ViewForest,
) -> (SkgrepoName, bool) {
  viewforest . nodes ()
    . find_map ( |node_ref| match &node_ref . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          if t . title == "new note"
          => Some (( t . home_skgrepo . clone (), t . claimsMembership )),
        _ => None } )
    . expect ("the new note should be in the forest") }

/// A headline without '(skgRepos (nodeRepo NAME))' inherits the skgrepo of its
/// nearest ancestor that is an unrestricted vognode, through a
/// historical phantom or a partner folder. A non-member note in a
/// folder stays a non-member. Below a write-protected occurrence the
/// skgrepo is the one its ID will have after the save, as for a
/// child. (Fixture node 'root-pid' lives in 'main'; 'alt' is a skgrepo
/// no fixture node has, so only inheritance can supply it.)
async fn test_new_headline_inherits_through_phantoms_and_folders (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  for (input, expected_skgrepo, expected_membership) in [
    ( // Under a historical phantom under an editable node.
      indoc! {"
        * (skg node (gnid editable) (skgRepos (nodeRepo alt))) editable
        ** (skg ghost (gnid gone) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) gone
        *** new note
      "}, "alt", true ),
    ( // In a partner folder of an editable node, as a non-member.
      indoc! {"
        * (skg node (gnid editable) (skgRepos (nodeRepo alt))) editable
        ** (skg containerFolder)
        *** (skg node noClaim) new note
      "}, "alt", false ),
    ( // In a partner folder under a write-protected occurrence whose
      // shown skgrepo is stale: the graph says 'main'.
      indoc! {"
        * (skg node (gnid other) (skgRepos (nodeRepo alt))) other
        ** (skg node (gnid root) (skgRepos (nodeRepo stale)) writeProtected) root again
        *** (skg containerFolder)
        **** (skg node noClaim) new note
      "}, "main", false ), ] {
    let viewforest : ViewForest =
      org_to_viewforest (input, &graph_from_config (config) ?) . unwrap() . 0;
    assert_eq! ( new_note_skgrepo_and_membership (&viewforest),
                 ( SkgrepoName::from (expected_skgrepo),
                   expected_membership ),
                 "new note, in:\n{}", input ); }
  Ok (( )) }

/// The buffer root supplies no skgrepo: a headline without '(skgRepos
/// (nodeRepo NAME))' at top level gets 'SkgrepoName::not_found', and
/// validation refuses it.
async fn test_new_headline_at_top_level_is_refused (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let viewforest : ViewForest =
    org_to_viewforest ("* new note\n", &graph_from_config (config) ?) . unwrap() . 0;
  assert_eq! ( new_note_skgrepo_and_membership (&viewforest) . 0,
               SkgrepoName::not_found () );
  let errors : Vec<BufferValidationError> =
    find_buffer_errors_for_saving (&viewforest, config) ?;
  assert! ( errors . iter () . any ( |error| matches! (
              error, BufferValidationError::LocalStructureViolation (message, _, _)
              if message == "UnrestrictedVognode must have a repo that exists in the config." )),
            "{:?}", errors );
  Ok (( )) }

/// A new child of a write-protected occurrence inherits the skgrepo its
/// parent's ID will have after the save, not the (stale) one that
/// occurrence shows: its editable occurrence's, if the buffer has one,
/// else the graph's. (Fixture node 'root-pid' lives in 'main'.)
async fn test_child_of_writeProtected_occurrence_inherits_its_gnid_skgrepo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  for (input, expected_skgrepo) in [
    ( // The editable occurrence moves root-pid to 'moved'.
      indoc! {"
        * (skg node (gnid root) (skgRepos (nodeRepo moved))) root
        * (skg node (gnid other) (skgRepos (nodeRepo main))) other
        ** (skg node (gnid root) (skgRepos (nodeRepo main)) writeProtected) root again
        *** new child
      "}, "moved" ),
    ( // No editable occurrence: the graph says 'main'.
      indoc! {"
        * (skg node (gnid other) (skgRepos (nodeRepo main))) other
        ** (skg node (gnid root) (skgRepos (nodeRepo stale)) writeProtected) root again
        *** new child
      "}, "main" ), ] {
    let viewforest : ViewForest =
      org_to_viewforest (input, &graph_from_config (config) ?) . unwrap() . 0;
    let new_child_skgrepo : SkgrepoName =
      viewforest . nodes ()
      . find_map ( |node_ref| match &node_ref . value () . kind {
          ViewnodeKind::Vognode (Vognode::Unrestricted (t))
            if t . title == "new child"
            => Some (t . home_skgrepo . clone ()),
          _ => None } )
      . expect ("the new child should be in the forest");
    assert_eq! ( new_child_skgrepo,
                 SkgrepoName::from (expected_skgrepo),
                 "new child of a write-protected occurrence, in:\n{}",
                 input ); }
  Ok (( )) }

/// Regression: a subscribee
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
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg subscribeeFolder)
            *** (skg node (gnid root) writeProtected) subscribee reference
        "};
  let viewforest : ViewForest =
    org_to_viewforest (input, &graph_from_config (config) ?) . unwrap() . 0;
  let recorder = viewforest . root() . first_child() . unwrap();
  let folder   = recorder . first_child() . unwrap();
  let member = folder . first_child() . unwrap();
  match &member . value() . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => {
      assert_eq! (
        t . home_skgrepo, SkgrepoName::from ("main"),
        "Repoless folder member should inherit its repo from the \
         graph (node root-pid lives in repo 'main'), not stay \
         repoless." );
      assert_eq! (
        t . gnid . 0, "root-pid",
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
  // Parsing with the missing info supplied should make
  // 'with_missing_info' equivalent to 'without_missing_info',
  // modulo the specific ID values added.
  // Also tests skgrepo inheritance from parent to children.
  let with_missing_info: &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg aliasFolder) aliases
            *** new alias
            *** (skg alias) preexisting alias
            ** no id
            *** also no id
        "};
  let without_missing_info: &str =
    indoc! {"
            * (skg node (gnid root-pid) (skgRepos (nodeRepo main))) root
            ** (skg aliasFolder) aliases
            *** (skg alias) new alias
            *** (skg alias) preexisting alias
            ** (skg node (gnid unpredictable) (skgRepos (nodeRepo main))) no id
            *** (skg node (gnid unpredictable) (skgRepos (nodeRepo main))) also no id
        "};
  let after_adding_missing_info : ViewForest =
    org_to_viewforest (with_missing_info, &graph_from_config (config) ?) . unwrap() . 0;
  let expected_viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (without_missing_info, &InRustGraph::new ()) . unwrap() . 0;
  assert_eq!(
    tree_forest_root_gnids (&expected_viewforest) . len(),
    1,
    "Expected exactly one tree in the expected viewforest" );
  assert!(
    compare_viewnode_trees_modulo_gnid(
      &after_adding_missing_info,
      &expected_viewforest),
    "org_to_viewforest: Forests not equivalent modulo ID." );

  { let actual_root : &Viewnode =
      after_adding_missing_info . root() . first_child() . unwrap() . value();
    let actual_root_gnid : &ID =
      actual_root . unrestricted_gnid () . unwrap ();
    assert_eq!(
      actual_root_gnid . 0,
      "root-pid",
      "Root ID 'root' should have changed to 'root-pid', based on the .skg file in fixtures/."
    ); }

  Ok (( )) }

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
            * (skg node (gnid 1) (skgRepos (nodeRepo main))) _
            ** (skg node (gnid 11)) _
            *** (skg node (gnid 111) (skgRepos (nodeRepo alt))) _
            **** (skg node (gnid 1111)) _
            *** (skg node (gnid 112)) _
            * (skg node (gnid 2)) _
            ** (skg node (gnid 21) (skgRepos (nodeRepo alt))) _
            ** (skg node (gnid 22)) _
        "};

  let expected: &str =
    indoc! {"
            * (skg node (gnid 1) (skgRepos (nodeRepo main))) _
            ** (skg node (gnid 11) (skgRepos (nodeRepo main))) _
            *** (skg node (gnid 111) (skgRepos (nodeRepo alt))) _
            **** (skg node (gnid 1111) (skgRepos (nodeRepo alt))) _
            *** (skg node (gnid 112) (skgRepos (nodeRepo main))) _
            * (skg node (gnid 2)) _
            ** (skg node (gnid 21) (skgRepos (nodeRepo alt))) _
            ** (skg node (gnid 22)) _
        "};

  let actual_viewforest : ViewForest =
    org_to_viewforest (input, &graph_from_config (config) ?) . unwrap() . 0;
  let expected_viewforest: Tree<Viewnode> =
    org_to_viewnode_tree (expected, &InRustGraph::new ()) . unwrap() . 0;

  assert!(
    compare_viewnode_trees(
      actual_viewforest . root(),
      expected_viewforest . root()),
    "Repo inheritance: Forests not equivalent.\n\
     Expected repos to inherit from parent, with explicit repos overriding." );

  Ok (( )) }
