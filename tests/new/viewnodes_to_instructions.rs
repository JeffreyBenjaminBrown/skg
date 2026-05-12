// todo ? These tests are AI-generated,
// and no human has verified (most of) them.
// (The library code looks bulletproof to me,
// so revising these tests feels low-priority.)

use indoc::indoc;
use ego_tree::Tree;
use skg::from_text::buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::validate::validate_and_filter_foreign_instructions;
use skg::from_text::viewnodes_to_instructions::to_naive_instructions::naive_saveinstructions_from_tree;
use skg::from_text::viewnodes_to_instructions::viewforest_to_nonmerge_save_instructions;
use skg::test_utils::extract_nodecomplete_if_save_else_error;
use skg::test_utils::run_with_test_db_from_config;
use skg::types::errors::BufferValidationError;
use skg::types::misc::{ID, MSV};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::types::unchecked_viewnode::{UncheckedViewNode, unchecked_to_checked_tree};
use skg::types::viewnode::{ViewNode, viewforest_root_viewnode};
use std::error::Error;

const SUBSCRIBEE_EDIT_CONFIG: &str =
  "tests/hidden_from_subscriptions/fixtures-subscribee-edit/skgconfig.toml";

fn save_ids (
  instructions : &[DefineNode],
) -> Vec<ID> {
  instructions . iter() . map (|instruction| match instruction {
    DefineNode::Save (SaveNode (node)) => node . pid . clone(),
    DefineNode::Delete (DeleteNode { id, .. }) => id . clone(),
  }) . collect() }

fn saved_node_by_id<'a> (
  instructions : &'a [DefineNode],
  id           : &str,
) -> &'a NodeComplete {
  for instruction in instructions {
    if let DefineNode::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (id) {
        return node; }}}
  panic! ("SaveNode not found: {}", id) }

async fn save_instructions_from_org_with_disk (
  org_text : &str,
  config   : &skg::types::misc::SkgConfig,
  driver   : &typedb_driver::TypeDBDriver,
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let (mut unchecked_viewforest, _parsing_errors) =
    org_to_uninterpreted_nodes (org_text) ?;
  add_missing_info_to_viewforest (
    &mut unchecked_viewforest, &config . db_name, driver) . await?;
  let viewforest : Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) ?;
  let (instructions, _source_moves) =
    viewforest_to_nonmerge_save_instructions (
      &viewforest, config, driver) . await?;
  Ok (instructions) }

#[test]
fn test_viewforest_to_nonmerge_save_instructions_basic() {
  let input: &str =
    indoc! {"
            * (skg (node (id root1) (source main))) root node 1
            Root body content
            ** (skg (node (id child1) (source main))) child 1
            Child body
            * (skg (node (id root2) (source main) (editRequest delete))) root node 2
            Root 2 body
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 3, "Should have 3 instructions");

  // Test root1
  let root1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root1_skg . title, "root node 1");
  assert_eq!(root1_skg . body, Some("Root body content" . to_string()));
  assert_eq!(root1_skg . pid, ID::from ("root1"));
  assert_eq!(root1_skg . contains, vec![ID::from ("child1")]);
  assert!(matches!(&instructions[0],
                   DefineNode::Save (_)));

  // Test child1
  let child1_skg : &NodeComplete = match &instructions[1] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(child1_skg . title, "child 1");
  assert_eq!(child1_skg . body, Some("Child body" . to_string()));
  assert_eq!(child1_skg . pid, ID::from ("child1"));
  assert_eq!(child1_skg . contains, vec![]); // No children
  assert!(matches!(&instructions[1],
                   DefineNode::Save (_)));

  // Test root2 with metadata flags
  assert!(matches!(&instructions[2],
                   DefineNode::Delete (_)));
  match &instructions[2] {
    DefineNode::Delete(DeleteNode { id, .. }) => {
      assert_eq!(id, &ID::from ("root2")); },
    DefineNode::Save (_) =>
      panic!("Expected Delete, got Save") }; }

#[test]
fn test_viewforest_to_nonmerge_save_instructions_with_aliases() {
  let input: &str =
    indoc! {"
            * (skg (node (id main) (source main))) main node
            Main body
            ** (skg aliasCol) aliases
            *** (skg alias) first alias
            *** (skg alias) second alias
            ** (skg (node (id content_child) (source main))) content child
            Content body
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  // Should have 2 instructions: main node and content_child
  // AliasCol and Alias nodes should not appear in output
  assert_eq!(instructions . len(), 2, "Should have 2 instructions");

  // Test main node
  let main_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(main_skg . title, "main node");
  assert_eq!(main_skg . pid, ID::from ("main"));
  assert_eq!(main_skg . contains, vec![ID::from ("content_child")]);

  // Test aliases collection
  assert_eq!(main_skg . aliases, MSV::Specified(vec!["first alias" . to_string(), "second alias" . to_string()]));

  // Test content child
  let content_skg : &NodeComplete = match &instructions[1] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(content_skg . title, "content child");
  assert_eq!(content_skg . pid, ID::from ("content_child"));
  assert_eq!(content_skg . aliases, MSV::Unspecified); // No aliases
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_no_aliases() {
  let input: &str =
    indoc! {"
            * (skg (node (id node1) (source main))) node without aliases
            Body content
            ** (skg (node (id child1) (source main))) regular child
            Child body
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 2);

  let node1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(node1_skg . aliases, MSV::Unspecified, "Should have no aliases");
  assert_eq!(node1_skg . contains, vec![ID::from ("child1")]);
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_multiple_alias_cols() {
  // Multiple AliasCols under the same node is invalid
  // (validate_tree rejects this). The function should error.
  let input: &str =
    indoc! {"
            * (skg (node (id main) (source main))) main node
            ** (skg aliasCol) first alias collection
            *** (skg alias) alias one
            *** (skg alias) alias two
            ** (skg aliasCol) second alias collection
            *** (skg alias) alias three
            ** (skg (node (id content1) (source main))) content node
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let result : Result<Vec<DefineNode>, String> =
    naive_saveinstructions_from_tree (viewforest);

  assert!(result . is_err());
  assert!(result . unwrap_err() . contains ("Expected at most one"));
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_mixed_relations() {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root node
            ** (skg (node (id unrelated1) (source main) (birth independent))) unrelated child
            ** (skg (node (id content1) (source main))) content child 1
            ** (skg aliasCol) aliases
            *** (skg alias) my alias
            ** (skg (node (id content2) (source main))) content child 2
            ** (skg (node (id unrelated2) (source main) (birth independent))) another unrelated child
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  // Should have instructions for: root, unrelated1, content1, content2, unrelated2
  // AliasCol and Alias should be skipped
  assert_eq!(instructions . len(), 5);

  let root_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root_skg . title, "root node");
  assert_eq!(root_skg . aliases, MSV::Specified(vec!["my alias" . to_string()]));
  assert_eq!(root_skg . contains, vec![ID::from ("content1"), ID::from ("content2")]); // Only Content relations
}

#[test]
fn role_aware_extraction_preserves_content_and_independent_children (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id ordinary) (source main))) ordinary
            ** (skg (node (id independent) (source main) (birth independent))) independent
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![ID::from ("root"), ID::from ("ordinary"), ID::from ("independent")]);
  assert_eq!(
    saved_node_by_id (&instructions, "root") . contains,
    vec![ID::from ("ordinary")]); }

#[test]
fn role_aware_extraction_skips_alias_and_id_display_nodes (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg aliasCol) aliases
            *** (skg alias) alias text
            ** (skg idCol) IDs
            *** (skg id) extra-id
            ** (skg (node (id child) (source main))) child
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![ID::from ("root"), ID::from ("child")]);
  assert_eq!(
    saved_node_by_id (&instructions, "root") . aliases,
    MSV::Specified (vec!["alias text" . to_string()]));
  assert_eq!(
    saved_node_by_id (&instructions, "root") . contains,
    vec![ID::from ("child")]); }

#[test]
fn role_aware_extraction_collects_subscribees_without_hidden_branches (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol) subscribees
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg hiddenInSubscribeeCol) hidden in
            ***** (skg (node (id hidden-in) (source main))) hidden in child
            **** (skg (node (id subscribee-content) (source main))) subscribee content
            *** (skg hiddenOutsideOfSubscribeeCol) hidden outside
            **** (skg (node (id hidden-outside) (source main))) hidden outside child
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![
      ID::from ("subscriber"),
      ID::from ("subscribee"),
      ID::from ("subscribee-content")]);
  assert_eq!(
    saved_node_by_id (&instructions, "subscriber") . subscribes_to,
    MSV::Specified (vec![ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_id (&instructions, "subscriber") . contains,
    vec![]);
  assert_eq!(
    saved_node_by_id (&instructions, "subscribee") . contains,
    vec![ID::from ("subscribee-content")]); }

#[test]
fn intent_layer_preserves_mixed_naive_instruction_shape (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            Root body
            ** (skg aliasCol) aliases
            *** (skg alias) root alias
            ** (skg (node (id child) (source main))) child
            Child body
            ** (skg subscribeeCol) subscribees
            *** (skg (node (id subscribee) (source main))) subscribee
            * (skg (node (id doomed) (source main) (editRequest delete))) doomed
            Doomed body
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("child"),
      ID::from ("subscribee"),
      ID::from ("doomed")]);
  let root : &NodeComplete =
    saved_node_by_id (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(root . aliases, MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(root . contains, vec![ID::from ("child")]);
  assert_eq!(root . subscribes_to, MSV::Specified (vec![
    ID::from ("subscribee")]));
  assert!(matches!(
    instructions . last(),
    Some (DefineNode::Delete (DeleteNode { id, .. }))
      if id == &ID::from ("doomed"))); }

#[test]
fn split_extraction_passes_preserve_mixed_instruction_shape (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            Root body
            ** (skg (node (id independent) (source main) (birth independent))) independent
            ** (skg (node (id content) (source main))) content
            ** (skg aliasCol) aliases
            *** (skg alias) root alias
            ** (skg subscribeeCol) subscribees
            *** (skg (node (id subscribee) (source main))) subscribee
            * (skg (node (id doomed) (source main) (editRequest delete))) doomed
            Doomed body
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("independent"),
      ID::from ("content"),
      ID::from ("subscribee"),
      ID::from ("doomed")]);
  let root : &NodeComplete =
    saved_node_by_id (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(root . contains, vec![ID::from ("content")]);
  assert_eq!(root . aliases, MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(root . subscribes_to, MSV::Specified (vec![
    ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_id (&instructions, "independent") . contains,
    vec![]);
  assert!(matches!(
    instructions . last(),
    Some (DefineNode::Delete (DeleteNode { id, .. }))
      if id == &ID::from ("doomed"))); }

#[test]
fn direct_as_subscribee_child_list_removal_keeps_disk_contains (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-direct-as-subscribee-keeps-contains",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol) subscribees
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&instructions, "e") . contains,
        vec![ID::from ("e1"), ID::from ("e2")]);
      Ok (()) })) }

#[test]
fn ordinary_owned_child_list_edit_still_changes_contains (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-ordinary-owned-keeps-contains-edit",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg (node (id r1) (source owned))) r1
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&instructions, "r") . contains,
        vec![ID::from ("r1")]);
      Ok (()) })) }

#[test]
fn recursive_descendant_under_as_subscribee_keeps_own_contains_edit (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-subscribee-descendant-keeps-contains-edit",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol) subscribees
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&instructions, "e") . contains,
        vec![ID::from ("e1"), ID::from ("e2")]);
      assert_eq!(
        saved_node_by_id (&instructions, "e2") . contains,
        Vec::<ID>::new());
      Ok (()) })) }

#[test]
fn foreign_direct_as_subscribee_title_edit_reaches_validation (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-subscribee-title-edit-validation",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol) subscribees
                *** (skg (node (id e) (source foreign))) changed title
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      let errors : Vec<BufferValidationError> =
        validate_and_filter_foreign_instructions (
          instructions, config, driver) . await . unwrap_err();
      assert!(
        errors . iter() . any (|error| matches!(
          error,
          BufferValidationError::ModifiedForeignNode (id, source)
            if id == &ID::from ("e") && source . as_str() == "foreign")),
        "expected ModifiedForeignNode for e, got {:?}",
        errors);
      Ok (()) })) }

#[test]
fn test_viewforest_to_nonmerge_save_instructions_deep_nesting() {
  let input: &str =
    indoc! {"
            * (skg (node (id level1) (source main))) level 1
            ** (skg (node (id level2a) (source main))) level 2a
            *** (skg (node (id level3a) (source main))) level 3a
            **** (skg (node (id level4) (source main))) level 4
            ** (skg (node (id level2b) (source main))) level 2b
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 5);

  // Check contains relationships
  let level1_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[0]);
  assert_eq!(level1_skg . contains, vec![ID::from ("level2a"), ID::from ("level2b")]);

  let level2a_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[1]);
  assert_eq!(level2a_skg . contains, vec![ID::from ("level3a")]);

  let level3a_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[2]);
  assert_eq!(level3a_skg . contains, vec![ID::from ("level4")]);

  let level4_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[3]);
  assert_eq!(level4_skg . contains, vec![]); // Leaf node

  let level2b_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[4]);
  assert_eq!(level2b_skg . contains, vec![]); // Leaf node
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_error_missing_id() {
  let input: &str =
    indoc! {"
            * (skg (node (id good_node) (source main))) good node
            * node without ID
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let result : Result<Tree<ViewNode>, String> =
    // This conversion fails because of missing ID
    unchecked_to_checked_tree (unchecked_viewforest);

  assert!(result . is_err(), "Should return error for missing ID");
  let error_msg : String = result . unwrap_err();
  assert!(error_msg . contains ("node without ID") ||
          error_msg . contains ("has no ID"));
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_empty_input() {
  let viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 0, "Empty input should produce empty output");
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_only_aliases() {
  let input: &str =
    indoc! {"
            * (skg (node (id main) (source main))) main node
            ** (skg aliasCol) aliases only
            *** (skg alias) alias one
            *** (skg alias) alias two
        "};

  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 1); // Only main node

  let main_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(main_skg . aliases, MSV::Specified(vec!["alias one" . to_string(), "alias two" . to_string()]));
  assert_eq!(main_skg . contains, vec![]); // No content children
}

#[test]
fn test_viewforest_to_nonmerge_save_instructions_complex_scenario() {
  let input: &str =
    indoc! {"
            * (skg (node (id doc1) (source main))) Document 1
            Document body
            ** (skg aliasCol) Doc1 Aliases
            *** (skg alias) First Document
            *** (skg alias) Primary Doc
            ** (skg (node (id section1) (source main))) Section 1
            Section 1 body
            *** (skg (node (id subsection1a) (source main))) Subsection 1a
            ** (skg (node (id section2) (source main) (editRequest delete))) Section 2
            ** (skg (node (id section3) (source main))) Section 3
            * (skg (node (id doc2) (source main))) Document 2
            ** (skg (node (id ref_section) (source main) (birth independent))) Reference Section
        "};
  let unchecked_viewforest : Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    unchecked_to_checked_tree (unchecked_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    naive_saveinstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 7); // doc1, section1, subsection1a, section2, section3, doc2, ref_section

  // Test doc1
  let doc1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(doc1_skg . title, "Document 1");
  assert_eq!(doc1_skg . aliases,
             MSV::Specified(vec!["First Document" . to_string(),
                       "Primary Doc" . to_string()]));
  assert_eq!(doc1_skg . contains,
             vec![ID::from ("section1"),
                       ID::from ("section3")]);
  assert!(matches!(&instructions[0],
                   DefineNode::Save (_)));

  // Test section2 with toDelete
  assert!(matches!(&instructions[3],
                   DefineNode::Delete (_)));
  match &instructions[3] {
    DefineNode::Delete(DeleteNode { id, .. }) => {
      assert_eq!(id, &ID::from ("section2")); },
    DefineNode::Save (_) =>
      panic!("Expected Delete, got Save") };

  // Test that subsection1a is child of section1
  let section1_skg : &NodeComplete = match &instructions[1] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(section1_skg . contains, vec![ID::from ("subsection1a")]);
}
