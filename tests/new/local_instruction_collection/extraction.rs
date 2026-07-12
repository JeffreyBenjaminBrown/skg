// These tests pin the behavior of save extraction via local
// instruction collection, lowered to DefineNodes. Most of the
// expectations were carried over from the old extraction path; the
// ones that changed (the new recursion surface) say so in comments.

use indoc::indoc;
use ego_tree::Tree;
use skg::from_text::buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_uninterpreted_nodes,
  org_to_uninterpreted_viewforest};
use skg::from_text::local_instruction_collection::extract_nonmergeSavePlan_locally;
use skg::from_text::local_instruction_collection::lower::{
  lower_collected_intents, LoweringOutput, NodeIntent };
use skg::from_text::local_instruction_collection::traverse::collect_instructions_locally;
use skg::from_text::local_instruction_collection::types::SubscribeeVisibility;
use skg::from_text::validate::validate_and_filter_foreign_instructions;
use skg::test_utils::extract_nodecomplete_if_save_else_error;
use skg::test_utils::run_with_shared_test_db;
use skg::types::errors::BufferValidationError;
use skg::types::git::Sign;
use skg::types::misc::{ID, MSV, SkgConfig, SourceName, members_of, members_msv};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use skg::types::maybe_placed_viewnode::{
  MpViewnode,
  maybePlaced_to_placed_tree,
  maybePlaced_to_placed_viewforest};
use skg::types::tree::forest::{MpViewForest, ViewForest};
use skg::types::viewnode::{ViewNode, ViewNodeKind, viewforest_root_viewnode};
use skg::types::viewnode::{Vognode, Phantom};
use std::error::Error;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

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

fn checked_viewforest_from_org (
  input : &str,
) -> Tree<ViewNode> {
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap() }

/// This runs collection plus lowering, with no disk involved: the
/// pure half of extraction.
fn definenodes_from_tree (
  viewforest : Tree<ViewNode>,
) -> Result<Vec<DefineNode>, String> {
  let forest : ViewForest =
    ViewForest::from_internal_tree (viewforest);
  let LoweringOutput { intents, .. } =
    lower_collected_intents (
      collect_instructions_locally (&forest) ? ) ?;
  intents . into_ordered_intents()
    . into_iter()
    . map (NodeIntent::into_define_node)
    . collect() }

/// This returns the (subscriber, visibility-signal) pairs that
/// collection emits.
fn visibility_pairs_from_tree (
  viewforest : Tree<ViewNode>,
) -> Vec<(ID, SubscribeeVisibility)> {
  let forest : ViewForest =
    ViewForest::from_internal_tree (viewforest);
  let LoweringOutput { visibility, .. } =
    lower_collected_intents (
      collect_instructions_locally (&forest) . unwrap() ) . unwrap();
  visibility }

fn visibility_pairs_from_org (
  input : &str,
) -> Vec<(ID, SubscribeeVisibility)> {
  visibility_pairs_from_tree (
    checked_viewforest_from_org (input) ) }

fn set_membership_unstaged_minus (
  tree : &mut Tree<ViewNode>,
  id   : &str,
) {
  set_membership_unstaged_minus_keeping_active (tree, id);
  let target_id : ego_tree::NodeId =
    find_active_or_phantom (tree, id);
  // The target is an Active node here; the next line flips it to a phantom.
  tree . get_mut (target_id) . unwrap()
    . value()
    . normal_to_phantom (); }

/// This is like 'set_membership_unstaged_minus', but it leaves the
/// node Active: a would-be diff phantom that has not been converted,
/// which is how such nodes reach save extraction.
fn set_membership_unstaged_minus_keeping_active (
  tree : &mut Tree<ViewNode>,
  id   : &str,
) {
  let target_id : ego_tree::NodeId =
    find_active_or_phantom (tree, id);
  if let ViewNodeKind::Vognode (Vognode::Active (t)) =
    &mut tree . get_mut (target_id) . unwrap() . value() . kind
  { t . membership . unstaged = Some (Sign::Minus); }}

fn find_active_or_phantom (
  tree : &Tree<ViewNode>,
  id   : &str,
) -> ego_tree::NodeId {
  for node_ref in tree . nodes() {
    let is_target : bool =
      match &node_ref . value() . kind {
        ViewNodeKind::Vognode (Vognode::Active (t)) =>
          t . id == ID::from (id),
        ViewNodeKind::Phantom (Phantom::Diff (p)) =>
          p . id == ID::from (id),
        _ => false,
      };
    if is_target {
      return node_ref . id(); }}
  panic! ("node not found: {}", id) }

async fn save_instructions_from_org_with_disk (
  org_text : &str,
  config   : &skg::types::misc::SkgConfig,
  driver   : &typedb_driver::TypeDBDriver,
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let (mut maybePlaced_viewforest, _parsing_errors, _warnings)
    : (MpViewForest, Vec<BufferValidationError>, Vec<String>) =
    org_to_uninterpreted_viewforest (org_text) ?;
  add_missing_info_to_viewforest (
    &mut maybePlaced_viewforest, &config . db_name, driver) . await?;
  let viewforest : ViewForest =
    maybePlaced_to_placed_viewforest (maybePlaced_viewforest) ?;
  let (save_plan, _nodeMerge_acquisitions) =
    extract_nonmergeSavePlan_locally (
      &viewforest, config, driver, None) . await?;
  Ok (save_plan . define_nodes) }

#[test]
fn test_extract_nonmergeSavePlan_basic() {
  let input: &str =
    indoc! {"
            * (skg (node (id root1) (source main))) root node 1
            Root body content
            ** (skg (node (id child1) (source main))) child 1
            Child body
            * (skg (node (id root2) (source main) (editRequest delete))) root node 2
            Root 2 body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 3, "Should have 3 instructions");

  // Test root1
  let root1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root1_skg . title, "root node 1");
  assert_eq!(root1_skg . body, Some("Root body content" . to_string()));
  assert_eq!(root1_skg . pid, ID::from ("root1"));
  assert_eq!(members_of (&root1_skg . contains), vec![ID::from ("child1")]);
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
fn test_extract_nonmergeSavePlan_with_aliases() {
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

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  // Should have 2 instructions: main node and content_child
  // AliasCol and Alias nodes should not appear in output
  assert_eq!(instructions . len(), 2, "Should have 2 instructions");

  // Test main node
  let main_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(main_skg . title, "main node");
  assert_eq!(main_skg . pid, ID::from ("main"));
  assert_eq!(members_of (&main_skg . contains), vec![ID::from ("content_child")]);

  // Test aliases collection
  assert_eq!(members_msv (&main_skg . aliases), MSV::Specified(vec!["first alias" . to_string(), "second alias" . to_string()]));

  // Test content child
  let content_skg : &NodeComplete = match &instructions[1] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(content_skg . title, "content child");
  assert_eq!(content_skg . pid, ID::from ("content_child"));
  assert_eq!(content_skg . aliases, MSV::Unspecified); // No aliases
}

#[test]
fn test_extract_nonmergeSavePlan_no_aliases() {
  let input: &str =
    indoc! {"
            * (skg (node (id node1) (source main))) node without aliases
            Body content
            ** (skg (node (id child1) (source main))) regular child
            Child body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 2);

  let node1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(node1_skg . aliases, MSV::Unspecified, "Should have no aliases");
  assert_eq!(members_of (&node1_skg . contains), vec![ID::from ("child1")]);
}

#[test]
fn inactive_placeholders_emit_neither_savenode_nor_contains () {
  // Raw extraction (no source set / weave): an inactive placeholder
  // emits no save intention at all. It produces no SaveNode, and it
  // does NOT appear in its container's extracted contains -- the
  // container's membership of an invisible node is owned by the disk
  // merge (weave), exercised under a restricted source set in
  // tests/source_sets.rs. Position in the buffer is irrelevant here.
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id active-a) (source main))) active A
            ** (skg (inactiveNode (id hidden) (source private)))
            ** (skg (node (id active-b) (source main))) active B
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();

  assert_eq!(
    save_ids (&instructions),
    vec![ID::from ("root"), ID::from ("active-a"), ID::from ("active-b")],
    "inactive placeholders should not produce SaveNodes");
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "root") . contains),
    vec![ID::from ("active-a"), ID::from ("active-b")],
    "an inactive placeholder is not an extracted content member");
}

#[test]
fn test_extract_nonmergeSavePlan_multiple_alias_cols() {
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

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let result : Result<Vec<DefineNode>, String> =
    definenodes_from_tree (viewforest);

  assert!(result . is_err());
  // The two cols emit conflicting alias intents for one ID.
  assert!(result . unwrap_err() . contains ("Conflicting aliases"));
}

#[test]
fn test_extract_nonmergeSavePlan_mixed_relations() {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root node
            ** (skg (node (id unrelated1) (source main) (parentIs independent))) unrelated child
            ** (skg (node (id content1) (source main))) content child 1
            ** (skg aliasCol) aliases
            *** (skg alias) my alias
            ** (skg (node (id content2) (source main))) content child 2
            ** (skg (node (id unrelated2) (source main) (parentIs independent))) another unrelated child
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  // Should have instructions for: root, unrelated1, content1, content2, unrelated2
  // AliasCol and Alias should be skipped
  assert_eq!(instructions . len(), 5);

  let root_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root_skg . title, "root node");
  assert_eq!(members_msv (&root_skg . aliases), MSV::Specified(vec!["my alias" . to_string()]));
  assert_eq!(members_of (&root_skg . contains), vec![ID::from ("content1"), ID::from ("content2")]); // Only Content relations
}

#[test]
fn extraction_preserves_content_and_independent_children (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id ordinary) (source main))) ordinary
            ** (skg (node (id independent) (source main) (parentIs independent))) independent
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![ID::from ("root"), ID::from ("ordinary"), ID::from ("independent")]);
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "root") . contains),
    vec![ID::from ("ordinary")]); }

#[test]
fn extraction_skips_alias_and_id_display_nodes (
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

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![ID::from ("root"), ID::from ("child")]);
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "root") . aliases),
    MSV::Specified (vec!["alias text" . to_string()]));
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "root") . contains),
    vec![ID::from ("child")]); }

#[test]
fn extraction_collects_subscribees_without_hidden_branches (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg hiddenInSubscribeeCol)
            ***** (skg (node (id hidden-in) (source main))) hidden in child
            **** (skg (node (id subscribee-content) (source main))) subscribee content
            *** (skg hiddenOutsideOfSubscribeeCol)
            **** (skg (node (id hidden-outside) (source main))) hidden outside child
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    // hidden-in and hidden-outside are definitive members of
    // read-only cols, so they are self-writers on the new recursion
    // surface.
    vec![
      ID::from ("subscriber"),
      ID::from ("hidden-in"),
      ID::from ("subscribee-content"),
      ID::from ("hidden-outside")]);
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "subscriber") . subscribes_to),
    MSV::Specified (vec![ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_id (&instructions, "subscriber") . contains,
    vec![]);
  assert!(
    ! save_ids (&instructions) . contains (&ID::from ("subscribee"))); }

#[test]
fn extraction_collects_overridden_col (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id overrider) (source main))) overrider
            ** (skg overriddenCol)
            *** (skg (node (id overridden-a) (source main))) overridden A
            *** (skg (node (id overridden-b) (source main))) overridden B
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    // OverriddenCol members are self-writers: their membership is
    // read for the parent's overrides_view_of, and they also save
    // their own gnodes. This is part of the new recursion surface.
    vec![ID::from ("overrider"),
         ID::from ("overridden-a"),
         ID::from ("overridden-b")]);
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "overrider") . overrides_view_of),
    MSV::Specified (vec![
      ID::from ("overridden-a"),
      ID::from ("overridden-b")])); }

#[test]
fn empty_overridden_col_means_empty_override_set (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id overrider) (source main))) overrider
            ** (skg overriddenCol)
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    saved_node_by_id (&instructions, "overrider") . overrides_view_of,
    MSV::Specified (vec![])); }

#[test]
fn read_only_col_members_save_themselves_but_not_their_owner (
) {
  let input: &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg subscriberCol)
            *** (skg (node (id subscriber) (source main))) subscriber
            ** (skg overriderCol)
            *** (skg (node (id overrider) (source main))) overrider
            ** (skg hiderCol)
            *** (skg (node (id hider) (source main))) hider
            ** (skg hiddenCol)
            *** (skg (node (id hidden) (source main))) hidden
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    // The members are self-writers, on the new recursion surface;
    // the owner is unaffected by any of these read-only cols.
    vec![ID::from ("owner"),
         ID::from ("subscriber"),
         ID::from ("overrider"),
         ID::from ("hider"),
         ID::from ("hidden")]);
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "owner") . contains),
    Vec::<ID>::new()); }

#[test]
fn subscribee_hiderel_intent_collects_visible_content (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg (node (id a) (source main))) a
            **** (skg (node (id b) (source main))) b
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("a"), ID::from ("b")],
       })]); }

#[test]
fn subscribee_hiderel_intents_preserve_subscribee_tree_order (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id first) (source main))) first
            **** (skg (node (id first-child) (source main))) first child
            *** (skg (node (id second) (source main))) second
            **** (skg (node (id second-child) (source main))) second child
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    vec![
      (ID::from ("subscriber"),
         SubscribeeVisibility {
           subscribee : ID::from ("first"),
           visible    : vec![ID::from ("first-child")],
         }),
      (ID::from ("subscriber"),
         SubscribeeVisibility {
           subscribee : ID::from ("second"),
           visible    : vec![ID::from ("second-child")],
         })]); }

#[test]
fn subscribee_hiderel_intent_uses_only_children (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg (node (id child) (source main))) child
            ***** (skg (node (id grandchild) (source main))) grandchild
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("child")],
       })]); }

#[test]
fn subscribee_hiderel_intent_ignores_indefinitive_subscribee (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main) indef (viewRequests definitiveView))) subscribee
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    Vec::<(ID, SubscribeeVisibility)>::new()); }

#[test]
fn subscribee_hiderel_intent_ignores_indefinitive_subscriber (
) {
  // At-most-one-writer-per-ID (plan_v2 §6.1): even though the subscribee
  // here is definitive with visible content, its subscriber instance is
  // indefinitive, so no hide/unhide edits are inferred for the subscriber
  // -- those belong only to the SubscribeeCol under the definitive
  // instance of that subscriber.
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main) indef (viewRequests definitiveView))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg (node (id a) (source main))) a
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    Vec::<(ID, SubscribeeVisibility)>::new()); }

#[test]
fn subscribee_hiderel_intent_excludes_non_content_delete_and_phantom_children (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg (node (id keep) (source main))) keep
            **** (skg (node (id independent) (source main) (parentIs independent))) independent
            **** (skg (node (id delete-me) (source main) (editRequest delete))) delete me
            **** (skg (node (id phantom) (source main))) phantom
            "};
  let mut viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  set_membership_unstaged_minus (&mut viewforest, "phantom");
  let intents : Vec<(ID, SubscribeeVisibility)> =
    visibility_pairs_from_tree (viewforest);

  assert_eq!(
    intents,
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("keep")],
       })]); }

#[test]
fn subscribee_hiderel_intent_ignores_hidden_scaffold_contents (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg hiddenInSubscribeeCol)
            ***** (skg (node (id hidden-in) (source main))) hidden in child
            **** (skg (node (id visible) (source main))) visible
            *** (skg hiddenOutsideOfSubscribeeCol)
            **** (skg (node (id hidden-outside) (source main))) hidden outside child
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("visible")],
       })]); }

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
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            * (skg (node (id doomed) (source main) (editRequest delete))) doomed
            Doomed body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("child"),
      ID::from ("doomed")]);
  let root : &NodeComplete =
    saved_node_by_id (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(members_msv (&root . aliases), MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(members_of (&root . contains), vec![ID::from ("child")]);
  assert_eq!(members_msv (&root . subscribes_to), MSV::Specified (vec![
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
            ** (skg (node (id independent) (source main) (parentIs independent))) independent
            ** (skg (node (id content) (source main))) content
            ** (skg aliasCol) aliases
            *** (skg alias) root alias
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            * (skg (node (id doomed) (source main) (editRequest delete))) doomed
            Doomed body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("independent"),
      ID::from ("content"),
      ID::from ("doomed")]);
  let root : &NodeComplete =
    saved_node_by_id (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(members_of (&root . contains), vec![ID::from ("content")]);
  assert_eq!(members_msv (&root . aliases), MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(members_msv (&root . subscribes_to), MSV::Specified (vec![
    ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_id (&instructions, "independent") . contains,
    vec![]);
  assert!(matches!(
    instructions . last(),
    Some (DefineNode::Delete (DeleteNode { id, .. }))
      if id == &ID::from ("doomed"))); }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-new-lic-extraction",
    |s| Box::pin ( async move {
      s . reset_from_config (
        "subscribee_as_such_child_list_removal_does_not_save_subscribee",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      subscribee_as_such_child_list_removal_does_not_save_subscribee (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_child_removal_is_not_foreign_contains_edit",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      subscribee_as_such_child_removal_is_not_foreign_contains_edit (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_child_list_removal_infers_subscriber_hide",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      subscribee_as_such_child_list_removal_infers_subscriber_hide (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "moving_subscribee_as_such_child_to_subscriber_does_not_hide",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      moving_subscribee_as_such_child_to_subscriber_does_not_hide (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_visible_child_removes_subscriber_hide",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without/skgconfig.toml") . await ?;
      subscribee_as_such_visible_child_removes_subscriber_hide (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_unhide_preserves_unrelated_hides",
        "tests/hidden_from_subscriptions/fixtures-every-kind-of-col/skgconfig.toml") . await ?;
      subscribee_as_such_unhide_preserves_unrelated_hides (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "overlapping_subscribee_hiderel_conflict_rejects_save",
        "tests/hidden_from_subscriptions/fixtures-overlapping-subscribees/skgconfig.toml") . await ?;
      overlapping_subscribee_hiderel_conflict_rejects_save (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "ordinary_owned_child_list_edit_still_changes_contains",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      ordinary_owned_child_list_edit_still_changes_contains (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "ordinary_same_id_occurrence_keeps_contains_edit_when_also_as_subscribee",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      ordinary_same_id_occurrence_keeps_contains_edit_when_also_as_subscribee (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "recursive_descendant_under_as_subscribee_keeps_own_contains_edit",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      recursive_descendant_under_as_subscribee_keeps_own_contains_edit (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "foreign_subscribee_as_such_title_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      foreign_subscribee_as_such_title_edit_is_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "owned_as_subscribee_title_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      owned_as_subscribee_title_edit_is_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config (
        "owned_as_subscribee_body_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) . await ?;
      owned_as_subscribee_body_edit_is_rejected (
        &s . config, &s . driver ) . await ?;
      Ok (( )) } )) }

async fn subscribee_as_such_child_list_removal_does_not_save_subscribee (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert!(
        ! save_ids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn subscribee_as_such_child_removal_is_not_foreign_contains_edit (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      // Editing within a subscribee-as-such is not a foreign-contains
      // edit of e: e is neither rejected as a ModifiedForeignNode, nor
      // written, nor forked. (e2, the foreign content shown under it, may
      // fork -- now resolving to the default owned source rather than
      // erroring, which is why this no longer returns Err.)
      // Supply a default clone source (as the production caller does),
      // so the foreign grandchild e2 forks cleanly and the call returns
      // Ok -- this test is about e, not e2.
      let clone_source_inputs : skg::from_text::fork::CloneSourceInputs =
        skg::from_text::fork::CloneSourceInputs {
          user_set          : std::collections::HashMap::new(),
          explicit_child    : std::collections::HashMap::new(),
          inferred_ancestor : std::collections::HashMap::new(),
          default           : Some (SourceName::from ("owned")), };
      let ( define_nodes, fork_specs ) =
        validate_and_filter_foreign_instructions (
          instructions, &[], &clone_source_inputs,
          &std::collections::HashMap::new(),
          config, driver) . await
        . expect ("the subscribee-as-such edit must not error");
      assert!(
        ! save_ids (&define_nodes) . contains (&ID::from ("e")),
        "e must not be written as a foreign node: {:?}", define_nodes);
      assert!(
        ! fork_specs . iter() . any (|spec| spec . original_id == ID::from ("e")),
        "e must not be forked: {:?}", fork_specs);
      Ok (()) }

async fn subscribee_as_such_child_list_removal_infers_subscriber_hide (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        members_msv (&saved_node_by_id (&instructions, "r")
          . hides_from_its_subscriptions),
        MSV::Specified (vec![ID::from ("e1")]));
      assert!(
        ! save_ids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn moving_subscribee_as_such_child_to_subscriber_does_not_hide (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                ** (skg (node (id e1) (source foreign) indef)) e1
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&instructions, "r")
          . hides_from_its_subscriptions,
        MSV::Unspecified);
      assert_eq!(
        members_of (&saved_node_by_id (&instructions, "r") . contains),
        vec![ID::from ("e1")]);
      Ok (()) }

async fn subscribee_as_such_visible_child_removes_subscriber_hide (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id R) (source main))) R
                ** (skg subscribeeCol)
                *** (skg (node (id E1) (source main))) subscribee-1
                **** (skg (node (id E11) (source main))) E11
                **** (skg (node (id H) (source main))) H
                **** (skg (node (id E12) (source main))) E12
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&instructions, "R")
          . hides_from_its_subscriptions,
        MSV::Specified (vec![]));
      assert!(
        ! save_ids (&instructions) . contains (&ID::from ("E1")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn subscribee_as_such_unhide_preserves_unrelated_hides (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id R) (source main))) R
                ** (skg subscribeeCol)
                *** (skg (node (id E1) (source main))) subscribee-1
                **** (skg (node (id hidden-in-E1) (source main))) hidden-in-E1
                **** (skg (node (id E11) (source main))) E11
                *** (skg (node (id E2) (source main))) subscribee-2
                **** (skg (node (id E21) (source main))) E21
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        members_msv (&saved_node_by_id (&instructions, "R")
          . hides_from_its_subscriptions),
        MSV::Specified (
          vec![ID::from ("hidden-in-E2"),
               ID::from ("hidden-for-no-reason")]));
      Ok (()) }

async fn overlapping_subscribee_hiderel_conflict_rejects_save (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id R) (source main))) R
                ** (skg subscribeeCol)
                *** (skg (node (id E1) (source main))) E1
                **** (skg (node (id shared) (source main))) shared
                *** (skg (node (id E2) (source main))) E2
                "};
      let result : Result<Vec<DefineNode>, Box<dyn Error>> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await;
      let error : Box<dyn Error> =
        result . unwrap_err();
      let buffer_error : &BufferValidationError =
        error . downcast_ref::<BufferValidationError>()
        . expect ("expected BufferValidationError::Other");
      assert!(
        matches!(
          buffer_error,
          BufferValidationError::Other (msg)
            if msg . contains ("Conflicting subscribee visibility edits")),
        "expected overlapping visibility conflict, got {:?}",
        buffer_error);
      Ok (()) }

async fn ordinary_owned_child_list_edit_still_changes_contains (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg (node (id r1) (source owned))) r1
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        members_of (&saved_node_by_id (&instructions, "r") . contains),
        vec![ID::from ("r1")]);
      Ok (()) }

async fn ordinary_same_id_occurrence_keeps_contains_edit_when_also_as_subscribee (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                * (skg (node (id e) (source foreign))) subscribee-e
                ** (skg (node (id e1) (source foreign))) e1
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert_eq!(
        members_of (&saved_node_by_id (&instructions, "e") . contains),
        vec![ID::from ("e1")]);
      assert_eq!(
        members_msv (&saved_node_by_id (&instructions, "r")
          . hides_from_its_subscriptions),
        MSV::Specified (vec![ID::from ("e1")]));
      Ok (()) }

#[test]
fn idcol_resident_activeNode_saves_itself_but_is_not_content (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg idCol) ids
            *** (skg (node (id display-child) (source main))) display child
            ** (skg (node (id real-child) (source main))) real child
            "};

  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_ids (&instructions),
    // display-child is a self-writer inside the IDCol, on the new
    // recursion surface; the IDCol's membership is never read.
    vec![ID::from ("root"),
         ID::from ("display-child"),
         ID::from ("real-child")]);
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "root") . contains),
    vec![ID::from ("real-child")]); }

async fn recursive_descendant_under_as_subscribee_keeps_own_contains_edit (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) subscribee-e
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let instructions : Vec<DefineNode> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await?;
      assert!(
        ! save_ids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      assert_eq!(
        members_of (&saved_node_by_id (&instructions, "e2") . contains),
        Vec::<ID>::new());
      Ok (()) }

async fn foreign_subscribee_as_such_title_edit_is_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id r) (source owned))) r
                ** (skg subscribeeCol)
                *** (skg (node (id e) (source foreign))) changed title
                **** (skg (node (id e2) (source foreign))) e2
                "};
      let result : Result<Vec<DefineNode>, Box<dyn Error>> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await;
      let error : Box<dyn Error> =
        result . unwrap_err();
      let buffer_error : &BufferValidationError =
        error . downcast_ref::<BufferValidationError>()
        . expect ("expected BufferValidationError::Other");
      assert!(
        matches!(
          buffer_error,
          BufferValidationError::Other (msg)
            if msg . contains ("Cannot edit title/body")
               && msg . contains ("subscribee-as-such")
               && msg . contains ("e")),
        "expected foreign subscribee-as-such title edit rejection, got {:?}",
        buffer_error);
      Ok (()) }

async fn owned_as_subscribee_title_edit_is_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id a) (source owned))) a
                ** (skg subscribeeCol)
                *** (skg (node (id r) (source owned))) changed title
                **** (skg (node (id r1) (source owned))) r1
                "};
      let result : Result<Vec<DefineNode>, Box<dyn Error>> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await;
      let error : Box<dyn Error> =
        result . unwrap_err();
      let buffer_error : &BufferValidationError =
        error . downcast_ref::<BufferValidationError>()
        . expect ("expected BufferValidationError::Other");
      assert!(
        matches!(
          buffer_error,
          BufferValidationError::Other (msg)
            if msg . contains ("Cannot edit title/body")
               && msg . contains ("subscribee-as-such")
               && msg . contains ("r")),
        "expected owned subscribee-as-such title edit rejection, got {:?}",
        buffer_error);
      Ok (()) }

async fn owned_as_subscribee_body_edit_is_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg (node (id a) (source owned))) a
                ** (skg subscribeeCol)
                *** (skg (node (id r) (source owned))) r
                body text that should not be accepted here
                **** (skg (node (id r1) (source owned))) r1
                "};
      let result : Result<Vec<DefineNode>, Box<dyn Error>> =
        save_instructions_from_org_with_disk (
          input, config, driver) . await;
      let error : Box<dyn Error> =
        result . unwrap_err();
      let buffer_error : &BufferValidationError =
        error . downcast_ref::<BufferValidationError>()
        . expect ("expected BufferValidationError::Other");
      assert!(
        matches!(
          buffer_error,
          BufferValidationError::Other (msg)
            if msg . contains ("Cannot edit title/body")
               && msg . contains ("subscribee-as-such")
               && msg . contains ("r")),
        "expected owned subscribee-as-such body edit rejection, got {:?}",
        buffer_error);
      Ok (()) }

#[test]
fn test_extract_nonmergeSavePlan_deep_nesting() {
  let input: &str =
    indoc! {"
            * (skg (node (id level1) (source main))) level 1
            ** (skg (node (id level2a) (source main))) level 2a
            *** (skg (node (id level3a) (source main))) level 3a
            **** (skg (node (id level4) (source main))) level 4
            ** (skg (node (id level2b) (source main))) level 2b
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 5);

  // Check contains relationships
  let level1_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[0]);
  assert_eq!(members_of (&level1_skg . contains), vec![ID::from ("level2a"), ID::from ("level2b")]);

  let level2a_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[1]);
  assert_eq!(members_of (&level2a_skg . contains), vec![ID::from ("level3a")]);

  let level3a_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[2]);
  assert_eq!(members_of (&level3a_skg . contains), vec![ID::from ("level4")]);

  let level4_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[3]);
  assert_eq!(level4_skg . contains, vec![]); // Leaf node

  let level2b_skg : &NodeComplete =
    extract_nodecomplete_if_save_else_error(&instructions[4]);
  assert_eq!(level2b_skg . contains, vec![]); // Leaf node
}

#[test]
fn test_extract_nonmergeSavePlan_error_missing_id() {
  let input: &str =
    indoc! {"
            * (skg (node (id good_node) (source main))) good node
            * node without ID
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let result : Result<Tree<ViewNode>, String> =
    // This conversion fails because of missing ID
    maybePlaced_to_placed_tree (maybePlaced_viewforest);

  assert!(result . is_err(), "Should return error for missing ID");
  let error_msg : String = result . unwrap_err();
  assert!(error_msg . contains ("node without ID") ||
          error_msg . contains ("has no ID"));
}

#[test]
fn test_extract_nonmergeSavePlan_empty_input() {
  let viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 0, "Empty input should produce empty output");
}

#[test]
fn test_extract_nonmergeSavePlan_only_aliases() {
  let input: &str =
    indoc! {"
            * (skg (node (id main) (source main))) main node
            ** (skg aliasCol) aliases only
            *** (skg alias) alias one
            *** (skg alias) alias two
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 1); // Only main node

  let main_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(members_msv (&main_skg . aliases), MSV::Specified(vec!["alias one" . to_string(), "alias two" . to_string()]));
  assert_eq!(main_skg . contains, vec![]); // No content children
}

#[test]
fn test_extract_nonmergeSavePlan_complex_scenario() {
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
            ** (skg (node (id ref_section) (source main) (parentIs independent))) Reference Section
        "};
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<ViewNode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 7); // doc1, section1, subsection1a, section2, section3, doc2, ref_section

  // Test doc1
  let doc1_skg : &NodeComplete = match &instructions[0] {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(doc1_skg . title, "Document 1");
  assert_eq!(members_msv (&doc1_skg . aliases),
             MSV::Specified(vec!["First Document" . to_string(),
                       "Primary Doc" . to_string()]));
  assert_eq!(members_of (&doc1_skg . contains),
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
  assert_eq!(members_of (&section1_skg . contains), vec![ID::from ("subsection1a")]);
}

// The next several tests pin the membership-predicate wiring of
// extraction, with one test per condition not already covered above
// (TODO/local-instruction-collection/3_plan.org, predicate test
// audit).

#[test]
fn would_be_diff_phantom_child_is_excluded_from_contains (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id a) (source main))) a
            ** (skg (node (id b) (source main))) b
        "};
  let mut viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  set_membership_unstaged_minus_keeping_active (&mut viewforest, "b");
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_of (&saved_node_by_id (&instructions, "root") . contains),
    vec![ID::from ("a")]); }

#[test]
fn toDelete_member_is_excluded_from_subscribees (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id keep) (source main))) keep
            *** (skg (node (id doomed) (source main) (editRequest delete))) doomed
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "subscriber") . subscribes_to),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn would_be_diff_phantom_member_is_excluded_from_subscribees (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id keep) (source main))) keep
            *** (skg (node (id ghost) (source main))) ghost
        "};
  let mut viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  set_membership_unstaged_minus_keeping_active (&mut viewforest, "ghost");
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "subscriber") . subscribes_to),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn toDelete_member_is_excluded_from_overriddens (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id overrider) (source main))) overrider
            ** (skg overriddenCol)
            *** (skg (node (id keep) (source main))) keep
            *** (skg (node (id doomed) (source main) (editRequest delete))) doomed
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "overrider") . overrides_view_of),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn independent_member_is_excluded_from_overriddens (
) {
  let input : &str =
    indoc! {"
            * (skg (node (id overrider) (source main))) overrider
            ** (skg overriddenCol)
            *** (skg (node (id keep) (source main))) keep
            *** (skg (node (id bystander) (source main) (parentIs independent))) bystander
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "overrider") . overrides_view_of),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn would_be_diff_phantom_child_still_counts_as_visible_content (
) {
  // This pins an asymmetry: the visible-content predicate has no
  // diff-phantom condition, unlike the contains and PartnerCol
  // membership predicates. See
  // 'active_child_counts_as_visible_content'.
  let input : &str =
    indoc! {"
            * (skg (node (id subscriber) (source main))) subscriber
            ** (skg subscribeeCol)
            *** (skg (node (id subscribee) (source main))) subscribee
            **** (skg (node (id still-visible) (source main))) still visible
        "};
  let mut viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  set_membership_unstaged_minus_keeping_active (
    &mut viewforest, "still-visible");
  let intents : Vec<(ID, SubscribeeVisibility)> =
    visibility_pairs_from_tree (viewforest);
  assert_eq!(
    intents,
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("still-visible")],
       })]); }

#[test]
fn duplicate_members_of_defining_cols_are_silently_deduplicated (
) {
  // Defining cols never squawk about repeats: emission
  // deduplicates, preserving first-occurrence order
  // (TODO/local-instruction-collection/3_plan.org).
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg aliasCol) aliases
            *** (skg alias) echo
            *** (skg alias) other
            *** (skg alias) echo
            ** (skg subscribeeCol)
            *** (skg (node (id s1) (source main) indef)) s1
            *** (skg (node (id s2) (source main) indef)) s2
            *** (skg (node (id s1) (source main) indef)) s1
            ** (skg overriddenCol)
            *** (skg (node (id o1) (source main) indef)) o1
            *** (skg (node (id o2) (source main) indef)) o2
            *** (skg (node (id o1) (source main) indef)) o1
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  let owner : &NodeComplete =
    saved_node_by_id (&instructions, "owner");
  assert_eq!(
    members_msv (&owner . aliases),
    MSV::Specified (vec![
      "echo" . to_string(), "other" . to_string()]));
  assert_eq!(
    members_msv (&owner . subscribes_to),
    MSV::Specified (vec![ID::from ("s1"), ID::from ("s2")]));
  assert_eq!(
    members_msv (&owner . overrides_view_of),
    MSV::Specified (vec![ID::from ("o1"), ID::from ("o2")])); }

// The next four tests pin the writable-col membership semantics that
// the relationship matrix (stage 13) asks for at the cheapest seam:
// subscribeeCol order and one-member removal persist to
// subscribes_to; overriddenCol order does not matter (the
// set-difference merge depends on this); and the read-only hiddenCol
// never writes the owner's hides.

#[test]
fn reordering_subscribees_reorders_subscribes_to (
) {
  // A subscribeeCol is writable: its member order is the owner's
  // subscribes_to order. Members [c, a, b] (a reorder of [a, b, c])
  // emit subscribes_to = [c, a, b].
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg subscribeeCol)
            *** (skg (node (id c) (source main))) c
            *** (skg (node (id a) (source main))) a
            *** (skg (node (id b) (source main))) b
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "owner") . subscribes_to),
    MSV::Specified (vec![
      ID::from ("c"), ID::from ("a"), ID::from ("b")])); }

#[test]
fn removing_one_subscribee_keeps_the_rest (
) {
  // Deleting one member of a writable col (b, from [a, b, c]) leaves
  // the rest: subscribes_to = [a, c], neither empty nor unspecified.
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg subscribeeCol)
            *** (skg (node (id a) (source main))) a
            *** (skg (node (id c) (source main))) c
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_id (&instructions, "owner") . subscribes_to),
    MSV::Specified (vec![ID::from ("a"), ID::from ("c")])); }

#[test]
fn reordering_overridden_col_is_harmless (
) {
  // overrides_view_of is order-free (the set-difference merge relies
  // on this). overriddenCol members [b, a] (a reorder of [a, b]) emit
  // the same set, and nothing else about the owner changes.
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg overriddenCol)
            *** (skg (node (id b) (source main))) b
            *** (skg (node (id a) (source main))) a
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  let owner : &NodeComplete =
    saved_node_by_id (&instructions, "owner");
  let overridden : Vec<ID> = match &owner . overrides_view_of {
    MSV::Specified (ids) => members_of (ids),
    other => panic! (
      "expected Specified override set, got {:?}", other), };
  assert_eq!(overridden . len(), 2);
  assert!(overridden . contains (&ID::from ("a")));
  assert!(overridden . contains (&ID::from ("b")));
  assert_eq!(members_of (&owner . contains), Vec::<ID>::new());
  assert_eq!(owner . subscribes_to, MSV::Unspecified);
  assert_eq!(owner . hides_from_its_subscriptions, MSV::Unspecified); }

#[test]
fn deleting_from_hiddenCol_emits_no_hide_change (
) {
  // hiddenCol is read-only: its membership is never collected into
  // the owner's hides_from_its_subscriptions. A member shown there
  // (and, equally, a member deleted from there) emits no hide intent;
  // the owner's hides stay Unspecified (no opinion), preserving
  // whatever disk holds. The filter cols have this guarantee tested;
  // the plain hiddenCol did not.
  let input : &str =
    indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg hiddenCol)
            *** (skg (node (id hidden) (source main))) hidden
        "};
  let viewforest : Tree<ViewNode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<DefineNode> =
    definenodes_from_tree (viewforest) . unwrap ();
  assert_eq!(
    saved_node_by_id (&instructions, "owner")
      . hides_from_its_subscriptions,
    MSV::Unspecified); }
