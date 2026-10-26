// These tests pin the behavior of save extraction via local
// instruction collection, lowered to NodeInstructions. Most of the
// expectations were carried over from the old extraction path; the
// ones that changed (the new recursion surface) say so in comments.

use indoc::indoc;
use ego_tree::Tree;
use skg::from_text::buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_uninterpreted_nodes,
  org_to_uninterpreted_viewforest};
use skg::from_text::local_fieldintent_collection::extract_nonmergeSavePlan_locally;
use skg::from_text::local_fieldintent_collection::lower::{
  lower_collected_fieldIntents, LoweringOutput, NodeIntent };
use skg::from_text::local_fieldintent_collection::traverse::collect_instructions_locally;
use skg::from_text::local_fieldintent_collection::types::SubscribeeVisibility;
use skg::from_text::validate::validate_and_filter_foreign_instructions;
use skg::test_utils::extract_graphnode_if_save_else_error;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::types::errors::BufferValidationError;
use skg::types::misc::{ID, MSV, SkgConfig, SkgrepoName, members_of, members_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, SaveNode, DeleteNode};
use skg::types::maybe_placed_viewnode::{
  MpViewnode,
  maybePlaced_to_placed_tree,
  maybePlaced_to_placed_viewforest};
use skg::types::tree::forest::{MpViewForest, ViewForest};
use skg::types::viewnode::{Viewnode, viewforest_root_viewnode};
use std::error::Error;

const SUBSCRIBEE_EDIT_CONFIG: &str =
  "tests/hidden_from_subscriptions/fixtures-subscribee-edit/skgconfig.toml";

fn save_skgids (
  instructions : &[NodeInstruction],
) -> Vec<ID> {
  instructions . iter() . map (|instruction| match instruction {
    NodeInstruction::Save (SaveNode (node)) => node . pid . clone(),
    NodeInstruction::Delete (DeleteNode { skgid, .. }) => skgid . clone(),
  }) . collect() }

fn saved_node_by_skgid<'a> (
  instructions : &'a [NodeInstruction],
  skgid           : &str,
) -> &'a Graphnode {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (skgid) {
        return node; }}}
  panic! ("SaveNode not found: {}", skgid) }

fn checked_viewforest_from_org (
  input : &str,
) -> Tree<Viewnode> {
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap() }

/// This runs collection plus lowering, with no disk involved: the
/// pure half of extraction.
fn nodeInstructions_from_tree (
  viewforest : Tree<Viewnode>,
) -> Result<Vec<NodeInstruction>, String> {
  let forest : ViewForest =
    ViewForest::from_internal_tree (viewforest);
  let LoweringOutput { intents, .. } =
    lower_collected_fieldIntents (
      collect_instructions_locally (&forest) ? ) ?;
  intents . into_ordered_intents()
    . into_iter()
    . map (NodeIntent::into_node_instruction)
    . collect() }

/// This returns the (subscriber, visibility-signal) pairs that
/// collection emits.
fn visibility_pairs_from_tree (
  viewforest : Tree<Viewnode>,
) -> Vec<(ID, SubscribeeVisibility)> {
  let forest : ViewForest =
    ViewForest::from_internal_tree (viewforest);
  let LoweringOutput { visibility, .. } =
    lower_collected_fieldIntents (
      collect_instructions_locally (&forest) . unwrap() ) . unwrap();
  visibility }

fn visibility_pairs_from_org (
  input : &str,
) -> Vec<(ID, SubscribeeVisibility)> {
  visibility_pairs_from_tree (
    checked_viewforest_from_org (input) ) }

fn hiddenoutside_edits_from_tree (
  viewforest : Tree<Viewnode>,
) -> Vec<(ID, skg::from_text::local_fieldintent_collection::types::HiddenOutsideEdit)> {
  let forest : ViewForest = ViewForest::from_internal_tree (viewforest);
  lower_collected_fieldIntents (
    collect_instructions_locally (&forest) . unwrap () ) . unwrap ()
    . hidden_outside
}

#[test]
fn unknown_members_write_their_editable_relationship_recorders_only () {
  let input = indoc! {"
    * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
    ** (skg unknown (gnid content-unknown))
    ** (skg subscribeeFolder)
    *** (skg unknown (gnid subscribee-unknown))
    *** (skg hiddenOutsideOfSubscribeeFolder)
    **** (skg unknown (gnid hiddenoutside-unknown))
    ** (skg overriddenFolder)
    *** (skg unknown (gnid overridden-unknown))
  "};
  let instructions = nodeInstructions_from_tree (checked_viewforest_from_org (input))
    . expect ("Unknown relationship members are valid save input");
  assert_eq! (save_skgids (&instructions), vec! [ID::from ("recorder")],
              "an Unknown must not create a node save, delete, merge, or fork instruction");
  let recorder = saved_node_by_skgid (&instructions, "recorder");
  assert_eq! (members_of (&recorder . contains), vec! [ID::from ("content-unknown")]);
  assert_eq! (members_msv (&recorder . subscribesTo),
              MSV::Specified (vec! [ID::from ("subscribee-unknown")]));
  assert_eq! (members_msv (&recorder . overrides),
              MSV::Specified (vec! [ID::from ("overridden-unknown")]));
  assert_eq! (hiddenoutside_edits_from_tree (checked_viewforest_from_org (input)),
              vec! [(ID::from ("recorder"),
                    skg::from_text::local_fieldintent_collection::types::HiddenOutsideEdit {
                      members: vec! [ID::from ("hiddenoutside-unknown")] })],
              "HiddenOutside passes the raw Unknown ID to its hide resolver");
}

async fn nodeInstructions_from_org_with_disk (
  org_text : &str,
  config   : &skg::types::misc::SkgConfig,

) -> Result<Vec<NodeInstruction>, Box<dyn Error>> {
  let (mut maybePlaced_viewforest, _parsing_errors, _warnings)
    : (MpViewForest, Vec<BufferValidationError>, Vec<String>) =
    org_to_uninterpreted_viewforest (org_text) ?;
  add_missing_info_to_viewforest (
    &mut maybePlaced_viewforest, &config)?;
  let viewforest : ViewForest =
    maybePlaced_to_placed_viewforest (maybePlaced_viewforest) ?;
  let (save_plan, _nodeMerge_acquisitions) =
    extract_nonmergeSavePlan_locally (
      &viewforest, config, None)?;
  Ok (save_plan . node_instructions) }

#[test]
fn test_extract_nonmergeSavePlan_basic() {
  let input: &str =
    indoc! {"
            * (skg node (gnid root1) (skgRepos (nodeRepo main))) root node 1
            Root body content
            ** (skg node (gnid child1) (skgRepos (nodeRepo main))) child 1
            Child body
            * (skg node (gnid root2) (skgRepos (nodeRepo main)) (editRequest delete)) root node 2
            Root 2 body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 3, "Should have 3 instructions");

  // Test root1
  let root1_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root1_skg . title, "root node 1");
  assert_eq!(root1_skg . body, Some("Root body content" . to_string()));
  assert_eq!(root1_skg . pid, ID::from ("root1"));
  assert_eq!(members_of (&root1_skg . contains), vec![ID::from ("child1")]);
  assert!(matches!(&instructions[0],
                   NodeInstruction::Save (_)));

  // Test child1
  let child1_skg : &Graphnode = match &instructions[1] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(child1_skg . title, "child 1");
  assert_eq!(child1_skg . body, Some("Child body" . to_string()));
  assert_eq!(child1_skg . pid, ID::from ("child1"));
  assert_eq!(child1_skg . contains, vec![]); // No children
  assert!(matches!(&instructions[1],
                   NodeInstruction::Save (_)));

  // Test root2 with metadata flags
  assert!(matches!(&instructions[2],
                   NodeInstruction::Delete (_)));
  match &instructions[2] {
    NodeInstruction::Delete(DeleteNode { skgid, .. }) => {
      assert_eq!(skgid, &ID::from ("root2")); },
    NodeInstruction::Save (_) =>
      panic!("Expected Delete, got Save") }; }

#[test]
fn test_extract_nonmergeSavePlan_with_aliases() {
  let input: &str =
    indoc! {"
            * (skg node (gnid main) (skgRepos (nodeRepo main))) main node
            Main body
            ** (skg aliasFolder) aliases
            *** (skg alias) first alias
            *** (skg alias) second alias
            ** (skg node (gnid content_child) (skgRepos (nodeRepo main))) content child
            Content body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  // Should have 2 instructions: main node and content_child
  // AliasFolder and Alias nodes should not appear in output
  assert_eq!(instructions . len(), 2, "Should have 2 instructions");

  // Test main node
  let main_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(main_skg . title, "main node");
  assert_eq!(main_skg . pid, ID::from ("main"));
  assert_eq!(members_of (&main_skg . contains), vec![ID::from ("content_child")]);

  // Test aliases folder
  assert_eq!(members_msv (&main_skg . aliases), MSV::Specified(vec!["first alias" . to_string(), "second alias" . to_string()]));

  // Test content child
  let content_skg : &Graphnode = match &instructions[1] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(content_skg . title, "content child");
  assert_eq!(content_skg . pid, ID::from ("content_child"));
  assert_eq!(content_skg . aliases, MSV::Unspecified); // No aliases
}

#[test]
fn test_extract_nonmergeSavePlan_no_aliases() {
  let input: &str =
    indoc! {"
            * (skg node (gnid node1) (skgRepos (nodeRepo main))) node without aliases
            Body content
            ** (skg node (gnid child1) (skgRepos (nodeRepo main))) regular child
            Child body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 2);

  let node1_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(node1_skg . aliases, MSV::Unspecified, "Should have no aliases");
  assert_eq!(members_of (&node1_skg . contains), vec![ID::from ("child1")]);
}

#[test]
fn tombstones_emit_neither_savenode_nor_contains () {
  // Raw extraction (no skgrepo set / weave): a tombstone, as stands
  // where a hidden node was, emits no save intention at all. It produces no SaveNode, and it
  // does not appear in its container's extracted contains -- the
  // container's membership of an invisible node is owned by the disk
  // merge (weave), exercised under a restricted skgrepo set in
  // tests/repo_sets.rs. Position in the buffer is irrelevant here.
  let input : &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg node (gnid active-a) (skgRepos (nodeRepo main))) unrestricted A
            ** (skg tombstone)
            ** (skg node (gnid active-b) (skgRepos (nodeRepo main))) unrestricted B
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();

  assert_eq!(
    save_skgids (&instructions),
    vec![ID::from ("root"), ID::from ("active-a"), ID::from ("active-b")],
    "tombstones should not produce SaveNodes");
  assert_eq!(
    members_of (&saved_node_by_skgid (&instructions, "root") . contains),
    vec![ID::from ("active-a"), ID::from ("active-b")],
    "a tombstone is not an extracted content member");
}

#[test]
fn test_extract_nonmergeSavePlan_multiple_alias_folders() {
  // Multiple AliasFolders under the same node is invalid
  // (validate_tree rejects this). The function should error.
  let input: &str =
    indoc! {"
            * (skg node (gnid main) (skgRepos (nodeRepo main))) main node
            ** (skg aliasFolder) first alias folder
            *** (skg alias) alias one
            *** (skg alias) alias two
            ** (skg aliasFolder) second alias folder
            *** (skg alias) alias three
            ** (skg node (gnid content1) (skgRepos (nodeRepo main))) content node
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let result : Result<Vec<NodeInstruction>, String> =
    nodeInstructions_from_tree (viewforest);

  assert!(result . is_err());
  // The two folders emit conflicting alias intents for one ID.
  assert!(result . unwrap_err() . contains ("Conflicting aliases"));
}

#[test]
fn test_extract_nonmergeSavePlan_mixed_relations() {
  let input: &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root node
            ** (skg node (gnid unrelated1) (skgRepos (nodeRepo main)) noClaim) unrelated child
            ** (skg node (gnid content1) (skgRepos (nodeRepo main))) content child 1
            ** (skg aliasFolder) aliases
            *** (skg alias) my alias
            ** (skg node (gnid content2) (skgRepos (nodeRepo main))) content child 2
            ** (skg node (gnid unrelated2) (skgRepos (nodeRepo main)) noClaim) another unrelated child
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  // Should have instructions for: root, unrelated1, content1, content2, unrelated2
  // AliasFolder and Alias should be skipped
  assert_eq!(instructions . len(), 5);

  let root_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(root_skg . title, "root node");
  assert_eq!(members_msv (&root_skg . aliases), MSV::Specified(vec!["my alias" . to_string()]));
  assert_eq!(members_of (&root_skg . contains), vec![ID::from ("content1"), ID::from ("content2")]); // Only Content relations
}

#[test]
fn extraction_preserves_content_and_independent_children (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg node (gnid ordinary) (skgRepos (nodeRepo main))) ordinary
            ** (skg node (gnid independent) (skgRepos (nodeRepo main)) noClaim) independent
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    vec![ID::from ("root"), ID::from ("ordinary"), ID::from ("independent")]);
  assert_eq!(
    members_of (&saved_node_by_skgid (&instructions, "root") . contains),
    vec![ID::from ("ordinary")]); }

#[test]
fn extraction_skips_alias_and_id_display_nodes (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg aliasFolder) aliases
            *** (skg alias) alias text
            ** (skg idFolder) IDs
            *** (skg id) extra-id
            ** (skg node (gnid child) (skgRepos (nodeRepo main))) child
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    vec![ID::from ("root"), ID::from ("child")]);
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "root") . aliases),
    MSV::Specified (vec!["alias text" . to_string()]));
  assert_eq!(
    members_of (&saved_node_by_skgid (&instructions, "root") . contains),
    vec![ID::from ("child")]); }

#[test]
fn extraction_collects_subscribees_without_hidden_branches (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg hiddenInSubscribeeFolder)
            ***** (skg node (gnid hidden-in) (skgRepos (nodeRepo main))) hidden in child
            **** (skg node (gnid subscribee-content) (skgRepos (nodeRepo main))) subscribee content
            *** (skg hiddenOutsideOfSubscribeeFolder)
            **** (skg node (gnid hidden-outside) (skgRepos (nodeRepo main))) hidden outside child
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    // hidden-in and hidden-outside are editable members of
    // write-protected folders, so they are self-writers on the new recursion
    // surface.
    vec![
      ID::from ("subscriber"),
      ID::from ("hidden-in"),
      ID::from ("subscribee-content"),
      ID::from ("hidden-outside")]);
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "subscriber") . subscribesTo),
    MSV::Specified (vec![ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_skgid (&instructions, "subscriber") . contains,
    vec![]);
  assert!(
    ! save_skgids (&instructions) . contains (&ID::from ("subscribee"))); }

#[test]
fn extraction_collects_overridden_folder (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid overrider) (skgRepos (nodeRepo main))) overrider
            ** (skg overriddenFolder)
            *** (skg node (gnid overridden-a) (skgRepos (nodeRepo main))) overridden A
            *** (skg node (gnid overridden-b) (skgRepos (nodeRepo main))) overridden B
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    // OverriddenFolder members are self-writers: their membership is
    // read for the parent's overrides, and they also save
    // their own vognodes. This is part of the new recursion surface.
    vec![ID::from ("overrider"),
         ID::from ("overridden-a"),
         ID::from ("overridden-b")]);
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "overrider") . overrides),
    MSV::Specified (vec![
      ID::from ("overridden-a"),
      ID::from ("overridden-b")])); }

#[test]
fn empty_overridden_folder_means_empty_override_set (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid overrider) (skgRepos (nodeRepo main))) overrider
            ** (skg overriddenFolder)
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    saved_node_by_skgid (&instructions, "overrider") . overrides,
    MSV::Specified (vec![])); }

#[test]
fn write_protected_folder_members_save_themselves_but_not_their_recorder (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg subscriberFolder)
            *** (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg overriderFolder)
            *** (skg node (gnid overrider) (skgRepos (nodeRepo main))) overrider
            ** (skg hiderFolder)
            *** (skg node (gnid hider) (skgRepos (nodeRepo main))) hider
            ** (skg hiddenFolder)
            *** (skg node (gnid hidden) (skgRepos (nodeRepo main))) hidden
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    // The members are self-writers, on the new recursion surface;
    // the recorder is unaffected by any of these write-protected folders.
    vec![ID::from ("recorder"),
         ID::from ("subscriber"),
         ID::from ("overrider"),
         ID::from ("hider"),
         ID::from ("hidden")]);
  assert_eq!(
    members_of (&saved_node_by_skgid (&instructions, "recorder") . contains),
    Vec::<ID>::new()); }

#[test]
fn subscribee_hiderel_intent_collects_visible_content (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg node (gnid a) (skgRepos (nodeRepo main))) a
            **** (skg node (gnid b) (skgRepos (nodeRepo main))) b
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
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid first) (skgRepos (nodeRepo main))) first
            **** (skg node (gnid first-child) (skgRepos (nodeRepo main))) first child
            *** (skg node (gnid second) (skgRepos (nodeRepo main))) second
            **** (skg node (gnid second-child) (skgRepos (nodeRepo main))) second child
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
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg node (gnid child) (skgRepos (nodeRepo main))) child
            ***** (skg node (gnid grandchild) (skgRepos (nodeRepo main))) grandchild
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    vec![(ID::from ("subscriber"),
       SubscribeeVisibility {
         subscribee : ID::from ("subscribee"),
         visible    : vec![ID::from ("child")],
       })]); }

#[test]
fn subscribee_hiderel_intent_ignores_writeProtected_subscribee (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) subscribee
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    Vec::<(ID, SubscribeeVisibility)>::new()); }

#[test]
fn subscribee_hiderel_intent_ignores_writeProtected_subscriber (
) {
  // At-most-one-writer-per-ID: even though the subscribee
  // here is editable with visible content, its subscriber instance is
  // write-protected, so no hide/unhide edits are inferred for the subscriber
  // -- those belong only to the SubscribeeFolder under the editable
  // instance of that subscriber.
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg node (gnid a) (skgRepos (nodeRepo main))) a
            "};

  assert_eq!(
    visibility_pairs_from_org (input),
    Vec::<(ID, SubscribeeVisibility)>::new()); }

#[test]
fn subscribee_hiderel_intent_excludes_non_content_delete_and_phantom_children (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg node (gnid keep) (skgRepos (nodeRepo main))) keep
            **** (skg node (gnid independent) (skgRepos (nodeRepo main)) noClaim) independent
            **** (skg node (gnid delete-me) (skgRepos (nodeRepo main)) (editRequest delete)) delete me
            **** (skg ghost (gnid phantom) (skgRepos (nodeRepo main))) phantom
            "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
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
fn subscribee_hiderel_intent_ignores_hidden_folder_contents (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            **** (skg hiddenInSubscribeeFolder)
            ***** (skg node (gnid hidden-in) (skgRepos (nodeRepo main))) hidden in child
            **** (skg node (gnid visible) (skgRepos (nodeRepo main))) visible
            *** (skg hiddenOutsideOfSubscribeeFolder)
            **** (skg node (gnid hidden-outside) (skgRepos (nodeRepo main))) hidden outside child
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
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            Root body
            ** (skg aliasFolder) aliases
            *** (skg alias) root alias
            ** (skg node (gnid child) (skgRepos (nodeRepo main))) child
            Child body
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            * (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
            Doomed body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("child"),
      ID::from ("doomed")]);
  let root : &Graphnode =
    saved_node_by_skgid (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(members_msv (&root . aliases), MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(members_of (&root . contains), vec![ID::from ("child")]);
  assert_eq!(members_msv (&root . subscribesTo), MSV::Specified (vec![
    ID::from ("subscribee")]));
  assert!(matches!(
    instructions . last(),
    Some (NodeInstruction::Delete (DeleteNode { skgid, .. }))
      if skgid == &ID::from ("doomed"))); }

#[test]
fn split_extraction_passes_preserve_mixed_instruction_shape (
) {
  let input: &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            Root body
            ** (skg node (gnid independent) (skgRepos (nodeRepo main)) noClaim) independent
            ** (skg node (gnid content) (skgRepos (nodeRepo main))) content
            ** (skg aliasFolder) aliases
            *** (skg alias) root alias
            ** (skg subscribeeFolder)
            *** (skg node (gnid subscribee) (skgRepos (nodeRepo main))) subscribee
            * (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
            Doomed body
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    vec![
      ID::from ("root"),
      ID::from ("independent"),
      ID::from ("content"),
      ID::from ("doomed")]);
  let root : &Graphnode =
    saved_node_by_skgid (&instructions, "root");
  assert_eq!(root . title, "root");
  assert_eq!(root . body, Some ("Root body" . to_string()));
  assert_eq!(members_of (&root . contains), vec![ID::from ("content")]);
  assert_eq!(members_msv (&root . aliases), MSV::Specified (vec![
    "root alias" . to_string()]));
  assert_eq!(members_msv (&root . subscribesTo), MSV::Specified (vec![
    ID::from ("subscribee")]));
  assert_eq!(
    saved_node_by_skgid (&instructions, "independent") . contains,
    vec![]);
  assert!(matches!(
    instructions . last(),
    Some (NodeInstruction::Delete (DeleteNode { skgid, .. }))
      if skgid == &ID::from ("doomed"))); }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-new-lic-extraction",
    |s| Box::pin ( async move {
      s . reset_from_config (
        "subscribee_as_such_child_list_removal_does_not_save_subscribee",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      subscribee_as_such_child_list_removal_does_not_save_subscribee (
        &s . config ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_child_removal_is_not_foreign_contains_edit",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      subscribee_as_such_child_removal_is_not_foreign_contains_edit (
        &s . config ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_child_list_removal_infers_subscriber_hide",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      subscribee_as_such_child_list_removal_infers_subscriber_hide (
        &s . config ) . await ?;
      s . reset_from_config (
        "moving_subscribee_as_such_child_to_subscriber_does_not_hide",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      moving_subscribee_as_such_child_to_subscriber_does_not_hide (
        &s . config ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_visible_child_removes_subscriber_hide",
        "tests/hidden_from_subscriptions/fixtures-hidden-within-but-none-without/skgconfig.toml") ?;
      subscribee_as_such_visible_child_removes_subscriber_hide (
        &s . config ) . await ?;
      s . reset_from_config (
        "subscribee_as_such_unhide_preserves_unrelated_hides",
        "tests/hidden_from_subscriptions/fixtures-every-kind-of-folder/skgconfig.toml") ?;
      subscribee_as_such_unhide_preserves_unrelated_hides (
        &s . config ) . await ?;
      s . reset_from_config (
        "overlapping_subscribee_hiderel_conflict_rejects_save",
        "tests/hidden_from_subscriptions/fixtures-overlapping-subscribees/skgconfig.toml") ?;
      overlapping_subscribee_hiderel_conflict_rejects_save (
        &s . config ) . await ?;
      s . reset_from_config (
        "ordinary_owned_child_list_edit_still_changes_contains",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      ordinary_owned_child_list_edit_still_changes_contains (
        &s . config ) . await ?;
      s . reset_from_config (
        "ordinary_same_id_occurrence_keeps_contains_edit_when_also_as_subscribee",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      ordinary_same_skgid_occurrence_keeps_contains_edit_when_also_as_subscribee (
        &s . config ) . await ?;
      s . reset_from_config (
        "recursive_descendant_under_as_subscribee_keeps_own_contains_edit",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      recursive_descendant_under_as_subscribee_keeps_own_contains_edit (
        &s . config ) . await ?;
      s . reset_from_config (
        "foreign_subscribee_as_such_title_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      foreign_subscribee_as_such_title_edit_is_rejected (
        &s . config ) . await ?;
      s . reset_from_config (
        "owned_as_subscribee_title_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      owned_as_subscribee_title_edit_is_rejected (
        &s . config ) . await ?;
      s . reset_from_config (
        "owned_as_subscribee_body_edit_is_rejected",
        SUBSCRIBEE_EDIT_CONFIG) ?;
      owned_as_subscribee_body_edit_is_rejected (
        &s . config ) . await ?;
      Ok (( )) } )) }

async fn subscribee_as_such_child_list_removal_does_not_save_subscribee (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert!(
        ! save_skgids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn subscribee_as_such_child_removal_is_not_foreign_contains_edit (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      // Editing within a subscribee-as-such is not a foreign-contains
      // edit of e: e is neither rejected as a ModifiedForeignNode, nor
      // written, nor forked. (e2, the foreign content shown under it, may
      // fork -- now resolving to the default owned skgrepo rather than
      // erroring, which is why this no longer returns Err.)
      // Supply a default clone skgrepo (as the production caller does),
      // so the foreign grandchild e2 forks cleanly and the call returns
      // Ok -- this test is about e, not e2.
      let clone_skgrepo_inputs : skg::from_text::fork::CloneSkgrepoInputs =
        skg::from_text::fork::CloneSkgrepoInputs {
          user_set          : std::collections::HashMap::new(),
          explicit_child    : std::collections::HashMap::new(),
          inferred_ancestor : std::collections::HashMap::new(),
          default           : Some (SkgrepoName::from ("owned")), };
      let ( node_instructions, fork_specs ) =
        validate_and_filter_foreign_instructions (
          instructions, &[],
          &graph_handle_from_config (config)? . load_full (),
          &clone_skgrepo_inputs,
          &std::collections::HashMap::new(),
          config)
        . expect ("the subscribee-as-such edit must not error");
      assert!(
        ! save_skgids (&node_instructions) . contains (&ID::from ("e")),
        "e must not be written as a foreign node: {:?}", node_instructions);
      assert!(
        ! fork_specs . iter() . any (|spec| spec . original_skgid == ID::from ("e")),
        "e must not be forked: {:?}", fork_specs);
      Ok (()) }

async fn subscribee_as_such_child_list_removal_infers_subscriber_hide (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        members_msv (&saved_node_by_skgid (&instructions, "r")
          . hidesFromSubs),
        MSV::Specified (vec![ID::from ("e1")]));
      assert!(
        ! save_skgids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn moving_subscribee_as_such_child_to_subscriber_does_not_hide (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                ** (skg node (gnid e1) (skgRepos (nodeRepo foreign)) writeProtected) e1
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        saved_node_by_skgid (&instructions, "r")
          . hidesFromSubs,
        MSV::Unspecified);
      assert_eq!(
        members_of (&saved_node_by_skgid (&instructions, "r") . contains),
        vec![ID::from ("e1")]);
      Ok (()) }

async fn subscribee_as_such_visible_child_removes_subscriber_hide (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid R) (skgRepos (nodeRepo main))) R
                ** (skg subscribeeFolder)
                *** (skg node (gnid E1) (skgRepos (nodeRepo main))) subscribee-1
                **** (skg node (gnid E11) (skgRepos (nodeRepo main))) E11
                **** (skg node (gnid H) (skgRepos (nodeRepo main))) H
                **** (skg node (gnid E12) (skgRepos (nodeRepo main))) E12
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        saved_node_by_skgid (&instructions, "R")
          . hidesFromSubs,
        MSV::Specified (vec![]));
      assert!(
        ! save_skgids (&instructions) . contains (&ID::from ("E1")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      Ok (()) }

async fn subscribee_as_such_unhide_preserves_unrelated_hides (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid R) (skgRepos (nodeRepo main))) R
                ** (skg subscribeeFolder)
                *** (skg node (gnid E1) (skgRepos (nodeRepo main))) subscribee-1
                **** (skg node (gnid hidden-in-E1) (skgRepos (nodeRepo main))) hidden-in-E1
                **** (skg node (gnid E11) (skgRepos (nodeRepo main))) E11
                *** (skg node (gnid E2) (skgRepos (nodeRepo main))) subscribee-2
                **** (skg node (gnid E21) (skgRepos (nodeRepo main))) E21
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        members_msv (&saved_node_by_skgid (&instructions, "R")
          . hidesFromSubs),
        MSV::Specified (
          vec![ID::from ("hidden-in-E2"),
               ID::from ("hidden-for-no-reason")]));
      Ok (()) }

async fn overlapping_subscribee_hiderel_conflict_rejects_save (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid R) (skgRepos (nodeRepo main))) R
                ** (skg subscribeeFolder)
                *** (skg node (gnid E1) (skgRepos (nodeRepo main))) E1
                **** (skg node (gnid shared) (skgRepos (nodeRepo main))) shared
                *** (skg node (gnid E2) (skgRepos (nodeRepo main))) E2
                "};
      let result : Result<Vec<NodeInstruction>, Box<dyn Error>> =
        nodeInstructions_from_org_with_disk (
          input, config) . await;
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
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg node (gnid r1) (skgRepos (nodeRepo owned))) r1
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        members_of (&saved_node_by_skgid (&instructions, "r") . contains),
        vec![ID::from ("r1")]);
      Ok (()) }

async fn ordinary_same_skgid_occurrence_keeps_contains_edit_when_also_as_subscribee (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                * (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                ** (skg node (gnid e1) (skgRepos (nodeRepo foreign))) e1
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert_eq!(
        members_of (&saved_node_by_skgid (&instructions, "e") . contains),
        vec![ID::from ("e1")]);
      assert_eq!(
        members_msv (&saved_node_by_skgid (&instructions, "r")
          . hidesFromSubs),
        MSV::Specified (vec![ID::from ("e1")]));
      Ok (()) }

#[test]
fn idfolder_resident_unrestrictedVognode_saves_itself_but_is_not_content (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg idFolder) ids
            *** (skg node (gnid display-child) (skgRepos (nodeRepo main))) display child
            ** (skg node (gnid real-child) (skgRepos (nodeRepo main))) real child
            "};

  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(
    save_skgids (&instructions),
    // display-child is a self-writer inside the IDFolder, on the new
    // recursion surface; the IDFolder's membership is never read.
    vec![ID::from ("root"),
         ID::from ("display-child"),
         ID::from ("real-child")]);
  assert_eq!(
    members_of (&saved_node_by_skgid (&instructions, "root") . contains),
    vec![ID::from ("real-child")]); }

async fn recursive_descendant_under_as_subscribee_keeps_own_contains_edit (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                "};
      let instructions : Vec<NodeInstruction> =
        nodeInstructions_from_org_with_disk (
          input, config) . await?;
      assert!(
        ! save_skgids (&instructions) . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode: {:?}",
        instructions);
      assert_eq!(
        members_of (&saved_node_by_skgid (&instructions, "e2") . contains),
        Vec::<ID>::new());
      Ok (()) }

async fn foreign_subscribee_as_such_title_edit_is_rejected (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                ** (skg subscribeeFolder)
                *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) changed title
                **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
                "};
      let result : Result<Vec<NodeInstruction>, Box<dyn Error>> =
        nodeInstructions_from_org_with_disk (
          input, config) . await;
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
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid a) (skgRepos (nodeRepo owned))) a
                ** (skg subscribeeFolder)
                *** (skg node (gnid r) (skgRepos (nodeRepo owned))) changed title
                **** (skg node (gnid r1) (skgRepos (nodeRepo owned))) r1
                "};
      let result : Result<Vec<NodeInstruction>, Box<dyn Error>> =
        nodeInstructions_from_org_with_disk (
          input, config) . await;
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
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
                * (skg node (gnid a) (skgRepos (nodeRepo owned))) a
                ** (skg subscribeeFolder)
                *** (skg node (gnid r) (skgRepos (nodeRepo owned))) r
                body text that should not be accepted here
                **** (skg node (gnid r1) (skgRepos (nodeRepo owned))) r1
                "};
      let result : Result<Vec<NodeInstruction>, Box<dyn Error>> =
        nodeInstructions_from_org_with_disk (
          input, config) . await;
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
            * (skg node (gnid level1) (skgRepos (nodeRepo main))) level 1
            ** (skg node (gnid level2a) (skgRepos (nodeRepo main))) level 2a
            *** (skg node (gnid level3a) (skgRepos (nodeRepo main))) level 3a
            **** (skg node (gnid level4) (skgRepos (nodeRepo main))) level 4
            ** (skg node (gnid level2b) (skgRepos (nodeRepo main))) level 2b
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 5);

  // Check contains relationships
  let level1_skg : &Graphnode =
    extract_graphnode_if_save_else_error(&instructions[0]);
  assert_eq!(members_of (&level1_skg . contains), vec![ID::from ("level2a"), ID::from ("level2b")]);

  let level2a_skg : &Graphnode =
    extract_graphnode_if_save_else_error(&instructions[1]);
  assert_eq!(members_of (&level2a_skg . contains), vec![ID::from ("level3a")]);

  let level3a_skg : &Graphnode =
    extract_graphnode_if_save_else_error(&instructions[2]);
  assert_eq!(members_of (&level3a_skg . contains), vec![ID::from ("level4")]);

  let level4_skg : &Graphnode =
    extract_graphnode_if_save_else_error(&instructions[3]);
  assert_eq!(level4_skg . contains, vec![]); // Leaf node

  let level2b_skg : &Graphnode =
    extract_graphnode_if_save_else_error(&instructions[4]);
  assert_eq!(level2b_skg . contains, vec![]); // Leaf node
}

#[test]
fn test_extract_nonmergeSavePlan_error_missing_skgid() {
  let input: &str =
    indoc! {"
            * (skg node (gnid good_node) (skgRepos (nodeRepo main))) good node
            * node without ID
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let result : Result<Tree<Viewnode>, String> =
    // This conversion fails because of missing ID
    maybePlaced_to_placed_tree (maybePlaced_viewforest);

  assert!(result . is_err(), "Should return error for missing ID");
  let error_msg : String = result . unwrap_err();
  assert!(error_msg . contains ("node without ID") ||
          error_msg . contains ("has no ID"));
}

#[test]
fn test_extract_nonmergeSavePlan_empty_input() {
  let viewforest: Tree<Viewnode> = Tree::new(viewforest_root_viewnode());
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 0, "Empty input should produce empty output");
}

#[test]
fn test_extract_nonmergeSavePlan_only_aliases() {
  let input: &str =
    indoc! {"
            * (skg node (gnid main) (skgRepos (nodeRepo main))) main node
            ** (skg aliasFolder) aliases only
            *** (skg alias) alias one
            *** (skg alias) alias two
        "};

  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 1); // Only main node

  let main_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(members_msv (&main_skg . aliases), MSV::Specified(vec!["alias one" . to_string(), "alias two" . to_string()]));
  assert_eq!(main_skg . contains, vec![]); // No content children
}

#[test]
fn test_extract_nonmergeSavePlan_complex_scenario() {
  let input: &str =
    indoc! {"
            * (skg node (gnid doc1) (skgRepos (nodeRepo main))) Document 1
            Document body
            ** (skg aliasFolder) Doc1 Aliases
            *** (skg alias) First Document
            *** (skg alias) Primary Doc
            ** (skg node (gnid section1) (skgRepos (nodeRepo main))) Section 1
            Section 1 body
            *** (skg node (gnid subsection1a) (skgRepos (nodeRepo main))) Subsection 1a
            ** (skg node (gnid section2) (skgRepos (nodeRepo main)) (editRequest delete)) Section 2
            ** (skg node (gnid section3) (skgRepos (nodeRepo main))) Section 3
            * (skg node (gnid doc2) (skgRepos (nodeRepo main))) Document 2
            ** (skg node (gnid ref_section) (skgRepos (nodeRepo main)) noClaim) Reference Section
        "};
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  let viewforest: Tree<Viewnode> =
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap();
  let instructions: Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap();

  assert_eq!(instructions . len(), 7); // doc1, section1, subsection1a, section2, section3, doc2, ref_section

  // Test doc1
  let doc1_skg : &Graphnode = match &instructions[0] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(doc1_skg . title, "Document 1");
  assert_eq!(members_msv (&doc1_skg . aliases),
             MSV::Specified(vec!["First Document" . to_string(),
                       "Primary Doc" . to_string()]));
  assert_eq!(members_of (&doc1_skg . contains),
             vec![ID::from ("section1"),
                       ID::from ("section3")]);
  assert!(matches!(&instructions[0],
                   NodeInstruction::Save (_)));

  // Test section2 with toDelete
  assert!(matches!(&instructions[3],
                   NodeInstruction::Delete (_)));
  match &instructions[3] {
    NodeInstruction::Delete(DeleteNode { skgid, .. }) => {
      assert_eq!(skgid, &ID::from ("section2")); },
    NodeInstruction::Save (_) =>
      panic!("Expected Delete, got Save") };

  // Test that subsection1a is child of section1
  let section1_skg : &Graphnode = match &instructions[1] {
    NodeInstruction::Save(SaveNode (node)) => node,
    NodeInstruction::Delete (_) => panic!("Expected Save, got Delete") };
  assert_eq!(members_of (&section1_skg . contains), vec![ID::from ("subsection1a")]);
}

// The next several tests pin the membership-predicate wiring of
// extraction, with one test per condition not already covered above.

#[test]
fn toDelete_member_is_excluded_from_subscribees (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid subscriber) (skgRepos (nodeRepo main))) subscriber
            ** (skg subscribeeFolder)
            *** (skg node (gnid keep) (skgRepos (nodeRepo main))) keep
            *** (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "subscriber") . subscribesTo),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn toDelete_member_is_excluded_from_overriddens (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid overrider) (skgRepos (nodeRepo main))) overrider
            ** (skg overriddenFolder)
            *** (skg node (gnid keep) (skgRepos (nodeRepo main))) keep
            *** (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "overrider") . overrides),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn independent_member_is_excluded_from_overriddens (
) {
  let input : &str =
    indoc! {"
            * (skg node (gnid overrider) (skgRepos (nodeRepo main))) overrider
            ** (skg overriddenFolder)
            *** (skg node (gnid keep) (skgRepos (nodeRepo main))) keep
            *** (skg node (gnid bystander) (skgRepos (nodeRepo main)) noClaim) bystander
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "overrider") . overrides),
    MSV::Specified (vec![ID::from ("keep")])); }

#[test]
fn duplicate_members_of_defining_folders_are_silently_deduplicated (
) {
  // Defining folders never squawk about repeats: emission
  // deduplicates, preserving first-occurrence order.
  let input : &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg aliasFolder) aliases
            *** (skg alias) echo
            *** (skg alias) other
            *** (skg alias) echo
            ** (skg subscribeeFolder)
            *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
            *** (skg node (gnid s2) (skgRepos (nodeRepo main)) writeProtected) s2
            *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
            ** (skg overriddenFolder)
            *** (skg node (gnid o1) (skgRepos (nodeRepo main)) writeProtected) o1
            *** (skg node (gnid o2) (skgRepos (nodeRepo main)) writeProtected) o2
            *** (skg node (gnid o1) (skgRepos (nodeRepo main)) writeProtected) o1
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  let recorder : &Graphnode =
    saved_node_by_skgid (&instructions, "recorder");
  assert_eq!(
    members_msv (&recorder . aliases),
    MSV::Specified (vec![
      "echo" . to_string(), "other" . to_string()]));
  assert_eq!(
    members_msv (&recorder . subscribesTo),
    MSV::Specified (vec![ID::from ("s1"), ID::from ("s2")]));
  assert_eq!(
    members_msv (&recorder . overrides),
    MSV::Specified (vec![ID::from ("o1"), ID::from ("o2")])); }

// The next four tests pin the editable-folder membership semantics that
// the relationship matrix asks for at the cheapest seam:
// subscribeeFolder order and one-member removal persist to
// subscribesTo; overriddenFolder order does not matter (the
// set-difference merge depends on this); and the write-protected hiddenFolder
// never writes the recorder's hides.

#[test]
fn reordering_subscribees_reorders_subscribesTo (
) {
  // A subscribeeFolder is writable: its member order is the recorder's
  // subscribesTo order. Members [c, a, b] (a reorder of [a, b, c])
  // emit subscribesTo = [c, a, b].
  let input : &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg subscribeeFolder)
            *** (skg node (gnid c) (skgRepos (nodeRepo main))) c
            *** (skg node (gnid a) (skgRepos (nodeRepo main))) a
            *** (skg node (gnid b) (skgRepos (nodeRepo main))) b
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "recorder") . subscribesTo),
    MSV::Specified (vec![
      ID::from ("c"), ID::from ("a"), ID::from ("b")])); }

#[test]
fn removing_one_subscribee_keeps_the_rest (
) {
  // Deleting one member of an editable folder (b, from [a, b, c]) leaves
  // the rest: subscribesTo = [a, c], neither empty nor unspecified.
  let input : &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg subscribeeFolder)
            *** (skg node (gnid a) (skgRepos (nodeRepo main))) a
            *** (skg node (gnid c) (skgRepos (nodeRepo main))) c
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    members_msv (&saved_node_by_skgid (&instructions, "recorder") . subscribesTo),
    MSV::Specified (vec![ID::from ("a"), ID::from ("c")])); }

#[test]
fn reordering_overridden_folder_is_harmless (
) {
  // overrides is order-free (the set-difference merge relies
  // on this). overriddenFolder members [b, a] (a reorder of [a, b]) emit
  // the same set, and nothing else about the recorder changes.
  let input : &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg overriddenFolder)
            *** (skg node (gnid b) (skgRepos (nodeRepo main))) b
            *** (skg node (gnid a) (skgRepos (nodeRepo main))) a
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  let recorder : &Graphnode =
    saved_node_by_skgid (&instructions, "recorder");
  let overridden : Vec<ID> = match &recorder . overrides {
    MSV::Specified (skgids) => members_of (skgids),
    other => panic! (
      "expected Specified override set, got {:?}", other), };
  assert_eq!(overridden . len(), 2);
  assert!(overridden . contains (&ID::from ("a")));
  assert!(overridden . contains (&ID::from ("b")));
  assert_eq!(members_of (&recorder . contains), Vec::<ID>::new());
  assert_eq!(recorder . subscribesTo, MSV::Unspecified);
  assert_eq!(recorder . hidesFromSubs, MSV::Unspecified); }

#[test]
fn deleting_from_hiddenFolder_emits_no_hide_change (
) {
  // hiddenFolder is write-protected: its membership is never collected into
  // the recorder's hidesFromSubs. A member shown there
  // (and, equally, a member deleted from there) emits no hide intent;
  // the recorder's hides stay Unspecified (no opinion), preserving
  // whatever disk holds. The compound partner folders have this guarantee tested;
  // the plain hiddenFolder did not.
  let input : &str =
    indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg hiddenFolder)
            *** (skg node (gnid hidden) (skgRepos (nodeRepo main))) hidden
        "};
  let viewforest : Tree<Viewnode> =
    checked_viewforest_from_org (input);
  let instructions : Vec<NodeInstruction> =
    nodeInstructions_from_tree (viewforest) . unwrap ();
  assert_eq!(
    saved_node_by_skgid (&instructions, "recorder")
      . hidesFromSubs,
    MSV::Unspecified); }
