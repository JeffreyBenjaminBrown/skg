/// These are full-pipeline regression tests for
/// 'extract_nonmergeSavePlan_locally' (which runs collection through
/// the noop filter), plus nodeMerge extraction from the same map.
/// .
/// The expectations were originally verified differentially against
/// the old extraction path before its deletion
/// (TODO/local-instruction-collection/3_plan.org). The cases marked
/// "new recursion surface" are the deliberate behavior changes.

use ego_tree::Tree;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::{
  org_to_uninterpreted_nodes,
  org_to_uninterpreted_viewforest };
use skg::from_text::local_instruction_collection::{
  extract_nonmergeSavePlan_locally, NonmergeSavePlan };
use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use skg::test_utils::run_with_test_db_from_config;
use skg::test_utils::run_with_test_db;
use skg::types::errors::BufferValidationError;
use skg::types::git::Sign;
use skg::types::maybe_placed_viewnode::{
  MpViewnode,
  maybePlaced_to_placed_tree,
  maybePlaced_to_placed_viewforest };
use skg::types::misc::{ID, MSV, SkgConfig};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, NodeMerge, SaveNode, DeleteNode};
use skg::types::tree::forest::{MpViewForest, ViewForest};
use skg::types::viewnode::{ViewNode, ViewNodeKind, Vognode};
use std::error::Error;
use typedb_driver::TypeDBDriver;

const SUBSCRIBEE_EDIT_CONFIG : &str =
  "tests/hidden_from_subscriptions/fixtures-subscribee-edit/skgconfig.toml";

fn placed_forest_from_org (
  input : &str,
) -> ViewForest {
  let maybePlaced_viewforest : Tree<MpViewnode> =
    org_to_uninterpreted_nodes (input) . unwrap() . 0;
  ViewForest::from_internal_tree (
    maybePlaced_to_placed_tree (maybePlaced_viewforest) . unwrap() ) }

/// This parses the way production does, including id/source
/// enrichment from the db. Orgs that mention on-disk nodes need it.
async fn placed_forest_from_org_with_disk (
  input  : &str,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<ViewForest, Box<dyn Error>> {
  let (mut maybePlaced_viewforest, _parsing_errors)
    : (MpViewForest, Vec<BufferValidationError>) =
    org_to_uninterpreted_viewforest (input) ?;
  add_missing_info_to_viewforest (
    &mut maybePlaced_viewforest, &config . db_name, driver) . await?;
  Ok ( maybePlaced_to_placed_viewforest (maybePlaced_viewforest) ? ) }

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

#[test]
fn pipeline_basic_mixed_tree (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-pipeline-basic",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-pipeline-basic",
    |config, driver, _tantivy| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id root) (source main))) root
            Root body
            ** (skg aliasCol) aliases
            *** (skg alias) first alias
            *** (skg alias) second alias
            ** (skg (node (id child) (source main))) child
            *** (skg (node (id grandchild) (source main))) grandchild
            ** (skg (node (id independent) (source main) (parentIs independent))) independent
            ** (skg subscribeeCol)
            *** (skg (node (id s1) (source main) indef)) s1
            ** (skg overriddenCol)
            *** (skg (node (id o1) (source main) indef)) o1
            * (skg (node (id explicit) (source main))) explicit
            ** (skg aliasCol) aliases
            ** (skg subscribeeCol)
            ** (skg overriddenCol)
            * (skg (node (id doomed) (source main) (editRequest delete))) doomed
            "};
      let (plan, nodeMerge_acquisitions)
        : (NonmergeSavePlan, Vec<(ID, ID)>) =
        extract_nonmergeSavePlan_locally (
          &placed_forest_from_org (input), config, driver) . await?;
      assert_eq!(
        save_ids (&plan . define_nodes),
        vec![ ID::from ("root"), ID::from ("child"),
              ID::from ("grandchild"), ID::from ("independent"),
              ID::from ("explicit"), ID::from ("doomed") ]);
      assert!( matches!(
        plan . define_nodes . last(),
        Some (DefineNode::Delete (DeleteNode { id, .. }))
          if id == &ID::from ("doomed") ));
      { let root : &NodeComplete =
          saved_node_by_id (&plan . define_nodes, "root");
        assert_eq!( root . body,
                    Some ("Root body" . to_string()) );
        assert_eq!( root . contains, vec![ID::from ("child")] );
        assert_eq!( root . aliases,
                    MSV::Specified (vec![
                      "first alias" . to_string(),
                      "second alias" . to_string()]) );
        assert_eq!( root . subscribes_to,
                    MSV::Specified (vec![ID::from ("s1")]) );
        assert_eq!( root . overrides_view_of,
                    MSV::Specified (vec![ID::from ("o1")]) ); }
      { let explicit : &NodeComplete =
          saved_node_by_id (&plan . define_nodes, "explicit");
        // Present-but-empty cols are explicit emptiness.
        assert_eq!( explicit . aliases,
                    MSV::Specified (vec![]) );
        assert_eq!( explicit . subscribes_to,
                    MSV::Specified (vec![]) );
        assert_eq!( explicit . overrides_view_of,
                    MSV::Specified (vec![]) ); }
      assert!( plan . source_moves . is_empty() );
      assert!( nodeMerge_acquisitions . is_empty() );
      Ok (( )) })) }

#[test]
fn pipeline_subscribee_hiderels (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-pipeline-hiderels",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id r) (source owned))) r
            ** (skg subscribeeCol)
            *** (skg (node (id e) (source foreign))) subscribee-e
            **** (skg (node (id e2) (source foreign))) e2
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config, driver) . await?;
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &forest, config, driver) . await?;
      assert_eq!(
        saved_node_by_id (&plan . define_nodes, "r")
          . hides_from_its_subscriptions,
        MSV::Specified (vec![ID::from ("e1")]));
      assert!(
        ! save_ids (&plan . define_nodes)
          . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode" );
      Ok (( )) })) }

#[test]
fn pipeline_readonly_col_member_edits (
) -> Result<(), Box<dyn Error>> {
  // This tests the new recursion surface: definitive members of
  // read-only cols (and their subtrees) save their own edits.
  run_with_test_db (
    "skg-test-pipeline-readonly",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-pipeline-readonly",
    |config, driver, _tantivy| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id owner) (source main))) owner
            ** (skg subscriberCol)
            *** (skg (node (id intruder) (source main))) intruder
            **** (skg (node (id intruder-child) (source main))) intruder child
            ** (skg hiddenCol)
            *** (skg (node (id lurker) (source main))) lurker
            "};
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &placed_forest_from_org (input), config, driver) . await?;
      assert_eq!(
        save_ids (&plan . define_nodes),
        vec![ ID::from ("owner"), ID::from ("intruder"),
              ID::from ("intruder-child"), ID::from ("lurker") ]);
      assert_eq!(
        saved_node_by_id (&plan . define_nodes, "owner") . contains,
        Vec::<ID>::new() );
      assert_eq!(
        saved_node_by_id (&plan . define_nodes, "intruder") . contains,
        vec![ID::from ("intruder-child")] );
      Ok (( )) })) }

#[test]
fn pipeline_inactive_subtree (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-pipeline-inactive",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-pipeline-inactive",
    |config, driver, _tantivy| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (inactiveNode (id hidden) (source main)))
            *** (skg (node (id stowaway) (source main))) stowaway
            "};
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &placed_forest_from_org (input), config, driver) . await?;
      assert_eq!(
        save_ids (&plan . define_nodes),
        // stowaway is on the new recursion surface.
        vec![ ID::from ("root"), ID::from ("stowaway") ]);
      assert_eq!(
        saved_node_by_id (&plan . define_nodes, "root") . contains,
        vec![ID::from ("hidden")] );
      Ok (( )) })) }

#[test]
fn pipeline_phantom_subtree (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-pipeline-phantom",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-pipeline-phantom",
    |config, driver, _tantivy| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id root) (source main))) root
            ** (skg (node (id fading) (source main))) fading
            *** (skg (node (id survivor) (source main))) survivor
            "};
      let forest : ViewForest = {
        let mut tree : Tree<ViewNode> = {
          let maybePlaced_viewforest : Tree<MpViewnode> =
            org_to_uninterpreted_nodes (input) . unwrap() . 0;
          maybePlaced_to_placed_tree (maybePlaced_viewforest)
            . unwrap() };
        let fading_treeid : ego_tree::NodeId =
          tree . nodes()
          . find ( |n| matches!(
              &n . value() . kind,
              ViewNodeKind::Vognode (Vognode::Active (t))
                if t . id == ID::from ("fading") ))
          . map ( |n| n . id() )
          . expect ("fading node not found");
        if let ViewNodeKind::Vognode (Vognode::Active (t)) =
          &mut tree . get_mut (fading_treeid) . unwrap() . value() . kind
        { t . membership . unstaged = Some (Sign::Minus); }
        tree . get_mut (fading_treeid) . unwrap()
          . value() . normal_to_phantom ();
        ViewForest::from_internal_tree (tree) };
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &forest, config, driver) . await?;
      assert_eq!(
        save_ids (&plan . define_nodes),
        // survivor is on the new recursion surface; the phantom
        // itself emits nothing and is not content of its parent.
        vec![ ID::from ("root"), ID::from ("survivor") ]);
      assert_eq!(
        saved_node_by_id (&plan . define_nodes, "root") . contains,
        Vec::<ID>::new() );
      Ok (( )) })) }

#[test]
fn pipeline_nodeMerge_requests (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-pipeline-nodemerge",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-pipeline-nodemerge",
    |config, driver, _tantivy| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id 1) (source main) (editRequest (merge 2)))) 1
            ** (skg (node (id 11) (source main))) 11
            ** (skg (node (id 12) (source main))) 12
            ** (skg (node (id overlap) (source main))) overlap
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config, driver) . await?;
      let (_plan, nodeMerge_acquisitions) =
        extract_nonmergeSavePlan_locally (
          &forest, config, driver) . await?;
      assert_eq!( nodeMerge_acquisitions,
                  vec![ (ID::from ("1"), ID::from ("2")) ]);
      let nodeMerges : Vec<NodeMerge> =
        nodeMerge_instructions_from_pairs (
          &nodeMerge_acquisitions, config, driver) . await?;
      assert_eq!( nodeMerges . len(), 1 );
      assert_eq!( nodeMerges [0] . acquirer_id(), &ID::from ("1") );
      assert_eq!( nodeMerges [0] . acquiree_id(), &ID::from ("2") );
      assert_eq!( nodeMerges [0] . acquiree_text_preserver . 0 . title,
                  "MERGED: 2" );
      Ok (( )) })) }

#[test]
fn pipeline_rejects_text_claim_mismatch (
) -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config (
    "skg-test-pipeline-claim-rejection",
    SUBSCRIBEE_EDIT_CONFIG,
    |config, driver| Box::pin (async move {
      let input : &str =
        indoc! {"
            * (skg (node (id r) (source owned))) r
            ** (skg subscribeeCol)
            *** (skg (node (id e) (source foreign))) changed title
            **** (skg (node (id e2) (source foreign))) e2
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config, driver) . await?;
      let error : String =
        extract_nonmergeSavePlan_locally (
          &forest, config, driver ) . await
        . err() . expect ("the title edit should be rejected")
        . to_string();
      assert!( error . contains ("Cannot edit title/body") );
      Ok (( )) })) }
