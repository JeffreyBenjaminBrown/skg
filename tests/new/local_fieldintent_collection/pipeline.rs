/// These are full-pipeline regression tests for
/// 'extract_nonmergeSavePlan_locally' (which runs collection through
/// the noop filter), plus nodeMerge extraction from the same map.
/// .
/// The expectations were originally verified differentially against
/// the old extraction path before its deletion. The cases marked
/// "new recursion surface" are the deliberate behavior changes.

use skg::from_text::buffer_to_viewnodes::add_missing_info::ParseProvenance;
use indoc::indoc;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::from_text::local_fieldintent_collection::{
  extract_nonmergeSavePlan_locally, NonmergeSavePlan };
use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config, graph_from_config};
use skg::types::errors::BufferValidationError;
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex, members_of, members_msv};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, NodeMerge, SaveNode, DeleteNode};
use skg::types::tree::forest::ViewForest;
use std::error::Error;

use crate::parse_helpers::viewforest_as_written;

const SUBSCRIBEE_EDIT_CONFIG : &str =
  "tests/hidden_from_subscriptions/fixtures-subscribee-edit/skgconfig.toml";

/// This parses the way production does, supplying ids and repos
/// from the graph on disk. Orgs that mention on-disk nodes need it.
async fn placed_forest_from_org_with_disk (
  input  : &str,
  config : &SkgConfig,
) -> Result<ViewForest, Box<dyn Error>> {
  let (viewforest, _parsing_errors, _warnings, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, ParseProvenance) =
    org_to_viewforest (input, IdResolution::ThroughGraph (&graph_from_config (config) ?)) ?;
  Ok ( viewforest ) }

fn save_gnids (
  instructions : &[NodeInstruction],
) -> Vec<ID> {
  instructions . iter() . map (|instruction| match instruction {
    NodeInstruction::Save (SaveNode (node)) => node . pid . clone(),
    NodeInstruction::Delete (DeleteNode { gnid, .. }) => gnid . clone(),
  }) . collect() }

fn saved_node_by_gnid<'a> (
  instructions : &'a [NodeInstruction],
  gnid           : &str,
) -> &'a Graphnode {
  for instruction in instructions {
    if let NodeInstruction::Save (SaveNode (node)) = instruction {
      if node . pid == ID::from (gnid) {
        return node; }}}
  panic! ("SaveNode not found: {}", gnid) }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-new-lic-pipeline",
    |s| Box::pin ( async move {
      s . reset ("pipeline_basic_mixed_tree",
                 "tests/merge/merge_nodes/fixtures") ?;
      pipeline_basic_mixed_tree (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("pipeline_subscribee_hiderels",
                             SUBSCRIBEE_EDIT_CONFIG) ?;
      pipeline_subscribee_hiderels (
        &s . config ) . await ?;
      s . reset ("pipeline_write_protected_folder_member_edits",
                 "tests/merge/merge_nodes/fixtures") ?;
      pipeline_write_protected_folder_member_edits (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("pipeline_restricted_subtree",
                 "tests/merge/merge_nodes/fixtures") ?;
      pipeline_restricted_subtree (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("pipeline_ghost_subtree",
                 "tests/merge/merge_nodes/fixtures") ?;
      pipeline_ghost_subtree (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("pipeline_nodeMerge_requests",
                 "tests/merge/merge_nodes/fixtures") ?;
      pipeline_nodeMerge_requests (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset_from_config ("pipeline_rejects_text_claim_mismatch",
                             SUBSCRIBEE_EDIT_CONFIG) ?;
      pipeline_rejects_text_claim_mismatch (
        &s . config ) . await ?;
      Ok (( )) } )) }

async fn pipeline_basic_mixed_tree (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            Root body
            ** (skg aliasFolder) aliases
            *** (skg alias) first alias
            *** (skg alias) second alias
            ** (skg node (gnid child) (skgRepos (nodeRepo main))) child
            *** (skg node (gnid grandchild) (skgRepos (nodeRepo main))) grandchild
            ** (skg node (gnid independent) (skgRepos (nodeRepo main)) noClaim) independent
            ** (skg subscribeeFolder)
            *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected) s1
            ** (skg overriddenFolder)
            *** (skg node (gnid o1) (skgRepos (nodeRepo main)) writeProtected) o1
            * (skg node (gnid explicit) (skgRepos (nodeRepo main))) explicit
            ** (skg aliasFolder) aliases
            ** (skg subscribeeFolder)
            ** (skg overriddenFolder)
            * (skg node (gnid doomed) (skgRepos (nodeRepo main)) (editRequest delete)) doomed
            "};
      let (plan, nodeMerge_acquisitions)
        : (NonmergeSavePlan, Vec<(ID, ID)>) =
        extract_nonmergeSavePlan_locally (
          &viewforest_as_written (input), config, None)?;
      assert_eq!(
        save_gnids (&plan . node_instructions),
        vec![ ID::from ("root"), ID::from ("child"),
              ID::from ("grandchild"), ID::from ("independent"),
              ID::from ("explicit"), ID::from ("doomed") ]);
      assert!( matches!(
        plan . node_instructions . last(),
        Some (NodeInstruction::Delete (DeleteNode { gnid, .. }))
          if gnid == &ID::from ("doomed") ));
      { let root : &Graphnode =
          saved_node_by_gnid (&plan . node_instructions, "root");
        assert_eq!( root . body,
                    Some ("Root body" . to_string()) );
        assert_eq!( members_of (&root . contains), vec![ID::from ("child")] );
        assert_eq!( members_msv (&root . aliases),
                    MSV::Specified (vec![
                      "first alias" . to_string(),
                      "second alias" . to_string()]) );
        assert_eq!( members_msv (&root . subscribesTo),
                    MSV::Specified (vec![ID::from ("s1")]) );
        assert_eq!( members_msv (&root . overrides),
                    MSV::Specified (vec![ID::from ("o1")]) ); }
      { let explicit : &Graphnode =
          saved_node_by_gnid (&plan . node_instructions, "explicit");
        // Present-but-empty folders are explicit emptiness.
        assert_eq!( explicit . aliases,
                    MSV::Specified (vec![]) );
        assert_eq!( explicit . subscribesTo,
                    MSV::Specified (vec![]) );
        assert_eq!( explicit . overrides,
                    MSV::Specified (vec![]) ); }
      assert!( plan . skgrepo_moves . is_empty() );
      assert!( nodeMerge_acquisitions . is_empty() );
      Ok (( )) }

async fn pipeline_subscribee_hiderels (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
            ** (skg subscribeeFolder)
            *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) subscribee-e
            **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config) . await?;
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &forest, config, None)?;
      assert_eq!(
        members_msv (&saved_node_by_gnid (&plan . node_instructions, "r")
          . hidesFromSubs),
        MSV::Specified (vec![ID::from ("e1")]));
      assert!(
        ! save_gnids (&plan . node_instructions)
          . contains (&ID::from ("e")),
        "subscribee-as-such should not produce a SaveNode" );
      Ok (( )) }

async fn pipeline_write_protected_folder_member_edits (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // This tests the new recursion surface: editable members of
  // write-protected folders (and their subtrees) save their own edits.
      let input : &str =
        indoc! {"
            * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
            ** (skg subscriberFolder)
            *** (skg node (gnid intruder) (skgRepos (nodeRepo main))) intruder
            **** (skg node (gnid intruder-child) (skgRepos (nodeRepo main))) intruder child
            ** (skg hiddenFolder)
            *** (skg node (gnid lurker) (skgRepos (nodeRepo main))) lurker
            "};
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &viewforest_as_written (input), config, None)?;
      assert_eq!(
        save_gnids (&plan . node_instructions),
        vec![ ID::from ("recorder"), ID::from ("intruder"),
              ID::from ("intruder-child"), ID::from ("lurker") ]);
      assert_eq!(
        members_of (&saved_node_by_gnid (&plan . node_instructions, "recorder") . contains),
        Vec::<ID>::new() );
      assert_eq!(
        members_of (&saved_node_by_gnid (&plan . node_instructions, "intruder") . contains),
        vec![ID::from ("intruder-child")] );
      Ok (( )) }

async fn pipeline_restricted_subtree (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg tombstone)
            *** (skg node (gnid stowaway) (skgRepos (nodeRepo main))) stowaway
            "};
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &viewforest_as_written (input), config, None)?;
      assert_eq!(
        save_gnids (&plan . node_instructions),
        // stowaway is on the new recursion surface.
        vec![ ID::from ("root"), ID::from ("stowaway") ]);
      assert_eq!(
        members_of (&saved_node_by_gnid (&plan . node_instructions, "root") . contains),
        Vec::<ID>::new(),
        "the tombstone emits no contains membership; under a \
         restricted set the weave restores a restricted node's from disk \
         (see tests/repo_sets.rs)" );
      Ok (( )) }

async fn pipeline_ghost_subtree (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid root) (skgRepos (nodeRepo main))) root
            ** (skg ghost (gnid fading) (skgRepos (nodeRepo main)) noClaim) fading
            *** (skg node (gnid underneath) (skgRepos (nodeRepo main))) underneath
            "};
      let forest : ViewForest = viewforest_as_written (input);
      let (plan, _) =
        extract_nonmergeSavePlan_locally (
          &forest, config, None)?;
      assert_eq!(
        save_gnids (&plan . node_instructions),
        // underneath is on the new recursion surface; the ghost
        // itself emits nothing and is not content of its parent.
        vec![ ID::from ("root"), ID::from ("underneath") ]);
      assert_eq!(
        members_of (&saved_node_by_gnid (&plan . node_instructions, "root") . contains),
        Vec::<ID>::new() );
      Ok (( )) }

async fn pipeline_nodeMerge_requests (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid 1) (skgRepos (nodeRepo main)) (editRequest (merge 2))) 1
            ** (skg node (gnid 11) (skgRepos (nodeRepo main))) 11
            ** (skg node (gnid 12) (skgRepos (nodeRepo main))) 12
            ** (skg node (gnid overlap) (skgRepos (nodeRepo main))) overlap
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config) . await?;
      let (_plan, nodeMerge_acquisitions) =
        extract_nonmergeSavePlan_locally (
          &forest, config, None)?;
      assert_eq!( nodeMerge_acquisitions,
                  vec![ (ID::from ("1"), ID::from ("2")) ]);
      let nodeMerges : Vec<NodeMerge> =
        nodeMerge_instructions_from_pairs (
          &nodeMerge_acquisitions,
          &graph_handle_from_config (config)? . load_full (),
          config)?;
      assert_eq!( nodeMerges . len(), 1 );
      assert_eq!( nodeMerges [0] . acquirer_gnid(), &ID::from ("1") );
      assert_eq!( nodeMerges [0] . acquiree_gnid(), &ID::from ("2") );
      assert_eq!( nodeMerges [0] . acquiree_text_preserver . 0 . title,
                  "MERGED: 2" );
      Ok (( )) }

async fn pipeline_rejects_text_claim_mismatch (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
      let input : &str =
        indoc! {"
            * (skg node (gnid r) (skgRepos (nodeRepo owned))) r
            ** (skg subscribeeFolder)
            *** (skg node (gnid e) (skgRepos (nodeRepo foreign))) changed title
            **** (skg node (gnid e2) (skgRepos (nodeRepo foreign))) e2
            "};
      let forest : ViewForest =
        placed_forest_from_org_with_disk (
          input, config) . await?;
      let error : String =
        extract_nonmergeSavePlan_locally (
          &forest, config , None)
        . err() . expect ("the title edit should be rejected")
        . to_string();
      assert!( error . contains ("Cannot edit title/body") );
      Ok (( )) }
