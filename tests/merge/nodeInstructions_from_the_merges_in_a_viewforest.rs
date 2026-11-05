// The save pipeline turns a merge request into a pair in its SavePlan
// ('buffer_to_validated_saveplan_in_graph'), and the pair into a
// NodeMerge triple only while preparing the save
// ('prepare_save_under_mutation_gate'). This tests both steps.

use indoc::indoc;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::from_text::buffer_to_validated_saveplan_in_graph;
use skg::save::{PreparedSave, prepare_save_under_mutation_gate};
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::types::misc::{ID, MSV, SkgrepoName, members_msv};
use skg::types::save::{NodeMerge, SaveNode, SavePlan};
use std::collections::HashSet;
use std::error::Error;

#[test]
fn test_single_merge() -> Result<(), Box<dyn Error>> {
  run_with_test_stores(
    "skg-test-merge-single",
    "tests/merge/nodeInstructions_from_the_merges_in_a_viewforest/fixtures",
    "/tmp/tantivy-test-merge-single",
    |config, _tantivy| {
      Box::pin(async move {
        // A buffer with node 1 requesting to merge node 2
        let input : &str = indoc! {"
          * (skg node (gnid 1) (skgRepos (nodeRepo main)) (editRequest (merge 2))) 1
          1 body
        "};
        let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
        let unrestricted : SkgrepoRestriction =
          SkgrepoRestriction::unrestricted (config);
        let save_plan : SavePlan =
          buffer_to_validated_saveplan_in_graph (
            input, &graph . load_full (), config, &unrestricted ) ? . 1;
        assert_eq! (
          save_plan . nodeMerge_pairs,
          vec! [ (ID::from ("1"), ID::from ("2")) ],
          "The plan should carry the merge as (acquirer pid, acquiree pid)" );
        let prepared : PreparedSave = prepare_save_under_mutation_gate (
          save_plan . node_instructions,
          &save_plan . nodeMerge_pairs,
          &save_plan . skgrepo_moves,
          config, &graph, &HashSet::new (), &unrestricted ) ?;
        let nodeMerge_instructions : &[NodeMerge] = prepared . nodeMerges ();

        // Should produce exactly 1 MergeInstructionTriple
        assert_eq!(
          nodeMerge_instructions . len(),
          1,
          "Expected 1 MergeInstructionTriple"
        );

        // Get the single merge instruction
        let merge = &nodeMerge_instructions[0];
        let ( acquiree_text_preserver,
              node1,
              (node2_gnid, node2_skgrepo) )
          = merge . targets_from_nodeMerge();

        // Verify acquiree_text_preserver properties
       assert_eq!(acquiree_text_preserver . title,
             "MERGED: 2", "acquiree_text_preserver title incorrect");
       assert_eq!( acquiree_text_preserver . body,
             Some("2 body" . to_string()),
             "acquiree_text_preserver body incorrect" );
       assert!(
        matches!(&merge . acquiree_text_preserver,
             SaveNode (_)),
        "acquiree_text_preserver should be SaveNode");
       assert_eq!(acquiree_text_preserver . contains . len(),
             0, "acquiree_text_preserver should have no contents");
        assert_eq!(
          acquiree_text_preserver . subscribesTo,
          MSV::Specified(vec![]),
          "acquiree_text_preserver should have empty subscriptions" );
        assert_eq!(
          acquiree_text_preserver . hidesFromSubs,
          MSV::Specified(vec![]),
          "acquiree_text_preserver should have empty hides" );
        assert_eq!(
          acquiree_text_preserver . overrides,
          MSV::Specified(vec![]),
          "acquiree_text_preserver should have empty overrides" );

        // Verify node 1 (acquirer) properties
        assert_eq!(
          &node1 . pid, &ID::from ("1"),
          "Node 1 should have PID '1'"
        );
        assert_eq!(
          node1 . extra_ids . len(),
          1,
          "Node 1 should have exactly 1 extra_id"
        );
        assert!(
          node1 . extra_ids . contains(&ID::from ("2")),
          "Node 1 should have acquired extra_id '2'"
        );

        // Aliases are unioned on merge (parallel to extra_ids), so the
        // merged node stays findable by the acquiree's old aliases.
        let node1_aliases : Vec<String> =
          members_msv (&node1 . aliases) . into_vec ();
        assert!(
          node1_aliases . contains(&"one-alias" . to_string()),
          "Node 1 should keep its own alias 'one-alias', got {:?}",
          node1_aliases );
        assert!(
          node1_aliases . contains(&"two-alias" . to_string()),
          "Node 1 should acquire node 2's alias 'two-alias', got {:?}",
          node1_aliases );
       assert!(
        matches!(&merge . updated_acquirer,
             SaveNode (_)),
        "Node 1 should be SaveNode");

        // Verify node 1's contents: [acquiree_text_preserver ID, original node 1 contents, node 2 contents]
        // In this case: [acquiree_text_preserver ID] (since both started with empty contents)
        let acquiree_text_preserver_gnid = &acquiree_text_preserver . pid;
        assert_eq!(
          node1 . contains . len(),
          1,
          "Node 1 should contain exactly the acquiree_text_preserver"
        );
        assert_eq!(
          &node1 . contains[0] . member, acquiree_text_preserver_gnid,
          "Node 1's first content should be the acquiree_text_preserver"
        );

        // Verify node 2 (acquiree) deletion instruction
        assert_eq!( node2_gnid, &ID::from ("2"),
                    "Node 2 should have ID '2'" );
        assert_eq!( node2_skgrepo, &SkgrepoName::from ("main"),
                    "Node 2 should have repo 'main'" );
        Ok(())
      })
    },
  )
}
