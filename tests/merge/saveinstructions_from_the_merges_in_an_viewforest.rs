use indoc::indoc;
use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_viewforest;
use skg::types::tree::forest::ViewForest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, MSV, SourceName, members_msv};
use skg::types::save::SaveNode;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use std::error::Error;

#[test]
fn test_single_merge() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-merge-single",
    "tests/merge/saveinstructions_from_the_merges_in_an_viewforest/fixtures",
    "/tmp/tantivy-test-merge-single",
    |config, driver, _tantivy| {
      Box::pin(async move {
        // Create a viewforest with node 1 requesting to merge node 2
        let input = indoc! {"
          * (skg (node (id 1) (source main) (editRequest (merge 2)))) 1
        "};

        let unchecked_viewforest = org_to_uninterpreted_nodes (input)?. 0;
        let viewforest = maybePlaced_to_placed_tree (unchecked_viewforest)?;
        let nodeMerge_instructions =
        nodeMerge_instructions_from_viewforest(
         &ViewForest::from_internal_tree (viewforest), config, driver)
        . await?;

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
              (node2_id, node2_source) )
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
          acquiree_text_preserver . subscribes_to,
          MSV::Specified(vec![]),
          "acquiree_text_preserver should have empty subscriptions" );
        assert_eq!(
          acquiree_text_preserver . hides_from_its_subscriptions,
          MSV::Specified(vec![]),
          "acquiree_text_preserver should have empty hides" );
        assert_eq!(
          acquiree_text_preserver . overrides_view_of,
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
        let acquiree_text_preserver_id = &acquiree_text_preserver . pid;
        assert_eq!(
          node1 . contains . len(),
          1,
          "Node 1 should contain exactly the acquiree_text_preserver"
        );
        assert_eq!(
          &node1 . contains[0] . member, acquiree_text_preserver_id,
          "Node 1's first content should be the acquiree_text_preserver"
        );

        // Verify node 2 (acquiree) deletion instruction
        assert_eq!( node2_id, &ID::from ("2"),
                    "Node 2 should have ID '2'" );
        assert_eq!( node2_source, &SourceName::from ("main"),
                    "Node 2 should have source 'main'" );
        Ok(())
      })
    },
  )
}
