use indoc::indoc;
use skg::merge::instructiontriples_from_the_merges_in_an_orgnode_forest;
use skg::save::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::ID;
use std::error::Error;

#[test]
fn test_single_merge() -> Result<(), Box<dyn Error>> {
    run_with_test_db(
        "skg-test-merge-single",
        "tests/merge/saveinstructions_from_the_merges_in_an_orgnode_forest/fixtures",
        "/tmp/tantivy-test-merge-single",
        |config, driver| {
            Box::pin(async move {
                // Create a forest with node 1 requesting to merge node 2
                let input = indoc! {"
                    * (skg (id 1) (code (requests (merge 2)))) 1
                "};

                let trees = org_to_uninterpreted_nodes(input)?;
                let merge_instructions =
                    instructiontriples_from_the_merges_in_an_orgnode_forest(&trees, config, driver)
                        .await?;

                // Should produce exactly 1 MergeInstructionTriple
                assert_eq!(
                    merge_instructions.len(),
                    1,
                    "Expected 1 MergeInstructionTriple"
                );

                // Get the single merge instruction
                let merge = &merge_instructions[0];
                let (acquiree_text_preserver, acquiree_text_preserver_action) = &merge.acquiree_text_preserver;
                let (node1, action1) = &merge.updated_acquirer;
                let (node2, action2) = &merge.deleted_acquiree;

                // Verify acquiree_text_preserver properties
                assert_eq!(acquiree_text_preserver.title, "MERGED_2", "acquiree_text_preserver title incorrect");
                assert_eq!(
                    acquiree_text_preserver.body,
                    Some("2 body".to_string()),
                    "acquiree_text_preserver body incorrect"
                );
                assert!(!acquiree_text_preserver_action.indefinitive, "acquiree_text_preserver should not be indefinitive");
                assert!(!acquiree_text_preserver_action.toDelete, "acquiree_text_preserver should not be marked for deletion");
                assert_eq!(acquiree_text_preserver.contains.as_ref().unwrap().len(), 0, "acquiree_text_preserver should have no contents");
                assert_eq!(
                    acquiree_text_preserver.subscribes_to,
                    Some(vec![]),
                    "acquiree_text_preserver should have empty subscriptions"
                );
                assert_eq!(
                    acquiree_text_preserver.hides_from_its_subscriptions,
                    Some(vec![]),
                    "acquiree_text_preserver should have empty hides"
                );
                assert_eq!(
                    acquiree_text_preserver.overrides_view_of,
                    Some(vec![]),
                    "acquiree_text_preserver should have empty overrides"
                );

                // Verify node 1 (acquirer) properties
                assert!(
                    node1.ids.contains(&ID::from("1")),
                    "Node 1 should have ID '1'"
                );
                assert!(
                    node1.ids.contains(&ID::from("2")),
                    "Node 1 should have acquired ID '2'"
                );
                assert_eq!(
                    node1.ids.len(),
                    2,
                    "Node 1 should have exactly 2 IDs: [1, 2]"
                );
                assert!(!action1.indefinitive, "Node 1 should not be indefinitive");
                assert!(!action1.toDelete, "Node 1 should not be marked for deletion");

                // Verify node 1's contents: [acquiree_text_preserver ID, original node 1 contents, node 2 contents]
                // In this case: [acquiree_text_preserver ID] (since both started with empty contents)
                let acquiree_text_preserver_id = &acquiree_text_preserver.ids[0];
                assert_eq!(
                    node1.contains.as_ref().unwrap().len(),
                    1,
                    "Node 1 should contain exactly the acquiree_text_preserver"
                );
                assert_eq!(
                    &node1.contains.as_ref().unwrap()[0], acquiree_text_preserver_id,
                    "Node 1's first content should be the acquiree_text_preserver"
                );

                // Verify node 2 (acquiree) deletion instruction
                assert!(
                    node2.ids.contains(&ID::from("2")),
                    "Node 2 should have ID '2'"
                );
                assert!(!action2.indefinitive, "Node 2 deletion should not be indefinitive");
                assert!(action2.toDelete, "Node 2 should be marked for deletion");

                // Verify node 2 has its original data from disk
                assert_eq!(node2.title, "2", "Node 2 title should be from disk");
                assert_eq!(
                    node2.body,
                    Some("2 body".to_string()),
                    "Node 2 body should be from disk"
                );

                Ok(())
            })
        },
    )
}
