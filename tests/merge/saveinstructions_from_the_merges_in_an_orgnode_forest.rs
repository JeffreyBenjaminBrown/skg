use indoc::indoc;
use skg::merge::saveinstructions_from_the_merges_in_an_orgnode_forest;
use skg::save::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::{ID, SaveInstruction};
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
                let instructions =
                    saveinstructions_from_the_merges_in_an_orgnode_forest(&trees, config, driver)
                        .await?;

                // Should produce exactly 3 SaveInstructions
                assert_eq!(
                    instructions.len(),
                    3,
                    "Expected 3 SaveInstructions (MERGED node, acquirer update, acquiree deletion)"
                );

                // Collect instructions by their characteristics
                let mut merged_node_instruction: Option<&SaveInstruction> = None;
                let mut node1_instruction: Option<&SaveInstruction> = None;
                let mut node2_instruction: Option<&SaveInstruction> = None;

                for instruction in &instructions {
                    let (node, action) = instruction;

                    if action.toDelete {
                        // This should be node 2 (acquiree marked for deletion)
                        assert!(
                            node2_instruction.is_none(),
                            "Found multiple deletion instructions"
                        );
                        node2_instruction = Some(instruction);
                    } else if node.title.starts_with("MERGED:") {
                        // This is the synthetic MERGED node
                        assert!(
                            merged_node_instruction.is_none(),
                            "Found multiple MERGED nodes"
                        );
                        merged_node_instruction = Some(instruction);
                    } else if node.ids.contains(&ID::from("1")) {
                        // This is the updated acquirer (node 1)
                        node1_instruction = Some(instruction);
                    }
                }

                // Verify we found all three
                assert!(
                    merged_node_instruction.is_some(),
                    "Missing MERGED node instruction"
                );
                assert!(
                    node1_instruction.is_some(),
                    "Missing node 1 (acquirer) instruction"
                );
                assert!(
                    node2_instruction.is_some(),
                    "Missing node 2 (acquiree) instruction"
                );

                // Verify MERGED node properties
                let (merged_node, merged_action) = merged_node_instruction.unwrap();
                assert_eq!(merged_node.title, "MERGED: 2", "MERGED node title incorrect");
                assert_eq!(
                    merged_node.body,
                    Some("2 body".to_string()),
                    "MERGED node body incorrect"
                );
                assert!(!merged_action.indefinitive, "MERGED node should not be indefinitive");
                assert!(!merged_action.toDelete, "MERGED node should not be marked for deletion");
                assert_eq!(merged_node.contains.len(), 0, "MERGED node should have no contents");
                assert!(
                    merged_node.subscribes_to.is_none(),
                    "MERGED node should have no subscriptions"
                );
                assert!(
                    merged_node.hides_from_its_subscriptions.is_none(),
                    "MERGED node should hide nothing"
                );
                assert!(
                    merged_node.overrides_view_of.is_none(),
                    "MERGED node should override nothing"
                );

                // Verify node 1 (acquirer) properties
                let (node1, action1) = node1_instruction.unwrap();
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

                // Verify node 1's contents: [MERGED node ID, original node 1 contents, node 2 contents]
                // In this case: [MERGED node ID] (since both started with empty contents)
                let merged_id = &merged_node.ids[0];
                assert_eq!(
                    node1.contains.len(),
                    1,
                    "Node 1 should contain exactly the MERGED node"
                );
                assert_eq!(
                    &node1.contains[0], merged_id,
                    "Node 1's first content should be the MERGED node"
                );

                // Verify node 2 (acquiree) deletion instruction
                let (node2, action2) = node2_instruction.unwrap();
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
