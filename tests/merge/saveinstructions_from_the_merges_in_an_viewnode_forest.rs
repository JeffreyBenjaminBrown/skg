use indoc::indoc;
use skg::merge::mergeInstructionTriple::instructiontriples_from_the_merges_in_an_viewnode_forest;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::misc::{ID, SourceName};
use skg::types::save::SaveNode;
use skg::types::unchecked_viewnode::unchecked_to_checked_tree;
use std::error::Error;

#[test]
fn test_single_merge() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-merge-single",
    "tests/merge/saveinstructions_from_the_merges_in_an_viewnode_forest/fixtures",
    "/tmp/tantivy-test-merge-single",
    |config, driver, _tantivy| {
      Box::pin(async move {
        // Create a forest with node 1 requesting to merge node 2
        let input = indoc! {"
          * (skg (node (id 1) (source main) (editRequest (merge 2)))) 1
        "};

        let unchecked_forest = org_to_uninterpreted_nodes(input)?.0;
        let forest = unchecked_to_checked_tree(unchecked_forest)?;
        let merge_instructions =
        instructiontriples_from_the_merges_in_an_viewnode_forest(
         &forest, config, driver)
        .await?;

        // Should produce exactly 1 MergeInstructionTriple
        assert_eq!(
          merge_instructions.len(),
          1,
          "Expected 1 MergeInstructionTriple"
        );

        // Get the single merge instruction
        let merge = &merge_instructions[0];
        let ( acquiree_text_preserver,
              node1,
              (node2_id, node2_source) )
          = merge.targets_from_merge();

        // Verify acquiree_text_preserver properties
       assert_eq!(acquiree_text_preserver.title,
             "MERGED: 2", "acquiree_text_preserver title incorrect");
       assert_eq!( acquiree_text_preserver.body,
             Some("2 body".to_string()),
             "acquiree_text_preserver body incorrect" );
       assert!(
        matches!(&merge.acquiree_text_preserver,
             SaveNode(_)),
        "acquiree_text_preserver should be SaveNode");
       assert_eq!(acquiree_text_preserver.contains.as_ref().unwrap().len(),
             0, "acquiree_text_preserver should have no contents");
        assert_eq!(
          acquiree_text_preserver.subscribes_to,
          Some(vec![]),
          "acquiree_text_preserver should have empty subscriptions" );
        assert_eq!(
          acquiree_text_preserver.hides_from_its_subscriptions,
          Some(vec![]),
          "acquiree_text_preserver should have empty hides" );
        assert_eq!(
          acquiree_text_preserver.overrides_view_of,
          Some(vec![]),
          "acquiree_text_preserver should have empty overrides" );

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
       assert!(
        matches!(&merge.updated_acquirer,
             SaveNode(_)),
        "Node 1 should be SaveNode");

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
        assert_eq!( node2_id, &ID::from("2"),
                    "Node 2 should have ID '2'" );
        assert_eq!( node2_source, &SourceName::from("main"),
                    "Node 2 should have source 'main'" );
        Ok(())
      })
    },
  )
}
