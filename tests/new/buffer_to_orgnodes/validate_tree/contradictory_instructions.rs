// cargo test contradictory_instructions

use ego_tree::Tree;
use indoc::indoc;
use skg::types::{OrgNode, ID, Buffer_Cannot_Be_Saved};
use skg::save::{org_to_uninterpreted_nodes, find_inconsistent_instructions, find_buffer_errors_for_saving};
use skg::test_utils::run_with_test_db;
use std::error::Error;

#[test]
fn test_find_inconsistent_toDelete_instructions() {
  // Test case with consistent toDelete instructions (no conflicts)
  let input_consistent: &str =
    indoc! {"
            * (skg (id node1) (code toDelete)) first node
            * (skg (id node2)) second node
            * (skg (id node1) (code toDelete)) duplicate of first node (same delete instruction)
        "};

  let trees_consistent: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input_consistent).unwrap();
  let (inconsistent_ids_consistent, _) =
    find_inconsistent_instructions(&trees_consistent);
  assert_eq!(inconsistent_ids_consistent.len(), 0, "Should have no inconsistent IDs when all nodes with same ID have same toDelete value");

  // Test case with inconsistent toDelete instructions (conflicts)
  let input_inconsistent: &str =
    indoc! {"
            * (skg (id conflict1) (code toDelete)) first conflicting node (toDelete=true)
            * (skg (id normal)) normal node
            * (skg (id conflict1)) duplicate node but toDelete=false
            * (skg (id conflict2) (code toDelete)) another conflict start
            * (skg (id conflict2)) another conflict end
        "};

  let trees_inconsistent: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input_inconsistent).unwrap();
  let (inconsistent_ids, _) = find_inconsistent_instructions(&trees_inconsistent);

  assert_eq!(inconsistent_ids.len(), 2, "Should find exactly 2 conflicting IDs");
  assert!(inconsistent_ids.contains(&ID::from("conflict1")), "Should include conflict1 ID");
  assert!(inconsistent_ids.contains(&ID::from("conflict2")), "Should include conflict2 ID");

  // Test case with nodes that have no IDs (should be skipped)
  let input_no_ids: &str =
    indoc! {"
            * (skg (code toDelete)) node without id (should be skipped)
            * regular node without any metadata
            * (skg (id valid_node)) only node with id
        "};

  let trees_no_ids: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input_no_ids).unwrap();
  let (inconsistent_no_ids, _) = find_inconsistent_instructions(&trees_no_ids);
  assert_eq!(inconsistent_no_ids.len(), 0, "Should have no conflicts when only one node has each ID");

  // Test empty forest
  let empty_trees: Vec<Tree<OrgNode>> = Vec::new();
  let (inconsistent_empty, _) = find_inconsistent_instructions(&empty_trees);
  assert_eq!(inconsistent_empty.len(), 0, "Should have no conflicts in empty forest");
}

#[test]
fn test_multiple_defining_containers() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-multiple-def",
    "tests/merge/merge_nodes_in_graph/fixtures",
    "/tmp/tantivy-test-validate-multiple-def",
    |config, driver| Box::pin(async move {
      // Test input with multiple nodes having the same ID and both repeated=false, indefinitive=false
      let input_with_multiple_defining_containers: &str =
        indoc! {"
                * (skg (id duplicate)) First defining container
                Regular node with shared ID
                * (skg (id duplicate)) Second defining container
                Another regular node with the same ID
                * (skg (id duplicate) (code repeated)) Repeated node (not defining)
                This one is ok because repeated=true
                * (skg (id duplicate) (code indefinitive)) Might contain more (not defining)
                This one is ok because indefinitive=true
                * (skg (id unique)) Unique node
                This one is fine
            "};

      let trees: Vec<Tree<OrgNode>> =
        org_to_uninterpreted_nodes(input_with_multiple_defining_containers).unwrap();
      let errors: Vec<Buffer_Cannot_Be_Saved> =
        find_buffer_errors_for_saving(&trees, config, driver).await?;

      let multiple_defining_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
        .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(_)))
        .collect();

      assert_eq!(multiple_defining_errors.len(), 1,
                 "Should find exactly 1 Multiple_DefiningContainers error for the problematic ID");

      // Check that the error points to the correct ID
      if let Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(id) = multiple_defining_errors[0] {
        assert_eq!(id.0, "duplicate",
                   "Multiple_DefiningContainers error should come from the duplicate ID");
      }
      Ok(())
    })
  )
}
