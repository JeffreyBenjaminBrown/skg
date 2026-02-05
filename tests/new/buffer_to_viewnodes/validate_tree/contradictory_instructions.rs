// cargo test contradictory_instructions

use ego_tree::Tree;
use indoc::indoc;
use skg::types::unchecked_viewnode::{UncheckedViewNode, unchecked_forest_root_viewnode};
use skg::types::misc::ID;
use skg::types::errors::BufferValidationError;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::test_utils::run_with_test_db;
use std::error::Error;

#[test]
fn test_find_inconsistent_toDelete_instructions() {
  // Test case with consistent toDelete instructions (no conflicts)
  let input_consistent: &str =
    indoc! {"
            * (skg (node (id node1) (editRequest delete))) first node
            * (skg (node (id node2))) second node
            * (skg (node (id node1) (editRequest delete))) duplicate of first node (same delete instruction)
        "};

  let forest_consistent: Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes(input_consistent).unwrap().0;
  let (inconsistent_ids_consistent, _, _) =
    find_inconsistent_instructions(&forest_consistent);
  assert_eq!(inconsistent_ids_consistent.len(), 0, "Should have no inconsistent IDs when all nodes with same ID have same toDelete value");

  // Test case with inconsistent toDelete instructions (conflicts)
  let input_inconsistent: &str =
    indoc! {"
            * (skg (node (id conflict1) (editRequest delete))) first conflicting node (toDelete=true)
            * (skg (node (id normal))) normal node
            * (skg (node (id conflict1))) duplicate node but toDelete=false
            * (skg (node (id conflict2) (editRequest delete))) another conflict start
            * (skg (node (id conflict2))) another conflict end
        "};

  let forest_inconsistent: Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes(input_inconsistent).unwrap().0;
  let (inconsistent_ids, _, _) = find_inconsistent_instructions(&forest_inconsistent);

  assert_eq!(inconsistent_ids.len(), 2, "Should find exactly 2 conflicting IDs");
  assert!(inconsistent_ids.contains(&ID::from("conflict1")), "Should include conflict1 ID");
  assert!(inconsistent_ids.contains(&ID::from("conflict2")), "Should include conflict2 ID");

  // Test case with nodes that have no IDs (should be skipped)
  let input_no_ids: &str =
    indoc! {"
            * (skg (node (editRequest delete))) node without id (should be skipped)
            * regular node without any metadata
            * (skg (node (id valid_node))) only node with id
        "};

  let forest_no_ids: Tree<UncheckedViewNode> =
    org_to_uninterpreted_nodes(input_no_ids).unwrap().0;
  let (inconsistent_no_ids, _, _) = find_inconsistent_instructions(&forest_no_ids);
  assert_eq!(inconsistent_no_ids.len(), 0, "Should have no conflicts when only one node has each ID");

  // Test empty forest (just BufferRoot, no tree roots)
  let empty_forest: Tree<UncheckedViewNode> =
    Tree::new(unchecked_forest_root_viewnode());
  let (inconsistent_empty, _, _) =
    find_inconsistent_instructions(&empty_forest);
  assert_eq!(inconsistent_empty.len(), 0,
             "Should have no conflicts in empty forest");
}

#[test]
fn test_multiple_defining_containers() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-multiple-def",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-multiple-def",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with multiple nodes having the same ID and indefinitive=false
      let input_with_multiple_defining_containers: &str =
        indoc! {"
                * (skg (node (id duplicate))) First defining container
                Regular node with shared ID
                * (skg (node (id duplicate))) Second defining container
                Another regular node with the same ID
                * (skg (node (id duplicate) indefinitive)) Repeated node (not defining)
                This one is ok because indefinitive=true
                * (skg (node (id duplicate) indefinitive)) Might contain more (not defining)
                This one is also ok because indefinitive=true
                * (skg (node (id unique))) Unique node
                This one is fine
            "};

      let forest: Tree<UncheckedViewNode> =
        org_to_uninterpreted_nodes(input_with_multiple_defining_containers).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let multiple_defining_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes(_)))
        .collect();

      assert_eq!(multiple_defining_errors.len(), 1,
                 "Should find exactly 1 Multiple_Defining_Viewnodes error for the problematic ID");

      // Check that the error points to the correct ID
      if let BufferValidationError::Multiple_Defining_Viewnodes(id) = multiple_defining_errors[0] {
        assert_eq!(id.0, "duplicate",
                   "Multiple_Defining_Viewnodes error should come from the duplicate ID"); }
      Ok(( )) } )) }
