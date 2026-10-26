// cargo test contradictory_instructions

use skg::dbs::in_rust_graph::InRustGraph;
use indoc::indoc;
use skg::types::tree::forest::ViewForest;
use skg::types::misc::ID;
use skg::types::errors::BufferValidationError;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::find_inconsistent_instructions;
use skg::test_utils::run_with_test_stores;
use std::error::Error;

#[test]
fn test_find_inconsistent_toDelete_instructions() {
  // Test case with consistent toDelete instructions (no conflicts)
  let input_consistent: &str =
    indoc! {"
            * (skg node (gnid node1) (editRequest delete)) first node
            * (skg node (gnid node2)) second node
            * (skg node (gnid node1) (editRequest delete)) duplicate of first node (same delete instruction)
        "};

  let viewforest_consistent: ViewForest =
    org_to_viewforest (input_consistent, &InRustGraph::new ()) . unwrap() . 0;
  let (inconsistent_ids_consistent, _) =
    find_inconsistent_instructions (&viewforest_consistent);
  assert_eq!(inconsistent_ids_consistent . len(), 0, "Should have no inconsistent IDs when all nodes with same ID have same toDelete value");

  // Test case with inconsistent toDelete instructions (conflicts)
  let input_inconsistent: &str =
    indoc! {"
            * (skg node (gnid conflict1) (editRequest delete)) first conflicting node (toDelete=true)
            * (skg node (gnid normal)) normal node
            * (skg node (gnid conflict1)) duplicate node but toDelete=false
            * (skg node (gnid conflict2) (editRequest delete)) another conflict start
            * (skg node (gnid conflict2)) another conflict end
        "};

  let viewforest_inconsistent: ViewForest =
    org_to_viewforest (input_inconsistent, &InRustGraph::new ()) . unwrap() . 0;
  let (inconsistent_gnids, _) = find_inconsistent_instructions (&viewforest_inconsistent);

  assert_eq!(inconsistent_gnids . len(), 2, "Should find exactly 2 conflicting IDs");
  assert!(inconsistent_gnids . iter () . any ( |(gnid, _)| gnid == &ID::from ("conflict1")), "Should include conflict1 ID");
  assert!(inconsistent_gnids . iter () . any ( |(gnid, _)| gnid == &ID::from ("conflict2")), "Should include conflict2 ID");

  // Test case with nodes that have no IDs (should be skipped)
  let input_no_gnids: &str =
    indoc! {"
            * (skg node (editRequest delete)) node without id (should be skipped)
            * regular node without any metadata
            * (skg node (gnid valid_node)) only node with id
        "};

  let viewforest_no_gnids: ViewForest =
    org_to_viewforest (input_no_gnids, &InRustGraph::new ()) . unwrap() . 0;
  let (inconsistent_no_gnids, _) = find_inconsistent_instructions (&viewforest_no_gnids);
  assert_eq!(inconsistent_no_gnids . len(), 0, "Should have no conflicts when only one node has each ID");

  // Test empty viewforest (just BufferRoot, no tree roots)
  let empty_viewforest: ViewForest =
    ViewForest::new();
  let (inconsistent_empty, _) =
    find_inconsistent_instructions (&empty_viewforest);
  assert_eq!(inconsistent_empty . len(), 0,
             "Should have no conflicts in empty viewforest");
}

#[test]
fn test_multiple_defining_containers() -> Result<(), Box<dyn Error>> {
  run_with_test_stores(
    "skg-test-validate-multiple-def",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-multiple-def",
    |config, _tantivy| Box::pin(async move {
      // Test input with multiple nodes having the same ID and write-protected=false
      let input_with_multiple_defining_containers: &str =
        indoc! {"
                * (skg node (gnid duplicate)) First defining container
                Regular node with shared ID
                * (skg node (gnid duplicate)) Second defining container
                Another regular node with the same ID
                * (skg node (gnid duplicate) writeProtected) Repeated node (not defining)
                This one is ok because writeProtected=true
                * (skg node (gnid duplicate) writeProtected) Might contain more (not defining)
                This one is also ok because writeProtected=true
                * (skg node (gnid unique)) Unique node
                This one is fine
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input_with_multiple_defining_containers, &InRustGraph::new ()) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let multiple_defining_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes (_, _)))
        . collect();

      assert_eq!(multiple_defining_errors . len(), 1,
                 "Should find exactly 1 Multiple_Defining_Viewnodes error for the problematic ID");

      // Check that the error points to the correct ID
      if let BufferValidationError::Multiple_Defining_Viewnodes (gnid, _) = multiple_defining_errors[0] {
        assert_eq!(gnid . 0, "duplicate",
                   "Multiple_Defining_Viewnodes error should come from the duplicate ID"); }
      Ok(( )) } )) }
