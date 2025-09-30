use indoc::indoc;
use skg::new::{org_to_uninterpreted_nodes2, find_inconsistent_toDelete_instructions};
use skg::types::{OrgNode2, ID};
use ego_tree::Tree;

#[test]
fn test_find_inconsistent_toDelete_instructions() {
  // Test case with consistent toDelete instructions (no conflicts)
  let input_consistent: &str =
    indoc! {"
            * <skg<id:node1,toDelete>> first node
            * <skg<id:node2>> second node
            * <skg<id:node1,toDelete>> duplicate of first node (same delete instruction)
        "};

  let trees_consistent: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input_consistent).unwrap();
  let inconsistent_ids_consistent = find_inconsistent_toDelete_instructions(&trees_consistent);
  assert_eq!(inconsistent_ids_consistent.len(), 0, "Should have no inconsistent IDs when all nodes with same ID have same toDelete value");

  // Test case with inconsistent toDelete instructions (conflicts)
  let input_inconsistent: &str =
    indoc! {"
            * <skg<id:conflict1,toDelete>> first conflicting node (toDelete=true)
            * <skg<id:normal>> normal node
            * <skg<id:conflict1>> duplicate node but toDelete=false
            * <skg<id:conflict2,toDelete>> another conflict start
            * <skg<id:conflict2>> another conflict end
        "};

  let trees_inconsistent: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input_inconsistent).unwrap();
  let inconsistent_ids = find_inconsistent_toDelete_instructions(&trees_inconsistent);

  assert_eq!(inconsistent_ids.len(), 2, "Should find exactly 2 conflicting IDs");
  assert!(inconsistent_ids.contains(&ID::from("conflict1")), "Should include conflict1 ID");
  assert!(inconsistent_ids.contains(&ID::from("conflict2")), "Should include conflict2 ID");

  // Test case with nodes that have no IDs (should be skipped)
  let input_no_ids: &str =
    indoc! {"
            * <skg<toDelete>> node without id (should be skipped)
            * regular node without any metadata
            * <skg<id:valid_node>> only node with id
        "};

  let trees_no_ids: Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2(input_no_ids).unwrap();
  let inconsistent_no_ids = find_inconsistent_toDelete_instructions(&trees_no_ids);
  assert_eq!(inconsistent_no_ids.len(), 0, "Should have no conflicts when only one node has each ID");

  // Test empty forest
  let empty_trees: Vec<Tree<OrgNode2>> = Vec::new();
  let inconsistent_empty = find_inconsistent_toDelete_instructions(&empty_trees);
  assert_eq!(inconsistent_empty.len(), 0, "Should have no conflicts in empty forest");
}