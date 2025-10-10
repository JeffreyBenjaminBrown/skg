// cargo test validate_tree

use indoc::indoc;
use ego_tree::Tree;
use skg::types::{OrgNode, Buffer_Cannot_Be_Saved};
use skg::save::{org_to_uninterpreted_nodes, find_buffer_errors_for_saving};

#[test]
fn test_find_buffer_errors_for_saving() {
  // Test input with various validation errors
  let input_with_errors: &str =
    indoc! {"
            * (skg (id root)) Valid root node
            ** (skg (relToOrgParent aliasCol)) AliasCol with body problem
            This body should not exist on AliasCol
            *** (skg (id bad_child)) Child of AliasCol with ID
            *** Regular child without ID
            ** (skg (relToOrgParent alias)) Alias with body problem and orphaned
            This body should not exist on Alias
            *** Any child of Alias (bad)
            ** (skg (relToOrgParent alias)) Alias under non-AliasCol parent
            * (skg (relToOrgParent alias)) Root level Alias (bad)
            * (skg (id conflict) toDelete) Node with deletion conflict
            * (skg (id conflict)) Same ID but no toDelete flag
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(
      input_with_errors).unwrap();
  let errors: Vec<Buffer_Cannot_Be_Saved> =
    find_buffer_errors_for_saving(&trees);

  assert_eq!(errors.len(), 9,
             "Should find exactly 9 validation errors (8 original + 1 Multiple_DefiningContainers)");

  { let aliasCol_body_errors: Vec<&Buffer_Cannot_Be_Saved> =
    errors.iter()
    . filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Body_of_AliasCol(_)))
    .collect();
    assert_eq!(aliasCol_body_errors.len(), 1,
               "Should find 1 Body_of_AliasCol error");
    if let Buffer_Cannot_Be_Saved::Body_of_AliasCol(node)
    = aliasCol_body_errors[0]
    { assert_eq!( node.title,
                  "AliasCol with body problem",
                  "Body_of_AliasCol error should come from correct node"); }}

  { let aliasCol_child_id_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Child_of_AliasCol_with_ID(_)))
    .collect();
  assert_eq!(aliasCol_child_id_errors.len(), 1, "Should find 1 Child_of_AliasCol_with_ID error");
  if let Buffer_Cannot_Be_Saved::Child_of_AliasCol_with_ID(node) = aliasCol_child_id_errors[0] {
    assert_eq!(node.title, "Child of AliasCol with ID", "Child_of_AliasCol_with_ID error should come from correct node"); }}

  { let alias_body_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Body_of_Alias(_)))
    .collect();
  assert_eq!(alias_body_errors.len(), 1, "Should find 1 Body_of_Alias error");
  if let Buffer_Cannot_Be_Saved::Body_of_Alias(node) = alias_body_errors[0] {
    assert_eq!(node.title, "Alias with body problem and orphaned", "Body_of_Alias error should come from correct node"); }}

  { let alias_child_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Child_of_Alias(_)))
    .collect();
  assert_eq!(alias_child_errors.len(), 1, "Should find 1 Child_of_Alias error");
  if let Buffer_Cannot_Be_Saved::Child_of_Alias(node) = alias_child_errors[0] {
    assert_eq!(node.title, "Any child of Alias (bad)", "Child_of_Alias error should come from correct node"); }}

  { let alias_no_aliascol_parent_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(_)))
    .collect();
    assert_eq!(alias_no_aliascol_parent_errors.len(), 3, "Should find 3 Alias_with_no_AliasCol_Parent errors");
    let expected_titles: Vec<&str> = vec![
      "Alias with body problem and orphaned",
      "Alias under non-AliasCol parent",
      "Root level Alias (bad)" ];
    for error in &alias_no_aliascol_parent_errors {
      if let Buffer_Cannot_Be_Saved::Alias_with_no_AliasCol_Parent(node) = error {
        assert!(expected_titles.contains(&node.title.as_str()),
                "Alias_with_no_AliasCol_Parent error should come from expected node, got: {}", node.title); }} }

  { let ambiguous_deletion_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::AmbiguousDeletion(_)))
    .collect();
  assert_eq!(ambiguous_deletion_errors.len(), 1, "Should find 1 AmbiguousDeletion error");
    if let Buffer_Cannot_Be_Saved::AmbiguousDeletion(id)
    = ambiguous_deletion_errors[0] {
      assert_eq!(id.0, "conflict",
                 "AmbiguousDeletion error should come from conflicting ID"); }}

  { let multiple_defining_errors: Vec<&Buffer_Cannot_Be_Saved> =
    errors.iter()
    .filter(|e| matches!(
      e, Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(_)))
    .collect();
  assert_eq!(multiple_defining_errors.len(), 1, "Should find 1 Multiple_DefiningContainers error");
    if let Buffer_Cannot_Be_Saved::Multiple_DefiningContainers(id)
    = multiple_defining_errors[0] {
      assert_eq!(id.0, "conflict",
                 "Multiple_DefiningContainers error should come from conflicting ID"); } }
}

#[test]
fn test_find_buffer_errors_for_saving_valid_input() {
  // Test input with no validation errors
  let valid_input: &str =
    indoc! {"
            * (skg (id root)) Valid root node
            ** (skg (relToOrgParent aliasCol)) AliasCol without body
            *** Regular child without ID
            *** Another child without ID
            *** (skg (relToOrgParent alias)) Alias without body
            ** (skg (id normal)) Normal node with body
            This body is allowed on normal nodes
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(valid_input).unwrap();
  let errors: Vec<Buffer_Cannot_Be_Saved> = find_buffer_errors_for_saving(&trees);

  assert_eq!(errors.len(), 0, "Should find no validation errors in valid input");
}

#[test]
fn test_find_buffer_errors_for_saving_empty_input() {
  // Test empty input
  let empty_trees: Vec<Tree<OrgNode>> = Vec::new();
  let errors: Vec<Buffer_Cannot_Be_Saved> = find_buffer_errors_for_saving(&empty_trees);

  assert_eq!(errors.len(), 0, "Should find no errors in empty input");
}

#[test]
fn test_multiple_aliascols_in_children() {
  // Test input with multiple AliasCol children
  let input_with_multiple_aliascols: &str =
    indoc! {"
            * (skg (id root)) Node with multiple AliasCol children
            ** (skg (relToOrgParent aliasCol)) First AliasCol
            *** (skg (relToOrgParent alias)) First alias
            ** (skg (relToOrgParent aliasCol)) Second AliasCol
            *** (skg (relToOrgParent alias)) Second alias
            ** Normal child
        "};

  let trees: Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes(input_with_multiple_aliascols).unwrap();
  let errors: Vec<Buffer_Cannot_Be_Saved> =
    find_buffer_errors_for_saving(&trees);

  let multiple_aliascols_errors: Vec<&Buffer_Cannot_Be_Saved> = errors.iter()
    .filter(|e| matches!(e, Buffer_Cannot_Be_Saved::Multiple_AliasCols_in_Children(_)))
    .collect();

  assert_eq!(multiple_aliascols_errors.len(), 1,
             "Should find exactly 1 Multiple_AliasCols_in_Children error");

  if let Buffer_Cannot_Be_Saved::Multiple_AliasCols_in_Children(node) = multiple_aliascols_errors[0] {
    assert_eq!(node.title, "Node with multiple AliasCol children",
               "Multiple_AliasCols_in_Children error should come from the parent node");
  }
}
