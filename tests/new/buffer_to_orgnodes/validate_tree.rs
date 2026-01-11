// cargo test validate_tree

use indoc::indoc;
use ego_tree::Tree;
use skg::types::orgnode::{OrgNode, forest_root_orgnode};
use skg::types::errors::BufferValidationError;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::buffer_to_orgnodes::validate_tree::find_buffer_errors_for_saving;
use skg::test_utils::run_with_test_db;
use std::error::Error;

#[test]
fn test_find_buffer_errors_for_saving() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-errors",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-errors",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with various validation errors
      let input_with_errors: &str =
        indoc! {"
                * (skg (id root) (source main)) Valid root node
                ** (skg (code (interp aliasCol))) AliasCol with body problem
                This body should not exist on AliasCol
                *** (skg (id bad_child)) Child of AliasCol with ID
                *** Regular child without ID
                ** (skg (code (interp alias))) Alias with body problem and orphaned
                This body should not exist on Alias
                *** Any child of Alias (bad)
                ** (skg (code (interp alias))) Alias under non-AliasCol parent
                * (skg (code (interp alias))) Root level Alias (bad)
                * (skg (id conflict) (code toDelete) (source main)) Node with deletion conflict
                * (skg (id conflict) (source main)) Same ID but no toDelete flag
            "};

      let (forest, parsing_errors)
        : (Tree<OrgNode>, Vec<BufferValidationError>)
        = org_to_uninterpreted_nodes(
            input_with_errors).unwrap();
      let validation_errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      // Combine parsing errors with validation errors
      let mut errors: Vec<BufferValidationError> = parsing_errors;
      errors.extend(validation_errors);

  // Errors: Body_of_Scaffold(2), Child_of_AliasCol_with_ID(1), Child_of_Alias(1),
  //         Alias_with_no_AliasCol_Parent(3), AmbiguousDeletion(1), Multiple_Defining_Orgnodes(1),
  //         RootWithoutSource(1) = 10
  assert_eq!(errors.len(), 10,
             "Should find exactly 10 validation errors");

  { let aliasCol_child_id_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::Child_of_AliasCol_with_ID(_)))
    .collect();
  assert_eq!(aliasCol_child_id_errors.len(), 1, "Should find 1 Child_of_AliasCol_with_ID error");
  if let BufferValidationError::Child_of_AliasCol_with_ID(node) = aliasCol_child_id_errors[0] {
    assert_eq!(node.title(), "Child of AliasCol with ID", "Child_of_AliasCol_with_ID error should come from correct node"); }}

  { let alias_child_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::Child_of_Alias(_)))
    .collect();
  assert_eq!(alias_child_errors.len(), 1, "Should find 1 Child_of_Alias error");
  if let BufferValidationError::Child_of_Alias(node) = alias_child_errors[0] {
    assert_eq!(node.title(), "Any child of Alias (bad)", "Child_of_Alias error should come from correct node"); }}

  { let alias_no_aliascol_parent_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::Alias_with_no_AliasCol_Parent(_)))
    .collect();
    assert_eq!(alias_no_aliascol_parent_errors.len(), 3, "Should find 3 Alias_with_no_AliasCol_Parent errors");
    let expected_titles: Vec<&str> = vec![
      "Alias with body problem and orphaned",
      "Alias under non-AliasCol parent",
      "Root level Alias (bad)" ];
    for error in &alias_no_aliascol_parent_errors {
      if let BufferValidationError::Alias_with_no_AliasCol_Parent(node) = error {
        assert!(expected_titles.contains(&node.title()),
                "Alias_with_no_AliasCol_Parent error should come from expected node, got: {}", node.title()); }} }

  { let ambiguous_deletion_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::AmbiguousDeletion(_)))
    .collect();
  assert_eq!(ambiguous_deletion_errors.len(), 1, "Should find 1 AmbiguousDeletion error");
    if let BufferValidationError::AmbiguousDeletion(id)
    = ambiguous_deletion_errors[0] {
      assert_eq!(id.0, "conflict",
                 "AmbiguousDeletion error should come from conflicting ID"); }}

  { let multiple_defining_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Orgnodes(_)))
    .collect();
  assert_eq!(multiple_defining_errors.len(), 1, "Should find 1 Multiple_Defining_Orgnodes error");
    if let BufferValidationError::Multiple_Defining_Orgnodes(id)
    = multiple_defining_errors[0] {
      assert_eq!(id.0, "conflict",
                 "Multiple_Defining_Orgnodes error should come from conflicting ID"); }}

  { let root_without_source_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::RootWithoutSource(_)))
    .collect();
  assert_eq!(root_without_source_errors.len(), 1, "Should find 1 RootWithoutSource error");
    if let BufferValidationError::RootWithoutSource(node)
    = root_without_source_errors[0] {
      assert_eq!(node.title(), "Root level Alias (bad)",
                 "RootWithoutSource error should come from root-level Alias"); }}

  { let body_of_scaffold_errors: Vec<&BufferValidationError> = errors.iter()
    .filter(|e| matches!(e, BufferValidationError::Body_of_Scaffold(_, _)))
    .collect();
  assert_eq!(body_of_scaffold_errors.len(), 2, "Should find 2 Body_of_Scaffold errors");
  assert!(body_of_scaffold_errors.iter().any(|e| {
    matches!(e, BufferValidationError::Body_of_Scaffold(title, kind)
             if title == "AliasCol with body problem" && kind == "aliasCol")
  }), "Should find Body_of_Scaffold error for aliasCol");
  assert!(body_of_scaffold_errors.iter().any(|e| {
    matches!(e, BufferValidationError::Body_of_Scaffold(title, kind)
             if title == "Alias with body problem and orphaned" && kind == "alias")
  }), "Should find Body_of_Scaffold error for alias"); }

      Ok (( )) } )) }

#[test]
fn test_find_buffer_errors_for_saving_valid_input() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-valid",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-valid",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with no validation errors
      let valid_input: &str =
        indoc! {"
                * (skg (id root) (source main)) Valid root node
                ** (skg (code (interp aliasCol))) AliasCol without body
                *** Regular child without ID
                *** Another child without ID
                *** (skg (code (interp alias))) Alias without body
                ** (skg (id normal)) Normal node with body
                This body is allowed on normal nodes
            "};

      let (forest, parsing_errors): (Tree<OrgNode>, Vec<BufferValidationError>) =
        org_to_uninterpreted_nodes(valid_input).unwrap();
      let errors: Vec<BufferValidationError> = find_buffer_errors_for_saving(&forest, config, driver).await?;

      assert_eq!(parsing_errors.len(), 0, "Should find no parsing errors in valid input");
      assert_eq!(errors.len(), 0, "Should find no validation errors in valid input");
      Ok(())
    })
  )
}

#[test]
fn test_find_buffer_errors_for_saving_empty_input() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-empty",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-empty",
    |config, driver, _tantivy| Box::pin(async move {
      // Test empty input (forest with just ForestRoot, no tree roots)
      let empty_forest: Tree<OrgNode> = Tree::new(forest_root_orgnode());
      let errors: Vec<BufferValidationError> = find_buffer_errors_for_saving(&empty_forest, config, driver).await?;

      assert_eq!(errors.len(), 0, "Should find no errors in empty input");
      Ok(())
    })
  )
}

#[test]
fn test_multiple_aliascols_in_children() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-aliascols",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-aliascols",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with multiple AliasCol children
      let input_with_multiple_aliascols: &str =
        indoc! {"
                * (skg (id root)) Node with multiple AliasCol children
                ** (skg (code (interp aliasCol))) First AliasCol
                *** (skg (code (interp alias))) First alias
                ** (skg (code (interp aliasCol))) Second AliasCol
                *** (skg (code (interp alias))) Second alias
                ** Normal child
            "};

      let forest: Tree<OrgNode> =
        org_to_uninterpreted_nodes(input_with_multiple_aliascols).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let multiple_aliascols_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::Multiple_AliasCols_in_Children(_)))
        .collect();

      assert_eq!(multiple_aliascols_errors.len(), 1,
                 "Should find exactly 1 Multiple_AliasCols_in_Children error");

      if let BufferValidationError::Multiple_AliasCols_in_Children(node) = multiple_aliascols_errors[0] {
        assert_eq!(node.title(), "Node with multiple AliasCol children",
                   "Multiple_AliasCols_in_Children error should come from the parent node");
      }
      Ok(())
    })
  )
}

#[test]
fn test_duplicated_content_error() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-duplicate",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-duplicate",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with duplicated Content children (same ID)
      let input_with_duplicated_content: &str =
        indoc! {"
                * (skg (id root)) Node with duplicated content
                ** (skg (id 1)) 1
                ** (skg (id 1)) 1
            "};

      let forest: Tree<OrgNode> =
        org_to_uninterpreted_nodes(input_with_duplicated_content).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let duplicated_content_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::DuplicatedContent(_)))
        .collect();

      assert_eq!(duplicated_content_errors.len(), 1,
                 "Should find exactly 1 DuplicatedContent error");

      if let BufferValidationError::DuplicatedContent(id) = duplicated_content_errors[0] {
        assert_eq!(id.0, "1",
                   "DuplicatedContent error should report ID '1'");
      }
      Ok(())
    })
  )
}

#[test]
fn test_no_duplicated_content_error_when_different_ids() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-validate-tree-no-dup",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-tree-no-dup",
    |config, driver, _tantivy| Box::pin(async move {
      // Test input with different Content children IDs (should be valid)
      let input_without_duplicated_content: &str =
        indoc! {"
                * (skg (id root) (source main)) Node with duplicated content
                ** (skg (id 1)) 1
                ** (skg (id 2)) 2
            "};

      let forest: Tree<OrgNode> =
        org_to_uninterpreted_nodes(input_without_duplicated_content).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let duplicated_content_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::DuplicatedContent(_)))
        .collect();

      assert_eq!(duplicated_content_errors.len(), 0,
                 "Should find no DuplicatedContent errors when IDs are different");
      Ok(())
    })
  )
}

#[test]
fn test_root_without_source_validation(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-root-without-source",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-root-without-source",
    |config, driver, _tantivy| Box::pin(async move {
      // root without source should be rejected
      let input: &str =
        indoc! {"
                * (skg (id root1) (source main)) Root with source (valid)
                * (skg (id root2)) Root without source (invalid)
            "};

      let forest: Tree<OrgNode> =
        org_to_uninterpreted_nodes(input).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;
      assert_eq!(errors.len(), 1,
                 "Should find 1 RootWithoutSource error");

      let root_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(
          |e| matches!(e,
                       BufferValidationError::RootWithoutSource(_)))
        .collect();
      assert_eq!(root_errors.len(), 1,
                 "Should find RootWithoutSource error");

      if let BufferValidationError::RootWithoutSource(node)
        = root_errors[0]
      { assert_eq!(
        node.title(), "Root without source (invalid)",
        "RootWithoutSource error should identify correct node"); }
      Ok(( )) } )) }

#[test]
fn test_nonexistent_source_validation(
) -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-nonexistent-source",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-nonexistent-source",
    |config, driver, _tantivy| Box::pin(async move {
      { // Node with nonexistent source should be rejected
        let input: &str =
          indoc! {"
                  * (skg (id root1) (source main)) Root with valid source
                  ** (skg (id child1) (source nonexistent)) Child with invalid source
                  * (skg (id root2) (source invalid_source)) Root with nonexistent source
              "};
        let forest: Tree<OrgNode> =
          org_to_uninterpreted_nodes(input).unwrap().0;
        let errors: Vec<BufferValidationError> =
          find_buffer_errors_for_saving(&forest, config, driver).await?;
        let nonexistent_source_errors: Vec<&BufferValidationError> =
          errors.iter()
          .filter(
            |e| matches!(e,
                         BufferValidationError::SourceNotInConfig(_, _)))
          .collect();

        assert_eq!(nonexistent_source_errors.len(), 2,
                   "Should find 2 SourceNotInConfig errors");

        { // Check first error (child1 with 'nonexistent' source)
          let found_child_error: bool =
            nonexistent_source_errors.iter().any(|e| {
              if let BufferValidationError::SourceNotInConfig(id, source) = e {
                id.0 == "child1" && source.0 == "nonexistent"
              } else { false } });
          assert!(found_child_error,
                  "Should find SourceNotInConfig error for child1 with source 'nonexistent'"); }

        { // Check second error (root2 with 'invalid_source')
          let found_root_error: bool =
            nonexistent_source_errors.iter().any(|e| {
              if let BufferValidationError::SourceNotInConfig(id, source) = e {
                id.0 == "root2" && source.0 == "invalid_source"
              } else { false } });
          assert!(found_root_error,
                  "Should find SourceNotInConfig error for root2 with source 'invalid_source'"); }}
      Ok(( )) } )) }
