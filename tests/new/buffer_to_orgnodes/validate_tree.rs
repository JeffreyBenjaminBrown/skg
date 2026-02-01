// cargo test validate_tree

use indoc::indoc;
use ego_tree::Tree;
use regex::Regex;
use skg::types::unchecked_orgnode::{UncheckedOrgNode, unchecked_forest_root_orgnode};
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
                * (skg (node (id root) (source main))) Valid root node
                ** (skg aliasCol) AliasCol with body problem
                This body should not exist on AliasCol
                *** (skg (node (id bad_child))) Child of AliasCol with ID
                ** (skg alias) Alias with body problem and orphaned
                This body should not exist on Alias
                *** (skg (node (id alias_child))) Any child of Alias (bad)
                ** (skg alias) Alias under non-AliasCol parent
                * (skg alias) Root level Alias (bad)
                * (skg (node (id conflict) (source main) (editRequest delete))) Node with deletion conflict
                * (skg (node (id conflict) (source main))) Same ID but no toDelete flag
            "};

      let (forest, parsing_errors)
        : (Tree<UncheckedOrgNode>, Vec<BufferValidationError>)
        = org_to_uninterpreted_nodes(
            input_with_errors).unwrap();
      let validation_errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      // Combine parsing errors with validation errors
      let mut errors: Vec<BufferValidationError> = parsing_errors;
      errors.extend(validation_errors);

      // Collect LocalStructureViolation errors for checks below
      let local_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(_, _)))
        .collect();

      // AliasCol children must be Aliases (bad_child is a TrueNode under AliasCol)
      { let aliascol_children_re =
          Regex::new(r"(?i)aliascol.*children.*must.*alias").unwrap();
        let aliascol_children_errors: Vec<_> = local_errors.iter()
          .filter(|e| matches!(
            e, BufferValidationError::LocalStructureViolation(msg, _)
            if aliascol_children_re.is_match(msg)))
          .collect();
        assert_eq!(aliascol_children_errors.len(), 1,
                   "Should find 1 'AliasCol children must be Aliases' error"); }

      // Alias must have no children ("Alias with body problem" has alias_child)
      { let alias_children_re = Regex::new(r"(?i)alias.*must.*no.*children").unwrap();
        let alias_children_errors: Vec<_> = local_errors.iter()
          .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                               if alias_children_re.is_match(msg)))
          .collect();
        assert_eq!(alias_children_errors.len(), 1,
                   "Should find 1 'Alias must have no children' error"); }

      // Alias must have AliasCol parent
      // (3 aliases: "Alias with body problem", "Alias under non-AliasCol", "Root level Alias")
      { let alias_parent_re =
          Regex::new(r"(?i)alias.*must.*aliascol.*parent").unwrap();
        let alias_parent_errors: Vec<_> =
          local_errors.iter()
          .filter(|e| matches!(
            e, BufferValidationError::LocalStructureViolation(msg, _)
            if alias_parent_re.is_match(msg)))
          .collect();
        assert_eq!(alias_parent_errors.len(), 3,
                   "Should find 3 'Alias must have AliasCol parent' errors"); }

      // AmbiguousDeletion
      { let ambiguous_deletion_errors: Vec<&BufferValidationError> = errors.iter()
          .filter(|e| matches!(e, BufferValidationError::AmbiguousDeletion(_)))
          .collect();
        assert_eq!(ambiguous_deletion_errors.len(), 1,
                   "Should find 1 AmbiguousDeletion error");
        if let BufferValidationError::AmbiguousDeletion(id)
          = ambiguous_deletion_errors[0] {
          assert_eq!(id.0, "conflict",
                     "AmbiguousDeletion error should come from conflicting ID"); }}

      // Multiple_Defining_Orgnodes
      { let multiple_defining_errors: Vec<&BufferValidationError> = errors.iter()
          .filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Orgnodes(_)))
          .collect();
        assert_eq!(multiple_defining_errors.len(), 1,
                   "Should find 1 Multiple_Defining_Orgnodes error");
        if let BufferValidationError::Multiple_Defining_Orgnodes(id)
          = multiple_defining_errors[0] {
          assert_eq!(id.0, "conflict",
                     "Multiple_Defining_Orgnodes error should come from conflicting ID"); }}

      // Source validation (bad_child and alias_child have no sources)
      { let source_re = Regex::new(r"(?i)must.*source").unwrap();
        let source_errors: Vec<_> = local_errors.iter()
          .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                               if source_re.is_match(msg)))
          .collect();
        assert_eq!(source_errors.len(), 2,
                   "Should find 2 source validation errors"); }

      // Body_of_Scaffold (from parsing phase)
      { let body_of_scaffold_errors: Vec<&BufferValidationError> = errors.iter()
          .filter(|e| matches!(e, BufferValidationError::Body_of_Scaffold(_, _)))
          .collect();
        assert_eq!(body_of_scaffold_errors.len(), 2,
                   "Should find 2 Body_of_Scaffold errors");
        assert!(body_of_scaffold_errors.iter().any(|e| {
          matches!(e, BufferValidationError::Body_of_Scaffold(title, kind)
                   if title == "AliasCol with body problem" && kind == "aliasCol")
        }), "Should find Body_of_Scaffold error for aliasCol");
        assert!(body_of_scaffold_errors.iter().any(|e| {
          matches!(e, BufferValidationError::Body_of_Scaffold(title, kind)
                   if title == "Alias with body problem and orphaned" && kind == "alias")
        }), "Should find Body_of_Scaffold error for alias"); }

      // BufferRoot children must be TrueNodes (Root level Alias is Scaffold at root)
      { let bufferroot_children_re = Regex::new(r"(?i)bufferroot.*children.*must.*truenode").unwrap();
        let bufferroot_errors: Vec<_> = local_errors.iter()
          .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                               if bufferroot_children_re.is_match(msg)))
          .collect();
        assert_eq!(bufferroot_errors.len(), 1,
                   "Should find 1 'BufferRoot children must be TrueNodes' error"); }

      // TrueNode children (root has Alias children directly, not via AliasCol)
      { let truenode_children_re = Regex::new(r"(?i)truenode.*children.*must.*include.*only").unwrap();
        let truenode_children_errors: Vec<_> = local_errors.iter()
          .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                               if truenode_children_re.is_match(msg)))
          .collect();
        assert_eq!(truenode_children_errors.len(), 1,
                   "Should find 1 'TrueNode children' error"); }

      // "Alias with body problem" fails two checks (no children + AliasCol parent),
      // but multiple errors on one node combine into a single LocalStructureViolation.
      assert_eq!(local_errors.len(), 8,
                 "Should find exactly 8 LocalStructureViolation errors");

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
                * (skg (node (id root) (source main))) Valid root node
                ** (skg aliasCol) AliasCol without body
                *** (skg alias) An alias
                *** (skg alias) Another alias
                ** (skg (node (id normal) (source main))) Normal node with body
                This body is allowed on normal nodes
            "};

      let (forest, parsing_errors): (Tree<UncheckedOrgNode>, Vec<BufferValidationError>) =
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
      // Test empty input (forest with just BufferRoot, no tree roots)
      let empty_forest: Tree<UncheckedOrgNode> = Tree::new(unchecked_forest_root_orgnode());
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
                * (skg (node (id root) (source main))) Node with multiple AliasCol children
                ** (skg aliasCol) First AliasCol
                *** (skg alias) First alias
                ** (skg aliasCol) Second AliasCol
                *** (skg alias) Second alias
            "};

      let forest: Tree<UncheckedOrgNode> =
        org_to_uninterpreted_nodes(input_with_multiple_aliascols).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      // "AliasCol must be unique among its siblings"
      // Both AliasCol siblings report the error (each has a sibling AliasCol)
      let unique_sibling_re = Regex::new(r"(?i)aliascol.*unique.*siblings").unwrap();
      let multiple_aliascols_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                             if unique_sibling_re.is_match(msg)))
        .collect();

      assert_eq!(multiple_aliascols_errors.len(), 2,
                 "Should find 2 'AliasCol must be unique' errors (one per AliasCol)");
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
                * (skg (node (id root) (source main))) Node with duplicated content
                ** (skg (node (id 1))) 1
                ** (skg (node (id 1))) 1
            "};

      let forest: Tree<UncheckedOrgNode> =
        org_to_uninterpreted_nodes(input_with_duplicated_content).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let dup_children_re =
        Regex::new(r"(?i)non-ignored.*children.*must.*unique").unwrap();
      let duplicated_content_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                             if dup_children_re.is_match(msg)))
        .collect();

      assert_eq!(duplicated_content_errors.len(), 1,
                 "Should find exactly 1 duplicate children error");

      if let BufferValidationError::LocalStructureViolation(_, id) = duplicated_content_errors[0] {
        assert_eq!(id.0, "root",
                   "Duplicate error should be reported at the parent node (root)");
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
                * (skg (node (id root) (source main))) Node with duplicated content
                ** (skg (node (id 1))) 1
                ** (skg (node (id 2))) 2
            "};

      let forest: Tree<UncheckedOrgNode> =
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
                * (skg (node (id root1) (source main))) Root with source (valid)
                * (skg (node (id root2))) Root without source (invalid)
            "};

      let forest: Tree<UncheckedOrgNode> =
        org_to_uninterpreted_nodes(input).unwrap().0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&forest, config, driver).await?;

      let source_re = Regex::new(r"(?i)must.*source").unwrap();
      let source_errors: Vec<&BufferValidationError> = errors.iter()
        .filter(
          |e| matches!(e,
                       BufferValidationError::LocalStructureViolation(msg, _)
                       if source_re.is_match(msg)))
        .collect();
      assert_eq!(source_errors.len(), 1,
                 "Should find 1 source validation error");

      if let BufferValidationError::LocalStructureViolation(_, id)
        = source_errors[0]
      { assert_eq!(id.0, "root2",
                   "Source error should be for root2"); }
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
                  * (skg (node (id root1) (source main))) Root with valid source
                  ** (skg (node (id child1) (source nonexistent))) Child with invalid source
                  * (skg (node (id root2) (source invalid_source))) Root with nonexistent source
              "};
        let forest: Tree<UncheckedOrgNode> =
          org_to_uninterpreted_nodes(input).unwrap().0;
        let errors: Vec<BufferValidationError> =
          find_buffer_errors_for_saving(&forest, config, driver).await?;

        let source_re = Regex::new(r"(?i)must.*source").unwrap();
        let nonexistent_source_errors: Vec<&BufferValidationError> =
          errors.iter()
          .filter(
            |e| matches!(e,
                         BufferValidationError::LocalStructureViolation(msg, _)
                         if source_re.is_match(msg)))
          .collect();

        assert_eq!(nonexistent_source_errors.len(), 2,
                   "Should find 2 source validation errors");

        { // Check first error (child1, with source 'nonexistent')
          let found_child_error: bool =
            nonexistent_source_errors.iter().any(|e| {
              matches!(e, BufferValidationError::LocalStructureViolation(msg, id)
                       if source_re.is_match(msg) && id.0 == "child1") });
          assert!(found_child_error,
                  "Should find source error for child1"); }

        { // Check second error (root2, with source 'invalid_source')
          let found_root_error: bool =
            nonexistent_source_errors.iter().any(|e| {
              matches!(e, BufferValidationError::LocalStructureViolation(msg, id)
                       if source_re.is_match(msg) && id.0 == "root2") });
          assert!(found_root_error,
                  "Should find source error for root2"); }}
      Ok(( )) } )) }
