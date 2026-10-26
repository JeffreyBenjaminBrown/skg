// cargo test validate_tree

use skg::from_text::buffer_to_viewnodes::add_missing_info::EnrichmentProvenance;
use indoc::indoc;
use regex::Regex;
use skg::types::errors::BufferValidationError;
use skg::types::misc::{SkgConfig, Skgrepo, SkgrepoName, TantivyIndex};
use skg::types::tree::forest::ViewForest;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::from_text::buffer_to_viewnodes::local::validate_local_structure;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::test_utils::run_with_test_stores;
use std::error::Error;
use std::collections::HashMap;
use std::path::PathBuf;
use skg::types::viewnode::ViewnodeKind;
use ego_tree::NodeId;

const FIXTURES : &str =
  "tests/merge/merge_nodes/fixtures";

#[test]
fn finds_buffer_errors_for_saving (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-find-buffer-errors-for-saving", FIXTURES,
    "/tmp/tantivy-test-validate-tree-find-buffer-errors-for-saving",
    |config, tantivy| Box::pin ( test_find_buffer_errors_for_saving (config, tantivy) )) }

#[test]
fn find_buffer_errors_for_saving_valid_input (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-find-buffer-errors-for-saving-valid-input", FIXTURES,
    "/tmp/tantivy-test-validate-tree-find-buffer-errors-for-saving-valid-input",
    |config, tantivy| Box::pin ( test_find_buffer_errors_for_saving_valid_input (config, tantivy) )) }

#[test]
fn find_buffer_errors_for_saving_empty_input (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-find-buffer-errors-for-saving-empty-input", FIXTURES,
    "/tmp/tantivy-test-validate-tree-find-buffer-errors-for-saving-empty-input",
    |config, tantivy| Box::pin ( test_find_buffer_errors_for_saving_empty_input (config, tantivy) )) }

#[test]
fn multiple_aliasfolders_in_children (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-multiple-aliasfolders-in-children", FIXTURES,
    "/tmp/tantivy-test-validate-tree-multiple-aliasfolders-in-children",
    |config, tantivy| Box::pin ( test_multiple_aliasfolders_in_children (config, tantivy) )) }

#[test]
fn duplicated_content_error (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-duplicated-content-error", FIXTURES,
    "/tmp/tantivy-test-validate-tree-duplicated-content-error",
    |config, tantivy| Box::pin ( test_duplicated_content_error (config, tantivy) )) }

#[test]
fn no_duplicated_content_error_when_different_gnids (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-no-duplicated-content-error-when-different-gnids", FIXTURES,
    "/tmp/tantivy-test-validate-tree-no-duplicated-content-error-when-different-gnids",
    |config, tantivy| Box::pin ( test_no_duplicated_content_error_when_different_gnids (config, tantivy) )) }

#[test]
fn no_duplicated_content_error_for_ghost_siblings (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-no-duplicated-content-error-for-ghost-siblings", FIXTURES,
    "/tmp/tantivy-test-validate-tree-no-duplicated-content-error-for-ghost-siblings",
    |config, tantivy| Box::pin ( test_no_duplicated_content_error_for_ghost_siblings (config, tantivy) )) }

#[test]
fn root_without_skgrepo_validation (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-root-without-skgrepo-validation", FIXTURES,
    "/tmp/tantivy-test-validate-tree-root-without-skgrepo-validation",
    |config, tantivy| Box::pin ( test_root_without_skgrepo_validation (config, tantivy) )) }

#[test]
fn nonexistent_skgrepo_validation (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-nonexistent-skgrepo-validation", FIXTURES,
    "/tmp/tantivy-test-validate-tree-nonexistent-skgrepo-validation",
    |config, tantivy| Box::pin ( test_nonexistent_skgrepo_validation (config, tantivy) )) }

#[test]
fn empty_title_rejected_for_editable_node (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-empty-title-rejected-for-editable-node", FIXTURES,
    "/tmp/tantivy-test-validate-tree-empty-title-rejected-for-editable-node",
    |config, tantivy| Box::pin ( test_empty_title_rejected_for_editable_node (config, tantivy) )) }

#[test]
fn empty_title_allowed_for_writeProtected_and_delete (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-empty-title-allowed-for-writeProtected-and-delete", FIXTURES,
    "/tmp/tantivy-test-validate-tree-empty-title-allowed-for-writeProtected-and-delete",
    |config, tantivy| Box::pin ( test_empty_title_allowed_for_writeProtected_and_delete (config, tantivy) )) }

#[test]
fn editable_view_request_on_an_editable_node_with_content_is_allowed (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-editable-view-request-on-an-editable-node-with-content-is-allowed", FIXTURES,
    "/tmp/tantivy-test-validate-tree-editable-view-request-on-an-editable-node-with-content-is-allowed",
    |config, tantivy| Box::pin ( test_editable_view_request_on_an_editable_node_with_content_is_allowed (config, tantivy) )) }

#[test]
fn two_editable_view_requests_for_one_node_are_rejected (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-two-editable-view-requests-for-one-node-are-rejected", FIXTURES,
    "/tmp/tantivy-test-validate-tree-two-editable-view-requests-for-one-node-are-rejected",
    |config, tantivy| Box::pin ( test_two_editable_view_requests_for_one_node_are_rejected (config, tantivy) )) }

#[test]
fn writeProtect_and_editable_requests_on_one_occurrence_are_rejected (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-writeProtect-and-editable-requests-on-one-occurrence-are-rejected", FIXTURES,
    "/tmp/tantivy-test-validate-tree-writeProtect-and-editable-requests-on-one-occurrence-are-rejected",
    |config, tantivy| Box::pin ( test_writeProtect_and_editable_requests_on_one_occurrence_are_rejected (config, tantivy) )) }

#[test]
fn writeProtected_relRepo_request_needs_an_editable_recorder (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-validate-tree-writeProtected-relRepo-request-needs-an-editable-recorder", FIXTURES,
    "/tmp/tantivy-test-validate-tree-writeProtected-relRepo-request-needs-an-editable-recorder",
    |config, tantivy| Box::pin ( test_writeProtected_relRepo_request_needs_an_editable_recorder (config, tantivy) )) }

/// A write-protected node may carry a relRepo request for the
/// relationship from its view-parent, unless no editable node collects
/// that relationship -- here, because the view-parent is write-protected.
async fn test_writeProtected_relRepo_request_needs_an_editable_recorder (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) root
      ** (skg node (gnid collected) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo main))) under an editable parent
      ** (skg subscribeeFolder)
      *** (skg node (gnid collected_subscribee) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo main))) in an editable recorder's folder
      ** (skg node (gnid protected_parent) (skgRepos (nodeRepo main)) writeProtected) write-protected parent
      *** (skg node (gnid orphaned) (skgRepos (nodeRepo main)) writeProtected (editRequest (relRepo main))) under a write-protected parent
    "};
  let (viewforest, parsing_errors, _warnings, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input, IdResolution::AsWritten) ?;
  assert! ( parsing_errors . is_empty (),
            "Parsing should accept every relRepo request: {:?}",
            parsing_errors );
  let errors : Vec<BufferValidationError> =
    find_buffer_errors_for_saving (&viewforest, config) ?;
  let relRepo_errors : Vec<String> = errors . iter ()
    . filter_map ( |e| match e {
        BufferValidationError::Other (message)
          if message . contains ("relRepo request on write-protected node")
          => Some (message . clone ()),
        _ => None } )
    . collect ();
  assert_eq! ( relRepo_errors . len (), 1,
               "Expected one relRepo error, for 'orphaned'. Errors: {:?}",
               errors );
  assert! ( relRepo_errors [0] . contains ("orphaned"),
            "The relRepo error should name 'orphaned': {}",
            relRepo_errors [0] );
  Ok (( )) }

async fn test_find_buffer_errors_for_saving (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test input with various validation errors
      let input_with_errors: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) Valid root node
                ** (skg aliasFolder) AliasFolder with body problem
                This body should not exist on AliasFolder
                *** (skg unknown (gnid bad_child))
                ** (skg alias) Alias with body problem and orphaned
                This body should not exist on Alias
                *** (skg node (gnid alias_child)) Any child of Alias (a note)
                ** (skg alias) Alias under non-AliasFolder parent
                * (skg alias) Root level Alias (bad)
                * (skg node (gnid conflict) (skgRepos (nodeRepo main)) (editRequest delete)) Node with deletion conflict
                * (skg node (gnid conflict) (skgRepos (nodeRepo main))) Same ID but no toDelete flag
            "};

      let (viewforest, parsing_errors, _warnings, _provenance)
        : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
        = org_to_viewforest (input_with_errors, IdResolution::AsWritten) . unwrap();
      let validation_errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      // Combine parsing errors with validation errors
      let mut errors: Vec<BufferValidationError> = parsing_errors;
      errors . extend (validation_errors);

      // Collect LocalStructureViolation errors for checks below
      let local_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(_, _, _)))
        . collect();

      // AliasFolder children must be Aliases (bad_child is an unknown
      // child of AliasFolder; an occurrence typed there the parse makes
      // an alias)
      { let aliasfolder_children_re =
          Regex::new(r"(?i)aliasFolder.*children.*must.*alias") . unwrap();
        let aliasfolder_children_errors: Vec<&BufferValidationError> =
          local_errors . iter() . copied()
          . filter(|e| matches!(
            e, BufferValidationError::LocalStructureViolation(msg, _, _)
            if aliasfolder_children_re . is_match (msg)))
          . collect();
        assert_eq!(aliasfolder_children_errors . len(), 1,
                   "Should find 1 'AliasFolder children must be Aliases' error"); }

      // An occurrence under an alias ("Alias with body problem" has
      // alias_child) claims nothing there, as under any parent that
      // reads no claims: it is a note, not an error.
      { let alias_children_re = Regex::new(r"(?i)alias.*must.*no.*children") . unwrap();
        let alias_children_errors: Vec<&BufferValidationError> =
          local_errors . iter() . copied()
          . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                               if alias_children_re . is_match (msg)))
          . collect();
        assert_eq!(alias_children_errors . len(), 0,
                   "An occurrence under an alias is a note"); }

      // Alias must have AliasFolder parent
      // (3 aliases: "Alias with body problem", "Alias under non-AliasFolder", "Root level Alias")
      { let alias_parent_re =
          Regex::new(r"(?i)alias.*must.*aliasFolder.*parent") . unwrap();
        let alias_parent_errors: Vec<&BufferValidationError> =
          local_errors . iter() . copied()
          . filter(|e| matches!(
            e, BufferValidationError::LocalStructureViolation(msg, _, _)
            if alias_parent_re . is_match (msg)))
          . collect();
        assert_eq!(alias_parent_errors . len(), 3,
                   "Should find 3 'Alias must have AliasFolder parent' errors"); }

      // AmbiguousDeletion
      { let ambiguous_deletion_errors: Vec<&BufferValidationError> = errors . iter()
          . filter(|e| matches!(e, BufferValidationError::AmbiguousDeletion (_, _)))
          . collect();
        assert_eq!(ambiguous_deletion_errors . len(), 1,
                   "Should find 1 AmbiguousDeletion error");
        if let BufferValidationError::AmbiguousDeletion (gnid, _)
          = ambiguous_deletion_errors[0] {
          assert_eq!(gnid . 0, "conflict",
                     "AmbiguousDeletion error should come from conflicting ID"); }}

      // Multiple_Defining_Viewnodes
      { let multiple_defining_errors: Vec<&BufferValidationError> = errors . iter()
          . filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes (_, _)))
          . collect();
        assert_eq!(multiple_defining_errors . len(), 1,
                   "Should find 1 Multiple_Defining_Viewnodes error");
        if let BufferValidationError::Multiple_Defining_Viewnodes (gnid, _)
          = multiple_defining_errors[0] {
          assert_eq!(gnid . 0, "conflict",
                     "Multiple_Defining_Viewnodes error should come from conflicting ID"); }}

      // Repo validation (alias_child has no skgrepo)
      { let skgrepo_re = Regex::new(r"(?i)must.*repo") . unwrap();
        let skgrepo_errors: Vec<&BufferValidationError> =
          local_errors . iter() . copied()
          . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                               if skgrepo_re . is_match (msg)))
          . collect();
        assert_eq!(skgrepo_errors . len(), 1,
                   "Should find 1 repo validation error"); }

      // Body_of_NonVognode (from parsing phase)
      { let body_of_non_vognode_errors: Vec<&BufferValidationError> = errors . iter()
          . filter(|e| matches!(e, BufferValidationError::Body_of_NonVognode(_, _, _)))
          . collect();
        assert_eq!(body_of_non_vognode_errors . len(), 2,
                   "Should find 2 Body_of_NonVognode errors");
        assert!(body_of_non_vognode_errors . iter() . any(|e| {
          matches!(e, BufferValidationError::Body_of_NonVognode(title, kind, _)
                   if title == "AliasFolder with body problem" && kind == "aliasFolder")
        }), "Should find Body_of_NonVognode error for aliasFolder");
        assert!(body_of_non_vognode_errors . iter() . any(|e| {
          matches!(e, BufferValidationError::Body_of_NonVognode(title, kind, _)
                   if title == "Alias with body problem and orphaned" && kind == "alias")
        }), "Should find Body_of_NonVognode error for alias"); }

      // View roots must be Occurrences, tombstones, unknowns or ghosts.
      { let viewroot_re = Regex::new(r"(?i)view roots.*must.*occurrence.*ghost") . unwrap();
        let viewroot_errors: Vec<&BufferValidationError> =
          errors . iter()
          . filter(|e| matches!(e, BufferValidationError::Other (msg)
                               if viewroot_re . is_match (msg)))
          . collect();
        assert_eq!(viewroot_errors . len(), 1,
                   "Should find 1 'View roots must be Occurrences, tombstones, unknowns or ghosts' error"); }

      // Occurrence child belongs elsewhere (root has Alias children directly, not via AliasFolder)
      { let occurrence_children_re =
          Regex::new(r"(?i)occurrence.*child.*belongs.*elsewhere") . unwrap();
        let occurrence_children_errors: Vec<&BufferValidationError> =
          local_errors . iter() . copied()
        . filter( |e| matches!( e,
                                BufferValidationError::LocalStructureViolation(msg, _, _)
                               if occurrence_children_re . is_match (msg)))
          . collect();
        assert_eq!(occurrence_children_errors . len(), 1,
                   "Should find 1 misplaced Occurrence child error"); }

      // "Alias with body problem" fails two checks (no children + AliasFolder parent),
      // but multiple errors on one node combine into a single LocalStructureViolation.
      assert_eq!(local_errors . len(), 6,
                 "Should find exactly 6 LocalStructureViolation errors");

      Ok (( )) }

async fn test_find_buffer_errors_for_saving_valid_input (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test input with no validation errors
      let valid_input: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) Valid root node
                ** (skg aliasFolder) AliasFolder without body
                *** (skg alias) An alias
                *** (skg alias) Another alias
                ** (skg node (gnid normal) (skgRepos (nodeRepo main))) Normal node with body
                This body is allowed on normal nodes
            "};

      let (viewforest, parsing_errors, _warnings, _provenance)
        : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance) =
        org_to_viewforest (valid_input, IdResolution::AsWritten) . unwrap();
      let errors: Vec<BufferValidationError> = find_buffer_errors_for_saving(&viewforest, config)?;

      assert_eq!(parsing_errors . len(), 0, "Should find no parsing errors in valid input");
      assert_eq!(errors . len(), 0, "Should find no validation errors in valid input");
      Ok(())
}

async fn test_find_buffer_errors_for_saving_empty_input (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test empty input (viewforest with just BufferRoot, no tree roots)
      let empty_viewforest: ViewForest = ViewForest::new();
      let errors: Vec<BufferValidationError> = find_buffer_errors_for_saving(&empty_viewforest, config)?;

      assert_eq!(errors . len(), 0, "Should find no errors in empty input");
      Ok(())
}

async fn test_multiple_aliasfolders_in_children (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test input with multiple AliasFolder children
      let input_with_multiple_aliasfolders: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) Node with multiple AliasFolder children
                ** (skg aliasFolder) First AliasFolder
                *** (skg alias) First alias
                ** (skg aliasFolder) Second AliasFolder
                *** (skg alias) Second alias
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input_with_multiple_aliasfolders, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      // "AliasFolder must be unique among its siblings"
      // Both AliasFolder siblings report the error (each has a sibling AliasFolder)
      let unique_sibling_re = Regex::new(r"(?i)aliasFolder.*unique.*siblings") . unwrap();
      let multiple_aliasfolders_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                             if unique_sibling_re . is_match (msg)))
        . collect();

      assert_eq!(multiple_aliasfolders_errors . len(), 2,
                 "Should find 2 'AliasFolder must be unique' errors (one per AliasFolder)");
      Ok(())
}

async fn test_duplicated_content_error (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test input with duplicated Content children (same ID)
      let input_with_duplicated_content: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) Node with duplicated content
                ** (skg node (gnid 1)) 1
                ** (skg node (gnid 1)) 1
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input_with_duplicated_content, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let dup_children_re =
        Regex::new(r"(?i)non-ignored.*children.*must.*unique") . unwrap();
      let duplicated_content_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                             if dup_children_re . is_match (msg)))
        . collect();

      assert_eq!(duplicated_content_errors . len(), 1,
                 "Should find exactly 1 duplicate children error");

      if let BufferValidationError::LocalStructureViolation(_, gnid, _) = duplicated_content_errors[0] {
        assert_eq!(gnid . 0, "root",
                   "Duplicate error should be reported at the parent node (root)");
      }
      Ok(())
}

async fn test_no_duplicated_content_error_when_different_gnids (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Test input with different Content children IDs (should be valid)
      let input_without_duplicated_content: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) Node with duplicated content
                ** (skg node (gnid 1)) 1
                ** (skg node (gnid 2)) 2
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input_without_duplicated_content, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let duplicated_content_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::DuplicatedContent (_)))
        . collect();

      assert_eq!(duplicated_content_errors . len(), 0,
                 "Should find no DuplicatedContent errors when IDs are different");
      Ok(())
}

async fn test_no_duplicated_content_error_for_ghost_siblings (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // A ghost sibling sharing an ID with a
      // real sibling should not trigger a duplicate error: a ghost is
      // not a member of its parent.
      let input: &str =
        indoc! {"
                * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
                ** (skg node (gnid 1) (skgRepos (nodeRepo main))) real child
                ** (skg ghost (gnid 1) (skgRepos (nodeRepo main)) (diff (node removed) (rel removed))) ghost child
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let dup_children_re =
        Regex::new(r"(?i)non-ignored.*children.*must.*unique") . unwrap();
      let duplicated_content_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                             if dup_children_re . is_match (msg)))
        . collect();

      assert_eq!(duplicated_content_errors . len(), 0,
                 "Ghost siblings should not trigger duplicate ID errors");
      Ok(())
}

async fn test_root_without_skgrepo_validation (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // root without skgrepo should be rejected
      let input: &str =
        indoc! {"
                * (skg node (gnid root1) (skgRepos (nodeRepo main))) Root with repo (valid)
                * (skg node (gnid root2)) Root without repo (invalid)
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let skgrepo_re = Regex::new(r"(?i)must.*repo") . unwrap();
      let skgrepo_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(
          |e| matches!(e,
                       BufferValidationError::LocalStructureViolation(msg, _, _)
                       if skgrepo_re . is_match (msg)))
        . collect();
      assert_eq!(skgrepo_errors . len(), 1,
                 "Should find 1 repo validation error");

      if let BufferValidationError::LocalStructureViolation(_, gnid, _)
        = skgrepo_errors[0]
      { assert_eq!(gnid . 0, "root2",
                   "Repo error should be for root2"); }
      Ok(( )) }

async fn test_nonexistent_skgrepo_validation (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      { // Node with nonexistent skgrepo should be rejected
        let input: &str =
          indoc! {"
                  * (skg node (gnid root1) (skgRepos (nodeRepo main))) Root with valid repo
                  ** (skg node (gnid child1) (skgRepos (nodeRepo nonexistent))) Child with invalid repo
                  * (skg node (gnid root2) (skgRepos (nodeRepo invalid_repo))) Root with nonexistent repo
              "};
        let viewforest: ViewForest =
          org_to_viewforest (input, IdResolution::AsWritten) . unwrap() . 0;
        let errors: Vec<BufferValidationError> =
          find_buffer_errors_for_saving(&viewforest, config)?;

        let skgrepo_re = Regex::new(r"(?i)must.*repo") . unwrap();
        let nonexistent_skgrepo_errors: Vec<&BufferValidationError> =
          errors . iter()
          . filter(
            |e| matches!(e,
                         BufferValidationError::LocalStructureViolation(msg, _, _)
                         if skgrepo_re . is_match (msg)))
          . collect();

        assert_eq!(nonexistent_skgrepo_errors . len(), 2,
                   "Should find 2 repo validation errors");

        { // Check first error (child1, with skgrepo 'nonexistent')
          let found_child_error: bool =
            nonexistent_skgrepo_errors . iter() . any(|e| {
              matches!(e, BufferValidationError::LocalStructureViolation(msg, gnid, _)
                       if skgrepo_re . is_match (msg) && gnid . 0 == "child1") });
          assert!(found_child_error,
                  "Should find repo error for child1"); }

        { // Check second error (root2, with skgrepo 'invalid_repo')
          let found_root_error: bool =
            nonexistent_skgrepo_errors . iter() . any(|e| {
              matches!(e, BufferValidationError::LocalStructureViolation(msg, gnid, _)
                       if skgrepo_re . is_match (msg) && gnid . 0 == "root2") });
          assert!(found_root_error,
                  "Should find repo error for root2"); }}
      Ok(( )) }

async fn test_empty_title_rejected_for_editable_node (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input: &str =
        indoc! {"
                * (skg node (gnid has-title) (skgRepos (nodeRepo main))) has a title
                * (skg node (gnid no-title) (skgRepos (nodeRepo main)))
            "};
      let viewforest: ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let empty_title_re =
        Regex::new(r"(?i)empty.*title") . unwrap();
      let empty_title_errors: Vec<&BufferValidationError> =
        errors . iter()
        . filter(
          |e| matches!(e,
                       BufferValidationError::LocalStructureViolation(msg, _, _)
                       if empty_title_re . is_match (msg)))
        . collect();
      assert_eq!(empty_title_errors . len(), 1,
                 "Should find 1 empty title error");
      if let BufferValidationError::LocalStructureViolation(_, gnid, _)
        = empty_title_errors[0]
      { assert_eq!(gnid . 0, "no-title",
                   "Empty title error should be for no-title"); }
      Ok(( )) }

async fn test_empty_title_allowed_for_writeProtected_and_delete (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input: &str =
        indoc! {"
                * (skg node (gnid writeProtected) (skgRepos (nodeRepo main)) writeProtected)
                * (skg node (gnid deleting) (skgRepos (nodeRepo main)) (editRequest delete))
            "};
      let viewforest: ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let empty_title_re =
        Regex::new(r"(?i)empty.*title") . unwrap();
      let empty_title_errors: Vec<&BufferValidationError> =
        errors . iter()
        . filter(
          |e| matches!(e,
                       BufferValidationError::LocalStructureViolation(msg, _, _)
                       if empty_title_re . is_match (msg)))
        . collect();
      assert_eq!(empty_title_errors . len(), 0,
                 "WriteProtected and delete-requested nodes should not trigger empty title errors");
      Ok(( )) }

async fn test_editable_view_request_on_an_editable_node_with_content_is_allowed (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // A request on an editable node with content children passes:
  // completion hands it to those children, extending them editable.
  // So does a request on a write-protected node with shown children.
      let input : &str =
        indoc! {"
                * (skg node (gnid parent) (skgRepos (nodeRepo main)) (viewRequests editableView)) parent
                ** (skg node (gnid c) (skgRepos (nodeRepo main))) content child
                * (skg node (gnid other) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) other
                ** (skg node (gnid d) (skgRepos (nodeRepo main)) writeProtected) shown child
            "};
      let viewforest : ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap () . 0;
      let errors : Vec<BufferValidationError> =
        find_buffer_errors_for_saving (
          &viewforest, config ) ?;
      assert! ( errors . is_empty (),
        "Requests on nodes with content children should pass. \
         Errors: {:?}",
        errors );
      Ok (( )) }

async fn test_two_editable_view_requests_for_one_node_are_rejected (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // A buffer carries at most one editable-view request per node.
      let input : &str =
        indoc! {"
                * (skg node (gnid parent) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) parent
                * (skg node (gnid parent) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) parent again
            "};
      let viewforest : ViewForest =
        org_to_viewforest (input, IdResolution::AsWritten) . unwrap () . 0;
      let errors : Vec<BufferValidationError> =
        find_buffer_errors_for_saving (
          &viewforest, config ) ?;
      let hits : Vec<&BufferValidationError> = errors . iter ()
        . filter ( |e| matches! (
          e, BufferValidationError::MultipleEditableViewRequestsForSameId (gnid, _)
            if gnid . 0 == "parent" ))
        . collect ();
      assert_eq! ( hits . len (), 1,
        "Two requests for 'parent' should trigger exactly one \
         MultipleEditableViewRequestsForSameId. Errors: {:?}",
        errors );
      Ok (( )) }

async fn test_writeProtect_and_editable_requests_on_one_occurrence_are_rejected (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // One occurrence cannot ask to be both write-protected and the
  // editable one, in either form of the write-protect request. On two
  // occurrences of a node, the requests move editability, and pass.
      for request in [ "writeProtectedView", "writeProtectedSubtreeView" ] {
        let contradictory : String = format! (
          "* (skg node (gnid parent) (skgRepos (nodeRepo main)) (viewRequests editableView {})) parent\n",
          request );
        let errors : Vec<BufferValidationError> =
          find_buffer_errors_for_saving (
            &org_to_viewforest (&contradictory, IdResolution::AsWritten) . unwrap () . 0,
            config ) ?;
        assert! ( matches! (
          &errors [..],
          [ BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence (gnid, _) ]
            if gnid . 0 == "parent" ),
          "{}: {:?}", request, errors );
        let moving : String = format! (
          "* (skg node (gnid parent) (skgRepos (nodeRepo main)) (viewRequests {})) parent\n\
           * (skg node (gnid parent) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) parent again\n",
          request );
        let errors : Vec<BufferValidationError> =
          find_buffer_errors_for_saving (
            &org_to_viewforest (&moving, IdResolution::AsWritten) . unwrap () . 0,
            config ) ?;
        assert! ( errors . is_empty (), "{}: {:?}", request, errors ); }
      Ok (( )) }

#[test]
fn test_edit_request_on_writeProtected_is_rejected_at_parse_time() {
  // A write-protected node marked for deletion cannot be saved:
  // Editability::WriteProtected has no slot for an edit_request, so the
  // parser would silently drop the user's instruction. Instead we emit
  // EditRequestOnWriteProtectedOccurrence so the save is rejected with a
  // clear message.
  let input_delete: &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
      ** (skg node (gnid protected) (skgRepos (nodeRepo main)) writeProtected (editRequest delete)) protected child
    "};
  let (_viewforest, parsing_errors, _warnings, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input_delete, IdResolution::AsWritten) . unwrap ();
  let matching : Vec<&BufferValidationError> =
    parsing_errors . iter ()
    . filter ( |e| matches! (
      e, BufferValidationError::EditRequestOnWriteProtectedOccurrence (gnid, _)
         if gnid . 0 == "protected" ))
    . collect ();
  assert_eq! ( matching . len (), 1,
    "(editRequest delete) on a write-protected node should produce exactly one EditRequestOnWriteProtectedOccurrence error. Parse errors: {:?}",
    parsing_errors );

  // Same error for (editRequest (merge X)) on a write-protected node.
  let input_merge: &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
      ** (skg node (gnid protected) (skgRepos (nodeRepo main)) writeProtected (editRequest (merge other))) protected child
    "};
  let (_viewforest2, parsing_errors2, _warnings__viewforest2, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input_merge, IdResolution::AsWritten) . unwrap ();
  let matching2 : Vec<&BufferValidationError> =
    parsing_errors2 . iter ()
    . filter ( |e| matches! (
      e, BufferValidationError::EditRequestOnWriteProtectedOccurrence (gnid, _)
         if gnid . 0 == "protected" ))
    . collect ();
  assert_eq! ( matching2 . len (), 1,
    "(editRequest (merge X)) on a write-protected node should also produce EditRequestOnWriteProtectedOccurrence. Parse errors: {:?}",
    parsing_errors2 );

  // An editable node with (editRequest delete) is legal -- no error.
  let input_editable: &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
      ** (skg node (gnid leaf) (skgRepos (nodeRepo main)) (editRequest delete)) leaf
    "};
  let (_viewforest3, parsing_errors3, _warnings__viewforest3, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input_editable, IdResolution::AsWritten) . unwrap ();
  assert! ( ! parsing_errors3 . iter () . any ( |e|
    matches! (e, BufferValidationError::EditRequestOnWriteProtectedOccurrence (_, _)) ),
    "(editRequest delete) on a editable node must not trigger EditRequestOnWriteProtectedOccurrence. Parse errors: {:?}",
    parsing_errors3 );
}

#[test]
fn a_tombstone_holding_a_visible_child_passes_local_validation () {
  // Where a switch of the skgrepo restriction kept a node out, a tombstone
  // stands, holding the node's visible children ('docs/api-and-formats.org',
  // "Restricted-skgrepo nodes: omission, retention, and save
  // preservation"), under a node or as a view root.
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
      ** (skg tombstone)
      *** (skg node (gnid child) (skgRepos (nodeRepo main))) visible child
      * (skg tombstone)
      ** (skg node (gnid other) (skgRepos (nodeRepo main))) another visible child
    "};
  let (viewforest, parsing_errors, _warnings_viewforest, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input, IdResolution::AsWritten) . unwrap ();
  assert_eq! (
    parsing_errors . len(), 0,
    "Test fixture should not have parse errors: {:?}",
    parsing_errors );
  let config : SkgConfig = validation_config ();
  let tombstones : Vec<NodeId> = viewforest . root () . descendants ()
    . filter ( |node_ref| matches! ( node_ref . value () . kind,
                                     ViewnodeKind::Tombstone ))
    . map ( |node_ref| node_ref . id () )
    . collect ();
  assert_eq! ( tombstones . len (), 2 );
  for treeid in tombstones
    . into_iter ()
    . chain ( viewforest . first_root () . map ( |r| r . id () )) {
    validate_local_structure (&viewforest, treeid, &config)
      . expect ("a tombstone, its parent and its children pass validation"); }
}

#[test]
fn a_tombstone_does_not_collide_with_content () {
  // A tombstone is not a content member (where it stands for a restricted
  // node, the disk weave owns that membership), so it cannot
  // duplicate a real content child's id -- the buffer validates even
  // when a visible child is the very node the tombstone stood for.
  let input : &str =
    indoc! {"
      * (skg node (gnid root) (skgRepos (nodeRepo main))) parent
      ** (skg node (gnid hidden) (skgRepos (nodeRepo main))) visible child
      ** (skg tombstone)
    "};
  let (viewforest, parsing_errors, _warnings_viewforest, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input, IdResolution::AsWritten) . unwrap ();
  assert_eq! (
    parsing_errors . len(), 0,
    "Test fixture should not have parse errors: {:?}",
    parsing_errors );
  let config : SkgConfig = validation_config ();
  let root_gnid = viewforest . first_root ()
    . expect ("root child should exist")
    . id ();
  validate_local_structure (&viewforest, root_gnid, &config)
    . expect ("a tombstone must not collide with content");
}

#[test]
fn duplicate_members_of_defining_folders_pass_validation () {
  // Defining folders (SubscribeeFolder, OverriddenFolder; aliases were always
  // exempt) silently deduplicate at emission instead of bouncing the
  // save.
  let input : &str =
    indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg subscribeeFolder)
      *** (skg node (gnid dup) (skgRepos (nodeRepo main)) writeProtected) dup
      *** (skg node (gnid dup) (skgRepos (nodeRepo main)) writeProtected) dup
      ** (skg overriddenFolder)
      *** (skg node (gnid dup2) (skgRepos (nodeRepo main)) writeProtected) dup2
      *** (skg node (gnid dup2) (skgRepos (nodeRepo main)) writeProtected) dup2
    "};
  let (viewforest, parsing_errors, _warnings_viewforest, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input, IdResolution::AsWritten) . unwrap ();
  assert_eq! (
    parsing_errors . len(), 0,
    "Test fixture should not have parse errors: {:?}",
    parsing_errors );
  let config : SkgConfig = validation_config ();
  for folder_gnid in partner_folder_treeids (&viewforest) {
    validate_local_structure (&viewforest, folder_gnid, &config)
      . expect ("duplicate members of a defining folder should pass validation"); }}

/// Under a write-protected folder no member claims membership, so the
/// save reads no list there: duplicates are notes, which the refresh
/// merges.
#[test]
fn duplicate_members_of_write_protected_folders_pass_validation () {
  let input : &str =
    indoc! {"
      * (skg node (gnid recorder) (skgRepos (nodeRepo main))) recorder
      ** (skg hiddenFolder)
      *** (skg node (gnid dup) (skgRepos (nodeRepo main)) writeProtected) dup
      *** (skg node (gnid dup) (skgRepos (nodeRepo main)) writeProtected) dup
    "};
  let (viewforest, parsing_errors, _warnings_viewforest, _provenance)
    : (ViewForest, Vec<BufferValidationError>, Vec<String>, EnrichmentProvenance)
    = org_to_viewforest (input, IdResolution::AsWritten) . unwrap ();
  assert_eq! (
    parsing_errors . len(), 0,
    "Test fixture should not have parse errors: {:?}",
    parsing_errors );
  let config : SkgConfig = validation_config ();
  for folder_gnid in partner_folder_treeids (&viewforest) {
    validate_local_structure (&viewforest, folder_gnid, &config)
      . expect ("duplicate members of a write-protected folder should pass validation"); }}

fn partner_folder_treeids (
  viewforest : &ViewForest,
) -> Vec<ego_tree::NodeId> {
  viewforest . nodes ()
    . filter ( |node_ref| node_ref . value () . kind . is_partner_folder () )
    . map ( |node_ref| node_ref . id () )
    . collect () }

fn validation_config () -> SkgConfig {
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  for skgrepo in ["main", "private"] {
    let skgrepo_name : SkgrepoName = SkgrepoName::from (skgrepo);
    skgrepos . insert (
      skgrepo_name . clone (),
      Skgrepo {
        name          : skgrepo_name,
        abbreviation  : None,
        path          : PathBuf::from (format! ("/tmp/{}", skgrepo)),
        owned         : true,
      }); }
  SkgConfig::dummyFromSkgrepos (skgrepos) }
