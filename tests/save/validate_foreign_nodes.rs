// cargo test validate_foreign_nodes

use futures::executor::block_on;
use indoc::indoc;
use skg::from_text::buffer_to_viewnode_forest_and_save_instructions;
use skg::test_utils::run_with_test_db_from_config;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::memory::SkgNodeMap;
use skg::types::misc::SkgConfig;
use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use std::error::Error;
use typedb_driver::TypeDBDriver;

const CONFIG_PATH: &str = "tests/save/validate_foreign_nodes/skgconfig.toml";

#[test]
fn test_unmodified_foreign_node_allowed() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-unmodified",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      assert!(result . is_ok(), "Unmodified foreign node should be allowed");
      let (_viewnode_forest, instructions, _merge_instructions) = result?;
      // Foreign nodes should be filtered out (no need to write)
      assert_eq!(instructions . len(), 0,
                 "Unmodified foreign nodes should be filtered out");
      Ok(())
    } ))
}

#[test]
fn test_modified_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-modified",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to save buffer with modified foreign node (title changed)
      let org_text: &str = indoc! {"
        * (skg (node (id foreign2) (source foreign))) MODIFIED TITLE
        Original body
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail with ModifiedForeignNode error
      assert!(result . is_err(), "Modified foreign node should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert_eq!(errors . len(), 1, "Should have one validation error");
          match &errors[0] {
            BufferValidationError::ModifiedForeignNode(id, source) => {
              assert_eq!(id . 0, "foreign2");
              assert_eq!(source . as_str(), "foreign"); }
            other => panic!("Expected ModifiedForeignNode error, got {:?}", other), } }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_modified_foreign_node_body_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-body",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to save buffer with modified foreign node (body changed)
      let org_text: &str = indoc! {"
        * (skg (node (id foreign2) (source foreign))) Foreign node to modify
        MODIFIED BODY
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail with ModifiedForeignNode error
      assert!(result . is_err(), "Foreign node with modified body should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _)))); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_indefinitive_foreign_node_filtered() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-indefinitive",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Save buffer with indefinitive foreign node
      let org_text: &str = indoc! {"
        * (skg (node (id foreign3) (source foreign) indefinitive)) Foreign indefinitive node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new()) . await;
      // Should succeed - indefinitive foreign nodes are allowed but filtered
      assert!(result . is_ok(), "Indefinitive foreign node should be allowed");
      let (_viewnode_forest, instructions, _merge_instructions) = result?;
      // Indefinitive foreign nodes should be filtered out (no append)
      assert_eq!(instructions . len(), 0,
                 "Indefinitive foreign nodes should be filtered out");
      Ok(())
    } ))
}

#[test]
fn test_owned_node_unchanged_behavior() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-owned",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Save buffer with owned node
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main))) Modified owned node
        ** (skg (node (id child1) (source main))) _
        ** (skg (node (id child2) (source main))) _
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should succeed - owned nodes can be modified
      assert!(result . is_ok(), "Owned node modification should be allowed");
      let (_viewnode_forest, instructions, _merge_instructions) = result?;
      // Owned node should be included in instructions
      assert!(instructions . len() > 0,
              "Owned node should be included in save instructions");
      Ok(())
    } ))
}

#[test]
fn test_delete_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-delete",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to delete a foreign node
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign) (editRequest delete))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail with ModifiedForeignNode error
      assert!(result . is_err(), "Deleting foreign node should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_new_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-new",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to create a new node in foreign source
      let org_text: &str = indoc! {"
        * (skg (node (id new_foreign) (source foreign))) New foreign node
        This should not be allowed
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail with ModifiedForeignNode error
      assert!(result . is_err(), "Creating new foreign node should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_mixed_owned_and_foreign_nodes() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-foreign-mixed",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Save buffer with mix of owned and unmodified foreign nodes
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main))) Modified owned node
        ** (skg (node (id child1) (source main))) _
        ** (skg (node (id child2) (source main))) _
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should succeed
      assert!(result . is_ok(), "Mixed owned and unmodified foreign should be allowed");
      let (_viewnode_forest, instructions, _merge_instructions) = result?;
      // Only owned node should be in instructions (foreign filtered out)
      assert!(instructions . len() > 0, "Should have owned node instructions");
      // Verify no foreign nodes in instructions
      for instr in &instructions {
        let source: &str = match instr {
          DefineNode::Save(SaveNode (node)) =>
            node . source . as_str(),
          DefineNode::Delete(DeleteNode { source, .. }) =>
            source . as_str() };
        assert_eq!( source, "main", "Only owned (in this case from source main) nodes should be in instructions"); }
      Ok(())
    } ))
}

#[test]
fn test_merge_with_foreign_acquirer_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-merge-foreign-acquirer",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to merge where the acquirer is foreign
      // Format: (skg (node ... (editRequest (merge ID)))) - acquiree merges into acquirer
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main) (editRequest (merge foreign1)))) Node merging into foreign
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail - can't merge into foreign node (would modify it)
      assert!(result . is_err(), "Merge into foreign acquirer should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error for foreign acquirer"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_merge_with_foreign_acquiree_rejected() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-merge-foreign-acquiree",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Try to merge where the acquiree is foreign
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign) (editRequest (merge node1)))) Foreign merging into owned
        This is a foreign node
        * (skg (node (id node1) (source main))) Owned node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should fail - can't merge foreign node (would delete it)
      assert!(result . is_err(), "Merge with foreign acquiree should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors (errors) => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error for foreign acquiree"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    } ))
}

#[test]
fn test_merge_with_both_owned_allowed() -> Result<(), Box<dyn Error>> {
  run_with_test_db_from_config(
    "skg-test-merge-owned",
    CONFIG_PATH,
    |config, driver| Box::pin(async move {
      // Merge where both nodes are owned - should work
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main) (editRequest (merge child1)))) Node merging into child
        * (skg (node (id child1) (source main))) Child node
      "};
      let result: Result<_, _> = buffer_to_viewnode_forest_and_save_instructions(
        org_text, config, driver, &SkgNodeMap::new() ) . await;
      // Should succeed - both nodes are owned
      assert!(result . is_ok(), "Merge with both owned nodes should be allowed");
      Ok(())
    } ))
}
