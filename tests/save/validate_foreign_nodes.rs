// cargo test validate_foreign_nodes

use indoc::indoc;
use skg::from_text::buffer_to_validated_saveplan;
use skg::test_utils::run_with_shared_test_db;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::misc::SkgConfig;

use skg::types::save::{DefineNode, SaveNode, DeleteNode};
use std::error::Error;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

const CONFIG_PATH: &str = "tests/save/validate_foreign_nodes/skgconfig.toml";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-save-validate-foreign-nodes",
    |s| Box::pin ( async move {
      s . reset_from_config ("test_unmodified_foreign_node_allowed", CONFIG_PATH) . await ?;
      test_unmodified_foreign_node_allowed (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_modified_foreign_node_rejected", CONFIG_PATH) . await ?;
      test_modified_foreign_node_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_modified_foreign_node_body_rejected", CONFIG_PATH) . await ?;
      test_modified_foreign_node_body_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_indefinitive_foreign_node_filtered", CONFIG_PATH) . await ?;
      test_indefinitive_foreign_node_filtered (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_owned_node_unchanged_behavior", CONFIG_PATH) . await ?;
      test_owned_node_unchanged_behavior (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_delete_foreign_node_rejected", CONFIG_PATH) . await ?;
      test_delete_foreign_node_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_new_foreign_node_rejected", CONFIG_PATH) . await ?;
      test_new_foreign_node_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_mixed_owned_and_foreign_nodes", CONFIG_PATH) . await ?;
      test_mixed_owned_and_foreign_nodes (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_merge_with_foreign_acquirer_rejected", CONFIG_PATH) . await ?;
      test_merge_with_foreign_acquirer_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_merge_with_foreign_acquiree_rejected", CONFIG_PATH) . await ?;
      test_merge_with_foreign_acquiree_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset_from_config ("test_merge_with_both_owned_allowed", CONFIG_PATH) . await ?;
      test_merge_with_both_owned_allowed (
        &s . config, &s . driver ) . await ?;
      Ok (( )) } )) }

async fn test_unmodified_foreign_node_allowed (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      assert!(result . is_ok(), "Unmodified foreign node should be allowed");
      let ( _viewforest, save_plan, _warnings ) = result?;
      // Foreign nodes should be filtered out (no need to write)
      assert_eq!(save_plan . define_nodes . len(), 0,
                 "Unmodified foreign nodes should be filtered out");
      Ok(())
    }

async fn test_modified_foreign_node_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Editing a foreign node now FORKS it. Here the foreign node is a
      // ROOT with no owned ancestor to inherit a clone source from, so
      // at this layer (no confirmation buffer to set one) the fork
      // cannot resolve a source: ForkSourceUnresolved. The save still
      // does not go through.
      let org_text: &str = indoc! {"
        * (skg (node (id foreign2) (source foreign))) MODIFIED TITLE
        Original body
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      assert!(result . is_err(), "A fork with no resolvable source must not save");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert_eq!(errors . len(), 1, "Should have one validation error");
          match &errors[0] {
            BufferValidationError::ForkSourceUnresolved(id) => {
              assert_eq!(id . 0, "foreign2"); }
            other => panic!("Expected ForkSourceUnresolved error, got {:?}", other), } }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_modified_foreign_node_body_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Editing a foreign node's body forks it too; same no-owned-ancestor
      // situation -> ForkSourceUnresolved.
      let org_text: &str = indoc! {"
        * (skg (node (id foreign2) (source foreign))) Foreign node to modify
        MODIFIED BODY
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      assert!(result . is_err(), "A fork with no resolvable source must not save");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ForkSourceUnresolved(_)))); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_indefinitive_foreign_node_filtered (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Save buffer with indef foreign node
      let org_text: &str = indoc! {"
        * (skg (node (id foreign3) (source foreign) indef)) Foreign indef node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver, None) . await;
      // Should succeed - indef foreign nodes are allowed but filtered
      assert!(result . is_ok(), "Indefinitive foreign node should be allowed");
      let ( _viewforest, save_plan, _warnings ) = result?;
      // Indefinitive foreign nodes should be filtered out (no append)
      assert_eq!(save_plan . define_nodes . len(), 0,
                 "Indefinitive foreign nodes should be filtered out");
      Ok(())
    }

async fn test_owned_node_unchanged_behavior (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Save buffer with owned node
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main))) Modified owned node
        ** (skg (node (id child1) (source main))) _
        ** (skg (node (id child2) (source main))) _
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should succeed - owned nodes can be modified
      assert!(result . is_ok(), "Owned node modification should be allowed");
      let ( _viewforest, save_plan, _warnings ) = result?;
      // Owned node should be included in instructions
      assert!(save_plan . define_nodes . len() > 0,
              "Owned node should be included in save instructions");
      Ok(())
    }

async fn test_delete_foreign_node_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Try to delete a foreign node
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign) (editRequest delete))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should fail with ModifiedForeignNode error
      assert!(result . is_err(), "Deleting foreign node should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_new_foreign_node_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Try to create a new node in foreign source
      let org_text: &str = indoc! {"
        * (skg (node (id new_foreign) (source foreign))) New foreign node
        This should not be allowed
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should fail with CreatedForeignNode error
      assert!(result . is_err(), "Creating new foreign node should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::CreatedForeignNode(_, _))),
            "Should have CreatedForeignNode error"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_mixed_owned_and_foreign_nodes (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Save buffer with mix of owned and unmodified foreign nodes
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main))) Modified owned node
        ** (skg (node (id child1) (source main))) _
        ** (skg (node (id child2) (source main))) _
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should succeed
      assert!(result . is_ok(), "Mixed owned and unmodified foreign should be allowed");
      let ( _viewforest, save_plan, _warnings ) = result?;
      // Only owned node should be in instructions (foreign filtered out)
      assert!(save_plan . define_nodes . len() > 0, "Should have owned node instructions");
      // Verify no foreign nodes in instructions
      for instr in &save_plan . define_nodes {
        let source: &str = match instr {
          DefineNode::Save(SaveNode (node)) =>
            node . source . as_str(),
          DefineNode::Delete(DeleteNode { source, .. }) =>
            source . as_str() };
        assert_eq!( source, "main", "Only owned (in this case from source main) nodes should be in instructions"); }
      Ok(())
    }

async fn test_merge_with_foreign_acquirer_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Try to merge where the acquirer is foreign
      // Format: (skg (node ... (editRequest (merge ID)))) - acquiree merges into acquirer
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main) (editRequest (merge foreign1)))) Node merging into foreign
        * (skg (node (id foreign1) (source foreign))) Foreign node unchanged
        This is a foreign node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should fail - can't merge into foreign node (would modify it)
      assert!(result . is_err(), "NodeMerge into foreign acquirer should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error for foreign acquirer"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_merge_with_foreign_acquiree_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // Try to merge where the acquiree is foreign
      let org_text: &str = indoc! {"
        * (skg (node (id foreign1) (source foreign) (editRequest (merge node1)))) Foreign merging into owned
        This is a foreign node
        * (skg (node (id node1) (source main))) Owned node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should fail - can't merge foreign node (would delete it)
      assert!(result . is_err(), "NodeMerge with foreign acquiree should be rejected");
      match result . unwrap_err() {
        SaveError::BufferValidationErrors { errors, .. } => {
          assert!(errors . iter() . any(|e| matches!(
            e, BufferValidationError::ModifiedForeignNode(_, _))),
            "Should have ModifiedForeignNode error for foreign acquiree"); }
        other => panic!("Expected BufferValidationErrors, got {:?}", other), }
      Ok(())
    }

async fn test_merge_with_both_owned_allowed (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
      // NodeMerge where both nodes are owned - should work
      let org_text: &str = indoc! {"
        * (skg (node (id node1) (source main) (editRequest (merge child1)))) Node merging into child
        * (skg (node (id child1) (source main))) Child node
      "};
      let result: Result<_, _> = buffer_to_validated_saveplan(
        org_text, config, driver , None) . await;
      // Should succeed - both nodes are owned
      assert!(result . is_ok(), "NodeMerge with both owned nodes should be allowed");
      Ok(())
    }
