// cargo test validate_foreign_nodes

use indoc::indoc;
use skg::init::{overwrite_new_empty_db, define_schema};
use skg::media::file_io::multiple_nodes::read_all_skg_files_from_sources;
use skg::media::typedb::nodes::create_all_nodes;
use skg::media::typedb::relationships::create_all_relationships;
use skg::save::buffer_to_save_instructions;
use skg::types::errors::{SaveError, BufferValidationError};
use skg::types::misc::{SkgConfig, SkgfileSource};
use skg::types::skgnode::SkgNode;
use futures::executor::block_on;
use std::collections::HashMap;
use std::error::Error;
use std::path::PathBuf;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};

/// Helper to set up multi-source test environment
async fn setup_multi_source_test(
  db_name: &str,
) -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
  let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
  sources.insert(
    "main".to_string(),
    SkgfileSource {
      nickname: "main".to_string(),
      path: PathBuf::from("tests/save/validate_foreign_nodes/fixtures/main"),
      user_owns_it: true,
    },
  );
  sources.insert(
    "foreign".to_string(),
    SkgfileSource {
      nickname: "foreign".to_string(),
      path: PathBuf::from("tests/save/validate_foreign_nodes/fixtures/foreign"),
      user_owns_it: false,
    },
  );

  let config = SkgConfig {
    db_name: db_name.to_string(),
    tantivy_folder: PathBuf::from(format!("/tmp/tantivy-{}", db_name)),
    sources,
    port: 1730,
    delete_on_quit: false,
  };

  let driver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?,
  ).await?;

  // Load fixtures from both sources
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(&config.sources)?;

  overwrite_new_empty_db(db_name, &driver).await?;
  define_schema(db_name, &driver).await?;
  create_all_nodes(db_name, &driver, &nodes).await?;
  create_all_relationships(db_name, &driver, &nodes).await?;

  Ok((config, driver))
}

#[test]
fn test_unmodified_foreign_node_allowed() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-unmodified").await?;

    let org_text = indoc! {"
      * (skg (id foreign1) (source foreign)) Foreign node unchanged
      This is a foreign node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    assert!(result.is_ok(), "Unmodified foreign node should be allowed");

    let (_orgnode_forest, instructions, _merge_instructions) = result?;

    // Foreign nodes should be filtered out (no need to write)
    assert_eq!(instructions.len(), 0,
               "Unmodified foreign nodes should be filtered out");

    Ok(())
  })
}

#[test]
fn test_modified_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-modified").await?;

    // Try to save buffer with modified foreign node (title changed)
    let org_text = indoc! {"
      * (skg (id foreign2) (source foreign)) MODIFIED TITLE
      Original body
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail with ModifiedForeignNode error
    assert!(result.is_err(), "Modified foreign node should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert_eq!(errors.len(), 1, "Should have one validation error");
        match &errors[0] {
          BufferValidationError::ModifiedForeignNode(id, source) => {
            assert_eq!(id.0, "foreign2");
            assert_eq!(source.as_str(), "foreign");
          }
          other => panic!("Expected ModifiedForeignNode error, got {:?}", other),
        }
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_modified_foreign_node_body_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-body").await?;

    // Try to save buffer with modified foreign node (body changed)
    let org_text = indoc! {"
      * (skg (id foreign2) (source foreign)) Foreign node to modify
      MODIFIED BODY
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail with ModifiedForeignNode error
    assert!(result.is_err(), "Foreign node with modified body should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert!(errors.iter().any(|e| matches!(
          e, BufferValidationError::ModifiedForeignNode(_, _))));
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_indefinitive_foreign_node_filtered() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-indefinitive").await?;

    // Save buffer with indefinitive foreign node
    let org_text = indoc! {"
      * (skg (id foreign3) (source foreign) (code indefinitive)) Foreign indefinitive node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should succeed - indefinitive foreign nodes are allowed but filtered
    assert!(result.is_ok(), "Indefinitive foreign node should be allowed");

    let (_orgnode_forest, instructions, _merge_instructions) = result?;

    // Indefinitive foreign nodes should be filtered out (no append)
    assert_eq!(instructions.len(), 0,
               "Indefinitive foreign nodes should be filtered out");

    Ok(())
  })
}

#[test]
fn test_owned_node_unchanged_behavior() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-owned").await?;

    // Save buffer with owned node
    let org_text = indoc! {"
      * (skg (id node1) (source main)) Modified owned node
      ** (skg (id child1) (source main)) _
      ** (skg (id child2) (source main)) _
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should succeed - owned nodes can be modified
    assert!(result.is_ok(), "Owned node modification should be allowed");

    let (_orgnode_forest, instructions, _merge_instructions) = result?;

    // Owned node should be included in instructions
    assert!(instructions.len() > 0,
            "Owned node should be included in save instructions");

    Ok(())
  })
}

#[test]
fn test_delete_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-delete").await?;

    // Try to delete a foreign node
    let org_text = indoc! {"
      * (skg (id foreign1) (source foreign) (code toDelete)) Foreign node unchanged
      This is a foreign node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail with ModifiedForeignNode error
    assert!(result.is_err(), "Deleting foreign node should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert!(errors.iter().any(|e| matches!(
          e, BufferValidationError::ModifiedForeignNode(_, _))),
          "Should have ModifiedForeignNode error");
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_new_foreign_node_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-new").await?;

    // Try to create a new node in foreign source
    let org_text = indoc! {"
      * (skg (id new_foreign) (source foreign)) New foreign node
      This should not be allowed
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail with ModifiedForeignNode error
    assert!(result.is_err(), "Creating new foreign node should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert!(errors.iter().any(|e| matches!(
          e, BufferValidationError::ModifiedForeignNode(_, _))),
          "Should have ModifiedForeignNode error");
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_mixed_owned_and_foreign_nodes() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-foreign-mixed").await?;

    // Save buffer with mix of owned and unmodified foreign nodes
    let org_text = indoc! {"
      * (skg (id node1) (source main)) Modified owned node
      ** (skg (id child1) (source main)) _
      ** (skg (id child2) (source main)) _
      * (skg (id foreign1) (source foreign)) Foreign node unchanged
      This is a foreign node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should succeed
    assert!(result.is_ok(), "Mixed owned and unmodified foreign should be allowed");

    let (_orgnode_forest, instructions, _merge_instructions) = result?;

    // Only owned node should be in instructions (foreign filtered out)
    assert!(instructions.len() > 0, "Should have owned node instructions");

    // Verify no foreign nodes in instructions
    for (node, _action) in &instructions {
      assert_eq!(node.source, "main",
                 "Only owned (main) nodes should be in instructions");
    }

    Ok(())
  })
}

#[test]
fn test_merge_with_foreign_acquirer_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-merge-foreign-acquirer").await?;

    // Try to merge where the acquirer is foreign
    // Format: (skg (code (merge ID))) - acquiree merges into acquirer
    let org_text = indoc! {"
      * (skg (id node1) (source main) (code (merge foreign1))) Node merging into foreign
      * (skg (id foreign1) (source foreign)) Foreign node unchanged
      This is a foreign node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail - can't merge into foreign node (would modify it)
    assert!(result.is_err(), "Merge into foreign acquirer should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert!(errors.iter().any(|e| matches!(
          e, BufferValidationError::ModifiedForeignNode(_, _))),
          "Should have ModifiedForeignNode error for foreign acquirer");
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_merge_with_foreign_acquiree_rejected() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-merge-foreign-acquiree").await?;

    // Try to merge where the acquiree is foreign
    let org_text = indoc! {"
      * (skg (id foreign1) (source foreign) (code (merge node1))) Foreign merging into owned
      This is a foreign node
      * (skg (id node1) (source main)) Owned node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should fail - can't merge foreign node (would delete it)
    assert!(result.is_err(), "Merge with foreign acquiree should be rejected");

    match result.unwrap_err() {
      SaveError::BufferValidationErrors(errors) => {
        assert!(errors.iter().any(|e| matches!(
          e, BufferValidationError::ModifiedForeignNode(_, _))),
          "Should have ModifiedForeignNode error for foreign acquiree");
      }
      other => panic!("Expected BufferValidationErrors, got {:?}", other),
    }

    Ok(())
  })
}

#[test]
fn test_merge_with_both_owned_allowed() -> Result<(), Box<dyn Error>> {
  block_on(async {
    let (config, driver) = setup_multi_source_test("skg-test-merge-owned").await?;

    // Merge where both nodes are owned - should work
    let org_text = indoc! {"
      * (skg (id node1) (source main) (code (merge child1))) Node merging into child
      * (skg (id child1) (source main)) Child node
    "};

    let result = buffer_to_save_instructions(org_text, &config, &driver).await;

    // Should succeed - both nodes are owned
    assert!(result.is_ok(), "Merge with both owned nodes should be allowed");

    Ok(())
  })
}
