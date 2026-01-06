// cargo test multi_source_errors

use indoc::indoc;
use skg::test_utils::{strip_org_comments, cleanup_test_tantivy_and_typedb_dbs};
use skg::from_text::buffer_to_orgnode_forest_and_save_instructions;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::from_text::buffer_to_orgnodes::validate_tree::find_buffer_errors_for_saving;
use skg::from_text::buffer_to_orgnodes::add_missing_info::add_missing_info_to_forest;
use skg::types::orgnode::OrgNode;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::SkgConfig;
use skg::types::skgnode::SkgNode;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::typedb::nodes::create_all_nodes;
use skg::dbs::typedb::relationships::create_all_relationships;
use skg::dbs::init::{overwrite_new_empty_db, define_schema};
use ego_tree::Tree;
use std::error::Error;
use std::path::PathBuf;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};
use futures::executor::block_on;

#[test]
fn test_multi_source_errors() -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Load config from file and override db_name for this test
    let mut config: SkgConfig =
      load_config(
        "tests/multi_source_errors/fixtures/skgconfig.toml")?;
    config.db_name = "skg-test-multi-source-errors-1".to_string();
    config.tantivy_folder = PathBuf::from("/tmp/tantivy-test-multi-source-errors-1");

    // Set up TypeDB driver
    let driver: TypeDBDriver =
      TypeDBDriver::new(
        "127.0.0.1:1729",
        Credentials::new("admin", "password"),
        DriverOptions::new(false, None)?
      ).await?;

    // Load fixtures into database
    let nodes: Vec<SkgNode> =
      read_all_skg_files_from_sources (&config)?;
    overwrite_new_empty_db(&config.db_name, &driver).await?;
    define_schema(&config.db_name, &driver).await?;
    create_all_nodes(&config.db_name, &driver, &nodes).await?;
    create_all_relationships(&config.db_name, &driver, &nodes).await?;

    // Test buffer with multiple error conditions
    // Comments indicate the expected error for each line/group
    let buffer_with_errors: &str =
      indoc! {"
        * (skg (id pub-1)) pub-1                                      # root with no source
        * (skg (id dub-1) (source dub)) dub-1                         # source does not exist
        * (skg (id priv-1) (source public)) priv-1 # This line includes an error, mismatch between buffer and disk sources, which is not caught yet, but it is caught by 'buffer_to_orgnode_forest_and_save_instructions', as verified by 'test_reconciliation_errors'.
        * (skg (id priv-1) (source private)) priv-1                   # error: multiple defining orgnodes for this id
      "};
    let buffer_text: String =
      strip_org_comments (buffer_with_errors);
    let mut forest: Tree<OrgNode> =
      org_to_uninterpreted_nodes (&buffer_text)?;
    add_missing_info_to_forest(
      &mut forest, &config.db_name, &driver).await?;
    let errors: Vec<BufferValidationError> =
      find_buffer_errors_for_saving(
        &forest, &config, &driver).await?;

    { let root_without_source_errors: Vec<&BufferValidationError>
      = ( errors.iter()
          . filter(
            |e| matches!(e, BufferValidationError::RootWithoutSource(_)))
          . collect() );
      assert_eq!(root_without_source_errors.len(), 1,
                 "Expected exactly 1 RootWithoutSource error for pub-1");
      if let BufferValidationError::RootWithoutSource(node)
      = root_without_source_errors[0]
      { assert_eq!(node.metadata.id.as_ref().map(|id| id.0.as_str()),
                   Some("pub-1"),
                   "RootWithoutSource should be for pub-1"); }}

    { let nonexistent_source_errors: Vec<&BufferValidationError>
      = ( errors.iter()
          . filter(
            |e| matches!(e, BufferValidationError::SourceNotInConfig(_, _)))
          . collect() );
      assert_eq!(nonexistent_source_errors.len(), 1,
                 "Expected exactly 1 SourceNotInConfig error for dub-1");
      if let BufferValidationError::SourceNotInConfig(id, source)
      = nonexistent_source_errors[0]
      { assert_eq!(id.0, "dub-1", "SourceNotInConfig should be for dub-1");
        assert_eq!(source.0, "dub", "SourceNotInConfig should be for source 'dub'"); }}

    { let multiple_defining_errors: Vec<&BufferValidationError>
      = ( errors.iter()
          . filter(
            |e| matches!(e, BufferValidationError::Multiple_Defining_Orgnodes(_)))
          . collect() );
      assert_eq!(multiple_defining_errors.len(), 1,
                 "Expected exactly 1 Multiple_Defining_Orgnodes error for priv-1");
      if let BufferValidationError::Multiple_Defining_Orgnodes(id) = multiple_defining_errors[0] {
        assert_eq!(id.0, "priv-1", "Multiple_Defining_Orgnodes should be for priv-1"); }}

    { let inconsistent_source_errors: Vec<&BufferValidationError>
      = ( errors.iter()
          . filter(
            |e| matches!(e, BufferValidationError::InconsistentSources(_, _)))
          . collect() );
      assert_eq!(inconsistent_source_errors.len(), 1,
                 "Expected exactly 1 InconsistentSources error for priv-1");
      if let BufferValidationError::InconsistentSources(id, sources) = inconsistent_source_errors[0] {
        assert_eq!(id.0, "priv-1", "InconsistentSources should be for priv-1");
        assert_eq!(sources.len(), 2, "Should have 2 different sources for priv-1"); }}

    assert_eq!(errors.len(), 4,
               "Expected exactly 4 errors: 1 RootWithoutSource, 1 SourceNotInConfig, 1 Multiple_Defining_Orgnodes, 1 InconsistentSources");

    cleanup_test_tantivy_and_typedb_dbs(
      &config.db_name,
      &driver,
      Some(config.tantivy_folder.as_path())
    ).await?;
    Ok(( )) } ) }

#[test]
fn test_foreign_node_modification_errors(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let mut config: SkgConfig =
      load_config(
        "tests/multi_source_errors/fixtures/skgconfig.toml")?;
    config.db_name = "skg-test-multi-source-errors-2".to_string();
    config.tantivy_folder = PathBuf::from("/tmp/tantivy-test-multi-source-errors-2");
    let driver: TypeDBDriver =
      TypeDBDriver::new(
        "127.0.0.1:1729",
        Credentials::new("admin", "password"),
        DriverOptions::new(false, None)?
      ).await?;

    // Load fixtures into database
    let nodes: Vec<SkgNode> =
      read_all_skg_files_from_sources (&config)?;
    overwrite_new_empty_db(&config.db_name, &driver).await?;
    define_schema(&config.db_name, &driver).await?;
    create_all_nodes(&config.db_name, &driver, &nodes).await?;
    create_all_relationships(&config.db_name, &driver, &nodes).await?;

    // Test 1: Foreign node modifications
    // (all other errors removed so initial validation passes)
    // Each line tests a different type of modification to get separate error reports
    {
      let buffer_with_errors: &str = indoc! {"
        * (skg (id ext-1) (source ext)) ext-1
        ** (skg (code (interp aliasCol))) aliases         # edit to aliases (set to empty)
        * (skg (id ext-2) (source ext)) ext-2-edited           # edit to title
        * (skg (id ext-3) (source ext)) ext-3
        new body                                               # edit to body
        * (skg (id ext-4) (source ext)) ext-4                  # edit to content
        ** (skg (id ext-5) (source ext)) ext-5
        * (skg (id ext-new) (source ext)) ext-new              # add new node to foreign source
        * (skg (id ext-6) (source ext) (code toDelete)) ext-6  # delete from foreign source
        * (skg (id ext-7) (source ext) (code toDelete)) ext-7  # delete with body modification
        Different body.
      "}; // note that nothing is wrong with ext-5

      let buffer_text: String =
        strip_org_comments (buffer_with_errors);
      let result = buffer_to_orgnode_forest_and_save_instructions(
        &buffer_text,
        &config,
        &driver
      ).await;

      assert!(result.is_err(), "Expected errors for foreign node modifications");

      if let Err(e) = result {
        if let SaveError::BufferValidationErrors(errors) = e {
          println!("\n=== Foreign node modification errors ({} total) ===", errors.len());
          for (i, error) in errors.iter().enumerate() {
            println!("{}: {:?}", i + 1, error);
          }

          // Check for ModifiedForeignNode errors
          let modified_foreign_errors: Vec<&BufferValidationError> =
            errors.iter()
            . filter(|e| matches!(e, BufferValidationError::ModifiedForeignNode(_, _)))
            . collect();

          println!("\nModifiedForeignNode errors: {}",
                   modified_foreign_errors.len());

          assert_eq!(modified_foreign_errors.len(), 7, // namely:
                     // ext-1 (aliases modified)
                     // ext-2 (title modified)
                     // ext-3 (body modified)
                     // ext-4 (content modified)
                     // ext-new (new node)
                     // ext-6 (deletion)
                     // ext-7 (deletion)
                     "Expected exactly 7 ModifiedForeignNode errors");

          let error_ids: Vec<String> = modified_foreign_errors.iter()
            .filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(id, _) = e {
                Some(id.0.clone())
              } else { None }
            } ).collect();

          println!("Errors for IDs: {:?}", error_ids);

          assert!(error_ids.contains(&"ext-1".to_string()), "Expected error for ext-1 (aliases)");
          assert!(error_ids.contains(&"ext-2".to_string()), "Expected error for ext-2 (title)");
          assert!(error_ids.contains(&"ext-3".to_string()), "Expected error for ext-3 (body)");
          assert!(error_ids.contains(&"ext-4".to_string()), "Expected error for ext-4 (content)");
          assert!(error_ids.contains(&"ext-new".to_string()), "Expected error for ext-new (new node)");
          assert!(error_ids.contains(&"ext-6".to_string()), "Expected error for ext-6 (deletion)");
          assert!(error_ids.contains(&"ext-7".to_string()), "Expected error for ext-7 (deletion)");
        } else {
          panic!("Expected SaveError::BufferValidationErrors, got: {:?}", e);
        }
      }
    }

    // Test 2: Foreign merge validations
    // Pipeline short-circuits on modification errors, so this tests merge errors separately
    {
      let buffer_with_merges: &str = indoc! {"
        * (skg (id pub-1) (source public) (code (merge ext-8))) pub-1  # merge into foreign acquirer (ext-8)
        * (skg (id ext-9) (source ext) (code (merge pub-2))) ext-9     # merge foreign acquiree (would delete ext-9)
      "};

      let buffer_text: String = strip_org_comments(
        buffer_with_merges);
      let result = buffer_to_orgnode_forest_and_save_instructions(
        &buffer_text,
        &config,
        &driver
      ).await;

      assert!(result.is_err(),
              "Expected errors for foreign merge operations");

      if let Err(e) = result {
        if let SaveError::BufferValidationErrors(errors) = e {
          println!("\n=== Foreign merge errors ({} total) ===", errors.len());
          for (i, error) in errors.iter().enumerate() {
            println!("{}: {:?}", i + 1, error);
          }

          // Both merges should generate ModifiedForeignNode errors
          let merge_foreign_errors: Vec<&BufferValidationError> =
            errors.iter()
            . filter(|e| matches!(e, BufferValidationError::ModifiedForeignNode(_, _)))
            . collect();

          assert_eq!(merge_foreign_errors.len(), 2,
                     "Expected exactly 2 ModifiedForeignNode errors for merges");

          let error_ids: Vec<String> = merge_foreign_errors.iter()
            .filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(id, _) = e {
                Some(id.0.clone())
              } else { None }
            } ).collect();

          println!("Merge errors for IDs: {:?}", error_ids);

          assert!(error_ids.contains(&"ext-8".to_string()), "Expected error for ext-8 (foreign acquirer)");
          assert!(error_ids.contains(&"ext-9".to_string()), "Expected error for ext-9 (foreign acquiree)");
        } else {
          panic!("Expected SaveError::BufferValidationErrors, got: {:?}", e);
        }
      }
    }

    // Cleanup
    cleanup_test_tantivy_and_typedb_dbs(
      &config.db_name,
      &driver,
      Some(config.tantivy_folder.as_path())
    ).await?;

    Ok(())
  })
}

#[test]
fn test_reconciliation_errors() -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Load config from file and override db_name for this test
    let mut config: SkgConfig = load_config(
      "tests/multi_source_errors/fixtures/skgconfig.toml")?;
    config.db_name = "skg-test-multi-source-errors-3".to_string();
    config.tantivy_folder = PathBuf::from("/tmp/tantivy-test-multi-source-errors-3");

    // Set up TypeDB driver
    let driver: TypeDBDriver = TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None)?
    ).await?;

    // Load fixtures into database
    let nodes: Vec<SkgNode> =
      read_all_skg_files_from_sources (&config)?;
    overwrite_new_empty_db(&config.db_name, &driver).await?;
    define_schema(&config.db_name, &driver).await?;
    create_all_nodes(&config.db_name, &driver, &nodes).await?;
    create_all_relationships(&config.db_name, &driver, &nodes).await?;

    // Test 1: DiskSourceBufferSourceConflict
    // priv-1 exists on disk in "private" source, but buffer specifies "public"
    {
      let buffer_with_conflict: &str = indoc! {"
        * (skg (id priv-1) (source public)) priv-1  # disk has 'private', buffer says 'public'
      "};

      let buffer_text: String =
        strip_org_comments (buffer_with_conflict);

      let result = buffer_to_orgnode_forest_and_save_instructions(
        &buffer_text,
        &config,
        &driver
      ).await;

      assert!(result.is_err(), "Expected DiskSourceBufferSourceConflict error");

      if let Err(e) = result {
        println!("\n=== DiskSourceBufferSourceConflict test ===");
        println!("Error: {:?}", e);

        // Should be a BufferValidationError wrapped in SaveError
        match e {
          SaveError::BufferValidationErrors(errors) => {
            let conflict_errors: Vec<&BufferValidationError> = errors.iter()
              .filter(|e| matches!(e, BufferValidationError::DiskSourceBufferSourceConflict(_, _, _)))
              .collect();
            assert!(!conflict_errors.is_empty(),
                    "Expected DiskSourceBufferSourceConflict error");
            println!("Found {} DiskSourceBufferSourceConflict error(s)", conflict_errors.len());
          }
          SaveError::DatabaseError(_) => {
            // Could also be wrapped in DatabaseError
            println!("Got DatabaseError (may contain DiskSourceBufferSourceConflict)");
          }
          _ => panic!("Unexpected error type: {:?}", e),
        }
      }
    }

    // Test 2: InconsistentSources
    // Two instances of pub-1 with different sources (validation should catch this)
    {
      let buffer_with_inconsistent_sources: &str = indoc! {"
        * (skg (id pub-1) (source public)) pub-1                # definitive instance with 'public'
        * (skg (id pub-1) (source private) (code indefinitive)) pub-1  # indefinitive instance with 'private'
      "};

      let buffer_text: String =
        strip_org_comments (buffer_with_inconsistent_sources);

      // This should fail during validation (before indefinitives are filtered)
      let result = buffer_to_orgnode_forest_and_save_instructions(
        &buffer_text,
        &config,
        &driver
      ).await;

      println!("\n=== InconsistentSources test ===");

      assert!(result.is_err(), "Expected InconsistentSources error");

      if let Err(e) = result {
        println!("Error: {:?}", e);

        match e {
          SaveError::BufferValidationErrors(errors) => {
            // Should contain InconsistentSources error
            let source_errors: Vec<&BufferValidationError> = errors.iter()
              .filter(|e| matches!(e, BufferValidationError::InconsistentSources(_, _)))
              .collect();
            assert!(!source_errors.is_empty(),
                    "Expected InconsistentSources error in validation");
            println!("Successfully caught InconsistentSources error during validation");
          }
          _ => panic!("Expected BufferValidationErrors, got: {:?}", e),
        }
      }
    }

    // Cleanup
    cleanup_test_tantivy_and_typedb_dbs(
      &config.db_name,
      &driver,
      Some(config.tantivy_folder.as_path())
    ).await?;

    Ok(())
  })
}
