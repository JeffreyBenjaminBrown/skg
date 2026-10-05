// cargo test multi_repo_errors

use indoc::indoc;
use regex::Regex;
use skg::test_utils::{strip_org_comments, cleanup_test_tantivy};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::from_text::buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use skg::types::tree::forest::MpViewForest;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::SkgConfig;

use skg::dbs::filesystem::not_nodes::load_config;
use std::error::Error;
use std::path::PathBuf;
use futures::executor::block_on;

#[test]
fn test_multi_skgrepo_errors() -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Load the multi-repo fixture config.
    let mut config: SkgConfig =
      load_config(
        "tests/multi_repo_errors/fixtures/skgconfig.toml")?;
    config . tantivy_folder = PathBuf::from ("/tmp/tantivy-test-multi-repo-errors-1");


    // Test buffer with multiple error conditions
    // Comments indicate the expected error for each line/group
    let buffer_with_errors: &str =
      indoc! {"
        * (skg (node (id pub-1))) pub-1                                      # root with no repo
        * (skg (node (id dub-1) (repo dub))) dub-1                         # repo does not exist
        * (skg (node (id priv-1) (repo public))) priv-1 # This line includes an error, mismatch between buffer and disk repos, which is not caught yet, but it is caught by 'buffer_to_validated_saveplan', as verified by 'test_reconciliation_errors'.
        * (skg (node (id priv-1) (repo private))) priv-1                   # error: multiple defining viewnodes for this id
      "};
    let buffer_text: String =
      strip_org_comments (buffer_with_errors);
    let mut viewforest: MpViewForest =
      org_to_uninterpreted_viewforest (&buffer_text)?. 0;
    add_missing_info_to_viewforest(
      &mut viewforest, &config)?;
    let errors: Vec<BufferValidationError> =
      find_buffer_errors_for_saving(
        &viewforest, &config)?;

    { // Repo validation errors: one for dub-1 (nonexistent skgrepo "dub")
      // and one for pub-1 (no skgrepo at all).
      let skgrepo_re = Regex::new(r"(?i)activevognode.*must.*repo") . unwrap();
      let skgrepo_errors: Vec<&BufferValidationError>
      = ( errors . iter()
          . filter(
            |e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _)
                         if skgrepo_re . is_match (msg)))
          . collect() );
      assert_eq!(skgrepo_errors . len(), 2,
                 "Expected 2 repo validation errors (pub-1 and dub-1)");
      let skgids: Vec<&str> = skgrepo_errors . iter()
        . filter_map(|e| {
          if let BufferValidationError::LocalStructureViolation(_, skgid) = e {
            Some(skgid . 0 . as_str())
          } else { None } })
        . collect();
      assert!(skgids . contains(&"dub-1"), "Repo error should include dub-1");
      assert!(skgids . contains(&"pub-1"), "Repo error should include pub-1"); }

    { let multiple_defining_errors: Vec<&BufferValidationError>
      = ( errors . iter()
          . filter(
            |e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes (_)))
          . collect() );
      assert_eq!(multiple_defining_errors . len(), 1,
                 "Expected exactly 1 Multiple_Defining_Viewnodes error for priv-1");
      if let BufferValidationError::Multiple_Defining_Viewnodes (skgid) = multiple_defining_errors[0] {
        assert_eq!(skgid . 0, "priv-1", "Multiple_Defining_Viewnodes should be for priv-1"); }}

    { let inconsistent_skgrepo_errors: Vec<&BufferValidationError>
      = ( errors . iter()
          . filter(
            |e| matches!(e, BufferValidationError::InconsistentSkgRepos(_, _)))
          . collect() );
      assert_eq!(inconsistent_skgrepo_errors . len(), 1,
                 "Expected exactly 1 InconsistentRepos error for priv-1");
      if let BufferValidationError::InconsistentSkgRepos(skgid, skgrepos) = inconsistent_skgrepo_errors[0] {
        assert_eq!(skgid . 0, "priv-1", "InconsistentRepos should be for priv-1");
        assert_eq!(skgrepos . len(), 2, "Should have 2 different repos for priv-1"); }}

    assert_eq!(errors . len(), 4,
               "Expected exactly 4 errors: 2 LocalStructureViolation (repo errors), 1 Multiple_Defining_Viewnodes, 1 InconsistentRepos");

    cleanup_test_tantivy(
      Some(config . tantivy_folder . as_path())
    )?;
    Ok(( )) } ) }

#[test]
fn test_foreign_node_modification_errors(
) -> Result<(), Box<dyn Error>> {
  block_on(async {
    let mut config: SkgConfig =
      load_config(
        "tests/multi_repo_errors/fixtures/skgconfig.toml")?;
    config . tantivy_folder = PathBuf::from ("/tmp/tantivy-test-multi-repo-errors-2");

    // Test 1: Foreign node modifications
    // (all other errors removed so initial validation passes)
    // Each line tests a different type of modification to get separate error reports
    {
      let buffer_with_errors: &str = indoc! {"
        * (skg (node (id ext-1) (repo ext))) ext-1
        ** (skg aliasFolder) aliases         # edit to aliases (set to empty)
        * (skg (node (id ext-2) (repo ext))) ext-2-edited           # edit to title
        * (skg (node (id ext-3) (repo ext))) ext-3
        new body                                               # edit to body
        * (skg (node (id ext-4) (repo ext))) ext-4                  # edit to content
        ** (skg (node (id ext-5) (repo ext))) ext-5
        * (skg (node (id ext-new) (repo ext))) ext-new              # add new node to foreign repo
        * (skg (node (id ext-6) (repo ext) (editRequest delete))) ext-6  # delete from foreign repo
        * (skg (node (id ext-7) (repo ext) (editRequest delete))) ext-7  # delete with body modification
        Different body.
      "}; // note that nothing is wrong with ext-5

      let buffer_text: String =
        strip_org_comments (buffer_with_errors);
      let result = buffer_to_validated_saveplan(
        &buffer_text,
        &config,
        None ) ;

      assert!(result . is_err(), "Expected errors for foreign node modifications");

      if let Err (e) = result {
        if let SaveError::BufferValidationErrors { errors, .. } = e {
          println!("\n=== Foreign node modification errors ({} total) ===", errors . len());
          for (i, error) in errors . iter() . enumerate() {
            println!("{}: {:?}", i + 1, error);
          }

          // Check for foreign write errors
          let modified_foreign_errors: Vec<&BufferValidationError> =
            errors . iter()
            . filter(|e| matches!(e, BufferValidationError::ModifiedForeignNode(_, _)))
            . collect();
          let created_foreign_errors: Vec<&BufferValidationError> =
            errors . iter()
            . filter(|e| matches!(e, BufferValidationError::CreatedForeignNode(_, _)))
            . collect();

          println!("\nModifiedForeignNode errors: {}",
                   modified_foreign_errors . len());
          println!("\nCreatedForeignNode errors: {}",
                   created_foreign_errors . len());

          // Editing a foreign node now FORKS it rather than erroring,
          // so the four content/field edits (ext-1..ext-4) are fork
          // candidates, not ModifiedForeignNode errors. Only the two
          // DELETES of foreign nodes still error as ModifiedForeignNode
          // (deleting a foreign node is not a fork). The delete/create
          // rejections return before the forks are even built, so the
          // edits surface no error here at all.
          assert_eq!(modified_foreign_errors . len(), 2, // namely:
                     // ext-6 (deletion)
                     // ext-7 (deletion)
                     "Expected exactly 2 ModifiedForeignNode errors (the deletes)");
          assert_eq!(created_foreign_errors . len(), 1,
                     "Expected exactly 1 CreatedForeignNode error");

          let error_skgids: Vec<String> = modified_foreign_errors . iter()
            . filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(skgid, _) = e {
                Some(skgid . 0 . clone())
              } else { None }
            } ) . collect();

          println!("Errors for IDs: {:?}", error_skgids);

          assert!(error_skgids . contains(&"ext-6" . to_string()), "Expected error for ext-6 (deletion)");
          assert!(error_skgids . contains(&"ext-7" . to_string()), "Expected error for ext-7 (deletion)");
          assert!(created_foreign_errors . iter() . any (|e| matches!(
            e, BufferValidationError::CreatedForeignNode(skgid, _)
              if skgid . 0 == "ext-new")),
            "Expected error for ext-new (new node)");
        } else {
          panic!("Expected SaveError::BufferValidationErrors, got: {:?}", e);
        }
      }
    }

    // Test 2: Foreign merge validations
    // Pipeline short-circuits on modification errors, so this tests merge errors separately
    {
      let buffer_with_merges: &str = indoc! {"
        * (skg (node (id pub-1) (repo public) (editRequest (merge ext-8)))) pub-1  # merge into foreign acquirer (ext-8)
        * (skg (node (id ext-9) (repo ext) (editRequest (merge pub-2)))) ext-9     # merge foreign acquiree (would delete ext-9)
      "};

      let buffer_text: String = strip_org_comments(
        buffer_with_merges);
      let result = buffer_to_validated_saveplan(
        &buffer_text,
        &config,
        None ) ;

      assert!(result . is_err(),
              "Expected errors for foreign merge operations");

      if let Err (e) = result {
        if let SaveError::BufferValidationErrors { errors, .. } = e {
          println!("\n=== Foreign merge errors ({} total) ===", errors . len());
          for (i, error) in errors . iter() . enumerate() {
            println!("{}: {:?}", i + 1, error);
          }

          // Both merges should generate ModifiedForeignNode errors
          let merge_foreign_errors: Vec<&BufferValidationError> =
            errors . iter()
            . filter(|e| matches!(e, BufferValidationError::ModifiedForeignNode(_, _)))
            . collect();

          assert_eq!(merge_foreign_errors . len(), 2,
                     "Expected exactly 2 ModifiedForeignNode errors for merges");

          let error_skgids: Vec<String> = merge_foreign_errors . iter()
            . filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(skgid, _) = e {
                Some(skgid . 0 . clone())
              } else { None }
            } ) . collect();

          println!("NodeMerge errors for IDs: {:?}", error_skgids);

          assert!(error_skgids . contains(&"ext-8" . to_string()), "Expected error for ext-8 (foreign acquirer)");
          assert!(error_skgids . contains(&"ext-9" . to_string()), "Expected error for ext-9 (foreign acquiree)");
        } else {
          panic!("Expected SaveError::BufferValidationErrors, got: {:?}", e);
        }
      }
    }

    // Cleanup
    cleanup_test_tantivy(
      Some(config . tantivy_folder . as_path())
    )?;

    Ok(())
  })
}

#[test]
fn test_reconciliation_errors() -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Load the multi-repo fixture config.
    let mut config: SkgConfig = load_config(
      "tests/multi_repo_errors/fixtures/skgconfig.toml")?;
    config . tantivy_folder = PathBuf::from ("/tmp/tantivy-test-multi-repo-errors-3");


    // Test 1: Repo move between owned skgrepos is now allowed
    // priv-1 exists on disk in "private" skgrepo, but buffer specifies "public"
    // Both skgrepos are owned, so this should succeed (producing a RepoMove).
    {
      let buffer_with_move: &str = indoc! {"
        * (skg (node (id priv-1) (repo public))) priv-1  # disk has 'private', buffer says 'public'
      "};

      let buffer_text: String =
        strip_org_comments (buffer_with_move);

      let result = buffer_to_validated_saveplan(
        &buffer_text,
        &config,
        None ) ;

      assert!(result . is_ok(),
              "Repo move between owned repos should succeed, got: {:?}",
              result . err());

      let ( _viewforest, save_plan, _warnings ) = result?;
      assert_eq!(save_plan . skgrepo_moves . len(), 1,
                 "Expected exactly 1 repo move");
      assert_eq!(save_plan . skgrepo_moves[0] . pid . 0, "priv-1");
      assert_eq!(save_plan . skgrepo_moves[0] . old_skgrepo . as_str(), "private");
      assert_eq!(save_plan . skgrepo_moves[0] . new_skgrepo . as_str(), "public");
    }

    // Test 2: InconsistentRepos
    // Two instances of pub-1 with different skgrepos (validation should catch this)
    {
      let buffer_with_inconsistent_skgrepos: &str = indoc! {"
        * (skg (node (id pub-1) (repo public))) pub-1                # editable instance with 'public'
        * (skg (node (id pub-1) (repo private) writeProtected)) pub-1  # write-protected instance with 'private'
      "};

      let buffer_text: String =
        strip_org_comments (buffer_with_inconsistent_skgrepos);

      // This should fail during validation (before write-protected_occurrences are filtered)
      let result = buffer_to_validated_saveplan(
        &buffer_text,
        &config,
        None ) ;

      println!("\n=== InconsistentRepos test ===");

      assert!(result . is_err(), "Expected InconsistentRepos error");

      if let Err (e) = result {
        println!("Error: {:?}", e);

        match e {
          SaveError::BufferValidationErrors { errors, .. } => {
            // Should contain InconsistentRepos error
            let skgrepo_errors: Vec<&BufferValidationError> = errors . iter()
              . filter(|e| matches!(e, BufferValidationError::InconsistentSkgRepos(_, _)))
              . collect();
            assert!(!skgrepo_errors . is_empty(),
                    "Expected InconsistentRepos error in validation");
            println!("Successfully caught InconsistentRepos error during validation");
          }
          _ => panic!("Expected BufferValidationErrors, got: {:?}", e),
        }
      }
    }

    // Cleanup
    cleanup_test_tantivy(
      Some(config . tantivy_folder . as_path())
    )?;

    Ok(())
  })
}
