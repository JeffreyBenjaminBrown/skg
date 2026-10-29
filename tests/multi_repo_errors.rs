// cargo test multi_repo_errors

use indoc::indoc;
use regex::Regex;
use skg::test_utils::{strip_org_comments, cleanup_test_tantivy, graph_from_config,
                      index_folder_for_this_process};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::types::tree::forest::ViewForest;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::SkgConfig;

use skg::dbs::filesystem::not_nodes::load_config;
use std::error::Error;
use futures::executor::block_on;

#[test]
fn test_multi_skgrepo_errors() -> Result<(), Box<dyn Error>> {
  block_on(async {
    // Load the multi-repo fixture config.
    let mut config: SkgConfig =
      load_config(
        "tests/multi_repo_errors/fixtures/skgconfig.toml")? . 0;
    config . tantivy_folder = index_folder_for_this_process ("/tmp/tantivy-test-multi-repo-errors-1");


    // Test buffer with multiple error conditions
    // Comments indicate the expected error for each line/group
    let buffer_with_errors: &str =
      indoc! {"
        * (skg node (gnid pub-1)) pub-1                                      # root with no repo
        * (skg node (gnid dub-1) (skgRepos (nodeRepo dub))) dub-1                         # repo does not exist
        * (skg node (gnid priv-1) (skgRepos (nodeRepo public))) priv-1 # This line includes an error, mismatch between buffer and disk repos, which is not caught yet, but it is caught by 'buffer_to_validated_saveplan', as verified by 'test_reconciliation_errors'.
        * (skg node (gnid priv-1) (skgRepos (nodeRepo private))) priv-1                   # error: multiple defining viewnodes for this id
      "};
    let buffer_text: String =
      strip_org_comments (buffer_with_errors);
    let viewforest: ViewForest =
      org_to_viewforest (&buffer_text, IdResolution::ThroughGraph (&graph_from_config (&config) ?))?. 0;
    let errors: Vec<BufferValidationError> =
      find_buffer_errors_for_saving(
        &viewforest, &config)?;

    { // Repo validation errors: one for dub-1 (nonexistent skgrepo "dub")
      // and one for pub-1 (no skgrepo at all).
      let skgrepo_re = Regex::new(r"(?i)occurrence.*must.*repo") . unwrap();
      let skgrepo_errors: Vec<&BufferValidationError>
      = ( errors . iter()
          . filter(
            |e| matches!(e, BufferValidationError::LocalStructureViolation(msg, _, _)
                         if skgrepo_re . is_match (msg)))
          . collect() );
      assert_eq!(skgrepo_errors . len(), 2,
                 "Expected 2 repo validation errors (pub-1 and dub-1)");
      let gnids: Vec<&str> = skgrepo_errors . iter()
        . filter_map(|e| {
          if let BufferValidationError::LocalStructureViolation(_, gnid, _) = e {
            Some(gnid . 0 . as_str())
          } else { None } })
        . collect();
      assert!(gnids . contains(&"dub-1"), "Repo error should include dub-1");
      assert!(gnids . contains(&"pub-1"), "Repo error should include pub-1"); }

    { let multiple_defining_errors: Vec<&BufferValidationError>
      = ( errors . iter()
          . filter(
            |e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes (_, _)))
          . collect() );
      assert_eq!(multiple_defining_errors . len(), 1,
                 "Expected exactly 1 Multiple_Defining_Viewnodes error for priv-1");
      if let BufferValidationError::Multiple_Defining_Viewnodes (gnid, _) = multiple_defining_errors[0] {
        assert_eq!(gnid . 0, "priv-1", "Multiple_Defining_Viewnodes should be for priv-1"); }}

    assert_eq!(errors . len(), 3,
               "Expected exactly 3 errors: 2 LocalStructureViolation (skgRepos (nodeRepo errors)), 1 Multiple_Defining_Viewnodes");

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
        "tests/multi_repo_errors/fixtures/skgconfig.toml")? . 0;
    config . tantivy_folder = index_folder_for_this_process ("/tmp/tantivy-test-multi-repo-errors-2");

    // Test 1: Foreign node modifications
    // (all other errors removed so initial validation passes)
    // Each line tests a different type of modification to get separate error reports
    {
      let buffer_with_errors: &str = indoc! {"
        * (skg node (gnid ext-1) (skgRepos (nodeRepo ext))) ext-1
        ** (skg aliasFolder) aliases         # edit to aliases (set to empty)
        * (skg node (gnid ext-2) (skgRepos (nodeRepo ext))) ext-2-edited           # edit to title
        * (skg node (gnid ext-3) (skgRepos (nodeRepo ext))) ext-3
        new body                                               # edit to body
        * (skg node (gnid ext-4) (skgRepos (nodeRepo ext))) ext-4                  # edit to content
        ** (skg node (gnid ext-5) (skgRepos (nodeRepo ext))) ext-5
        * (skg node (gnid ext-new) (skgRepos (nodeRepo ext))) ext-new              # add new node to foreign repo
        * (skg node (gnid ext-6) (skgRepos (nodeRepo ext)) (editRequest delete)) ext-6  # delete from foreign repo
        * (skg node (gnid ext-7) (skgRepos (nodeRepo ext)) (editRequest delete)) ext-7  # delete with body modification
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

          // Editing a foreign node now forks it rather than erroring,
          // so the four content/field edits (ext-1..ext-4) are fork
          // candidates, not ModifiedForeignNode errors. Only the two
          // deletes of foreign nodes still error as ModifiedForeignNode
          // (deleting a foreign node is not a fork). The delete/create
          // rejections return before the forks are even built, so the
          // edits surface no error here at all.
          assert_eq!(modified_foreign_errors . len(), 2, // namely:
                     // ext-6 (deletion)
                     // ext-7 (deletion)
                     "Expected exactly 2 ModifiedForeignNode errors (the deletes)");
          assert_eq!(created_foreign_errors . len(), 1,
                     "Expected exactly 1 CreatedForeignNode error");

          let error_gnids: Vec<String> = modified_foreign_errors . iter()
            . filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(gnid, _) = e {
                Some(gnid . 0 . clone())
              } else { None }
            } ) . collect();

          println!("Errors for IDs: {:?}", error_gnids);

          assert!(error_gnids . contains(&"ext-6" . to_string()), "Expected error for ext-6 (deletion)");
          assert!(error_gnids . contains(&"ext-7" . to_string()), "Expected error for ext-7 (deletion)");
          assert!(created_foreign_errors . iter() . any (|e| matches!(
            e, BufferValidationError::CreatedForeignNode(gnid, _)
              if gnid . 0 == "ext-new")),
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
        * (skg node (gnid pub-1) (skgRepos (nodeRepo public)) (editRequest (merge ext-8))) pub-1  # merge into foreign acquirer (ext-8)
        * (skg node (gnid ext-9) (skgRepos (nodeRepo ext)) (editRequest (merge pub-2))) ext-9     # merge foreign acquiree (would delete ext-9)
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

          let error_gnids: Vec<String> = merge_foreign_errors . iter()
            . filter_map(|e| {
              if let BufferValidationError::ModifiedForeignNode(gnid, _) = e {
                Some(gnid . 0 . clone())
              } else { None }
            } ) . collect();

          println!("NodeMerge errors for IDs: {:?}", error_gnids);

          assert!(error_gnids . contains(&"ext-8" . to_string()), "Expected error for ext-8 (foreign acquirer)");
          assert!(error_gnids . contains(&"ext-9" . to_string()), "Expected error for ext-9 (foreign acquiree)");
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
      "tests/multi_repo_errors/fixtures/skgconfig.toml")? . 0;
    config . tantivy_folder = index_folder_for_this_process ("/tmp/tantivy-test-multi-repo-errors-3");


    // Test 1: Repo move between owned skgrepos is now allowed
    // priv-1 exists on disk in "private" skgrepo, but buffer specifies "public"
    // Both skgrepos are owned, so this should succeed (producing a RepoMove).
    {
      let buffer_with_move: &str = indoc! {"
        * (skg node (gnid priv-1) (skgRepos (nodeRepo public))) priv-1  # disk has 'private', buffer says 'public'
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
      assert_eq!(save_plan . skgrepo_moves[0] . skgrepo_change . old . as_str(), "private");
      assert_eq!(save_plan . skgrepo_moves[0] . skgrepo_change . new . as_str(), "public");
    }

    // Test 2: A write-protected occurrence's skgrepo is not an instruction.
    // Save ignores it, so it cannot disagree with the editable occurrence.
    // (This arises with a skgrepo move: the move edits only the
    // editable occurrence, leaving its write-protected repeats as rendered.)
    // A write-protected occurrence must agree with the graph, though, so
    // one whose skgrepo is stale is refused.
    {
      let buffer_moving_with_a_write_protected_repeat: &str = indoc! {"
        * (skg node (gnid pub-1) (skgRepos (nodeRepo private))) pub-1               # editable instance moved to 'private'
        * (skg node (gnid pub-1) (skgRepos (nodeRepo public)) writeProtected) pub-1  # write-protected instance as rendered
      "};
      let buffer_text: String =
        strip_org_comments (buffer_moving_with_a_write_protected_repeat);
      let result = buffer_to_validated_saveplan(
        &buffer_text,
        &config,
        None ) ;
      assert!(result . is_ok(),
              "A write-protected occurrence's skgrepo should be ignored, got: {:?}",
              result . err());
      let ( _viewforest, save_plan, _warnings ) = result?;
      assert_eq!(save_plan . skgrepo_moves . len(), 1,
                 "Only the editable occurrence should move pub-1");
      assert_eq!(save_plan . skgrepo_moves[0] . skgrepo_change . new . as_str(), "private");
      let buffer_with_stale_write_protected_skgrepo: &str = indoc! {"
        * (skg node (gnid pub-1) (skgRepos (nodeRepo public))) pub-1
        * (skg node (gnid pub-1) (skgRepos (nodeRepo private)) writeProtected) pub-1
      "};
      let result = buffer_to_validated_saveplan(
        buffer_with_stale_write_protected_skgrepo,
        &config,
        None ) ;
      assert!(matches!(
                &result,
                Err (SaveError::BufferValidationErrors { errors, .. })
                if matches!(&errors[..],
                  [BufferValidationError::EditedWriteProtectedOccurrence { .. }])),
              "A stale write-protected skgrepo should be refused, got: {:?}",
              result . err());
    }

    // Cleanup
    cleanup_test_tantivy(
      Some(config . tantivy_folder . as_path())
    )?;

    Ok(())
  })
}
