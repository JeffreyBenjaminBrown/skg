/// Tests for collateral view updates preserving diff annotations.
/// When a user saves one buffer, other open views ("collateral views")
/// are re-completed. In diff mode, the collateral views should
/// retain diff annotations (textChanged, new-here, etc.).

use super::common::*;

/// After saving buffer 1 with a new child, collateral buffer 2
/// should still show textChanged on b and new-here on the new child.
#[test]
fn test_collateral_view_preserves_diff_annotations()
  -> Result<(), Box<dyn Error>>
{
  let db_name : &str = "skg-test-collateral-diff";
  let tantivy_folder : String =
    format!("/tmp/tantivy-{}", db_name);
  let temp_dir : TempDir = TempDir::new()?;
  let repo_path : &Path = temp_dir.path();
  setup_git_repo_with_fixtures(repo_path)?;

  // Phase 1 (async): set up DBs, get initial view, save with new child.
  let (config, driver, _tantivy, pipeline_result, mut conn_state,
       uri_1, uri_2, initial_buffer)
    : (SkgConfig, TypeDBDriver, TantivyIndex, SaveResult,
       ConnectionState, ViewUri, ViewUri, String) =
    block_on ( async {
      let (config, driver, tantivy)
        : (SkgConfig, TypeDBDriver, TantivyIndex) =
        setup_test_dbs (
          db_name, repo_path.to_str().unwrap(),
          &tantivy_folder ). await ?;

      // 1. Get an initial diff view of "a".
      let root_ids : Vec<ID> = vec![ID("a".to_string())];
      let (initial_buffer, map, pids, forest)
        : (String, SkgNodeMap, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          &driver, &config, &root_ids, true ). await ?;

      // Sanity: initial view should match expected diff output.
      assert_buffer_contains(&initial_buffer, GIT_DIFF_VIEW);

      // 2. Build a ConnectionState with diff_mode_enabled.
      let mut conn_state : ConnectionState = ConnectionState {
        diff_mode_enabled : true,
        memory            : SkgnodesInMemory::new () };
      for (pid, skgnode) in &map {
        conn_state . memory . pool . insert (
          pid . clone (), skgnode . clone () ); }

      // 3. Register buffer 1 and buffer 2,
      //    both viewing the same forest rooted at "a".
      let uri_1 : ViewUri = ViewUri ( "buffer-1".to_string() );
      let uri_2 : ViewUri = ViewUri ( "buffer-2".to_string() );
      conn_state . memory . register_view (
        uri_1 . clone (), forest . clone (), &pids );
      conn_state . memory . register_view (
        uri_2 . clone (), forest . clone (), &pids );

      // 4. Save buffer 1 with a new child "c" under "a".
      let save_input : String = insert_after (
        &initial_buffer, "(id a)",
        "** (skg (node (id c))) c" );
      let pipeline_result : SaveResult =
        update_from_and_rerender_buffer (
          &save_input, &driver, &config, &tantivy,
          true, map ). await ?;

      // 5. Update memory for saved view
      //    (replicates update_memory_for_saved_view).
      for (pid, skgnode)
        in &pipeline_result . skgnodemap_after_completion
        { conn_state . memory . pool . insert (
            pid . clone (), skgnode . clone () ); }
      conn_state . memory . update_view (
        &uri_1,
        pipeline_result . completed_forest . clone () );

      Result::<_, Box<dyn Error>>::Ok ((
        config, driver, tantivy, pipeline_result,
        conn_state, uri_1, uri_2, initial_buffer )) } ) ?;

  // Phase 2 (sync): rerender_collateral_views calls block_on
  // internally, so it must run outside the outer block_on.
  let collateral_updates : Vec<(ViewUri, String)> =
    rerender_collateral_views (
      &uri_1, &pipeline_result,
      &mut conn_state, &driver, &config );

  // 6. There should be exactly one collateral update (buffer 2).
  assert_eq!( collateral_updates . len (), 1,
    "Expected one collateral update (buffer 2), got {}.\n\
     Initial buffer:\n{}\n\
     Saved view:\n{}",
    collateral_updates . len (),
    initial_buffer,
    pipeline_result . response . saved_view );
  let (ref collateral_uri, ref collateral_buffer)
    : (ViewUri, String) = collateral_updates[0];
  assert_eq!( collateral_uri, &uri_2,
    "Collateral update should be for buffer 2" );

  // 7. Verify diff annotations in the collateral buffer.

  // b should still have textChanged.
  assert_buffer_contains ( collateral_buffer,
    "** (skg (node (id b) (source main))) b\n\
     *** (skg textChanged)" );

  // c should appear with diff new
  // (its .skg file didn't exist at HEAD).
  assert_buffer_contains ( collateral_buffer,
    "** (skg (node (id c) (diff new))) c" );

  // DISK: c.skg should exist (created by the save).
  assert!( repo_path . join ("c.skg") . exists (),
    "c.skg should have been created on disk" );

  // Cleanup.
  block_on ( async {
    cleanup_test_dbs (
      db_name, &driver,
      Some ( Path::new (&tantivy_folder) )) . await
  } ) ?;
  Ok (( ))
}
