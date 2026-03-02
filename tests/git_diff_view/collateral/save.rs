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
  let repo_path : &Path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;

  let (_config, driver, _tantivy, save_response, initial_buffer,
       uri_2)
    : (SkgConfig, TypeDBDriver, TantivyIndex, SaveResponse,
       String, ViewUri) =
    block_on ( async {
      let (config, driver, mut tantivy)
        : (SkgConfig, TypeDBDriver, TantivyIndex) =
        setup_test_dbs (
          db_name, repo_path . to_str() . unwrap(),
          &tantivy_folder ) . await ?;

      // 1. Get an initial diff view of "a".
      let root_ids : Vec<ID> = vec![ID("a" . to_string())];
      let (initial_buffer, map, pids, forest)
        : (String, SkgNodeMap, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          &driver, &config, &root_ids, true ) . await ?;

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
      let uri_1 : ViewUri = ViewUri ( "buffer-1" . to_string() );
      let uri_2 : ViewUri = ViewUri ( "buffer-2" . to_string() );
      conn_state . memory . register_view (
        uri_1 . clone (), forest . clone (), &pids );
      conn_state . memory . register_view (
        uri_2 . clone (), forest . clone (), &pids );

      // 4. Save buffer 1 with a new child "c" under "a".
      //    This also updates memory and re-renders collateral views.
      let save_input : String = insert_after (
        &initial_buffer, "(id a)",
        "** (skg (node (id c))) c" );
      let save_response : SaveResponse =
        update_from_and_rerender_buffer (
          &save_input, &driver, &config, &mut tantivy,
          true, map,
          &Ok ( uri_1 . clone () ),
          &mut conn_state ) . await ?;

      Result::<_, Box<dyn Error>>::Ok ((
        config, driver, tantivy, save_response,
        initial_buffer, uri_2 )) } ) ?;

  // 5. There should be exactly one collateral update (buffer 2).
  let collateral_updates : &Vec<(ViewUri, String)> =
    &save_response . collateral_views;
  assert_eq!( collateral_updates . len (), 1,
    "Expected one collateral update (buffer 2), got {}.\n\
     Initial buffer:\n{}\n\
     Saved view:\n{}",
    collateral_updates . len (),
    initial_buffer,
    save_response . saved_view );
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
