/// Tests for collateral view updates preserving diff annotations.
/// When a user saves one buffer, other open views ("collateral views")
/// are re-completed. In diff mode, the collateral views should
/// retain diff annotations (textChanged, new-here, etc.).

use super::common::*;

use std::io::BufReader;

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

  let (_config, driver, _tantivy, _save_response, _initial_buffer,
       uri_2, read_end)
    : (SkgConfig, TypeDBDriver, TantivyIndex, SaveResponse,
       String, ViewUri, TcpStream) =
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
      let uri_1 : ViewUri = ViewUri::ContentView ( "buffer-1" . to_string() );
      let uri_2 : ViewUri = ViewUri::ContentView ( "buffer-2" . to_string() );
      conn_state . memory . register_view (
        uri_1 . clone (), forest . clone (), &pids );
      conn_state . memory . register_view (
        uri_2 . clone (), forest . clone (), &pids );

      // 4. Save buffer 1 with a new child "c" under "a".
      //    This also updates memory and re-renders collateral views.
      let save_input : String = insert_after (
        &initial_buffer, "(id a)",
        "** (skg (node (id c))) c" );
      let (mut stream, read_end) = mk_test_tcp_stream_pair ();
      let save_response : SaveResponse =
        update_from_and_rerender_buffer (
          &mut stream,
          &save_input, &driver, &config, &mut tantivy,
          true, map,
          &Ok ( uri_1 . clone () ),
          &mut conn_state ) . await ?;

      Result::<_, Box<dyn Error>>::Ok ((
        config, driver, tantivy, save_response,
        initial_buffer, uri_2, read_end )) } ) ?;

  // 5. Read streamed collateral views from the TCP stream.
  //    There should be exactly one, for buffer 2.
  let mut reader : BufReader<TcpStream> =
    BufReader::new (read_end);
  let collateral_msgs : Vec<String> =
    skg::test_utils::read_all_lp_messages (&mut reader);
  assert_eq! ( collateral_msgs . len (), 1,
    "Expected 1 collateral view, got {}", collateral_msgs . len () );
  let body : &str = &collateral_msgs[0];
  assert! ( body . contains ("collateral-view"),
    "Expected collateral-view response, got: {}", body );
  assert! ( body . contains (&uri_2 . repr_in_client ()),
    "Collateral update should be for buffer 2, got: {}", body );
  let collateral_buffer : String =
    skg::test_utils::extract_string_field_from_sexp (body, "content")
    . expect ("content field not found in collateral-view sexp");

  // 7. Verify diff annotations in the collateral buffer.

  // b should still have textChanged.
  assert_buffer_contains ( &collateral_buffer,
    "** (skg (node (id b) (source main))) b\n\
     *** (skg textChanged)" );

  // c should appear with diff new
  // (its .skg file didn't exist at HEAD).
  assert_buffer_contains ( &collateral_buffer,
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
