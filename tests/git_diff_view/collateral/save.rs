/// Tests for collateral view updates preserving diff annotations.
/// When a user saves one buffer, other open views ("collateral views")
/// are re-completed. In diff mode, the collateral views should
/// retain diff annotations (textChanged, new-here, etc.).

use super::common::*;
use std::sync::Arc;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

use std::io::BufReader;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-collateral-save",
    |s| Box::pin ( async move {
      test_collateral_view_preserves_diff_annotations (s) . await ?;
      test_collateral_view_staged_text_and_unstaged_add (s) . await ?;
      Ok (( )) } )) }

/// After saving buffer 1 with a new child, collateral buffer 2
/// should still show textChanged on b and new-here on the new child.
async fn test_collateral_view_preserves_diff_annotations (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir : TempDir = TempDir::new()?;
  let repo_path : &Path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_collateral_view_preserves_diff_annotations",
    repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  // 1. Get an initial diff view of "a".
  let root_ids : Vec<ID> = vec![ID("a" . to_string())];
  let (initial_buffer, pids, viewforest)
    : (String, Vec<ID>, Tree<ViewNode>) =
    multi_root_view (
      &driver, &config, None, &root_ids, true ) . await ?;

  // Sanity: initial view should match expected diff output.
  assert_buffer_contains(&initial_buffer, GIT_DIFF_VIEW);

  // 2. Build a ViewsState with diff_mode_enabled.
  // (Fixture nodes are on disk via setup_git_repo_with_fixtures;
  // the rerender pipeline reads them as needed.)
  let graph : InRustGraphHandle =
    new_handle (InRustGraph::new ());
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : true,
    open_views            : OpenViews::new (),};

  // 3. Register buffer 1 and buffer 2,
  //    both viewing the same viewforest rooted at "a".
  let uri_1 : ViewUri = ViewUri::ContentView ( "buffer-1" . to_string() );
  let uri_2 : ViewUri = ViewUri::ContentView ( "buffer-2" . to_string() );
  views_state . open_views . register_view (
    uri_1 . clone (), viewforest . clone (), &pids );
  views_state . open_views . register_view (
    uri_2 . clone (), viewforest . clone (), &pids );

  // 4. Save buffer 1 with a new child "c" of "a".
  //    This also updates the in-Rust graph and re-renders collateral views.
  let save_input : String = insert_after (
    &initial_buffer, "(id a)",
    "** (skg (node (id c))) c" );
  let (mut stream, read_end) = mk_test_tcp_stream_pair ();
  let _save_response : SaveResponse =
    update_from_and_rerender_buffer (
      &mut stream,
      &save_input, &driver, &config, tantivy, &graph, true,
      &Ok ( uri_1 . clone () ),
      &mut views_state ) . await ?;
  // Close the write end so read_all_lp_messages (which reads to
  // EOF) terminates. (Before the shared-db conversion the stream
  // was dropped when the setup block_on's scope ended.)
  drop (stream);

  // 5. Read streamed collateral views from the TCP stream.
  //    There should be exactly one, for buffer 2.
  let mut reader : BufReader<TcpStream> =
    BufReader::new (read_end);
  // The stream also carries a save-relax-lock message (plan_v2 §8.1) before
  // the collateral-view(s); select the collateral-view by its response-type.
  let collateral_msgs : Vec<String> =
    skg::test_utils::read_all_lp_messages (&mut reader)
    . into_iter ()
    . filter ( |m| m . contains ("collateral-view") )
    . collect ();
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
     *** (skg (textChanged unstaged))" );

  // c should appear with diff new
  // (its .skg file didn't exist at HEAD).
  assert_buffer_contains ( &collateral_buffer,
    "** (skg (node (id c) (unstaged newX newM))) c" );

  // DISK: c.skg should exist (created by the save).
  assert!( repo_path . join ("c.skg") . exists (),
    "c.skg should have been created on disk" );

  Ok (( ))
}

/// Same scenario, but with b's body change staged (git add) before
/// the save. After save, the viewforest mixes stages:
/// - b's textChanged lives on the STAGED side (from the pre-save
///   git add; save didn't touch b).
/// - c is new on the UNSTAGED side (save created it in the
///   worktree; save doesn't git add).
/// - a's contains picked up c on the UNSTAGED side (save rewrote
///   a.skg but didn't stage it).
///
/// Asserts that the collateral re-render correctly attributes each
/// change to the right stage.
async fn test_collateral_view_staged_text_and_unstaged_add (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir : TempDir = TempDir::new()?;
  let repo_path : &Path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;
  s . reset_with_source_path (
    "test_collateral_view_staged_text_and_unstaged_add",
    repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  let root_ids : Vec<ID> = vec![ID("a" . to_string())];
  let (initial_buffer, pids, viewforest)
    : (String, Vec<ID>, Tree<ViewNode>) =
    multi_root_view (
      &driver, &config, None, &root_ids, true ) . await ?;

  // Sanity: initial view has textChanged attributed to staged.
  assert_buffer_contains(&initial_buffer, GIT_DIFF_VIEW_STAGED);

  let graph : InRustGraphHandle =
    new_handle (InRustGraph::new ());
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : true,
    open_views            : OpenViews::new (),};

  let uri_1 : ViewUri = ViewUri::ContentView ( "buffer-1" . to_string() );
  let uri_2 : ViewUri = ViewUri::ContentView ( "buffer-2" . to_string() );
  views_state . open_views . register_view (
    uri_1 . clone (), viewforest . clone (), &pids );
  views_state . open_views . register_view (
    uri_2 . clone (), viewforest . clone (), &pids );

  let save_input : String = insert_after (
    &initial_buffer, "(id a)",
    "** (skg (node (id c))) c" );
  let (mut stream, read_end) = mk_test_tcp_stream_pair ();
  let _save_response : SaveResponse =
    update_from_and_rerender_buffer (
      &mut stream,
      &save_input, &driver, &config, tantivy, &graph, true,
      &Ok ( uri_1 . clone () ),
      &mut views_state ) . await ?;
  // Close the write end so read_all_lp_messages (which reads to
  // EOF) terminates. (Before the shared-db conversion the stream
  // was dropped when the setup block_on's scope ended.)
  drop (stream);

  let mut reader : BufReader<TcpStream> =
    BufReader::new (read_end);
  // The stream also carries a save-relax-lock message (plan_v2 §8.1) before
  // the collateral-view(s); select the collateral-view by its response-type.
  let collateral_msgs : Vec<String> =
    skg::test_utils::read_all_lp_messages (&mut reader)
    . into_iter ()
    . filter ( |m| m . contains ("collateral-view") )
    . collect ();
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

  // b still has textChanged, attributed to staged (save didn't
  // touch b and didn't stage anything).
  assert_buffer_contains ( &collateral_buffer,
    "** (skg (node (id b) (source main))) b\n\
     *** (skg (textChanged staged))" );

  // c is new on the unstaged side — save wrote c.skg to worktree
  // but didn't git add it.
  assert_buffer_contains ( &collateral_buffer,
    "** (skg (node (id c) (unstaged newX newM))) c" );

  assert!( repo_path . join ("c.skg") . exists (),
    "c.skg should have been created on disk" );

  Ok (( ))
}
