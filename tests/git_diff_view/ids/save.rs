/// Tests for git diff view - save behavior with id changes.
/// Deleting the whole idFolder is a no-op (absence means no opinion),
/// but editing an idFolder's membership -- deleting, adding, editing or
/// relocating id properties -- aborts the save with IDFolder_Edited
/// (TODO/full-schema/8_readonly-set-ergonomics.org). Net-removed
/// diff entries (removedR) are git history, not membership claims,
/// and do not trip the check.

use super::common::*;
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-ids-save",
    |s| Box::pin ( async move {
      test_delete_id_folder_respawns (s) . await ?;
      test_delete_id_properties_aborts (s) . await ?;
      test_edit_id_property_aborts (s) . await ?;
      test_reorder_id_properties_saves (s) . await ?;
      test_move_id_properties_to_child_aborts (s) . await ?;
      test_delete_id_folder_respawns_staged (s) . await ?;
      Ok (( )) } )) }

/// Deleting an idFolder should be a no-op.
/// The non-vognode respawns in the returned buffer.
async fn test_delete_id_folder_respawns (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-del-idFolder",
    |config, tantivy, gitrepo_path| { Box::pin(async move {
      // User deletes the entire idFolder (and its children)
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "skg id");

      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_nodecomplete(gitrepo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("1" . to_string())),
        "1.skg should still have id '1'");
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      assert!(node_1 . all_ids () . any(|id| id == &ID("3" . to_string())),
        "1.skg should still have id '3'");
      assert!(!node_1 . all_ids () . any(|id| id == &ID("2" . to_string())),
        "1.skg should not have id '2'");

      // BUFFER: idFolder should respawn
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Deleting individual id properties (keeping the idFolder) aborts the
/// save with an IDFolder_Edited error, and the disk is untouched.
async fn test_delete_id_properties_aborts (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-del-ids",
    |config, tantivy, gitrepo_path| { Box::pin(async move {
      // User deletes the id properties but keeps the idFolder
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "(skg id)");

      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("editing idFolder membership must abort the save") );
      assert!(err . contains ("IDFolder_Edited"),
        "the error should be IDFolder_Edited: {}", err);

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_nodecomplete(gitrepo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      Ok(()) }) }) . await
}

/// Editing an id property's text aborts the save with an
/// IDFolder_Edited error, and the disk is untouched.
async fn test_edit_id_property_aborts (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-edit-id",
    |config, tantivy, gitrepo_path| { Box::pin(async move {
      // User tries to change an id value in the non-vognode
      let input = GIT_DIFF_VIEW . replace(
        "(unstaged addedR)) 2'", "(unstaged addedR)) 2-modified");

      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("editing an id property must abort the save") );
      assert!(err . contains ("IDFolder_Edited"),
        "the error should be IDFolder_Edited: {}", err);

      // DISK: 1.skg should still have the original worktree ids
      let node_1 = read_nodecomplete(gitrepo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      assert!(!node_1 . all_ids () . any(|id| id == &ID("2-modified" . to_string())),
        "1.skg should not have the modified id");
      Ok(()) }) }) . await
}

/// Reordering id properties passes the membership check (multiset
/// equality); the rerender re-sorts them anyway.
async fn test_reorder_id_properties_saves (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-reorder-ids",
    |config, tantivy, _gitrepo_path| { Box::pin(async move {
      let input = GIT_DIFF_VIEW
        // Swap the two plain id lines (1 and 3).
        . replace ("*** (skg id) 1", "*** (skg id) SWAP")
        . replace ("*** (skg id) 3", "*** (skg id) 1")
        . replace ("*** (skg id) SWAP", "*** (skg id) 3");
      assert_ne! (input, GIT_DIFF_VIEW);
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Moving the idFolder to another node aborts the save: the receiving
/// node's real ID list does not match the moved idFolder's claims.
async fn test_move_id_properties_to_child_aborts (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-move-ids",
    |config, tantivy, gitrepo_path| { Box::pin(async move {
      // User moves id properties to be children of 'child' node
      let input = "\
* (skg (node (id 1) (repo main))) 1
** (skg (node (id child) (repo main))) child
*** (skg idFolder)
**** (skg id) 1
**** (skg id (unstaged removedR)) 2
**** (skg id (unstaged addedR)) 2'
**** (skg id) 3
";

      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("an idFolder moved under another node must abort the save") );
      assert!(err . contains ("IDFolder_Edited"),
        "the error should be IDFolder_Edited: {}", err);

      // DISK: child.skg should not have any new ids
      let node_child = read_nodecomplete(gitrepo_path, "child")?;
      assert_eq!(1 + node_child . extra_ids . len(), 1,
        "child.skg should still only have its original id");
      assert_eq!(&node_child . pid, &ID("child" . to_string()),
        "child.skg should still have id 'child'");

      // DISK: 1.skg should still have its ids
      let node_1 = read_nodecomplete(gitrepo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      Ok(()) }) }) . await
}

/// Same as 'test_delete_id_folder_respawns' but with the fixture
/// transition staged (git add) rather than unstaged. The respawned IDFolder
/// children should report '(staged ...)' tags — this verifies that the
/// save-rerender pipeline (reconcile_idFolder_children + complete_viewforest) honors the
/// staged/unstaged distinction instead of merging stages and defaulting
/// to unstaged.
async fn test_delete_id_folder_respawns_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test_staged(
    s,
    "skg-test-save-del-idFolder-staged",
    |config, tantivy, _gitrepo_path| { Box::pin(async move {
      let input = without_lines_containing(
        GIT_DIFF_VIEW_STAGED, "skg id");

      let graph : InRustGraphHandle =
        graph_handle_from_config (&config)?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW_STAGED);
      Ok(()) }) }) . await
}

//
// Test runner helpers
//

async fn run_save_test<F>(
  s: &mut SharedStoreSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,

    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_gitrepo_with_fixtures, test_fn) . await
}

async fn run_save_test_staged<F>(
  s: &mut SharedStoreSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,

    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_gitrepo_with_fixtures_staged, test_fn) . await
}

async fn run_save_test_with_setup<S, F>(
  s: &mut SharedStoreSession,
  subtest_name: &str,
  setup   : S,
  test_fn : F,
) -> Result<(), Box<dyn Error>>
where
  S: FnOnce (&Path) -> Result<Repository, Box<dyn Error>>,
  F: for<'a> FnOnce(
    &'a SkgConfig,

    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let temp_dir = TempDir::new()?;
  let gitrepo_path = temp_dir . path();
  setup (gitrepo_path)?;
  s . reset_with_repo_path (subtest_name, gitrepo_path) ?;

  test_fn(&s . config, &mut s . tantivy, gitrepo_path) . await
}
