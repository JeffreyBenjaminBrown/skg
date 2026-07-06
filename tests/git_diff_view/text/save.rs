/// Tests for git diff view - save behavior with title/body changes.
/// TextChanged scaffolds should be disregarded during save.

use super::common::*;
use std::sync::Arc;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-text-save",
    |s| Box::pin ( async move {
      test_delete_text_changed_scaffold_respawns (s) . await ?;
      test_edit_text_changed_node_updates_disk (s) . await ?;
      test_edit_text_changed_scaffold_respawns (s) . await ?;
      test_move_text_changed_scaffold_respawns (s) . await ?;
      test_move_text_changed_to_unedited_node_respawns (s) . await ?;
      test_delete_text_changed_scaffold_respawns_staged (s) . await ?;
      Ok (( )) } )) }

/// Deleting a textChanged scaffold should be a no-op.
/// The scaffold respawns in the returned buffer.
async fn test_delete_text_changed_scaffold_respawns (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-del-textchanged",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User deletes the textChanged scaffold under node 1
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "textChanged");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: 1.skg should still have the new title
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert_eq!(node_1 . title, "1 has a new title.",
        "1.skg should still have the new title");

      // DISK: 11.skg should still have the new body
      let node_11 = read_nodecomplete(repo_path, "11")?;
      assert_eq!(node_11 . body, Some("11 has a new body." . to_string()),
        "11.skg should still have the new body");

      // BUFFER: textChanged scaffolds should respawn
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(( )) }) }) . await }

/// Editing a node with textChanged should update the disk normally.
/// The scaffold should still appear since worktree differs from HEAD.
async fn test_edit_text_changed_node_updates_disk (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(s, "skg-test-save-edit-textchanged", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // User changes the title of node 1 again
      let input = GIT_DIFF_VIEW . replace(
        "1 has a new title.", "1 has an even newer title.");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: 1.skg should have the newest title
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert_eq!(node_1 . title, "1 has an even newer title.",
        "1.skg should have the edited title");

      // BUFFER: textChanged scaffold should still appear (still differs from HEAD)
      assert!(
        response . saved_view . contains ("textChanged"),
        "textChanged scaffold should still appear");
      Ok(())
    })
  }) . await
}

/// Editing a textChanged scaffold itself should be a no-op.
/// The scaffold respawns unchanged in the returned buffer.
async fn test_edit_text_changed_scaffold_respawns (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-edit-scaffold",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User tries to change the title of a textChanged scaffold
      let input = GIT_DIFF_VIEW . replace(
        "** (skg (textChanged unstaged))",
        "** (skg (textChanged unstaged)) User edited this scaffold.");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: No changes should occur
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert_eq!(node_1 . title, "1 has a new title.",
        "1.skg should be unchanged");

      // BUFFER: textChanged scaffolds respawn with original text
      assert_buffer_contains( &response . saved_view,
                              GIT_DIFF_VIEW);
      Ok (( )) }) }) . await }

/// Moving a textChanged scaffold should be a no-op.
/// The scaffold respawns in its original location.
async fn test_move_text_changed_scaffold_respawns (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-move-scaffold",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // Below, user moves the textChanged scaffold
      // from first among its siblings to last.
      let input = "\
* (skg (node (id 1) (source main))) 1 has a new title.
** (skg (node (id 11) (source main))) 11
11 has a new body.
*** (skg (textChanged unstaged))
** (skg (node (id 12) (source main))) 12
** (skg (textChanged unstaged))
";

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: No changes should occur
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert_eq!(node_1 . title, "1 has a new title.",
        "1.skg should be unchanged");

      // BUFFER: textChanged scaffolds should respawn in correct locations
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Moving a textChanged scaffold to an unedited node should be a no-op.
/// The scaffold respawns where it belongs (under the edited node).
async fn test_move_text_changed_to_unedited_node_respawns (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-move-to-unedited",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User moves a textChanged scaffold to under node 12 (which wasn't edited)
      let input = without_lines_containing(GIT_DIFF_VIEW, "textChanged");
      let input = insert_after(&input, "(id 12)",
        "*** (skg (textChanged unstaged))");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: No changes should occur
      let node_12 = read_nodecomplete(repo_path, "12")?;
      assert_eq!(node_12 . title, "12",
        "12.skg should be unchanged");
      let contains_12 = node_12 . contains;
      assert!(contains_12 . is_empty(),
        "12.skg should not have any children");

      // BUFFER: textChanged scaffolds should respawn in their correct locations
      // (under nodes 1 and 11, not under 12)
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Same as 'test_delete_text_changed_scaffold_respawns' but with the
/// fixture transition staged (git add) rather than unstaged. The
/// respawned scaffold should report '(textChanged staged)' instead
/// of '(textChanged unstaged)' -- guards the save-rerender pipeline's
/// per-stage attribution for text changes, mirroring
/// ids::save::test_delete_id_col_scaffold_respawns_staged.
async fn test_delete_text_changed_scaffold_respawns_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test_staged(
    s,
    "skg-test-save-del-textchanged-staged",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User deletes the textChanged scaffold under node 1
      let input = without_lines_containing(
        GIT_DIFF_VIEW_STAGED, "textChanged");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: 1.skg should still have the new title
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert_eq!(node_1 . title, "1 has a new title.",
        "1.skg should still have the new title");

      // DISK: 11.skg should still have the new body
      let node_11 = read_nodecomplete(repo_path, "11")?;
      assert_eq!(node_11 . body, Some("11 has a new body." . to_string()),
        "11.skg should still have the new body");

      // BUFFER: textChanged scaffolds should respawn, staged
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW_STAGED);
      Ok(( )) }) }) . await }

//
// Test runner helper
//

async fn run_save_test<F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_git_repo_with_fixtures, test_fn) . await
}

async fn run_save_test_staged<F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_git_repo_with_fixtures_staged, test_fn) . await
}

async fn run_save_test_with_setup<S, F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  setup   : S,
  test_fn : F,
) -> Result<(), Box<dyn Error>>
where
  S: FnOnce (&Path) -> Result<Repository, Box<dyn Error>>,
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup (repo_path)?;
  s . reset_with_source_path (subtest_name, repo_path) . await ?;

  test_fn(&s . config, &s . driver, &mut s . tantivy, repo_path) . await
}
