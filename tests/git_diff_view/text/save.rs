/// Tests for git diff view - save behavior with title/body changes.
/// TextChanged scaffolds should be disregarded during save.

use super::common::*;

/// Deleting a textChanged scaffold should be a no-op.
/// The scaffold respawns in the returned buffer.
#[test]
fn test_delete_text_changed_scaffold_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-del-textchanged",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User deletes the textChanged scaffold under node 1
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "textChanged");

      let response = update_from_and_rerender_buffer(
        &input, driver, config, tantivy, true).await?;

      // DISK: 1.skg should still have the new title
      let node_1 = read_skgnode(repo_path, "1")?;
      assert_eq!(node_1.title, "1 has a new title.",
        "1.skg should still have the new title");

      // DISK: 11.skg should still have the new body
      let node_11 = read_skgnode(repo_path, "11")?;
      assert_eq!(node_11.body, Some("11 has a new body.".to_string()),
        "11.skg should still have the new body");

      // BUFFER: textChanged scaffolds should respawn
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(( )) }) }) }

/// Editing a node with textChanged should update the disk normally.
/// The scaffold should still appear since worktree differs from HEAD.
#[test]
fn test_edit_text_changed_node_updates_disk()
  -> Result<(), Box<dyn Error>>
{
  run_save_test("skg-test-save-edit-textchanged", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // User changes the title of node 1 again
      let input = GIT_DIFF_VIEW.replace(
        "1 has a new title.", "1 has an even newer title.");

      let response = update_from_and_rerender_buffer(
        &input, driver, config, tantivy, true).await?;

      // DISK: 1.skg should have the newest title
      let node_1 = read_skgnode(repo_path, "1")?;
      assert_eq!(node_1.title, "1 has an even newer title.",
        "1.skg should have the edited title");

      // BUFFER: textChanged scaffold should still appear (still differs from HEAD)
      assert!(response.buffer_content.contains("textChanged"),
        "textChanged scaffold should still appear");
      Ok(())
    })
  })
}

/// Editing a textChanged scaffold itself should be a no-op.
/// The scaffold respawns unchanged in the returned buffer.
#[test]
fn test_edit_text_changed_scaffold_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-edit-scaffold",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User tries to change the title of a textChanged scaffold
      let input = GIT_DIFF_VIEW.replace(
        "** (skg textChanged)",
        "** (skg textChanged) User edited this scaffold.");

      let response = update_from_and_rerender_buffer(
        &input, driver, config, tantivy, true).await?;

      // DISK: No changes should occur
      let node_1 = read_skgnode(repo_path, "1")?;
      assert_eq!(node_1.title, "1 has a new title.",
        "1.skg should be unchanged");

      // BUFFER: textChanged scaffolds respawn with original text
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

/// Moving a textChanged scaffold should be a no-op.
/// The scaffold respawns in its original location.
#[test]
fn test_move_text_changed_scaffold_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-move-scaffold",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // Below, user moves the textChanged scaffold
      // from first among its siblings to last.
      let input = "\
* (skg (node (id 1) (source main))) 1 has a new title.
** (skg (node (id 11) (source main))) 11
11 has a new body.
*** (skg textChanged)
** (skg (node (id 12) (source main))) 12
** (skg textChanged)
";

      let response = update_from_and_rerender_buffer(
        &input, driver, config, tantivy, true).await?;

      // DISK: No changes should occur
      let node_1 = read_skgnode(repo_path, "1")?;
      assert_eq!(node_1.title, "1 has a new title.",
        "1.skg should be unchanged");

      // BUFFER: textChanged scaffolds should respawn in correct locations
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

/// Moving a textChanged scaffold to an unedited node should be a no-op.
/// The scaffold respawns where it belongs (under the edited node).
#[test]
fn test_move_text_changed_to_unedited_node_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-move-to-unedited",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User moves a textChanged scaffold to under node 12 (which wasn't edited)
      let input = without_lines_containing(GIT_DIFF_VIEW, "textChanged");
      let input = insert_after(&input, "(id 12)",
        "*** (skg textChanged)");

      let response = update_from_and_rerender_buffer(
        &input, driver, config, tantivy, true).await?;

      // DISK: No changes should occur
      let node_12 = read_skgnode(repo_path, "12")?;
      assert_eq!(node_12.title, "12",
        "12.skg should be unchanged");
      let contains_12 = node_12.contains.unwrap_or_default();
      assert!(contains_12.is_empty(),
        "12.skg should not have any children");

      // BUFFER: textChanged scaffolds should respawn in their correct locations
      // (under nodes 1 and 11, not under 12)
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

//
// Test runner helper
//

fn run_save_test<F>(db_name: &str, test_fn: F) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a TypeDBDriver,
    &'a TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let tantivy_folder = format!("/tmp/tantivy-{}", db_name);

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir.path();
  setup_git_repo_with_fixtures(repo_path)?;

  block_on(async {
    let (config, driver, tantivy) =
      setup_test_dbs(db_name, repo_path.to_str().unwrap(), &tantivy_folder).await?;

    let result = test_fn(&config, &driver, &tantivy, repo_path).await;

    cleanup_test_dbs(db_name, &driver, Some(Path::new(&tantivy_folder))).await?;
    result
  })
}
