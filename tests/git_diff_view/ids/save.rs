/// Tests for git diff view - save behavior with id changes.
/// IdCol scaffolds and their children should be disregarded during save.

use super::common::*;

/// Deleting an idCol scaffold should be a no-op.
/// The scaffold respawns in the returned buffer.
#[test]
fn test_delete_id_col_scaffold_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-del-idcol",
    |config, graph, tantivy, repo_path| { Box::pin(async move {
      // User deletes the entire idCol scaffold (and its children)
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "skg id");

      let response = update_from_and_rerender_buffer(
        &input, graph, config, tantivy, true).await?;

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_skgnode(repo_path, "1")?;
      assert!(node_1.ids.contains(&ID("1".to_string())),
        "1.skg should still have id '1'");
      assert!(node_1.ids.contains(&ID("2'".to_string())),
        "1.skg should still have id '2''");
      assert!(node_1.ids.contains(&ID("3".to_string())),
        "1.skg should still have id '3'");
      assert!(!node_1.ids.contains(&ID("2".to_string())),
        "1.skg should not have id '2'");

      // BUFFER: idCol scaffold should respawn
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

/// Deleting individual id scaffolds should be a no-op.
/// The scaffolds respawn in the returned buffer.
#[test]
fn test_delete_id_scaffolds_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-del-ids",
    |config, graph, tantivy, repo_path| { Box::pin(async move {
      // User deletes the id scaffolds but keeps the idCol
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "(skg id)");

      let response = update_from_and_rerender_buffer(
        &input, graph, config, tantivy, true).await?;

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_skgnode(repo_path, "1")?;
      assert!(node_1.ids.contains(&ID("2'".to_string())),
        "1.skg should still have id '2''");

      // BUFFER: id scaffolds should respawn
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

/// Editing an id scaffold should be a no-op.
/// The scaffold respawns unchanged in the returned buffer.
#[test]
fn test_edit_id_scaffold_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-edit-id",
    |config, graph, tantivy, repo_path| { Box::pin(async move {
      // User tries to change an id value in the scaffold
      let input = GIT_DIFF_VIEW.replace(
        "(diff new)) 2'", "(diff new)) 2-modified");

      let response = update_from_and_rerender_buffer(
        &input, graph, config, tantivy, true).await?;

      // DISK: 1.skg should still have the original worktree ids
      let node_1 = read_skgnode(repo_path, "1")?;
      assert!(node_1.ids.contains(&ID("2'".to_string())),
        "1.skg should still have id '2''");
      assert!(!node_1.ids.contains(&ID("2-modified".to_string())),
        "1.skg should not have the modified id");

      // BUFFER: id scaffolds should respawn with original values
      assert_buffer_contains(
        &response.buffer_content, GIT_DIFF_VIEW);
      Ok(()) }) })
}

/// Moving id scaffolds to another node should be a no-op.
/// The scaffolds respawn in their original location.
#[test]
fn test_move_id_scaffolds_to_child_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test(
    "skg-test-save-move-ids",
    |config, graph, tantivy, repo_path| { Box::pin(async move {
      // User moves id scaffolds to be children of 'child' node
      let input = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id child) (source main))) child
*** (skg idCol) its IDs
**** (skg id) 1
**** (skg id (diff removed)) 2
**** (skg id (diff new)) 2'
**** (skg id) 3
";

      let response = update_from_and_rerender_buffer(
        &input, graph, config, tantivy, true).await?;

      // DISK: child.skg should not have any new ids
      let node_child = read_skgnode(repo_path, "child")?;
      assert_eq!(node_child.ids.len(), 1,
        "child.skg should still only have its original id");
      assert!(node_child.ids.contains(&ID("child".to_string())),
        "child.skg should still have id 'child'");

      // DISK: 1.skg should still have its ids
      let node_1 = read_skgnode(repo_path, "1")?;
      assert!(node_1.ids.contains(&ID("2'".to_string())),
        "1.skg should still have id '2''");

      // BUFFER: id scaffolds should respawn under node 1
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
    &'a Graph,
    &'a TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let tantivy_folder = format!("/tmp/tantivy-{}", db_name);

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir.path();
  setup_git_repo_with_fixtures(repo_path)?;

  { let rt : Runtime = Runtime::new()?;
  rt.block_on(async {
    let (config, graph, tantivy) =
      setup_test_dbs(db_name, repo_path.to_str().unwrap(), &tantivy_folder).await?;

    let result = test_fn(&config, &graph, &tantivy, repo_path).await;

    cleanup_test_dbs(&graph, Some(Path::new(&tantivy_folder))).await?;
    result
  }) }
}
