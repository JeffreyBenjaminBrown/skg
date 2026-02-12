/// Tests for git diff view - newhere cycle.
/// A node added as its own child should show (diff new-here).

use super::common::*;

#[test]
fn test_newhere_cycle() -> Result<(), Box<dyn Error>> {
  let db_name = "skg-test-git-diff-view-newhere-cycle";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-view-newhere-cycle";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir.path();
  setup_git_repo_with_fixtures(repo_path)?;

  block_on(async {
    let (config, driver, _tantivy) =
      setup_test_dbs(db_name, repo_path.to_str().unwrap(), tantivy_folder).await?;

    let root_ids = vec![ID("1".to_string())];
    let actual = multi_root_view(&driver, &config, &root_ids, true).await?;

    assert_buffer_contains(&actual, GIT_DIFF_VIEW);

    cleanup_test_dbs(db_name, &driver, Some(Path::new(tantivy_folder))).await?;
    Ok(())
  })
}
