/// Tests for git diff view - aliases-list changes.
/// A node's `aliases` change between HEAD and worktree should surface an
/// AliasCol scaffold with per-alias diff markers -- emitted by the diff
/// overlay (render/diff.rs prepend_aliascol_with_children), the alias mirror
/// of the IDCol diff.

use super::common::*;

#[test]
fn test_aliases_diff_shows_alias_col_scaffold()
  -> Result<(), Box<dyn Error>>
{
  let db_name = "skg-test-git-diff-aliases";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-aliases";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;

  block_on(async {
    let (config, driver, _tantivy) =
      setup_test_dbs(db_name, repo_path . to_str() . unwrap(), tantivy_folder) . await?;

    let root_ids = vec![ID("1" . to_string())];
    let (actual, _pids, _) : (String, Vec<ID>, _) =
      multi_root_view(&driver, &config, None, &root_ids, true) . await?;

    assert_buffer_contains(&actual, GIT_DIFF_VIEW);

    cleanup_test_dbs(db_name, &driver,
                     Some(Path::new (tantivy_folder))
                    ) . await?;
    Ok(())
  })
}
