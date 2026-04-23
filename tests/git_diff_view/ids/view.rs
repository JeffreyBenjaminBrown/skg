/// Tests for git diff view - id changes.
/// See fixtures/ and README.org for the test scenario.

use super::common::*;

#[test]
fn test_ids_diff_shows_id_col_scaffold()
  -> Result<(), Box<dyn Error>>
{
  let db_name = "skg-test-git-diff-ids";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-ids";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;

  block_on(async {
    let (config, driver, _tantivy) =
      setup_test_dbs(db_name, repo_path . to_str() . unwrap(), tantivy_folder) . await?;

    let root_ids = vec![ID("1" . to_string())];
    let (actual, _pids, _) : (String, Vec<ID>, _) =
      multi_root_view(&driver, &config, &root_ids, true) . await?;

    assert_buffer_contains(&actual, GIT_DIFF_VIEW);

    cleanup_test_dbs(db_name, &driver,
                     Some(Path::new (tantivy_folder))
                    ) . await?;
    Ok(())
  })
}

/// When the same id changes are staged (git add) rather than unstaged,
/// the IDCol children should say `(staged ...)` not `(unstaged ...)`.
#[test]
fn test_ids_diff_staged_shows_staged_tag()
  -> Result<(), Box<dyn Error>>
{
  let db_name = "skg-test-git-diff-ids-staged";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-ids-staged";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;

  block_on(async {
    let (config, driver, _tantivy) =
      setup_test_dbs(db_name, repo_path . to_str() . unwrap(), tantivy_folder) . await?;

    let root_ids = vec![ID("1" . to_string())];
    let (actual, _pids, _) : (String, Vec<ID>, _) =
      multi_root_view(&driver, &config, &root_ids, true) . await?;

    assert_buffer_contains(&actual, GIT_DIFF_VIEW_STAGED);

    cleanup_test_dbs(db_name, &driver,
                     Some(Path::new (tantivy_folder))
                    ) . await?;
    Ok(())
  })
}
