/// Tests for git diff view - title/body changes.
/// See fixtures/ for the test scenario.

use super::common::*;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-text-view",
    |s| Box::pin ( async move {
      test_title_diff_shows_text_changed_scaffolds (s) . await ?;
      test_title_diff_staged_shows_staged_scaffolds (s) . await ?;
      Ok (( )) } )) }

async fn test_title_diff_shows_text_changed_scaffolds (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_title_diff_shows_text_changed_scaffolds",
    repo_path ) . await ?;
  let (config, driver, _tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&driver, &config, None, &root_ids, true) . await?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW);

  Ok(())
}

/// Same scenario but with the text changes staged rather than
/// unstaged — asserts the TextChanged scaffold says 'staged' not
/// 'unstaged'. Exercises the per-stage scaffold emission after a
/// 'git add' that matches what the save-rerender path would see
/// when the worktree has been rewritten to match the index.
async fn test_title_diff_staged_shows_staged_scaffolds (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;
  s . reset_with_source_path (
    "test_title_diff_staged_shows_staged_scaffolds",
    repo_path ) . await ?;
  let (config, driver, _tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&driver, &config, None, &root_ids, true) . await?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW_STAGED);

  Ok(())
}
