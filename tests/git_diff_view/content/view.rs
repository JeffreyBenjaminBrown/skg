/// Tests for git diff view - content changes.
/// See fixtures/README.md for the test scenario.

use super::common::*;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-content-view",
    |s| Box::pin ( async move {
      test_content_diff_with_moved_and_deleted_nodes (s) . await ?;
      test_content_diff_staged (s) . await ?;
      Ok (( )) } )) }

async fn test_content_diff_with_moved_and_deleted_nodes (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_content_diff_with_moved_and_deleted_nodes",
    repo_path ) . await ?;
  let (config, driver, _tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string()), ID("new" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&driver, &config, None, &root_ids, true) . await?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW);

  Ok(())
}

/// Staged variant — the phantom axes should report 'staged' not
/// 'unstaged'. Exercises phantom_axes, mk_removed_child_viewnode,
/// and content_goal_list's per-stage paths together.
async fn test_content_diff_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;
  s . reset_with_source_path (
    "test_content_diff_staged",
    repo_path ) . await ?;
  let (config, driver, _tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string()), ID("new" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&driver, &config, None, &root_ids, true) . await?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW_STAGED);

  Ok(())
}
