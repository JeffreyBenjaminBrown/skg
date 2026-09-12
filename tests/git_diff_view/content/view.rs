/// Tests for git diff view - content changes.
/// See fixtures/README.md for the test scenario.

use super::common::*;
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-content-view",
    |s| Box::pin ( async move {
      test_content_diff_with_moved_and_deleted_nodes (s) . await ?;
      test_content_diff_staged (s) . await ?;
      test_no_ghosts_under_indefinitive_occurrence (s) . await ?;
      Ok (( )) } )) }

async fn test_content_diff_with_moved_and_deleted_nodes (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_content_diff_with_moved_and_deleted_nodes",
    repo_path ) ?;
  let (config, _tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string()), ID("new" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&config, None, &root_ids, true)?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW);

  Ok(())
}

/// TODO/fork-fixes.org: no git ghosts under indefinitive nodes. The
/// same transition as above, but with 11 also a view ROOT, so the
/// copy of 11 under 1 draws indefinitive. The removed-member
/// phantoms of 11 must appear only under its definitive (root) copy;
/// the indefinitive copy gets none.
async fn test_no_ghosts_under_indefinitive_occurrence (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_no_ghosts_under_indefinitive_occurrence",
    repo_path ) ?;
  let (config, _tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string()), ID("11" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&config, None, &root_ids, true)?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW_INDEF_NO_GHOSTS);
  assert_eq!(
    // The removed child appears exactly once: under the definitive copy.
    actual . matches ("(id gets-removed)") . count (), 1,
    "expected exactly one occurrence of the removed-member phantom, got:\n{}",
    actual );

  Ok(())
}

/// Staged variant — the phantom axes should report 'staged' not
/// 'unstaged'. Exercises phantom_axes, mk_removed_child_viewnode,
/// and content_goal_list's per-stage paths together.
async fn test_content_diff_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;
  s . reset_with_source_path (
    "test_content_diff_staged",
    repo_path ) ?;
  let (config, _tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);

  let root_ids = vec![ID("1" . to_string()), ID("new" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&config, None, &root_ids, true)?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW_STAGED);

  Ok(())
}
