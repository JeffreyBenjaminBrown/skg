/// Tests for git diff view - id changes.
/// See fixtures/ and README.org for the test scenario.

use super::common::*;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-ids-view",
    |s| Box::pin ( async move {
      test_ids_diff_shows_id_col_scaffold (s) . await ?;
      test_ids_diff_staged_shows_staged_tag (s) . await ?;
      Ok (( )) } )) }

async fn test_ids_diff_shows_id_col_scaffold (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;
  s . reset_with_source_path (
    "test_ids_diff_shows_id_col_scaffold",
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

/// When the same id changes are staged (git add) rather than unstaged,
/// the IDCol children should say `(staged ...)` not `(unstaged ...)`.
async fn test_ids_diff_staged_shows_staged_tag (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures_staged (repo_path)?;
  s . reset_with_source_path (
    "test_ids_diff_staged_shows_staged_tag",
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
