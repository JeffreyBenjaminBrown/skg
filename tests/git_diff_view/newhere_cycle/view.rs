/// Tests for git diff view - newhere cycle.
/// A node added as its own child should show (diff new-here).

use super::common::*;
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-newhere-cycle-view",
    |s| Box::pin ( async move {
      test_newhere_cycle (s) . await ?;
      test_newhere_cycle_staged (s) . await ?;
      Ok (( )) } )) }

async fn test_newhere_cycle (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir = TempDir::new()?;
  let gitrepo_path = temp_dir . path();
  setup_gitrepo_with_fixtures (gitrepo_path)?;
  s . reset_with_skgrepo_path (
    "test_newhere_cycle",
    gitrepo_path ) ?;
  let (config, _tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);

  let root_skgids = vec![ID("1" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&config, None, &root_skgids, true)?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW);

  Ok(())
}

async fn test_newhere_cycle_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir = TempDir::new()?;
  let gitrepo_path = temp_dir . path();
  setup_gitrepo_with_fixtures_staged (gitrepo_path)?;
  s . reset_with_skgrepo_path (
    "test_newhere_cycle_staged",
    gitrepo_path ) ?;
  let (config, _tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);

  let root_skgids = vec![ID("1" . to_string())];
  let (actual, _pids, _) : (String, Vec<ID>, _) =
    multi_root_view(&config, None, &root_skgids, true)?;

  assert_buffer_contains(&actual, GIT_DIFF_VIEW_STAGED);

  Ok(())
}
