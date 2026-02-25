/// Tests for git diff view - save pipeline with newhere cycle.
/// The save pipeline (complete_viewtree) should mark a cycle child
/// as (diff new-here) when it was newly added to its parent's contains.

use super::common::*;

/// Round-trip through the save pipeline: render the initial view,
/// then save and re-render. The cycle child should keep (diff new-here).
#[test]
fn test_newhere_cycle_survives_save()
  -> Result<(), Box<dyn Error>>
{
  let db_name = "skg-test-git-diff-view-newhere-cycle-save";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-view-newhere-cycle-save";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir.path();
  setup_git_repo_with_fixtures(repo_path)?;

  block_on(async {
    let (config, driver, tantivy) =
      setup_test_dbs(db_name, repo_path.to_str().unwrap(), tantivy_folder).await?;

    // First render the initial view (view pipeline — known to work).
    let root_ids = vec![ID("1".to_string())];
    let (initial_view, _map, _pids, _) : (String, SkgNodeMap, Vec<ID>, _) =
      multi_root_view(&driver, &config, &root_ids, true).await?;

    // Round-trip through the save pipeline.
    let response = update_from_and_rerender_buffer(
      &initial_view, &driver, &config, &tantivy, true, SkgNodeMap::new() ).await?;

    assert_buffer_contains( &response . response . saved_view,
                            GIT_DIFF_VIEW);

    cleanup_test_dbs(db_name, &driver, Some(Path::new(tantivy_folder))).await?;
    Ok(())
  })
}
