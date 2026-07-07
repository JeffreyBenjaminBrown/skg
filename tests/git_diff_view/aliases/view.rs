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

/// Regression for TODO/more.org, "aliases should be merged, not
/// added": saving a diff-mode buffer that ALREADY shows its aliases
/// (any rendered diff view does) must not duplicate them. The bug:
/// process_activeNode_diff prepended a SECOND AliasCol without
/// checking for the one the buffer carried, and both then reconciled
/// to the full alias list.
#[test]
fn test_saving_a_diff_view_with_aliases_shown_does_not_duplicate_them()
  -> Result<(), Box<dyn Error>>
{
  let db_name = "skg-test-git-diff-aliases-resave";
  let tantivy_folder = "/tmp/tantivy-test-git-diff-aliases-resave";

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup_git_repo_with_fixtures (repo_path)?;

  block_on(async {
    let (config, driver, mut tantivy) =
      setup_test_dbs(db_name, repo_path . to_str() . unwrap(), tantivy_folder) . await?;

    let root_ids = vec![ID("1" . to_string())];
    let (rendered, _pids, _) : (String, Vec<ID>, _) =
      multi_root_view(&driver, &config, None, &root_ids, true) . await?;

    let graph : InRustGraphHandle = new_handle (InRustGraph::new ());
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let (mut stream, _) = mk_test_tcp_stream_pair ();
    let response = update_from_and_rerender_buffer (
      &mut stream,
      &rendered, &driver, &config, &mut tantivy, &graph, true,
      &Err ( String::new () ), &mut views_state ) . await?;
    assert! ( response . errors . is_empty (),
      "re-saving the rendered diff view must not error: {:?}",
      response . errors );
    let saved : &str = & response . saved_view;
    for alias in ["old-alias", "new-alias", "keep"] {
      let count : usize = saved . matches (alias) . count ();
      assert_eq! ( count, 1,
        "alias {:?} must appear exactly once, appears {} times:\n{}",
        alias, count, saved ); }
    assert_eq! ( saved . matches ("aliasCol") . count (), 1,
      "exactly one aliasCol must survive the re-save:\n{}", saved );

    cleanup_test_dbs(db_name, &driver,
                     Some(Path::new (tantivy_folder))
                    ) . await?;
    Ok(())
  })
}
