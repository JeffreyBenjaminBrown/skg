/// Tests for git diff view - save behavior.
/// See fixtures/README.md for the test scenario.

use super::common::*;
use std::sync::Arc;

/// Deleting a 'removed' node (deleted from disk) should be a no-op.
/// The node respawns in the returned buffer.
#[test]
fn test_delete_removed_node_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test("skg-test-save-del-removed", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // Scenario: User deletes the gets-removed line
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "gets-removed");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
          &mut stream,
          &input, driver, config, tantivy, &graph, true,
          &Err ( String::new () ), &mut views_state
        ) . await?;

      // DISK: gets-removed.skg should still not exist
      assert!(!repo_path . join ("gets-removed.skg") . exists(),
        "gets-removed.skg should stay deleted");

      // DISK: 11.skg should still contain moves and not gets-removed
      let node_11 = read_nodecomplete(repo_path, "11")?;
      let contains_11 = node_11 . contains;
      assert!(contains_11 . contains(&ID("moves" . to_string())),
        "11.skg should still contain moves");
      assert!(!contains_11 . contains(&ID("gets-removed" . to_string())),
        "11.skg should not contain gets-removed");

      // BUFFER: gets-removed should respawn with (diff removed)
      assert_buffer_contains(&response . saved_view,
                             GIT_DIFF_VIEW);
      Ok (( )) } ) } ) }

/// Deleting a 'removed-here' phantom node should be a no-op.
/// The node respawns in the returned buffer.
#[test]
fn test_delete_removed_here_node_respawns()
  -> Result<(), Box<dyn Error>>
{
  run_save_test("skg-test-save-del-removed-here", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // User deletes the removed-here node under 12 (called 'moves')
      let input =
        without_lines_containing(GIT_DIFF_VIEW, "(unstaged removedM)");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state
      ) . await?;

      // DISK: 12.skg should still have empty contains
      let node_12 = read_nodecomplete(repo_path, "12")?;
      let contains_12 = node_12 . contains;
      assert!(!contains_12 . contains(&ID("moves" . to_string())),
        "12.skg should not contain moves");

      // DISK: moves.skg should still exist
      assert!(repo_path . join ("moves.skg") . exists(),
        "moves.skg should still exist");

      // BUFFER: phantom moves should respawn under 12
      assert_buffer_contains( &response . saved_view,
                              GIT_DIFF_VIEW);
      Ok (( )) }) }) }

/// Deleting a 'new-here' node should update the disk.
/// The node disappears from its new location but remains as phantom in old location.
#[test]
fn test_delete_new_here_updates_disk()
  -> Result<(), Box<dyn Error>>
{
  run_save_test("skg-test-save-del-new-here", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // User deleted 'moves' under 11 (the new-here one)
      // The "moves under 11" line is the new-here phantom (membership added).
      let input = without_lines_containing(GIT_DIFF_VIEW, "(unstaged newM)");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state
      ) . await?;

      // DISK: 11.skg should no longer contain moves
      let node_11 = read_nodecomplete(repo_path, "11")?;
      let contains_11 = node_11 . contains;
      assert!(!contains_11 . contains(&ID("moves" . to_string())),
        "11.skg should no longer contain moves");

      // DISK: moves.skg should still exist
      assert!(repo_path . join ("moves.skg") . exists(),
        "moves.skg should still exist");

      // BUFFER: moves gone from 11, still under 12 as removed-here
      let expected = without_lines_containing(
        GIT_DIFF_VIEW, "(unstaged newM)");
      assert_buffer_contains(&response . saved_view,
                             &expected);
      Ok(())
    })
  })
}

/// Adding a new child should create it on disk.
#[test]
fn test_add_new_child_creates_on_disk()
  -> Result<(), Box<dyn Error>>
{
  run_save_test("skg-test-save-add-child", |config, driver, tantivy, repo_path| {
    Box::pin(async move {
      // User added 'newer' as child of 12
      let input = insert_after(
        // PITFALL: It's weird but legal to specify a new node's id.
        GIT_DIFF_VIEW, "(id 12)",
        "*** (skg (node (id newer))) newer");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state
      ) . await?;

      // DISK: newer.skg should be created with correct id and title
      assert!(repo_path . join ("newer.skg") . exists(),
        "newer.skg should be created");
      let node_newer = read_nodecomplete(repo_path, "newer")?;
      assert_eq!(&node_newer . pid, &ID("newer" . to_string()),
        "newer.skg should have id 'newer'");
      assert_eq!(node_newer . title, "newer",
        "newer.skg should have title 'newer'");

      // DISK: 12.skg should contain newer
      let node_12 = read_nodecomplete(repo_path, "12")?;
      let contains_12 = node_12 . contains;
      assert!(contains_12 . contains(&ID("newer" . to_string())),
        "12.skg should contain newer");

      // BUFFER: 12 has moves (removed-here) and newer (new).
      // PITFALL: I'm not sure 12's children will be in this order.
      let expected = insert_after(GIT_DIFF_VIEW, "(id 12)",
        "*** (skg (node (id newer) (unstaged newX newM))) newer");
      assert_buffer_contains(&response . saved_view,
                             &expected);
      Ok(())
    })
  })
}

#[test]
fn test_diff_mode_as_subscribee_regenerates_phantom_children()
  -> Result<(), Box<dyn Error>>
{
  run_save_test_with_setup(
    "skg-test-save-diff-as-subscribee-regenerates",
    setup_git_repo_with_subscribee_fixtures,
    |config, driver, tantivy, _repo_path| { Box::pin(async move {
      let input = "\
* (skg (node (id 1) (source main))) 1
** (skg subscribeeCol)
*** (skg (node (id 11) (source main))) 11
**** (skg (node (id moves) (unstaged newM))) moves
";

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state
      ) . await?;

      assert_buffer_contains(
        &response . saved_view,
        "*** (skg (node (id 11) (source main))) 11\n\
         **** (skg (node (id gets-removed) (source main) indef (unstaged removedX removedM))) gets-removed\n\
         **** (skg (node (id moves) (source main))) moves" );
      Ok (( )) }) })
}

/// #1 fix: a subscribee removed from the subscriber's subscribes_to list (but
/// whose .skg still exists) renders as a phantom with (unstaged removedM). Its
/// relation is subscribes_to, not contains, so the membership marker comes from
/// build_child_data's net-removal fallback rather than phantom_axes(contains).
/// Without the fix the phantom would carry NO membership marker.
#[test]
fn test_diff_mode_removed_subscribee_shows_removedM()
  -> Result<(), Box<dyn Error>>
{
  run_save_test_with_setup(
    "skg-test-save-diff-removed-subscribee",
    setup_git_repo_with_removed_subscribee_fixtures,
    |config, driver, tantivy, _repo_path| { Box::pin(async move {
      let input = "\
* (skg (node (id 1) (source main))) 1
** (skg subscribeeCol)
*** (skg (node (id 11) (source main))) 11
";

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views        : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state
      ) . await?;

      assert_buffer_contains(
        &response . saved_view,
        "*** (skg (node (id 22) (source main) indef (unstaged removedM))) 22" );
      Ok (( )) }) })
}

//
// Test runner helper
//

fn run_save_test<F>(db_name: &str, test_fn: F) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    db_name, setup_git_repo_with_fixtures, test_fn)
}

fn run_save_test_with_setup<S, F>(
  db_name: &str,
  setup: S,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  S: FnOnce(&Path) -> Result<Repository, Box<dyn Error>>,
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let tantivy_folder = format!("/tmp/tantivy-{}", db_name);

  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup (repo_path)?;

  block_on(async {
    let (config, driver, mut tantivy) =
      setup_test_dbs(db_name, repo_path . to_str() . unwrap(), &tantivy_folder) . await?;

    let result = test_fn(&config, &driver, &mut tantivy, repo_path) . await;

    cleanup_test_dbs(db_name, &driver, Some(Path::new (&tantivy_folder))) . await?;
    result
  })
}
