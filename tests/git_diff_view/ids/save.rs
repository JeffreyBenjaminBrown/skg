/// Tests for git diff view - save behavior with id changes.
/// Deleting the whole idCol is a no-op (absence means no opinion),
/// but editing an idCol's membership -- deleting, adding, editing or
/// relocating id scaffolds -- aborts the save with IDCol_Edited
/// (TODO/full-schema/8_readonly-set-ergonomics.org). Net-removed
/// diff entries (removedM) are git history, not membership claims,
/// and do not trip the check.

use super::common::*;
use std::sync::Arc;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-ids-save",
    |s| Box::pin ( async move {
      test_delete_id_col_scaffold_respawns (s) . await ?;
      test_delete_id_scaffolds_aborts (s) . await ?;
      test_edit_id_scaffold_aborts (s) . await ?;
      test_reorder_id_scaffolds_saves (s) . await ?;
      test_move_id_scaffolds_to_child_aborts (s) . await ?;
      test_delete_id_col_scaffold_respawns_staged (s) . await ?;
      Ok (( )) } )) }

/// Deleting an idCol scaffold should be a no-op.
/// The scaffold respawns in the returned buffer.
async fn test_delete_id_col_scaffold_respawns (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-del-idcol",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User deletes the entire idCol scaffold (and its children)
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "skg id");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("1" . to_string())),
        "1.skg should still have id '1'");
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      assert!(node_1 . all_ids () . any(|id| id == &ID("3" . to_string())),
        "1.skg should still have id '3'");
      assert!(!node_1 . all_ids () . any(|id| id == &ID("2" . to_string())),
        "1.skg should not have id '2'");

      // BUFFER: idCol scaffold should respawn
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Deleting individual id scaffolds (keeping the idCol) aborts the
/// save with an IDCol_Edited error, and the disk is untouched.
async fn test_delete_id_scaffolds_aborts (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-del-ids",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User deletes the id scaffolds but keeps the idCol
      let input = without_lines_containing(
        GIT_DIFF_VIEW, "(skg id)");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("editing idCol membership must abort the save") );
      assert!(err . contains ("IDCol_Edited"),
        "the error should be IDCol_Edited: {}", err);

      // DISK: 1.skg should still have the worktree ids
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      Ok(()) }) }) . await
}

/// Editing an id scaffold's text aborts the save with an
/// IDCol_Edited error, and the disk is untouched.
async fn test_edit_id_scaffold_aborts (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-edit-id",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User tries to change an id value in the scaffold
      let input = GIT_DIFF_VIEW . replace(
        "(unstaged newM)) 2'", "(unstaged newM)) 2-modified");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("editing an id scaffold must abort the save") );
      assert!(err . contains ("IDCol_Edited"),
        "the error should be IDCol_Edited: {}", err);

      // DISK: 1.skg should still have the original worktree ids
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      assert!(!node_1 . all_ids () . any(|id| id == &ID("2-modified" . to_string())),
        "1.skg should not have the modified id");
      Ok(()) }) }) . await
}

/// Reordering id scaffolds passes the membership check (multiset
/// equality); the rerender re-sorts them anyway.
async fn test_reorder_id_scaffolds_saves (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-reorder-ids",
    |config, driver, tantivy, _repo_path| { Box::pin(async move {
      let input = GIT_DIFF_VIEW
        // Swap the two plain id lines (1 and 3).
        . replace ("*** (skg id) 1", "*** (skg id) SWAP")
        . replace ("*** (skg id) 3", "*** (skg id) 1")
        . replace ("*** (skg id) SWAP", "*** (skg id) 3");
      assert_ne! (input, GIT_DIFF_VIEW);
      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;
      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW);
      Ok(()) }) }) . await
}

/// Moving the idCol to another node aborts the save: the receiving
/// node's real ID list does not match the moved idCol's claims.
async fn test_move_id_scaffolds_to_child_aborts (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test(
    s,
    "skg-test-save-move-ids",
    |config, driver, tantivy, repo_path| { Box::pin(async move {
      // User moves id scaffolds to be children of 'child' node
      let input = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id child) (source main))) child
*** (skg idCol)
**** (skg id) 1
**** (skg id (unstaged removedM)) 2
**** (skg id (unstaged newM)) 2'
**** (skg id) 3
";

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await;

      let err : String =
        format! ( "{:?}",
                  result . err ()
                  . expect ("an idCol moved under another node must abort the save") );
      assert!(err . contains ("IDCol_Edited"),
        "the error should be IDCol_Edited: {}", err);

      // DISK: child.skg should not have any new ids
      let node_child = read_nodecomplete(repo_path, "child")?;
      assert_eq!(1 + node_child . extra_ids . len(), 1,
        "child.skg should still only have its original id");
      assert_eq!(&node_child . pid, &ID("child" . to_string()),
        "child.skg should still have id 'child'");

      // DISK: 1.skg should still have its ids
      let node_1 = read_nodecomplete(repo_path, "1")?;
      assert!(node_1 . all_ids () . any(|id| id == &ID("2'" . to_string())),
        "1.skg should still have id '2''");
      Ok(()) }) }) . await
}

/// Same as 'test_delete_id_col_scaffold_respawns' but with the fixture
/// transition staged (git add) rather than unstaged. The respawned IDCol
/// children should report '(staged ...)' tags — this verifies that the
/// save-rerender pipeline (reconcile_id_col_children + complete_viewforest) honors the
/// staged/unstaged distinction instead of merging stages and defaulting
/// to unstaged.
async fn test_delete_id_col_scaffold_respawns_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>>
{
  run_save_test_staged(
    s,
    "skg-test-save-del-idcol-staged",
    |config, driver, tantivy, _repo_path| { Box::pin(async move {
      let input = without_lines_containing(
        GIT_DIFF_VIEW_STAGED, "skg id");

      let graph : InRustGraphHandle =
        new_handle (InRustGraph::new ());
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views            : OpenViews::new (),};
      let (mut stream, _) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer(
        &mut stream,
        &input, driver, config, tantivy, &graph, true,
        &Err ( String::new () ), &mut views_state ) . await?;

      assert_buffer_contains(
        &response . saved_view, GIT_DIFF_VIEW_STAGED);
      Ok(()) }) }) . await
}

//
// Test runner helpers
//

async fn run_save_test<F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_git_repo_with_fixtures, test_fn) . await
}

async fn run_save_test_staged<F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  run_save_test_with_setup(
    s, subtest_name, setup_git_repo_with_fixtures_staged, test_fn) . await
}

async fn run_save_test_with_setup<S, F>(
  s: &mut SharedDbSession,
  subtest_name: &str,
  setup   : S,
  test_fn : F,
) -> Result<(), Box<dyn Error>>
where
  S: FnOnce (&Path) -> Result<Repository, Box<dyn Error>>,
  F: for<'a> FnOnce(
    &'a SkgConfig,
    &'a Arc<TypeDBDriver>,
    &'a mut TantivyIndex,
    &'a Path
  ) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<(), Box<dyn Error>>> + 'a>>
{
  let temp_dir = TempDir::new()?;
  let repo_path = temp_dir . path();
  setup (repo_path)?;
  s . reset_with_source_path (subtest_name, repo_path) . await ?;

  test_fn(&s . config, &s . driver, &mut s . tantivy, repo_path) . await
}
