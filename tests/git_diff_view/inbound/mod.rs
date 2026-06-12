/// Git diff view tests for the INBOUND cols (subscriberCol,
/// overriderCol, hiderCol): the edges live in the MEMBERS' files, so
/// the INVERSE SCAN supplies the per-stage signs
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org).  Members
/// removed since HEAD appear as phantoms appended after the real
/// members; members added since HEAD carry per-stage 'newM'.
///
/// Fixture, all transitions HEAD -> worktree:
/// - del-r overrode N1; del-r's FILE was deleted
///   -> phantom under N1's overriderCol with removedX AND removedM.
/// - edge-r overrode N2; only the EDGE was removed
///   -> phantom under N2's overriderCol with removedM alone.
/// - new-r (an old file) newly overrides N3 -> member with newM.
/// - newfile-r (a NEW file) overrides N4 -> member with newX newM.
/// - del-s subscribed to SN; file deleted -> subscriberCol analogue.
/// - new-s newly subscribes to SN -> subscriberCol newM.
/// - edge-h hid HN; edge removed -> hiderCol analogue.
/// - new-h newly hides HN -> hiderCol newM.

use super::common::*;
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

fn setup_inbound_fixtures (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures (
    repo_path,
    "tests/git_diff_view/inbound/fixtures/head",
    "tests/git_diff_view/inbound/fixtures/worktree" ) }

fn setup_inbound_fixtures_staged (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures_staged (
    repo_path,
    "tests/git_diff_view/inbound/fixtures/head",
    "tests/git_diff_view/inbound/fixtures/worktree" ) }

/// The buffer a user would save: each owner with its inbound col
/// holding the current (worktree) members.  N1's and N2's cols are
/// present but empty -- their only members are gone from the
/// worktree -- so everything those cols show must come from the
/// inverse scan.
const INPUT : &str = "\
* (skg (node (id N1) (source main))) N1
** (skg overriderCol)
* (skg (node (id N2) (source main))) N2
** (skg overriderCol)
* (skg (node (id N3) (source main))) N3
** (skg overriderCol)
*** (skg (node (id new-r) (source main))) new-r
* (skg (node (id N4) (source main))) N4
** (skg overriderCol)
*** (skg (node (id newfile-r) (source main))) newfile-r
* (skg (node (id SN) (source main))) SN
** (skg subscriberCol)
*** (skg (node (id new-s) (source main))) new-s
* (skg (node (id HN) (source main))) HN
** (skg hiderCol)
*** (skg (node (id new-h) (source main))) new-h
";

const EXPECTED_UNSTAGED : &str = "\
* (skg (node (id N1) (source main))) N1
** (skg overriderCol)
*** (skg (node (id del-r) (source main) indef (unstaged removedX removedM))) del-r
* (skg (node (id N2) (source main))) N2
** (skg overriderCol)
*** (skg (node (id edge-r) (source main) indef (unstaged removedM))) edge-r
* (skg (node (id N3) (source main))) N3
** (skg overriderCol)
*** (skg (node (id new-r) (source main) (unstaged newM))) new-r
* (skg (node (id N4) (source main))) N4
** (skg overriderCol)
*** (skg (node (id newfile-r) (source main) (unstaged newX newM))) newfile-r
* (skg (node (id SN) (source main))) SN
** (skg subscriberCol)
*** (skg (node (id del-s) (source main) indef (unstaged removedX removedM))) del-s
*** (skg (node (id new-s) (source main) (unstaged newM))) new-s
* (skg (node (id HN) (source main))) HN
** (skg hiderCol)
*** (skg (node (id edge-h) (source main) indef (unstaged removedM))) edge-h
*** (skg (node (id new-h) (source main) (unstaged newM))) new-h
";

const EXPECTED_STAGED : &str = "\
*** (skg (node (id del-r) (source main) indef (staged removedX removedM))) del-r
*** (skg (node (id edge-r) (source main) indef (staged removedM))) edge-r
*** (skg (node (id new-r) (source main) (staged newM))) new-r
*** (skg (node (id newfile-r) (source main) (staged newX newM))) newfile-r
*** (skg (node (id del-s) (source main) indef (staged removedX removedM))) del-s
*** (skg (node (id new-s) (source main) (staged newM))) new-s
*** (skg (node (id edge-h) (source main) indef (staged removedM))) edge-h
*** (skg (node (id new-h) (source main) (staged newM))) new-h
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-inbound",
    |s| Box::pin ( async move {
      inbound_cols_show_phantoms_and_newM_unstaged (s) . await ?;
      inbound_cols_show_phantoms_and_newM_staged (s) . await ?;
      Ok (( )) } )) }

async fn run_inbound_save_test (
  s            : &mut SharedDbSession,
  subtest_name : &str,
  staged   : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  if staged { setup_inbound_fixtures_staged (repo_path)?; }
  else      { setup_inbound_fixtures        (repo_path)?; }
  s . reset_with_source_path (subtest_name, repo_path) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let first = {
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      update_from_and_rerender_buffer (
        &mut stream, INPUT, &driver, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ? };
    assert_buffer_contains (&first . saved_view, expected);
    { // Read-only-col saves remain unaffected by phantoms: saving
      // the rendered result (phantoms included) resurrects no file
      // and re-adds no edge, and the phantoms regenerate.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &driver, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (&second . saved_view, expected);
      assert! (
        ! repo_path . join ("del-r.skg") . exists (),
        "a deleted member's file must not resurrect" );
      let edge_r : NodeComplete =
        read_nodecomplete (repo_path, "edge-r")?;
      assert! (
        edge_r . overrides_view_of . or_default () . is_empty (),
        "a removed inbound edge must not return: the relation \
         lives in edge-r's file, which the col cannot edit" ); }
    Ok (( )) }

async fn inbound_cols_show_phantoms_and_newM_unstaged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_inbound_save_test (
    s, "skg-test-git-diff-inbound-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

async fn inbound_cols_show_phantoms_and_newM_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_inbound_save_test (
    s, "skg-test-git-diff-inbound-staged", true,
    EXPECTED_STAGED ) . await }
