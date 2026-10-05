/// Git diff view tests for the INBOUND folders (subscriberFolder,
/// overriderFolder, hiderFolder): the relationships live in the MEMBERS' files, so
/// the INVERSE SCAN supplies the per-stage signs
/// (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org).  Members
/// removed since HEAD appear as phantoms appended after the real
/// members; members added since HEAD carry per-stage 'addedR'.
///
/// Fixture, all transitions HEAD -> worktree:
/// - del-r overrode N1; del-r's FILE was deleted
///   -> phantom under N1's overriderFolder with deletedN AND removedR.
/// - edge-r overrode N2; only the EDGE was removed
///   -> phantom under N2's overriderFolder with removedR alone.
/// - new-r (an old file) newly overrides N3 -> member with addedR.
/// - newfile-r (a NEW file) overrides N4 -> member with addedN addedR.
/// - del-s subscribed to SN; file deleted -> subscriberFolder analogue.
/// - new-s newly subscribes to SN -> subscriberFolder addedR.
/// - edge-h hid HN; relationship removed -> hiderFolder analogue.
/// - new-h newly hides HN -> hiderFolder addedR.

use super::common::*;
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};

fn setup_inbound_fixtures (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures (
    gitrepo_path,
    "tests/git_diff_view/inbound/fixtures/head",
    "tests/git_diff_view/inbound/fixtures/worktree" ) }

fn setup_inbound_fixtures_staged (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures_staged (
    gitrepo_path,
    "tests/git_diff_view/inbound/fixtures/head",
    "tests/git_diff_view/inbound/fixtures/worktree" ) }

/// The buffer a user would save: each recorder with its inbound folder
/// holding the current (worktree) members.  N1's and N2's folders are
/// present but empty -- their only members are gone from the
/// worktree -- so everything those folders show must come from the
/// inverse scan.
const INPUT : &str = "\
* (skg (node (id N1) (repo main))) N1
** (skg overriderFolder)
* (skg (node (id N2) (repo main))) N2
** (skg overriderFolder)
* (skg (node (id N3) (repo main))) N3
** (skg overriderFolder)
*** (skg (node (id new-r) (repo main))) new-r
* (skg (node (id N4) (repo main))) N4
** (skg overriderFolder)
*** (skg (node (id newfile-r) (repo main))) newfile-r
* (skg (node (id SN) (repo main))) SN
** (skg subscriberFolder)
*** (skg (node (id new-s) (repo main))) new-s
* (skg (node (id HN) (repo main))) HN
** (skg hiderFolder)
*** (skg (node (id new-h) (repo main))) new-h
";

const EXPECTED_UNSTAGED : &str = "\
* (skg (node (id N1) (repo main))) N1
** (skg overriderFolder)
*** (skg (node (id del-r) (repo main) writeProtected (unstaged deletedN removedR))) del-r
* (skg (node (id N2) (repo main))) N2
** (skg overriderFolder)
*** (skg (node (id edge-r) (repo main) writeProtected (unstaged removedR))) edge-r
* (skg (node (id N3) (repo main))) N3
** (skg overriderFolder)
*** (skg (node (id new-r) (repo main) (unstaged addedR))) new-r
* (skg (node (id N4) (repo main))) N4
** (skg overriderFolder)
*** (skg (node (id newfile-r) (repo main) (unstaged addedN addedR))) newfile-r
* (skg (node (id SN) (repo main))) SN
** (skg subscriberFolder)
*** (skg (node (id del-s) (repo main) writeProtected (unstaged deletedN removedR))) del-s
*** (skg (node (id new-s) (repo main) (unstaged addedR))) new-s
* (skg (node (id HN) (repo main))) HN
** (skg hiderFolder)
*** (skg (node (id edge-h) (repo main) writeProtected (unstaged removedR))) edge-h
*** (skg (node (id new-h) (repo main) (unstaged addedR))) new-h
";

const EXPECTED_STAGED : &str = "\
*** (skg (node (id del-r) (repo main) writeProtected (staged deletedN removedR))) del-r
*** (skg (node (id edge-r) (repo main) writeProtected (staged removedR))) edge-r
*** (skg (node (id new-r) (repo main) (staged addedR))) new-r
*** (skg (node (id newfile-r) (repo main) (staged addedN addedR))) newfile-r
*** (skg (node (id del-s) (repo main) writeProtected (staged deletedN removedR))) del-s
*** (skg (node (id new-s) (repo main) (staged addedR))) new-s
*** (skg (node (id edge-h) (repo main) writeProtected (staged removedR))) edge-h
*** (skg (node (id new-h) (repo main) (staged addedR))) new-h
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-inbound",
    |s| Box::pin ( async move {
      inbound_folders_show_phantoms_and_addedR_unstaged (s) . await ?;
      inbound_folders_show_phantoms_and_addedR_staged (s) . await ?;
      Ok (( )) } )) }

async fn run_inbound_save_test (
  s            : &mut SharedStoreSession,
  subtest_name : &str,
  staged   : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  if staged { setup_inbound_fixtures_staged (gitrepo_path)?; }
  else      { setup_inbound_fixtures        (gitrepo_path)?; }
  s . reset_with_skgrepo_path (subtest_name, gitrepo_path) ?;
  let (config, tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let first = {
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      update_from_and_rerender_buffer (
        &mut stream, INPUT, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ? };
    assert_buffer_contains (&first . saved_view, expected);
    { // Write-protected-folder saves remain unaffected by phantoms: saving
      // the rendered result (phantoms included) resurrects no file
      // and re-adds no relationship, and the phantoms regenerate.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (&second . saved_view, expected);
      assert! (
        ! gitrepo_path . join ("del-r.skg") . exists (),
        "a deleted member's file must not resurrect" );
      let edge_r : Graphnode =
        read_graphnode (gitrepo_path, "edge-r")?;
      assert! (
        edge_r . overrides . or_default () . is_empty (),
        "a removed inbound edge must not return: the relation \
         lives in edge-r's file, which the folder cannot edit" ); }
    Ok (( )) }

async fn inbound_folders_show_phantoms_and_addedR_unstaged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_inbound_save_test (
    s, "skg-test-git-diff-inbound-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

async fn inbound_folders_show_phantoms_and_addedR_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_inbound_save_test (
    s, "skg-test-git-diff-inbound-staged", true,
    EXPECTED_STAGED ) . await }
