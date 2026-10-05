/// Git diff view tests for the FILTER folders (hiddenInSubscribeeFolder,
/// hiddenOutsideOfSubscribeeFolder): their membership is DERIVED, so
/// per-stage signs come from comparing the derived membership at the
/// three snapshots -- HEAD, index, worktree -- rather than from any
/// one relation's diff
/// (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org).
///
/// Fixture, all HEAD -> worktree:
///   S subscribes to B throughout.
///   S hides:     [h1, h3, h4, h6] -> [h1, h2, h3, h5]
///   B contains:  [h1, h2, v, h4]  -> [h1, h2, v, h4, h3]
/// Derived hiddenIn  = hides ∩ contains: [h1, h4] -> [h1, h2, h3]
/// Derived hiddenOut = hides − contains: [h3, h6] -> [h5]
/// So:
/// - h2 newly hidden-in because the HIDES list gained it -> addedR;
/// - h3 newly hidden-in because the CONTAINS list gained it -> addedR
///   under hiddenIn, AND a removedR phantom under hiddenOutside
///   (it stopped being hidden-outside without any hides change);
/// - h4 stopped being hidden-in (hides dropped it) -> exact-label
///   phantom under hiddenIn;
/// - h5 newly hidden-outside -> addedR there;
/// - h6 no longer hidden at all -> exact-label phantom under
///   hiddenOutside.

use super::common::*;
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};
use skg::types::misc::members_msv;

fn setup_filter_fixtures (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures (
    gitrepo_path,
    "tests/git_diff_view/filter_folders/fixtures/head",
    "tests/git_diff_view/filter_folders/fixtures/worktree" ) }

fn setup_filter_fixtures_staged (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures_staged (
    gitrepo_path,
    "tests/git_diff_view/filter_folders/fixtures/head",
    "tests/git_diff_view/filter_folders/fixtures/worktree" ) }

/// The worktree state of the view, as a user's diff-mode buffer
/// would hold it (B expanded as an editable subscribee-as-such).
const INPUT : &str = "\
* (skg (node (id S) (repo main))) S
** (skg subscribeeFolder)
*** (skg (node (id B) (repo main))) B
**** (skg hiddenInSubscribeeFolder)
***** (skg (node (id h1) (repo main))) h1
***** (skg (node (id h2) (repo main))) h2
***** (skg (node (id h3) (repo main))) h3
**** (skg (node (id v) (repo main))) v
**** (skg (node (id h4) (repo main))) h4
*** (skg hiddenOutsideOfSubscribeeFolder)
**** (skg (node (id h5) (repo main))) h5
";

const EXPECTED_UNSTAGED : &str = "\
***** (skg (node (id h1) (repo main))) h1
***** (skg (node (id h2) (repo main) (unstaged addedR))) h2
***** (skg (node (id h3) (repo main) (unstaged addedR))) h3
***** (skg (node (id h4) (repo main) writeProtected (unstaged removedR))) h4
**** (skg (node (id h5) (repo main) (unstaged addedR))) h5
**** (skg (node (id h3) (repo main) writeProtected (unstaged removedR))) h3
**** (skg (node (id h6) (repo main) writeProtected (unstaged removedR))) h6
";

const EXPECTED_STAGED : &str = "\
***** (skg (node (id h2) (repo main) (staged addedR))) h2
***** (skg (node (id h3) (repo main) (staged addedR))) h3
***** (skg (node (id h4) (repo main) writeProtected (staged removedR))) h4
**** (skg (node (id h5) (repo main) (staged addedR))) h5
**** (skg (node (id h3) (repo main) writeProtected (staged removedR))) h3
**** (skg (node (id h6) (repo main) writeProtected (staged removedR))) h6
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-filter-folders",
    |s| Box::pin ( async move {
      filter_folders_show_exact_phantoms_and_addedR_unstaged (s) . await ?;
      emptied_filter_folders_still_render_in_diff_mode (s) . await ?;
      filter_folders_show_exact_phantoms_and_addedR_staged (s) . await ?;
      Ok (( )) } )) }

async fn run_filter_folder_test (
  s            : &mut SharedStoreSession,
  subtest_name : &str,
  staged   : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  if staged { setup_filter_fixtures_staged (gitrepo_path)?; }
  else      { setup_filter_fixtures        (gitrepo_path)?; }
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
    { // Idempotence: saving the rendered result (phantoms included)
      // regenerates the same picture and edits no hides.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (&second . saved_view, expected);
      let s : Graphnode = read_graphnode (gitrepo_path, "S")?;
      assert_eq! (
        members_msv (&s . hides_from_its_subscriptions) . or_default () . to_vec (),
        vec! [ ID::from ("h1"), ID::from ("h2"),
               ID::from ("h3"), ID::from ("h5") ],
        "filter-folder phantoms must not edit the hides list" ); }
    Ok (( )) }

async fn filter_folders_show_exact_phantoms_and_addedR_unstaged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_filter_folder_test (
    s, "skg-test-git-diff-filter-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

/// Folder existence for the filter folders: a DERIVED membership emptied
/// since HEAD (S2 stopped hiding x2 and y2; x2 was hidden-in B2, y2
/// hidden-outside) still yields each folder, holding only phantoms.
/// Outside diff mode the emptied folders do not appear.
async fn emptied_filter_folders_still_render_in_diff_mode (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  setup_filter_fixtures (gitrepo_path)?;
  s . reset_with_skgrepo_path (
    "emptied_filter_folders_still_render_in_diff_mode",
    gitrepo_path ) ?;
  let (config, tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);
  let input : &str = "\
* (skg (node (id S2) (repo main))) S2
** (skg subscribeeFolder)
*** (skg (node (id B2) (repo main))) B2
**** (skg (node (id x2) (repo main))) x2
";
    let graph = graph_handle_from_config (&config)?;
    { let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views        : OpenViews::new (), };
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream, input, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ?;
      assert_buffer_contains ( &response . saved_view, "\
**** (skg hiddenInSubscribeeFolder)
***** (skg (node (id x2) (repo main) writeProtected (unstaged removedR))) x2
**** (skg (node (id x2) (repo main))) x2
*** (skg hiddenOutsideOfSubscribeeFolder)
**** (skg (node (id y2) (repo main) writeProtected (unstaged removedR))) y2
" ); }
    { // The same save outside diff mode creates neither folder.
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream, input, &config, &tantivy, &graph,
        false, &Err (String::new ()), &mut views_state ) . await ?;
      for folder in [ "hiddenInSubscribeeFolder",
                   "hiddenOutsideOfSubscribeeFolder" ] {
        assert! ( ! response . saved_view . contains (folder),
          "an empty {} must not render outside diff mode:\n{}",
          folder, response . saved_view ); }}
    Ok (( )) }

async fn filter_folders_show_exact_phantoms_and_addedR_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_filter_folder_test (
    s, "skg-test-git-diff-filter-staged", true,
    EXPECTED_STAGED ) . await }
