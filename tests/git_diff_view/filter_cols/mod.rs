/// Git diff view tests for the FILTER cols (hiddenInSubscribeeCol,
/// hiddenOutsideOfSubscribeeCol): their membership is DERIVED, so
/// per-stage signs come from comparing the derived membership at the
/// three snapshots -- HEAD, index, worktree -- rather than from any
/// one relation's diff
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org).
///
/// Fixture, all HEAD -> worktree:
///   S subscribes to B throughout.
///   S hides:     [h1, h3, h4, h6] -> [h1, h2, h3, h5]
///   B contains:  [h1, h2, v, h4]  -> [h1, h2, v, h4, h3]
/// Derived hiddenIn  = hides ∩ contains: [h1, h4] -> [h1, h2, h3]
/// Derived hiddenOut = hides − contains: [h3, h6] -> [h5]
/// So:
/// - h2 newly hidden-in because the HIDES list gained it -> newM;
/// - h3 newly hidden-in because the CONTAINS list gained it -> newM
///   under hiddenIn, AND a removedM phantom under hiddenOutside
///   (it stopped being hidden-outside without any hides change);
/// - h4 stopped being hidden-in (hides dropped it) -> exact-label
///   phantom under hiddenIn;
/// - h5 newly hidden-outside -> newM there;
/// - h6 no longer hidden at all -> exact-label phantom under
///   hiddenOutside.

use super::common::*;
use skg::test_utils::graph_handle_from_config;
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};

fn setup_filter_fixtures (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures (
    repo_path,
    "tests/git_diff_view/filter_cols/fixtures/head",
    "tests/git_diff_view/filter_cols/fixtures/worktree" ) }

fn setup_filter_fixtures_staged (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures_staged (
    repo_path,
    "tests/git_diff_view/filter_cols/fixtures/head",
    "tests/git_diff_view/filter_cols/fixtures/worktree" ) }

/// The worktree state of the view, as a user's diff-mode buffer
/// would hold it (B expanded as a definitive subscribee-as-such).
const INPUT : &str = "\
* (skg (node (id S) (source main))) S
** (skg subscribeeCol)
*** (skg (node (id B) (source main))) B
**** (skg hiddenInSubscribeeCol)
***** (skg (node (id h1) (source main))) h1
***** (skg (node (id h2) (source main))) h2
***** (skg (node (id h3) (source main))) h3
**** (skg (node (id v) (source main))) v
**** (skg (node (id h4) (source main))) h4
*** (skg hiddenOutsideOfSubscribeeCol)
**** (skg (node (id h5) (source main))) h5
";

const EXPECTED_UNSTAGED : &str = "\
***** (skg (node (id h1) (source main))) h1
***** (skg (node (id h2) (source main) (unstaged newM))) h2
***** (skg (node (id h3) (source main) (unstaged newM))) h3
***** (skg (node (id h4) (source main) indef (unstaged removedM))) h4
**** (skg (node (id h5) (source main) (unstaged newM))) h5
**** (skg (node (id h3) (source main) indef (unstaged removedM))) h3
**** (skg (node (id h6) (source main) indef (unstaged removedM))) h6
";

const EXPECTED_STAGED : &str = "\
***** (skg (node (id h2) (source main) (staged newM))) h2
***** (skg (node (id h3) (source main) (staged newM))) h3
***** (skg (node (id h4) (source main) indef (staged removedM))) h4
**** (skg (node (id h5) (source main) (staged newM))) h5
**** (skg (node (id h3) (source main) indef (staged removedM))) h3
**** (skg (node (id h6) (source main) indef (staged removedM))) h6
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-filter-cols",
    |s| Box::pin ( async move {
      filter_cols_show_exact_phantoms_and_newM_unstaged (s) . await ?;
      emptied_filter_cols_still_render_in_diff_mode (s) . await ?;
      filter_cols_show_exact_phantoms_and_newM_staged (s) . await ?;
      Ok (( )) } )) }

async fn run_filter_col_test (
  s            : &mut SharedDbSession,
  subtest_name : &str,
  staged   : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  if staged { setup_filter_fixtures_staged (repo_path)?; }
  else      { setup_filter_fixtures        (repo_path)?; }
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
    { // Idempotence: saving the rendered result (phantoms included)
      // regenerates the same picture and edits no hides.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &driver, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (&second . saved_view, expected);
      let s : NodeComplete = read_nodecomplete (repo_path, "S")?;
      assert_eq! (
        s . hides_from_its_subscriptions . or_default () . to_vec (),
        vec! [ ID::from ("h1"), ID::from ("h2"),
               ID::from ("h3"), ID::from ("h5") ],
        "filter-col phantoms must not edit the hides list" ); }
    Ok (( )) }

async fn filter_cols_show_exact_phantoms_and_newM_unstaged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_filter_col_test (
    s, "skg-test-git-diff-filter-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

/// Col existence for the filter cols: a DERIVED membership emptied
/// since HEAD (S2 stopped hiding x2 and y2; x2 was hidden-in B2, y2
/// hidden-outside) still yields each col, holding only phantoms.
/// Outside diff mode the emptied cols do not appear.
async fn emptied_filter_cols_still_render_in_diff_mode (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  setup_filter_fixtures (repo_path)?;
  s . reset_with_source_path (
    "emptied_filter_cols_still_render_in_diff_mode",
    repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);
  let input : &str = "\
* (skg (node (id S2) (source main))) S2
** (skg subscribeeCol)
*** (skg (node (id B2) (source main))) B2
**** (skg (node (id x2) (source main))) x2
";
    let graph = graph_handle_from_config (&config)?;
    { let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views        : OpenViews::new (), };
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream, input, &driver, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ?;
      assert_buffer_contains ( &response . saved_view, "\
**** (skg hiddenInSubscribeeCol)
***** (skg (node (id x2) (source main) indef (unstaged removedM))) x2
**** (skg (node (id x2) (source main))) x2
*** (skg hiddenOutsideOfSubscribeeCol)
**** (skg (node (id y2) (source main) indef (unstaged removedM))) y2
" ); }
    { // The same save outside diff mode creates neither col.
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer (
        &mut stream, input, &driver, &config, &tantivy, &graph,
        false, &Err (String::new ()), &mut views_state ) . await ?;
      for col in [ "hiddenInSubscribeeCol",
                   "hiddenOutsideOfSubscribeeCol" ] {
        assert! ( ! response . saved_view . contains (col),
          "an empty {} must not render outside diff mode:\n{}",
          col, response . saved_view ); }}
    Ok (( )) }

async fn filter_cols_show_exact_phantoms_and_newM_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_filter_col_test (
    s, "skg-test-git-diff-filter-staged", true,
    EXPECTED_STAGED ) . await }
