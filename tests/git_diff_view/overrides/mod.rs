/// Git diff view tests for the OUTBOUND sharing cols (overriddenCol,
/// hiddenCol): members removed since HEAD appear as phantoms carrying
/// per-stage 'removedM', and members added since HEAD carry per-stage
/// 'newM', all read from the owner's per-stage relation diff
/// (TODO/full-schema/12-2_diff-mode-policy_discussion.org).
///
/// Fixture: R overrides [Z, W] and hides [ha, hb] at HEAD;
/// in the worktree R overrides [Z, O] and hides [ha, hc].  Every
/// leaf file exists unchanged on both sides, so all signs are
/// membership-only (no X axes).
///
/// Col-existence companions (every relation EMPTIED since HEAD, so
/// in diff mode each col must still render, holding only phantoms):
/// E overrode [EZ] and hid [EH]; ER overrode [EN] (so EN's
/// overriderCol is the inbound case); ES subscribed to [EB].

use super::common::*;
use skg::dbs::in_rust_graph::install_or_swap_global_handle;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
use skg::test_utils::{run_with_shared_test_db, SharedDbSession};
use skg::to_org::render::content_view::multi_root_view_via_env;
use skg::types::env::SkgEnv;

fn setup_overrides_fixtures (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures (
    repo_path,
    "tests/git_diff_view/overrides/fixtures/head",
    "tests/git_diff_view/overrides/fixtures/worktree" ) }

fn setup_overrides_fixtures_staged (
  repo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_git_repo_with_fixtures_staged (
    repo_path,
    "tests/git_diff_view/overrides/fixtures/head",
    "tests/git_diff_view/overrides/fixtures/worktree" ) }

const EXPECTED_UNSTAGED : &str = "\
** (skg overriddenCol)
*** (skg (node (id Z) (source main))) Z
*** (skg (node (id W) (source main) indef (unstaged removedM))) W
*** (skg (node (id O) (source main) (unstaged newM))) O
** (skg hiddenCol)
*** (skg (node (id ha) (source main))) ha
*** (skg (node (id hb) (source main) indef (unstaged removedM))) hb
*** (skg (node (id hc) (source main) (unstaged newM))) hc
";

const EXPECTED_STAGED : &str = "\
** (skg overriddenCol)
*** (skg (node (id Z) (source main))) Z
*** (skg (node (id W) (source main) indef (staged removedM))) W
*** (skg (node (id O) (source main) (staged newM))) O
** (skg hiddenCol)
*** (skg (node (id ha) (source main))) ha
*** (skg (node (id hb) (source main) indef (staged removedM))) hb
*** (skg (node (id hc) (source main) (staged newM))) hc
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-git-diff-overrides",
    |s| Box::pin ( async move {
      outbound_cols_show_phantoms_and_newM_de_novo_unstaged (s) . await ?;
      emptied_cols_still_render_in_diff_mode_de_novo (s) . await ?;
      outbound_cols_show_phantoms_and_newM_de_novo_staged (s) . await ?;
      diff_mode_save_is_noop_and_regenerates_outbound_phantoms (s) . await ?;
      Ok (( )) } )) }

async fn run_overrides_view_test (
  s            : &mut SharedDbSession,
  subtest_name : &str,
  staged  : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  if staged { setup_overrides_fixtures_staged (repo_path)?; }
  else      { setup_overrides_fixtures        (repo_path)?; }
  s . reset_with_source_path (subtest_name, repo_path) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    // De novo PartnerCol creation reads the process-global graph
    // handle (maybe_add_partnerCol_branches). Sub-tests share this
    // process, so install_or_swap_global_handle re-points the
    // handle at this sub-test's fixtures each time.
    install_or_swap_global_handle (
      graph_handle_from_config (&config)? );
    let env : SkgEnv =
      skg_env_from_parts (
        &config, Arc::clone (&driver), &tantivy, &graph );
    let mut warnings : Vec<String> = Vec::new ();
    let (actual, _pids, _tree) =
      multi_root_view_via_env (
        &env, &[ ID::from ("R") ], true, None, &mut warnings
      ) . await ?;
    assert_buffer_contains (&actual, expected);
    Ok (( )) }

async fn outbound_cols_show_phantoms_and_newM_de_novo_unstaged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    s, "skg-test-git-diff-overrides-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

/// Col existence: in diff mode, a col whose worktree membership is
/// EMPTY but whose HEAD side is not still renders, holding only
/// phantoms -- in a de novo render, for an outbound col
/// (overriddenCol, hiddenCol), an inbound col (overriderCol, via the
/// inverse scan), and the subscribeeCol.  Outside diff mode the
/// emptied cols still do not appear.
async fn emptied_cols_still_render_in_diff_mode_de_novo (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  setup_overrides_fixtures (repo_path)?;
  s . reset_with_source_path (
    "emptied_cols_still_render_in_diff_mode_de_novo",
    repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    install_or_swap_global_handle (
      graph_handle_from_config (&config)? );
    let env : SkgEnv =
      skg_env_from_parts (
        &config, Arc::clone (&driver), &tantivy, &graph );
    let roots : [ID; 3] =
      [ ID::from ("E"), ID::from ("EN"), ID::from ("ES") ];
    { let mut warnings : Vec<String> = Vec::new ();
      let (diff_view, _pids, _tree) =
        multi_root_view_via_env (
          &env, &roots, true, None, &mut warnings ) . await ?;
      assert_buffer_contains ( &diff_view, "\
* (skg (node (id E) (source main))) E
** (skg overriddenCol)
*** (skg (node (id EZ) (source main) indef (unstaged removedM))) EZ
** (skg hiddenCol)
*** (skg (node (id EH) (source main) indef (unstaged removedM))) EH
* (skg (node (id EN) (source main))) EN
** (skg overriderCol)
*** (skg (node (id ER) (source main) indef (unstaged removedM))) ER
* (skg (node (id ES) (source main))) ES
** (skg subscribeeCol)
*** (skg (node (id EB) (source main) indef (unstaged removedM))) EB
" ); }
    { // Outside diff mode, the emptied cols still do not appear.
      let mut warnings : Vec<String> = Vec::new ();
      let (plain_view, _pids, _tree) =
        multi_root_view_via_env (
          &env, &roots, false, None, &mut warnings ) . await ?;
      for col in [ "overriddenCol", "hiddenCol",
                   "overriderCol", "subscribeeCol" ] {
        assert! ( ! plain_view . contains (col),
          "an empty {} must not render outside diff mode:\n{}",
          col, plain_view ); }}
    Ok (( )) }

async fn outbound_cols_show_phantoms_and_newM_de_novo_staged (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    s, "skg-test-git-diff-overrides-staged", true,
    EXPECTED_STAGED ) . await }

/// A diff-mode save of a buffer holding the worktree members is a
/// no-op for the relations, regenerates the phantoms (idempotence:
/// saving the rendered result changes nothing further), and never
/// collects a phantom as a writable-col member (saving the phantom
/// line must not re-add W to R's overrides_view_of).
async fn diff_mode_save_is_noop_and_regenerates_outbound_phantoms (
  s : &mut SharedDbSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  setup_overrides_fixtures (repo_path)?;
  s . reset_with_source_path (
    "diff_mode_save_is_noop_and_regenerates_outbound_phantoms",
    repo_path ) . await ?;
  let (config, driver, tantivy)
    : (&SkgConfig, &Arc<TypeDBDriver>, &mut TantivyIndex)
    = (&s . config, &s . driver, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    let input : &str = "\
* (skg (node (id R) (source main))) R
** (skg overriddenCol)
*** (skg (node (id Z) (source main))) Z
*** (skg (node (id O) (source main))) O
** (skg hiddenCol)
*** (skg (node (id ha) (source main))) ha
*** (skg (node (id hc) (source main))) hc
";
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let first = {
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      update_from_and_rerender_buffer (
        &mut stream, input, &driver, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ? };
    assert_buffer_contains (
      &first . saved_view, EXPECTED_UNSTAGED );
    { // Saving the RENDERED RESULT (phantoms included) is a no-op:
      // the relations on disk keep their worktree values, and the
      // phantoms regenerate.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &driver, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (
        &second . saved_view, EXPECTED_UNSTAGED );
      let r : NodeComplete = read_nodecomplete (repo_path, "R")?;
      assert_eq! (
        r . overrides_view_of . or_default () . to_vec (),
        vec! [ ID::from ("Z"), ID::from ("O") ],
        "a phantom under a writable col is never collected: W must \
         not return to R's overrides_view_of" );
      assert_eq! (
        r . hides_from_its_subscriptions . or_default () . to_vec (),
        vec! [ ID::from ("ha"), ID::from ("hc") ],
        "hb must not return to R's hides list" ); }
    Ok (( )) }
