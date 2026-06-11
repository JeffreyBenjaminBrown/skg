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

use super::common::*;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
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

fn run_overrides_view_test (
  db_name : &str,
  staged  : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let tantivy_folder : String =
    format! ("/tmp/tantivy-{}", db_name);
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  if staged { setup_overrides_fixtures_staged (repo_path)?; }
  else      { setup_overrides_fixtures        (repo_path)?; }
  block_on ( async {
    let (config, driver, tantivy) =
      setup_test_dbs (
        db_name, repo_path . to_str () . unwrap (),
        &tantivy_folder ) . await ?;
    let graph = graph_handle_from_config (&config)?;
    // De novo PartnerCol creation reads the process-global graph
    // handle (maybe_add_partnerCol_branches). The fixture ids are
    // unique to this module, so the handle cannot mislead sibling
    // tests under plain 'cargo test' (nextest isolates processes).
    skg::dbs::in_rust_graph::try_init_global_handle (
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
    cleanup_test_dbs (
      db_name, &driver, Some (Path::new (&tantivy_folder))
    ) . await ?;
    Ok (( )) } ) }

#[test]
fn outbound_cols_show_phantoms_and_newM_de_novo_unstaged (
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    "skg-test-git-diff-overrides-unstaged", false,
    EXPECTED_UNSTAGED ) }

#[test]
fn outbound_cols_show_phantoms_and_newM_de_novo_staged (
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    "skg-test-git-diff-overrides-staged", true,
    EXPECTED_STAGED ) }

/// A diff-mode save of a buffer holding the worktree members is a
/// no-op for the relations, regenerates the phantoms (idempotence:
/// saving the rendered result changes nothing further), and never
/// collects a phantom as a writable-col member (saving the phantom
/// line must not re-add W to R's overrides_view_of).
#[test]
fn diff_mode_save_is_noop_and_regenerates_outbound_phantoms (
) -> Result<(), Box<dyn Error>> {
  let db_name : &str =
    "skg-test-git-diff-overrides-save";
  let tantivy_folder : String =
    format! ("/tmp/tantivy-{}", db_name);
  let temp_dir : TempDir = TempDir::new ()?;
  let repo_path : &Path = temp_dir . path ();
  setup_overrides_fixtures (repo_path)?;
  block_on ( async {
    let (config, driver, tantivy) =
      setup_test_dbs (
        db_name, repo_path . to_str () . unwrap (),
        &tantivy_folder ) . await ?;
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
    cleanup_test_dbs (
      db_name, &driver, Some (Path::new (&tantivy_folder))
    ) . await ?;
    Ok (( )) } ) }
