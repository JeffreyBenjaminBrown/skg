/// Git diff view tests for explicitly requested OUTBOUND sharing folders
/// (overriddenFolder, hiddenFolder): members removed since HEAD appear as phantoms carrying
/// per-stage 'removedR', and members added since HEAD carry per-stage
/// 'addedR', all read from the recorder's per-stage relation diff
/// (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org).
///
/// Fixture: R overrides [Z, W] and hides [ha, hb] at HEAD;
/// in the worktree R overrides [Z, O] and hides [ha, hc].  Every
/// leaf file exists unchanged on both sides, so all signs are
/// relationship-only (no N axes).
///
/// Folder-existence companions (every relation EMPTIED since HEAD, so
/// in diff mode each folder must still render, holding only phantoms):
/// E overrode [EZ] and hid [EH]; ER overrode [EN] (so EN's
/// overriderFolder is the inbound case); ES subscribed to [EB].

use super::common::*;
use skg::test_utils::{graph_handle_from_config, skg_env_from_parts};
use skg::test_utils::{run_with_shared_test_stores, SharedStoreSession};
use skg::to_org::render::content_view::multi_root_view_via_env;
use skg::types::env::SkgEnv;
use skg::types::misc::members_msv;

fn setup_overrides_fixtures (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures (
    gitrepo_path,
    "tests/git_diff_view/overrides/fixtures/head",
    "tests/git_diff_view/overrides/fixtures/worktree" ) }

fn setup_overrides_fixtures_staged (
  gitrepo_path : &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::common::setup_gitrepo_with_fixtures_staged (
    gitrepo_path,
    "tests/git_diff_view/overrides/fixtures/head",
    "tests/git_diff_view/overrides/fixtures/worktree" ) }

const EXPECTED_UNSTAGED : &str = "\
** (skg overriddenFolder)
*** (skg (node (id Z) (repo main))) Z
*** (skg (node (id W) (repo main) writeProtected (unstaged removedR))) W
*** (skg (node (id O) (repo main) (unstaged addedR))) O
** (skg hiddenFolder)
*** (skg (node (id ha) (repo main))) ha
*** (skg (node (id hb) (repo main) writeProtected (unstaged removedR))) hb
*** (skg (node (id hc) (repo main) (unstaged addedR))) hc
";

const EXPECTED_STAGED : &str = "\
** (skg overriddenFolder)
*** (skg (node (id Z) (repo main))) Z
*** (skg (node (id W) (repo main) writeProtected (staged removedR))) W
*** (skg (node (id O) (repo main) (staged addedR))) O
** (skg hiddenFolder)
*** (skg (node (id ha) (repo main))) ha
*** (skg (node (id hb) (repo main) writeProtected (staged removedR))) hb
*** (skg (node (id hc) (repo main) (staged addedR))) hc
";

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-git-diff-overrides",
    |s| Box::pin ( async move {
      requested_outbound_folders_show_phantoms_and_addedR_unstaged (s) . await ?;
      emptied_requested_folders_still_render_in_diff_mode (s) . await ?;
      requested_outbound_folders_show_phantoms_and_addedR_staged (s) . await ?;
      diff_mode_save_is_noop_and_regenerates_outbound_phantoms (s) . await ?;
      Ok (( )) } )) }

async fn run_overrides_view_test (
  s            : &mut SharedStoreSession,
  subtest_name : &str,
  staged  : bool,
  expected : &str,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  if staged { setup_overrides_fixtures_staged (gitrepo_path)?; }
  else      { setup_overrides_fixtures        (gitrepo_path)?; }
  s . reset_with_skgrepo_path (subtest_name, gitrepo_path) ?;
  let (config, tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    // De novo PartnerFolder creation reads this fixture-local graph.
    let env : SkgEnv =
      skg_env_from_parts (&config, &tantivy, &graph);
    let mut warnings : Vec<String> = Vec::new ();
    let (initial, _pids, _tree) =
      multi_root_view_via_env (
        &env, &[ ID::from ("R") ], true, None, &mut warnings
      ) ?;
    assert! ( ! initial . contains ("overriddenFolder")
              && ! initial . contains ("hiddenFolder"),
      "unrequested exotic folders should be absent from the initial diff \
       view:\n{}", initial );
    let request : String = initial . replace (
      "(affectsParent na)",
      "(affectsParent na) (viewRequests (folder overrides) \
       (folder hidesFromSubs))" );
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let rendered = {
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      update_from_and_rerender_buffer (
        &mut stream, &request, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ? };
    assert_buffer_contains (&rendered . saved_view, expected);
    Ok (( )) }

async fn requested_outbound_folders_show_phantoms_and_addedR_unstaged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    s, "skg-test-git-diff-overrides-unstaged", false,
    EXPECTED_UNSTAGED ) . await }

/// Folder node_axes: in diff mode, a requested folder whose worktree
/// membership is EMPTY but whose HEAD side is not still renders, holding only
/// phantoms -- for an outbound folder
/// (overriddenFolder, hiddenFolder), an inbound folder (overriderFolder, via the
/// inverse scan), and the subscribeeFolder.  Outside diff mode the
/// emptied folders still do not appear.
async fn emptied_requested_folders_still_render_in_diff_mode (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  setup_overrides_fixtures (gitrepo_path)?;
  s . reset_with_skgrepo_path (
    "emptied_requested_folders_still_render_in_diff_mode",
    gitrepo_path ) ?;
  let (config, tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    (
      graph_handle_from_config (&config)? );
    let env : SkgEnv =
      skg_env_from_parts (&config, &tantivy, &graph);
    let roots : [ID; 3] =
      [ ID::from ("E"), ID::from ("EN"), ID::from ("ES") ];
    { let mut warnings : Vec<String> = Vec::new ();
      let (initial_diff_view, _pids, _tree) =
        multi_root_view_via_env (
          &env, &roots, true, None, &mut warnings ) ?;
      assert! ( ! initial_diff_view . contains ("overriddenFolder")
                && ! initial_diff_view . contains ("hiddenFolder")
                && ! initial_diff_view . contains ("overriderFolder"),
        "emptied exotic folders should require requests even in diff mode:\n{}",
        initial_diff_view );
      let request : String = initial_diff_view
        . replace (
          "(id E) (repo main) (affectsParent na)",
          "(id E) (repo main) (affectsParent na) \
           (viewRequests (folder overrides) (folder hidesFromSubs))" )
        . replace (
          "(id EN) (repo main) (affectsParent na)",
          "(id EN) (repo main) (affectsParent na) \
           (viewRequests (folder overrides))" );
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : true,
        open_views        : OpenViews::new (), };
      let response = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &request, &config, &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains ( &response . saved_view, "\
* (skg (node (id E) (repo main))) E
** (skg overriddenFolder)
*** (skg (node (id EZ) (repo main) writeProtected (unstaged removedR))) EZ
** (skg hiddenFolder)
*** (skg (node (id EH) (repo main) writeProtected (unstaged removedR))) EH
* (skg (node (id EN) (repo main))) EN
** (skg overriderFolder)
*** (skg (node (id ER) (repo main) writeProtected (unstaged removedR))) ER
* (skg (node (id ES) (repo main))) ES
** (skg subscribeeFolder)
*** (skg (node (id EB) (repo main) writeProtected (unstaged removedR))) EB
" ); }
    { // Outside diff mode, the emptied folders still do not appear.
      let mut warnings : Vec<String> = Vec::new ();
      let (plain_view, _pids, _tree) =
        multi_root_view_via_env (
          &env, &roots, false, None, &mut warnings ) ?;
      for folder in [ "overriddenFolder", "hiddenFolder",
                   "overriderFolder", "subscribeeFolder" ] {
        assert! ( ! plain_view . contains (folder),
          "an empty {} must not render outside diff mode:\n{}",
          folder, plain_view ); }}
    Ok (( )) }

async fn requested_outbound_folders_show_phantoms_and_addedR_staged (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  run_overrides_view_test (
    s, "skg-test-git-diff-overrides-staged", true,
    EXPECTED_STAGED ) . await }

/// A diff-mode save of a buffer holding the worktree members is a
/// no-op for the relations, regenerates the phantoms (idempotence:
/// saving the rendered result changes nothing further), and never
/// collects a phantom as an editable-folder member (saving the phantom
/// line must not re-add W to R's overrides).
async fn diff_mode_save_is_noop_and_regenerates_outbound_phantoms (
  s : &mut SharedStoreSession,
) -> Result<(), Box<dyn Error>> {
  let temp_dir : TempDir = TempDir::new ()?;
  let gitrepo_path : &Path = temp_dir . path ();
  setup_overrides_fixtures (gitrepo_path)?;
  s . reset_with_skgrepo_path (
    "diff_mode_save_is_noop_and_regenerates_outbound_phantoms",
    gitrepo_path ) ?;
  let (config, tantivy)
    : (&SkgConfig, &mut TantivyIndex)
    = (&s . config, &mut s . tantivy);
    let graph = graph_handle_from_config (&config)?;
    let input : &str = "\
* (skg (node (id R) (repo main))) R
** (skg overriddenFolder)
*** (skg (node (id Z) (repo main))) Z
*** (skg (node (id O) (repo main))) O
** (skg hiddenFolder)
*** (skg (node (id ha) (repo main))) ha
*** (skg (node (id hc) (repo main))) hc
";
    let mut views_state : ViewsState = ViewsState {
      diff_mode_enabled : true,
      open_views        : OpenViews::new (), };
    let first = {
      let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
      update_from_and_rerender_buffer (
        &mut stream, input, &config, &tantivy, &graph,
        true, &Err (String::new ()), &mut views_state ) . await ? };
    assert_buffer_contains (
      &first . saved_view, EXPECTED_UNSTAGED );
    { // Saving the RENDERED RESULT (phantoms included) is a no-op:
      // the relations on disk keep their worktree values, and the
      // phantoms regenerate.
      let second = {
        let (mut stream, _keepalive) = mk_test_tcp_stream_pair ();
        update_from_and_rerender_buffer (
          &mut stream, &first . saved_view, &config,
          &tantivy, &graph,
          true, &Err (String::new ()), &mut views_state ) . await ? };
      assert_buffer_contains (
        &second . saved_view, EXPECTED_UNSTAGED );
      let r : Graphnode = read_graphnode (gitrepo_path, "R")?;
      assert_eq! (
        members_msv (&r . overrides) . or_default () . to_vec (),
        vec! [ ID::from ("Z"), ID::from ("O") ],
        "a phantom under a writable folder is never collected: W must \
         not return to R's overrides" );
      assert_eq! (
        members_msv (&r . hidesFromSubs) . or_default () . to_vec (),
        vec! [ ID::from ("ha"), ID::from ("hc") ],
        "hb must not return to R's hides list" ); }
    Ok (( )) }
