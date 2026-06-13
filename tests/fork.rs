// cargo nextest run --test grouped_overrides -E 'test(fork::)'
//
// The fork/clone feature: editing a foreign node N (read-only, in a
// source the user does not own) is read as a request to clone it. The
// clone C lives in an owned source, copies N's edited title/body/
// contains, subscribes to N and overrides N; N itself is untouched.
//
// Installs the process-global graph handle (override substitution and
// subscribeeCol content read it), so it belongs among the
// grouped_overrides installers.

use std::error::Error;
use std::sync::Arc;

use indoc::indoc;

use std::collections::BTreeSet;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::in_rust_graph::{
  InRustGraphHandle, install_or_swap_global_handle};
use skg::from_text::buffer_to_validated_saveplan;
use skg::serve::ViewsState;
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_db};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_approval_test;
use skg::to_org::render::content_view::single_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::ForkSpec;
use skg::types::views_state::OpenViews;
use typedb_driver::TypeDBDriver;

/// A foreign node N (title "N-original", contains [N1, N2]) lives under
/// an OWNED container P. The buffer makes N definitive and edits its
/// title to "N-edited" -- a real change, so saving forks N. (N's
/// children stay indefinitive foreign content.)
const FORK_BUFFER : &str = indoc! {"
  * (skg (node (id P) (source owned))) P-container
  ** (skg (node (id N) (source foreign))) N-edited
  *** (skg (node (id N1) (source foreign) indef)) N1
  *** (skg (node (id N2) (source foreign) indef)) N2
  "};

fn mk_test_tcp_stream () -> std::net::TcpStream {
  let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  std::net::TcpStream::connect (
    listener . local_addr () . unwrap () ) . unwrap () }

async fn fork_specs_from (
  buffer : &str,
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<Vec<ForkSpec>, Box<dyn Error>> {
  Ok ( buffer_to_validated_saveplan ( buffer, config, driver, None )
       . await ? . 1 . fork_specs ) }

fn node_from_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<NodeComplete, Box<dyn Error>> {
  let id : ID = ID::from (pid);
  read_all_skg_files_from_sources (config) ?
    . into_iter ()
    . find ( |node| node . pid == id )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into () ) }

/// The single clone produced by saving the FORK_BUFFER -- the owned
/// node whose overrides_view_of names N. (Its pid is a fresh uuid we
/// do not know in advance.)
fn clone_on_disk (
  config : &SkgConfig,
) -> Result<NodeComplete, Box<dyn Error>> {
  read_all_skg_files_from_sources (config) ?
    . into_iter ()
    . find ( |node|
      node . overrides_view_of . or_default ()
        . contains (& ID::from ("N")) )
    . ok_or_else ( || "no clone (overrides_view_of [N]) on disk" . into () ) }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/fork/fixtures-multi";
  run_with_shared_test_db (
    "skg-test-fork",
    |s| Box::pin ( async move {
      s . reset ("fork_save_instruction", fixtures) . await ?;
      fork_save_instruction (
        &s . config, &s . driver ) . await ?;
      s . reset ("fork_fixture_files", fixtures) . await ?;
      fork_fixture_files (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("fork_round_trip", fixtures) . await ?;
      fork_round_trip (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("fork_monogamy", fixtures) . await ?;
      fork_monogamy (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("fork_source_inactive", fixtures) . await ?;
      fork_source_inactive (
        &s . config, &s . driver ) . await ?;
      s . reset ("fork_confirmation_gates_commit", fixtures) . await ?;
      fork_confirmation_gates_commit (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The confirmation stage: a save that finds forks but is NOT approved
/// returns a fork-confirmation buffer and commits NOTHING; re-issuing
/// the save approved then commits.
async fn fork_confirmation_gates_commit (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };

  // Unapproved save -> fork-confirmation, nothing committed.
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ false ) . await ?;
  assert! ( response . fork_confirmation . is_some (),
    "an unapproved save with forks must return a fork-confirmation" );
  assert! ( response . saved_view . contains ("FORK CONFIRMATION")
            && response . saved_view . contains ("(id N)"),
    "the confirmation buffer must list N:\n{}", response . saved_view );
  assert! ( clone_on_disk (config) . is_err (),
    "no clone may be committed before approval" );
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original",
    "N must be untouched before approval" );

  // Approved re-issue -> commits the clone.
  let mut stream2 : std::net::TcpStream = mk_test_tcp_stream ();
  let response2 = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream2, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response2 . fork_confirmation . is_none (),
    "an approved save commits and returns a normal save-result" );
  assert! ( clone_on_disk (config) . is_ok (),
    "the clone must be committed after approval" );
  Ok (( )) }

/// Monogamy: a node may have at most one user-owned overrider. Forking
/// N once creates a clone; forking it again is rejected with
/// 'ForkAlreadyExists' naming the existing clone -- not the raw
/// MultipleUserOwnedOverriders crash.
async fn fork_monogamy (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  let c1 : ID = clone_on_disk (config) ? . pid;

  // Refresh the global graph so the monogamy pre-check sees the new
  // clone, then try to fork the same N again.
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, driver, None ) . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkAlreadyExists (orig, existing)
          if *orig == ID::from ("N") && *existing == c1 )),
        "expected ForkAlreadyExists(N, {}), got {:?}", c1 . 0, errors ); }
    other => panic! (
      "expected ForkAlreadyExists rejecting the re-fork, got {:?}", other ), }
  Ok (( )) }

/// Source-set: a fork whose resolved owned source is inactive under the
/// active source-set is forbidden ('ForkSourceInactive'), so an
/// invisible clone is never created silently. Here the active set is
/// {foreign} only, so the clone's inferred source "owned" is inactive.
async fn fork_source_inactive (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  // Refresh the process-global graph from the freshly-reset fixtures so
  // the monogamy pre-check does not see a prior sub-test's clone (reset
  // wipes disk/DB but not the global handle).
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let active : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName ("only-foreign" . to_string ()),
    sources : BTreeSet::from ([ SourceName::from ("foreign") ]), };
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, driver, Some (&active) ) . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkSourceInactive (orig, source)
          if *orig == ID::from ("N")
             && *source == SourceName::from ("owned") )),
        "expected ForkSourceInactive(N, owned), got {:?}", errors ); }
    other => panic! (
      "expected ForkSourceInactive, got {:?}", other ), }
  Ok (( )) }

/// The SavePlan a foreign edit produces carries one ForkSpec whose
/// clone copies N's title/body/contains (SHALLOW -- the child IDs only,
/// no descendants), subscribes_to=[N], overrides_view_of=[N], and no
/// hides; in an OWNED source inferred from the owned ancestor P.
async fn fork_save_instruction (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (FORK_BUFFER, config, driver) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one fork (of N): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_id, ID::from ("N") );
  let c : &NodeComplete = &spec . clone . 0;
  assert_eq! ( c . title, "N-edited",
    "clone copies the edited title" );
  assert_eq! ( c . contains, vec! [ ID::from ("N1"), ID::from ("N2") ],
    "clone copies N's child IDs shallow (not descendants)" );
  assert_eq! ( c . subscribes_to . or_default (), &[ ID::from ("N") ],
    "clone subscribes to N" );
  assert_eq! ( c . overrides_view_of . or_default (), &[ ID::from ("N") ],
    "clone overrides N" );
  assert! ( c . hides_from_its_subscriptions . or_default () . is_empty (),
    "clone records no hides" );
  assert! ( config . user_owns_source (& c . source),
    "clone lives in an owned source, got {:?}", c . source );
  assert_eq! ( c . source, SourceName::from ("owned"),
    "clone's source is inferred from the owned ancestor P" );
  assert_ne! ( c . pid, ID::from ("N"),
    "clone has a fresh pid, not N's" );
  Ok (( )) }

/// After saving the fork: a clone .skg appears in the OWNED source with
/// the four fields, and N's foreign .skg is byte-unchanged.
async fn fork_fixture_files (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let n_path : std::path::PathBuf =
    config . sources . get (& SourceName::from ("foreign")) . unwrap ()
    . path . join ("N.skg");
  let n_before : String = std::fs::read_to_string (&n_path) ?;

  // Use the RETURNED handle: when this process already has a global
  // handle (a prior sub-test set it), install_or_swap stores our graph
  // into the existing global ArcSwap and hands back THAT handle, so the
  // save's in-place updates stay visible to snapshot_global (the
  // coherence check reads it).
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "fork save must not error: {:?}", response . errors );

  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . title, "N-edited" );
  assert_eq! ( c . contains, vec! [ ID::from ("N1"), ID::from ("N2") ] );
  assert_eq! ( c . subscribes_to . or_default (), &[ ID::from ("N") ] );
  assert_eq! ( c . source, SourceName::from ("owned") );

  let n_after : String = std::fs::read_to_string (&n_path) ?;
  assert_eq! ( n_before, n_after,
    "N's foreign .skg must be byte-unchanged by the fork" );
  // And the in-memory disk node still says N-original.
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original" );
  Ok (( )) }

/// Round-trip: after the fork, reopening P draws the clone in N's
/// place, carrying (overridesHere N), with both an overriddenCol and a
/// subscribeeCol listing N. The load-bearing property: P's stored
/// 'contains' is NOT rewritten to the clone -- it still lists N, so the
/// marker round-trips and a save never silently re-points containers at
/// the clone. (That the clone's subscribee-as-such view of N starts
/// empty and fills as N gains children is the prerequisite rule,
/// exercised in hidden_from_subscriptions; here C.contains ==
/// N.contains == [N1,N2] gives it nothing to show.)
async fn fork_round_trip (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  let clone_id : ID = clone_on_disk (config) ? . pid;

  // Reopen P de-novo. N is P's content; override substitution draws the
  // clone in its place with (overridesHere N).
  let (p_view, _pids, _) =
    single_root_view (
      driver, config, Some (tantivy), &ID::from ("P"), false ) . await ?;
  assert! ( p_view . contains ("(overridesHere N)"),
    "the clone must be drawn in N's place with (overridesHere N):\n{}",
    p_view );
  assert! ( p_view . contains (& format! ("(id {})", clone_id . 0)),
    "the drawn substitute must be the clone {}:\n{}", clone_id . 0, p_view );
  assert! ( p_view . contains ("subscribeeCol"),
    "the clone (a subscriber of N) shows a subscribeeCol:\n{}", p_view );
  assert! ( p_view . contains ("overriddenCol"),
    "the clone (an overrider of N) shows an overriddenCol:\n{}", p_view );

  // The load-bearing round-trip: P's stored contains was NOT rewritten
  // to the clone; it still lists N (the marker collected N, not C).
  let p_disk : NodeComplete = node_from_disk (config, "P") ?;
  assert_eq! ( p_disk . contains, vec! [ ID::from ("N") ],
    "P's contains must still point at N, not the clone {}", clone_id . 0 );
  Ok (( )) }
