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

use std::collections::{BTreeSet, HashMap};

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::in_rust_graph::{
  InRustGraphHandle, install_or_swap_global_handle};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_validated_saveplan_with_fork_sources;
use skg::serve::ViewsState;
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_db};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_approval_test;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_sources_test;
use skg::to_org::render::content_view::single_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, ForkSpec, SaveNode};
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

/// A foreign node N opened as a bare ROOT -- no owned ancestor to infer
/// a clone source from. Editing its title forks it; the clone's source
/// must then default to the user's first owned source.
const FORK_ROOT_BUFFER : &str = indoc! {"
  * (skg (node (id N) (source foreign))) N-edited
  ** (skg (node (id N1) (source foreign) indef)) N1
  ** (skg (node (id N2) (source foreign) indef)) N2
  "};

/// Case 1 of TODO/fork-fixes.org: a BARE new headline (no metadata at
/// all) appended under a foreign root. Enrichment mints it an id and
/// inherits the foreign source; that must NOT be a foreign-creation
/// error -- appending it edits N's contains, which forks N, and the
/// new node rides that fork, adopting the clone's source.
const FORK_WITH_BARE_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg (node (id N) (source foreign))) N-original
  ** (skg (node (id N1) (source foreign) indef)) N1
  ** (skg (node (id N2) (source foreign) indef)) N2
  ** Can I add to this?
  "};

/// Same shape, but the new node EXPLICITLY claims the foreign source:
/// a deliberate attempt to create a node in a read-only source, which
/// must stay rejected.
const FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg (node (id N) (source foreign))) N-original
  ** (skg (node (id N1) (source foreign) indef)) N1
  ** (skg (node (id N2) (source foreign) indef)) N2
  ** (skg (node (source foreign))) Can I add to this?
  "};

/// The explicit 'skg-fork-node' gesture: an OWNED node (P) carries
/// (viewRequests fork). Unlike the implicit foreign fork, P keeps its
/// own save; saving adds a clone C that overrides P, built from P's disk
/// snapshot.
const EXPLICIT_FORK_BUFFER : &str = indoc! {"
  * (skg (node (id P) (source owned) (viewRequests fork))) P-container
  ** (skg (node (id N) (source foreign) indef)) N
  "};

/// An explicit fork request on a brand-new (id-less) headline: enrichment
/// mints a fresh pid that is not in the graph, so the fork is rejected.
const EXPLICIT_FORK_UNKNOWN_BUFFER : &str = indoc! {"
  * (skg (node (source owned) (viewRequests fork))) Brand New
  "};

fn clone_overriding_on_disk (
  config : &SkgConfig,
  target : &str,
) -> Result<NodeComplete, Box<dyn Error>> {
  read_all_skg_files_from_sources (config) ?
    . into_iter ()
    . find ( |node| node . overrides_view_of . or_default ()
             . contains (& ID::from (target)) )
    . ok_or_else ( || format! (
        "no clone (overrides_view_of [{}]) on disk", target ) . into () ) }

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
      s . reset ("fork_from_bare_new_child_plan", fixtures) . await ?;
      fork_from_bare_new_child_plan (
        &s . config, &s . driver ) . await ?;
      s . reset ("fork_from_bare_new_child_commits", fixtures) . await ?;
      fork_from_bare_new_child_commits (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("explicitly_foreign_new_child_still_rejected", fixtures) . await ?;
      explicitly_foreign_new_child_still_rejected (
        &s . config, &s . driver ) . await ?;
      let two_owned : &str = "tests/fork/fixtures-two-owned";
      s . reset ("fork_no_owned_ancestor_defaults", two_owned) . await ?;
      fork_no_owned_ancestor_defaults (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("fork_user_set_source_overrides", two_owned) . await ?;
      fork_user_set_source_overrides (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("fork_default_prefers_active_owned_source", two_owned) . await ?;
      fork_default_prefers_active_owned_source (
        &s . config, &s . driver ) . await ?;
      s . reset ("fork_user_set_source_not_owned_rejected", two_owned) . await ?;
      fork_user_set_source_not_owned_rejected (
        &s . config, &s . driver ) . await ?;
      s . reset ("explicit_fork_save_instruction", fixtures) . await ?;
      explicit_fork_save_instruction (
        &s . config, &s . driver ) . await ?;
      s . reset ("explicit_fork_on_unknown_node_errors", fixtures) . await ?;
      explicit_fork_on_unknown_node_errors (
        &s . config, &s . driver ) . await ?;
      s . reset ("explicit_fork_round_trip_and_monogamy", fixtures) . await ?;
      explicit_fork_round_trip_and_monogamy (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The explicit fork of an OWNED node P: the SavePlan carries one
/// ForkSpec whose clone copies P's DISK snapshot (title/contains),
/// subscribes_to=[P], overrides_view_of=[P], in the config-first owned
/// source. P itself keeps its own (owned) save -- it is not dropped like
/// a foreign fork's original.
async fn explicit_fork_save_instruction (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (EXPLICIT_FORK_BUFFER, config, driver) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one explicit fork (of P): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_id, ID::from ("P") );
  let c : &NodeComplete = &spec . clone . 0;
  assert_eq! ( c . title, "P-container",
    "clone copies P's disk title (not a buffer edit)" );
  assert_eq! ( c . contains, vec! [ ID::from ("N") ],
    "clone copies P's disk contains (shallow)" );
  assert_eq! ( c . subscribes_to . or_default (), &[ ID::from ("P") ],
    "clone subscribes to P" );
  assert_eq! ( c . overrides_view_of . or_default (), &[ ID::from ("P") ],
    "clone overrides P" );
  assert_eq! ( c . source, SourceName::from ("owned"),
    "clone defaults to the config-first owned source; got {:?}",
    c . source );
  assert_ne! ( c . pid, ID::from ("P"),
    "clone has a fresh pid, not P's" );
  Ok (( )) }

/// An explicit fork request on a node not in the graph (a brand-new
/// headline, whose minted pid is unknown) is rejected with
/// 'ForkRequestOnUnknownNode' -- you can only fork a saved node.
async fn explicit_fork_on_unknown_node_errors (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let result = buffer_to_validated_saveplan (
    EXPLICIT_FORK_UNKNOWN_BUFFER, config, driver, None ) . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkRequestOnUnknownNode (_) )),
        "expected ForkRequestOnUnknownNode, got {:?}", errors ); }
    other => panic! (
      "expected ForkRequestOnUnknownNode, got {:?}", other ), }
  Ok (( )) }

/// The explicit fork commits a clone overriding the owned P (override
/// substitution then draws it in P's place), and a SECOND explicit fork
/// of the now-overridden P is rejected with 'ForkAlreadyExists'.
async fn explicit_fork_round_trip_and_monogamy (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, EXPLICIT_FORK_BUFFER, driver, config, tantivy, &graph,
    false, &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved explicit fork must commit: {:?}", response . errors );
  let clone : NodeComplete = clone_overriding_on_disk (config, "P") ?;
  assert_eq! ( clone . subscribes_to . or_default (), &[ ID::from ("P") ],
    "the clone subscribes to P" );
  assert! ( config . user_owns_source (& clone . source),
    "the clone lives in an owned source" );
  // P is untouched (still owns its container role).
  assert_eq! ( node_from_disk (config, "P") ? . contains,
               vec! [ ID::from ("N") ],
    "P keeps its own contains; the explicit fork does not rewrite it" );

  // Forking P again is rejected: it now has a user-owned overrider.
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let result = buffer_to_validated_saveplan (
    EXPLICIT_FORK_BUFFER, config, driver, None ) . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkAlreadyExists (orig, existing)
          if *orig == ID::from ("P") && *existing == clone . pid )),
        "expected ForkAlreadyExists(P, {}), got {:?}",
        clone . pid . 0, errors ); }
    other => panic! (
      "expected ForkAlreadyExists rejecting the re-fork, got {:?}",
      other ), }
  Ok (( )) }

/// Under a restricted source-set, the clone-source DEFAULT must prefer an
/// owned source that is ACTIVE. Here "owned" (alphabetically first owned)
/// is inactive and "owned2" is the only active owned source; a foreign
/// node with no owned ancestor must default to "owned2" and reach the
/// confirmation stage, not dead-end on ForkSourceInactive.
async fn fork_default_prefers_active_owned_source (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let active : ActiveSourceSet = ActiveSourceSet {
    name    : SourceSetName ("only-owned2" . to_string ()),
    sources : BTreeSet::from ([ SourceName::from ("owned2"),
                               SourceName::from ("foreign") ]), };
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    FORK_ROOT_BUFFER, config, driver, Some (&active) ) . await ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "the fork must resolve, not dead-end on an inactive default source" );
  assert_eq! ( save_plan . fork_specs[0] . clone . 0 . source,
               SourceName::from ("owned2"),
    "with 'owned' inactive, the default must prefer the active owned 'owned2'; got {:?}",
    save_plan . fork_specs[0] . clone . 0 . source );
  Ok (( )) }

/// A user-set clone source that the user does NOT own (a configured but
/// foreign source, which the C-c s s prompt would let one type) is
/// rejected with ForkSourceNotOwned.
async fn fork_user_set_source_not_owned_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let fork_sources : HashMap<ID, SourceName> =
    HashMap::from ([ ( ID::from ("N"), SourceName::from ("foreign") ) ]);
  let result = buffer_to_validated_saveplan_with_fork_sources (
    FORK_BUFFER, config, driver, None, &fork_sources ) . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkSourceNotOwned (orig, source)
          if *orig == ID::from ("N")
             && *source == SourceName::from ("foreign") )),
        "expected ForkSourceNotOwned(N, foreign), got {:?}", errors ); }
    other => panic! (
      "expected ForkSourceNotOwned rejecting a non-owned chosen source, got {:?}",
      other ), }
  Ok (( )) }

/// A foreign node with NO owned ancestor still forks: with no user-set
/// source and nothing to infer, the clone's source defaults to the
/// user's first owned source (alphabetically "owned", here). No more
/// 'ForkSourceUnresolved' hard-fail.
async fn fork_no_owned_ancestor_defaults (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_ROOT_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "a fork with no owned ancestor must complete: {:?}", response . errors );
  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . source, SourceName::from ("owned"),
    "with no owned ancestor and no user-set source, the clone defaults to \
     the first owned source (alphabetically 'owned'); got {:?}", c . source );
  Ok (( )) }

/// The user-set clone source (the 'fork-sources' transport) overrides
/// the inferred one. FORK_BUFFER infers "owned" from the owned ancestor
/// P; passing N -> "owned2" lands the clone in "owned2" instead.
async fn fork_user_set_source_overrides (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let fork_sources : HashMap<ID, SourceName> =
    HashMap::from ([ ( ID::from ("N"), SourceName::from ("owned2") ) ]);
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_sources_test (
    &mut stream, FORK_BUFFER, driver, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true, &fork_sources ) . await ?;
  assert! ( response . errors . is_empty (),
    "the user-set fork must commit: {:?}", response . errors );
  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . source, SourceName::from ("owned2"),
    "the user-set source 'owned2' must override the inferred 'owned'; \
     got {:?}", c . source );
  Ok (( )) }

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

/// TODO/fork-fixes.org Case 1, at the plan level: appending a bare
/// (metadata-less) new headline under a foreign root forks the root
/// rather than dying with "Cannot create node in foreign source". The
/// SavePlan must hold one ForkSpec for N, whose clone's contains end
/// with the new node's minted id, and a kept Save instruction for the
/// new node REWRITTEN into the clone's source.
async fn fork_from_bare_new_child_plan (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let ( _vf, save_plan, _warnings ) = buffer_to_validated_saveplan (
    FORK_WITH_BARE_NEW_CHILD_BUFFER, config, driver, None ) . await ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "appending a bare new child must fork N: {:?}",
    save_plan . fork_specs );
  let clone : &NodeComplete = & save_plan . fork_specs[0] . clone . 0;
  assert_eq! ( save_plan . fork_specs[0] . original_id, ID::from ("N") );
  assert_eq! ( clone . contains . len (), 3,
    "the clone's contains must be N1, N2 and the new node: {:?}",
    clone . contains );
  assert_eq! ( & clone . contains [..2],
               & [ ID::from ("N1"), ID::from ("N2") ] );
  let new_node : &NodeComplete =
    save_plan . define_nodes . iter ()
    . find_map ( |dn| match dn {
        DefineNode::Save ( SaveNode (n) )
          if n . title == "Can I add to this?" => Some (n),
        _ => None } )
    . expect ("the new node must survive as a Save instruction");
  assert_eq! ( new_node . source, clone . source,
    "the new node must adopt the clone's source" );
  assert_eq! ( new_node . pid, clone . contains [2],
    "the clone's last child must be the new node" );
  Ok (( )) }

/// TODO/fork-fixes.org Case 1, committed: the approved save creates
/// the clone AND the new node, both in the owned source; N's foreign
/// .skg is untouched.
async fn fork_from_bare_new_child_commits (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_WITH_BARE_NEW_CHILD_BUFFER, driver, config, tantivy,
    &graph, false, &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved bare-new-child fork must commit: {:?}",
    response . errors );
  let clone : NodeComplete = clone_on_disk (config) ?;
  let new_node : NodeComplete =
    read_all_skg_files_from_sources (config) ?
    . into_iter ()
    . find ( |node| node . title == "Can I add to this?" )
    . ok_or ("the new node must be on disk") ?;
  assert_eq! ( new_node . source, clone . source,
    "the new node must land in the clone's source" );
  assert_eq! ( clone . source, SourceName::from ("owned") );
  assert! ( clone . contains . contains (& new_node . pid),
    "the clone must contain the new node: {:?}", clone . contains );
  assert_eq! ( node_from_disk (config, "N") ? . contains,
               vec! [ ID::from ("N1"), ID::from ("N2") ],
    "N's own contains must be untouched" );
  Ok (( )) }

/// An EXPLICITLY foreign-sourced new node stays a rejection: only an
/// inherited (guessed) foreign source rides the fork.
async fn explicitly_foreign_new_child_still_rejected (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  let _ : InRustGraphHandle =
    install_or_swap_global_handle ( graph_handle_from_config (config) ? );
  let result = buffer_to_validated_saveplan (
    FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER, config, driver, None )
    . await;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::CreatedForeignNode (..) )),
        "expected CreatedForeignNode, got {:?}", errors ); }
    other => panic! (
      "expected CreatedForeignNode for an explicitly foreign new node, got {:?}",
      other ), }
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
