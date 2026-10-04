// cargo nextest run --test grouped_overrides -E 'test(fork::)'
//
// The fork/clone feature: editing a foreign node N (write-protected, in a
// repo the user does not own) is read as a request to clone it. The
// clone C lives in an owned repo, copies N's edited title/body/
// contains, subscribes to N and overrides N; N itself is untouched.
//
// Installs the explicit graph handle (override substitution and
// subscribeeFolder content read it), so it belongs among the
// grouped_overrides installers.

use std::error::Error;

use indoc::indoc;

use std::collections::{BTreeSet, HashMap};

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_repos;
use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_validated_saveplan_with_fork_repos;
use skg::org_to_text::viewforest_to_string;
use skg::serve::ViewsState;
use skg::repo_sets::{ActiveRepoSet, RepoSetName};
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_approval_test;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_repos_test;
use skg::to_org::render::content_view::single_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, RepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::{DefineNode, ForkSpec, SaveNode};
use skg::types::views_state::{OpenViews, ViewUri};

/// A foreign node N (title "N-original", contains [N1, N2]) lives under
/// an OWNED container P. The buffer makes N definitive and edits its
/// title to "N-edited" -- a real change, so saving forks N. (N's
/// children stay write-protected foreign content.)
const FORK_BUFFER : &str = indoc! {"
  * (skg (node (id P) (repo owned))) P-container
  ** (skg (node (id N) (repo foreign))) N-edited
  *** (skg (node (id N1) (repo foreign) writeProtected)) N1
  *** (skg (node (id N2) (repo foreign) writeProtected)) N2
  "};

/// A foreign node N opened as a bare ROOT -- no owned ancestor to infer
/// a clone repo from. Editing its title forks it; the clone's repo
/// must then default to the user's first owned repo.
const FORK_ROOT_BUFFER : &str = indoc! {"
  * (skg (node (id N) (repo foreign))) N-edited
  ** (skg (node (id N1) (repo foreign) writeProtected)) N1
  ** (skg (node (id N2) (repo foreign) writeProtected)) N2
  "};

/// Case 1 of TODO/fork-fixes.org: a BARE new headline (no metadata at
/// all) appended under a foreign root. Enrichment mints it an id and
/// inherits the foreign repo; that must NOT be a foreign-creation
/// error -- appending it edits N's contains, which forks N, and the
/// new node rides that fork, adopting the clone's repo.
const FORK_WITH_BARE_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg (node (id N) (repo foreign))) N-original
  ** (skg (node (id N1) (repo foreign) writeProtected)) N1
  ** (skg (node (id N2) (repo foreign) writeProtected)) N2
  ** Can I add to this?
  "};

/// The structural fork gesture: insert a bare new parent under foreign N,
/// then move old content N1 beneath it. The new node and its inherited
/// `contains N1` edge must both adopt the clone's owned repo.
const FORK_WITH_NEW_PARENT_FOR_OLD_CHILD_BUFFER : &str = indoc! {"
  * (skg (node (id N) (repo foreign))) N-original
  ** New parent
  *** (skg (node (id N1) (repo foreign) writeProtected)) N1
  ** (skg (node (id N2) (repo foreign) writeProtected)) N2
  "};

/// Same shape, but the new node EXPLICITLY claims the foreign repo:
/// a deliberate attempt to create a node in a write-protected repo, which
/// must stay rejected.
const FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg (node (id N) (repo foreign))) N-original
  ** (skg (node (id N1) (repo foreign) writeProtected)) N1
  ** (skg (node (id N2) (repo foreign) writeProtected)) N2
  ** (skg (node (repo foreign))) Can I add to this?
  "};

/// The explicit 'skg-fork-node' gesture: an OWNED node (P) carries
/// (viewRequests fork). Unlike the implicit foreign fork, P keeps its
/// own save; saving adds a clone C that overrides P, built from P's disk
/// snapshot.
const EXPLICIT_FORK_BUFFER : &str = indoc! {"
  * (skg (node (id P) (repo owned) (viewRequests fork))) P-container
  ** (skg (node (id N) (repo foreign) writeProtected)) N
  "};

/// An explicit fork request on a brand-new (id-less) headline: enrichment
/// mints a fresh pid that is not in the graph, so the fork is rejected.
const EXPLICIT_FORK_UNKNOWN_BUFFER : &str = indoc! {"
  * (skg (node (repo owned) (viewRequests fork))) Brand New
  "};

fn clone_overriding_on_disk (
  config : &SkgConfig,
  target : &str,
) -> Result<NodeComplete, Box<dyn Error>> {
  read_all_skg_files_from_repos (config) ?
    . into_iter ()
    . find ( |node| node . overrides_view_of . or_default () . iter ()
             . any ( |m| m . member == ID::from (target) ) )
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
) -> Result<Vec<ForkSpec>, Box<dyn Error>> {
  Ok ( buffer_to_validated_saveplan ( buffer, config, None )
        ? . 1 . fork_specs ) }

fn node_from_disk (
  config : &SkgConfig,
  pid    : &str,
) -> Result<NodeComplete, Box<dyn Error>> {
  let id : ID = ID::from (pid);
  read_all_skg_files_from_repos (config) ?
    . into_iter ()
    . find ( |node| node . pid == id )
    . ok_or_else ( || format! ("node not found on disk: {}", pid) . into () ) }

/// The single clone produced by saving the FORK_BUFFER -- the owned
/// node whose overrides_view_of names N. (Its pid is a fresh uuid we
/// do not know in advance.)
fn clone_on_disk (
  config : &SkgConfig,
) -> Result<NodeComplete, Box<dyn Error>> {
  read_all_skg_files_from_repos (config) ?
    . into_iter ()
    . find ( |node|
      node . overrides_view_of . or_default () . iter ()
        . any ( |m| m . member == ID::from ("N") ) )
    . ok_or_else ( || "no clone (overrides_view_of [N]) on disk" . into () ) }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/fork/fixtures-multi";
  run_with_shared_test_stores (
    "skg-test-fork",
    |s| Box::pin ( async move {
      s . reset ("fork_save_instruction", fixtures) ?;
      fork_save_instruction (
        &s . config ) . await ?;
      s . reset ("fork_fixture_files", fixtures) ?;
      fork_fixture_files (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_round_trip", fixtures) ?;
      fork_round_trip (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_collateral_rerender_without_substitution", fixtures) ?;
      fork_collateral_rerender_without_substitution (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_monogamy", fixtures) ?;
      fork_monogamy (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_repo_inactive", fixtures) ?;
      fork_repo_inactive (
        &s . config ) . await ?;
      s . reset ("fork_confirmation_gates_commit", fixtures) ?;
      fork_confirmation_gates_commit (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_from_bare_new_child_plan", fixtures) ?;
      fork_from_bare_new_child_plan (
        &s . config ) . await ?;
      s . reset ("fork_new_parent_adopts_relationship_repos", fixtures) ?;
      fork_new_parent_adopts_relationship_repos (
        &s . config ) . await ?;
      s . reset ("fork_from_bare_new_child_commits", fixtures) ?;
      fork_from_bare_new_child_commits (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("explicitly_foreign_new_child_still_rejected", fixtures) ?;
      explicitly_foreign_new_child_still_rejected (
        &s . config ) . await ?;
      let two_owned : &str = "tests/fork/fixtures-two-owned";
      s . reset ("fork_no_owned_ancestor_defaults", two_owned) ?;
      fork_no_owned_ancestor_defaults (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_user_set_repo_overrides", two_owned) ?;
      fork_user_set_repo_overrides (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_default_prefers_active_owned_repo", two_owned) ?;
      fork_default_prefers_active_owned_repo (
        &s . config ) . await ?;
      s . reset ("fork_user_set_repo_not_owned_rejected", two_owned) ?;
      fork_user_set_repo_not_owned_rejected (
        &s . config ) . await ?;
      s . reset ("explicit_new_child_repo_confirms_clone_repo", two_owned) ?;
      explicit_new_child_repo_confirms_clone_repo (
        &s . config ) . await ?;
      s . reset ("disagreeing_new_child_repos_leave_clone_repo_unconfirmed", two_owned) ?;
      disagreeing_new_child_repos_leave_clone_repo_unconfirmed (
        &s . config ) . await ?;
      s . reset ("explicit_fork_save_instruction", fixtures) ?;
      explicit_fork_save_instruction (
        &s . config ) . await ?;
      s . reset ("explicit_fork_on_unknown_node_errors", fixtures) ?;
      explicit_fork_on_unknown_node_errors (
        &s . config ) . await ?;
      s . reset ("explicit_fork_round_trip_and_monogamy", fixtures) ?;
      explicit_fork_round_trip_and_monogamy (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The explicit fork of an OWNED node P: the SavePlan carries one
/// ForkSpec whose clone copies P's DISK snapshot (title/contains),
/// subscribes_to=[P], overrides_view_of=[P], in the config-first owned
/// repo. P itself keeps its own (owned) save -- it is not dropped like
/// a foreign fork's original.
async fn explicit_fork_save_instruction (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (EXPLICIT_FORK_BUFFER, config) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one explicit fork (of P): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_id, ID::from ("P") );
  let c : &NodeComplete = &spec . clone . 0;
  assert_eq! ( c . title, "P-container",
    "clone copies P's disk title (not a buffer edit)" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N") ],
    "clone copies P's disk contains (shallow)" );
  assert_eq! ( members_of ( c . subscribes_to . or_default () ), vec! [ ID::from ("P") ],
    "clone subscribes to P" );
  assert_eq! ( members_of ( c . overrides_view_of . or_default () ), vec! [ ID::from ("P") ],
    "clone overrides P" );
  assert_eq! ( c . home_repo, RepoName::from ("owned"),
    "clone defaults to the config-first owned repo; got {:?}",
    c . home_repo );
  assert_ne! ( c . pid, ID::from ("P"),
    "clone has a fresh pid, not P's" );
  Ok (( )) }

/// An explicit fork request on a node not in the graph (a brand-new
/// headline, whose minted pid is unknown) is rejected with
/// 'ForkRequestOnUnknownNode' -- you can only fork a saved node.
async fn explicit_fork_on_unknown_node_errors (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let result = buffer_to_validated_saveplan (
    EXPLICIT_FORK_UNKNOWN_BUFFER, config, None ) ;
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
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, EXPLICIT_FORK_BUFFER, config, tantivy, &graph,
    false, &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved explicit fork must commit: {:?}", response . errors );
  let clone : NodeComplete = clone_overriding_on_disk (config, "P") ?;
  assert_eq! ( members_of ( clone . subscribes_to . or_default () ), vec! [ ID::from ("P") ],
    "the clone subscribes to P" );
  assert! ( config . user_owns_repo (& clone . home_repo),
    "the clone lives in an owned repo" );
  // P is untouched (still owns its container role).
  assert_eq! ( members_of (& node_from_disk (config, "P") ? . contains),
               vec! [ ID::from ("N") ],
    "P keeps its own contains; the explicit fork does not rewrite it" );

  // Forking P again is rejected: it now has a user-owned overrider.
  let result = buffer_to_validated_saveplan (
    EXPLICIT_FORK_BUFFER, config, None ) ;
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

/// Under a restricted repo-set, the clone-repo DEFAULT must prefer an
/// owned repo that is ACTIVE. Here "owned" (alphabetically first owned)
/// is inactive and "owned2" is the only active owned repo; a foreign
/// node with no owned ancestor must default to "owned2" and reach the
/// confirmation stage, not dead-end on ForkRepoInactive.
async fn fork_default_prefers_active_owned_repo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let active : ActiveRepoSet = ActiveRepoSet {
    name    : RepoSetName ("only-owned2" . to_string ()),
    repos : BTreeSet::from ([ RepoName::from ("owned2"),
                               RepoName::from ("foreign") ]), };
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    FORK_ROOT_BUFFER, config, Some (&active) )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "the fork must resolve, not dead-end on an inactive default repo" );
  assert_eq! ( save_plan . fork_specs[0] . clone . 0 . home_repo,
               RepoName::from ("owned2"),
    "with 'owned' inactive, the default must prefer the active owned 'owned2'; got {:?}",
    save_plan . fork_specs[0] . clone . 0 . home_repo );
  Ok (( )) }

/// A user-set clone repo that the user does NOT own (a configured but
/// foreign repo, which the C-c s s prompt would let one type) is
/// rejected with ForkRepoNotOwned.
async fn fork_user_set_repo_not_owned_rejected (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_repos : HashMap<ID, RepoName> =
    HashMap::from ([ ( ID::from ("N"), RepoName::from ("foreign") ) ]);
  let result = buffer_to_validated_saveplan_with_fork_repos (
    FORK_BUFFER, config, None, &fork_repos ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkRepoNotOwned (orig, repo)
          if *orig == ID::from ("N")
             && *repo == RepoName::from ("foreign") )),
        "expected ForkRepoNotOwned(N, foreign), got {:?}", errors ); }
    other => panic! (
      "expected ForkRepoNotOwned rejecting a non-owned chosen repo, got {:?}",
      other ), }
  Ok (( )) }

/// A foreign node with NO owned ancestor still forks: with no user-set
/// repo and nothing to infer, the clone's repo defaults to the
/// user's first owned repo (alphabetically "owned", here). No more
/// 'ForkRepoUnresolved' hard-fail.
async fn fork_no_owned_ancestor_defaults (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_ROOT_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "a fork with no owned ancestor must complete: {:?}", response . errors );
  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . home_repo, RepoName::from ("owned"),
    "with no owned ancestor and no user-set repo, the clone defaults to \
     the first owned repo (alphabetically 'owned'); got {:?}", c . home_repo );
  let root_line : &str = response . saved_view . lines () . next ()
    . ok_or ("the saved root view must not be empty") ?;
  assert! ( root_line . contains (&format! ("(id {})", c . pid . 0))
            && root_line . contains ("(overridesHere N)"),
    "the buffer that forked root N must immediately show its clone in N's place:\n{}",
    response . saved_view );
  assert! ( ! root_line . contains ("(id N)"),
    "the saved buffer must not leave the fork origin as its root:\n{}",
    response . saved_view );
  Ok (( )) }

/// The user-set clone repo (the 'fork-repos' transport) overrides
/// the inferred one. FORK_BUFFER infers "owned" from the owned ancestor
/// P; passing N -> "owned2" lands the clone in "owned2" instead.
async fn fork_user_set_repo_overrides (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let fork_repos : HashMap<ID, RepoName> =
    HashMap::from ([ ( ID::from ("N"), RepoName::from ("owned2") ) ]);
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_repos_test (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true, &fork_repos ) . await ?;
  assert! ( response . errors . is_empty (),
    "the user-set fork must commit: {:?}", response . errors );
  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . home_repo, RepoName::from ("owned2"),
    "the user-set repo 'owned2' must override the inferred 'owned'; \
     got {:?}", c . home_repo );
  Ok (( )) }

/// TODO/fork-fixes.org Case 2: appending a new child that EXPLICITLY
/// names an owned repo specifies the clone's repo. The spec must
/// resolve to that repo, CONFIRMED, and the confirmation buffer must
/// show it as settled -- no PICK-A-REPO, no suggestion comment.
async fn explicit_new_child_repo_confirms_clone_repo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg (node (id N) (repo foreign))) N-original
    ** (skg (node (id N1) (repo foreign) writeProtected)) N1
    ** (skg (node (id N2) (repo foreign) writeProtected)) N2
    ** (skg (node (repo owned2))) Can I add to this?
    "};
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    buffer, config, None )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1 );
  let spec : &ForkSpec = & save_plan . fork_specs[0];
  assert_eq! ( spec . clone . 0 . home_repo, RepoName::from ("owned2"),
    "the clone's repo must be the new child's explicit repo" );
  assert! ( spec . repo_confirmed,
    "an explicitly-specified repo must be confirmed" );
  let confirmation : String =
    skg::from_text::fork::build_fork_confirmation_buffer (
      & save_plan . fork_specs );
  assert! ( confirmation . contains ("(repo owned2)"),
    "the buffer must show the specified repo as settled:\n{}",
    confirmation );
  assert! ( ! confirmation . contains ("(repo PICK-A-REPO)"),
    // (The instructions body may MENTION the placeholder; only the
    // metadata form matters.)
    "no placeholder repo when the repo was specified:\n{}",
    confirmation );
  Ok (( )) }

/// New children naming DIFFERENT owned repos are ambiguous: the
/// clone's repo falls back to inference/default, UNCONFIRMED, so
/// the flow asks.
async fn disagreeing_new_child_repos_leave_clone_repo_unconfirmed (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg (node (id N) (repo foreign))) N-original
    ** (skg (node (repo owned))) New thing one
    ** (skg (node (repo owned2))) New thing two
    "};
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    buffer, config, None )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1 );
  assert! ( ! save_plan . fork_specs[0] . repo_confirmed,
    "disagreeing explicit child repos must not confirm a clone repo" );
  Ok (( )) }

/// The confirmation stage: a save that finds forks but is NOT approved
/// returns a fork-confirmation buffer and commits NOTHING; re-issuing
/// the save approved then commits.
async fn fork_confirmation_gates_commit (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };

  // Unapproved save -> fork-confirmation, nothing committed.
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ false ) . await ?;
  assert! ( response . fork_confirmation . is_some (),
    "an unapproved save with forks must return a fork-confirmation" );
  assert! ( response . saved_view . contains ("* Fork confirmation")
            && response . saved_view . contains ("(id N)"),
    "the confirmation buffer must list N:\n{}", response . saved_view );
  assert! ( clone_on_disk (config) . is_err (),
    "no clone may be committed before approval" );
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original",
    "N must be untouched before approval" );

  // Approved re-issue -> commits the clone.
  let mut stream2 : std::net::TcpStream = mk_test_tcp_stream ();
  let response2 = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream2, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response2 . fork_confirmation . is_none (),
    "an approved save commits and returns a normal save-result" );
  assert! ( clone_on_disk (config) . is_ok (),
    "the clone must be committed after approval" );
  Ok (( )) }

/// TODO/fork-fixes.org Case 1, at the plan level: appending a bare
/// (metadata-less) new headline under a foreign root forks the root
/// rather than dying with "Cannot create node in foreign repo". The
/// SavePlan must hold one ForkSpec for N, whose clone's contains end
/// with the new node's minted id, and a kept Save instruction for the
/// new node REWRITTEN into the clone's repo.
async fn fork_from_bare_new_child_plan (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let ( _vf, save_plan, _warnings ) = buffer_to_validated_saveplan (
    FORK_WITH_BARE_NEW_CHILD_BUFFER, config, None )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "appending a bare new child must fork N: {:?}",
    save_plan . fork_specs );
  let clone : &NodeComplete = & save_plan . fork_specs[0] . clone . 0;
  assert_eq! ( save_plan . fork_specs[0] . original_id, ID::from ("N") );
  assert_eq! ( clone . contains . len (), 3,
    "the clone's contains must be N1, N2 and the new node: {:?}",
    clone . contains );
  assert_eq! ( members_of (& clone . contains [..2]),
               vec! [ ID::from ("N1"), ID::from ("N2") ] );
  let new_node : &NodeComplete =
    save_plan . define_nodes . iter ()
    . find_map ( |dn| match dn {
        DefineNode::Save ( SaveNode (n) )
          if n . title == "Can I add to this?" => Some (n),
        _ => None } )
    . expect ("the new node must survive as a Save instruction");
  assert_eq! ( new_node . home_repo, clone . home_repo,
    "the new node must adopt the clone's repo" );
  assert_eq! ( new_node . pid, clone . contains [2] . member,
    "the clone's last child must be the new node" );
  Ok (( )) }

async fn fork_new_parent_adopts_relationship_repos (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let ( _vf, save_plan, _warnings ) = buffer_to_validated_saveplan (
    FORK_WITH_NEW_PARENT_FOR_OLD_CHILD_BUFFER, config, None ) ?;
  let clone_repo : RepoName =
    save_plan . fork_specs . first ()
    . expect ("structural edit must fork N")
    . clone . 0 . home_repo . clone ();
  let new_parent : &NodeComplete =
    save_plan . define_nodes . iter ()
    . find_map ( |dn| match dn {
        DefineNode::Save (SaveNode (n)) if n . title == "New parent" => Some (n),
        _ => None } )
    . expect ("the new parent must survive as a Save instruction");
  assert_eq! (new_parent . home_repo, clone_repo,
    "the new parent must adopt the clone's repo");
  assert_eq! (members_of (&new_parent . contains), vec![ID::from ("N1")]);
  assert! (new_parent . contains . iter ()
           . all (|member| member . relRepo == clone_repo),
    "the new parent's inherited relationships must adopt the clone repo: {:?}",
    new_parent . contains);
  Ok (( )) }

/// TODO/fork-fixes.org Case 1, committed: the approved save creates
/// the clone AND the new node, both in the owned repo; N's foreign
/// .skg is untouched.
async fn fork_from_bare_new_child_commits (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_WITH_BARE_NEW_CHILD_BUFFER, config, tantivy,
    &graph, false, &Err ( String::new () ), &mut views_state,
    /* fork_approved = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved bare-new-child fork must commit: {:?}",
    response . errors );
  let clone : NodeComplete = clone_on_disk (config) ?;
  let new_node : NodeComplete =
    read_all_skg_files_from_repos (config) ?
    . into_iter ()
    . find ( |node| node . title == "Can I add to this?" )
    . ok_or ("the new node must be on disk") ?;
  assert_eq! ( new_node . home_repo, clone . home_repo,
    "the new node must land in the clone's repo" );
  assert_eq! ( clone . home_repo, RepoName::from ("owned") );
  assert! ( clone . contains . iter () . any ( |m| m . member == new_node . pid ),
    "the clone must contain the new node: {:?}", clone . contains );
  assert_eq! ( members_of (& node_from_disk (config, "N") ? . contains),
               vec! [ ID::from ("N1"), ID::from ("N2") ],
    "N's own contains must be untouched" );
  Ok (( )) }

/// An EXPLICITLY foreign-repo new node stays a rejection: only an
/// inherited (guessed) foreign repo rides the fork.
async fn explicitly_foreign_new_child_still_rejected (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let result = buffer_to_validated_saveplan (
    FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER, config, None )
    ;
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
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  let c1 : ID = clone_on_disk (config) ? . pid;

  // Save planning reloads the fixture graph and therefore sees the new clone.
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, None ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkAlreadyExists (orig, existing)
          if *orig == ID::from ("N") && *existing == c1 )),
        "expected ForkAlreadyExists(N, {}), got {:?}", c1 . 0, errors ); }
    other => panic! (
      "expected ForkAlreadyExists rejecting the re-fork, got {:?}", other ), }
  Ok (( )) }

/// Repo-set: a fork whose resolved owned repo is inactive under the
/// active repo-set is forbidden ('ForkRepoInactive'), so an
/// invisible clone is never created silently. Here the active set is
/// {foreign} only, so the clone's inferred repo "owned" is inactive.
async fn fork_repo_inactive (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  // Save planning reloads the freshly reset fixture graph.
  let active : ActiveRepoSet = ActiveRepoSet {
    name    : RepoSetName ("only-foreign" . to_string ()),
    repos : BTreeSet::from ([ RepoName::from ("foreign") ]), };
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, Some (&active) ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkRepoInactive (orig, repo)
          if *orig == ID::from ("N")
             && *repo == RepoName::from ("owned") )),
        "expected ForkRepoInactive(N, owned), got {:?}", errors ); }
    other => panic! (
      "expected ForkRepoInactive, got {:?}", other ), }
  Ok (( )) }

/// The SavePlan a foreign edit produces carries one ForkSpec whose
/// clone copies N's title/body/contains (SHALLOW -- the child IDs only,
/// no descendants), subscribes_to=[N], overrides_view_of=[N], and no
/// hides; in an OWNED repo inferred from the owned ancestor P.
async fn fork_save_instruction (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (FORK_BUFFER, config) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one fork (of N): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_id, ID::from ("N") );
  let c : &NodeComplete = &spec . clone . 0;
  assert_eq! ( c . title, "N-edited",
    "clone copies the edited title" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N1"), ID::from ("N2") ],
    "clone copies N's child IDs shallow (not descendants)" );
  assert_eq! ( members_of ( c . subscribes_to . or_default () ), vec! [ ID::from ("N") ],
    "clone subscribes to N" );
  assert_eq! ( members_of ( c . overrides_view_of . or_default () ), vec! [ ID::from ("N") ],
    "clone overrides N" );
  assert! ( c . hides_from_its_subscriptions . or_default () . is_empty (),
    "clone records no hides" );
  assert! ( config . user_owns_repo (& c . home_repo),
    "clone lives in an owned repo, got {:?}", c . home_repo );
  assert_eq! ( c . home_repo, RepoName::from ("owned"),
    "clone's repo is inferred from the owned ancestor P" );
  assert_ne! ( c . pid, ID::from ("N"),
    "clone has a fresh pid, not N's" );
  Ok (( )) }

/// After saving the fork: a clone .skg appears in the OWNED repo with
/// the four fields, and N's foreign .skg is byte-unchanged.
async fn fork_fixture_files (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let n_path : std::path::PathBuf =
    config . repos . get (& RepoName::from ("foreign")) . unwrap ()
    . path . join ("N.skg");
  let n_before : String = std::fs::read_to_string (&n_path) ?;

  // The save and its coherence check share this fixture-local handle.
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "fork save must not error: {:?}", response . errors );

  let c : NodeComplete = clone_on_disk (config) ?;
  assert_eq! ( c . title, "N-edited" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N1"), ID::from ("N2") ] );
  assert_eq! ( members_of ( c . subscribes_to . or_default () ), vec! [ ID::from ("N") ] );
  assert_eq! ( c . home_repo, RepoName::from ("owned") );

  let n_after : String = std::fs::read_to_string (&n_path) ?;
  assert_eq! ( n_before, n_after,
    "N's foreign .skg must be byte-unchanged by the fork" );
  // And the in-memory disk node still says N-original.
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original" );
  Ok (( )) }

/// Round-trip: after the fork, reopening P draws the clone in N's
/// place, carrying (overridesHere N), with the default subscribeeFolder
/// listing N but no unrequested overriddenFolder. The load-bearing property:
/// P's stored
/// 'contains' is NOT rewritten to the clone -- it still lists N, so the
/// marker round-trips and a save never silently re-points containers at
/// the clone. (That the clone's subscribee-as-such view of N starts
/// empty and fills as N gains children is the prerequisite rule,
/// exercised in hidden_from_subscriptions; here C.contains ==
/// N.contains == [N1,N2] gives it nothing to show.)
async fn fork_round_trip (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, false,
    &Err ( String::new () ), &mut views_state ) . await ?;

  let clone_id : ID = clone_on_disk (config) ? . pid;

  // Reopen P de-novo. N is P's content; override substitution draws the
  // clone in its place with (overridesHere N).
  let (p_view, _pids, _) =
    single_root_view (
      config, Some (tantivy), &ID::from ("P"), false ) ?;
  assert! ( p_view . contains ("(overridesHere N)"),
    "the clone must be drawn in N's place with (overridesHere N):\n{}",
    p_view );
  assert! ( p_view . contains (& format! ("(id {})", clone_id . 0)),
    "the drawn substitute must be the clone {}:\n{}", clone_id . 0, p_view );
  assert! ( p_view . contains ("subscribeeFolder"),
    "the clone (a subscriber of N) shows a subscribeeFolder:\n{}", p_view );
  assert! ( ! p_view . contains ("overriddenFolder"),
    "the clone's overriddenFolder should require an explicit request:\n{}",
    p_view );

  // The load-bearing round-trip: P's stored contains was NOT rewritten
  // to the clone; it still lists N (the marker collected N, not C).
  let p_disk : NodeComplete = node_from_disk (config, "P") ?;
  assert_eq! ( members_of (& p_disk . contains), vec! [ ID::from ("N") ],
    "P's contains must still point at N, not the clone {}", clone_id . 0 );
  Ok (( )) }

/// An ordinary collateral rerender retains a raw original while refreshing
/// its relationship heralds. Editing P as well as forking N makes the second
/// P view collateral through the normal changed-PID path; no fork-specific
/// invalidation or rendering path participates.
async fn fork_collateral_rerender_without_substitution (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let ( _text, pids, p_view ) = single_root_view (
    config, Some (tantivy), &ID::from ("P"), false ) ?;
  let saved_uri : ViewUri = ViewUri::ContentView ("saved-P" . to_string ());
  let collateral_uri : ViewUri = ViewUri::SearchView (
    "P before fork" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    diff_mode_enabled : false, open_views : OpenViews::new () };
  let graph_snap = graph . load_full ();
  views_state . open_views . register_view (
    &graph_snap, saved_uri . clone (), p_view . clone (), &pids );
  views_state . open_views . register_view (
    &graph_snap, collateral_uri . clone (), p_view, &pids );

  let mut stream : std::net::TcpStream = mk_test_tcp_stream ();
  let buffer : String = FORK_BUFFER . replace ("P-container", "P-edited");
  let response = update_from_and_rerender_buffer (
    &mut stream, &buffer, config, tantivy, &graph, false,
    &Ok (saved_uri), &mut views_state ) . await ?;
  assert! ( response . saved_view . contains ("(overridesHere N)"),
    "the saved view must substitute the clone:\n{}", response . saved_view );

  let collateral = views_state . open_views
    . viewuri_to_view (&collateral_uri)
    .ok_or ("the collateral view must remain open") ?;
  let collateral_text : String = viewforest_to_string (collateral, config) ?;
  assert! ( collateral_text . contains ("(id N)"),
    "the collateral view must retain raw N:\n{}", collateral_text );
  assert! ( ! collateral_text . contains ("(overridesHere N)"),
    "the collateral view must not substitute the clone:\n{}", collateral_text );
  assert! ( collateral_text . contains ("(subscribes_to (in 1))")
            && collateral_text . contains ("(overrides_view_of (in 1))"),
    "the raw original must show its new inbound relationship heralds:\n{}",
    collateral_text );
  Ok (( )) }
