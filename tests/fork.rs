// cargo nextest run --test grouped_forks -E 'test(fork::)'
//
// The fork/clone feature: editing a foreign node N (write-protected, in a
// skgrepo the user does not own) is read as a request to clone it. The
// clone C lives in an owned skgrepo, copies N's edited title/body/
// contains, and forks N; N itself is untouched.
//
// Installs the explicit graph handle (mirror substitution and
// forksFolder content read it), so it belongs among the
// grouped_forks installers.

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use crate::view_and_save_helpers::node_from_disk;
use std::error::Error;
use std::net::TcpStream;

use indoc::indoc;

use std::collections::HashMap;
use std::iter::once;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::from_text::buffer_to_validated_saveplan;
use skg::from_text::buffer_to_validated_saveplan_with_fork_skgrepos;
use skg::org_to_text::viewforest_to_string;
use skg::serve::ViewsState;
use skg::skgrepo_sets::SkgrepoRestriction;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_approval_test;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_skgrepos_test;
use skg::test_utils::single_root_view;
use skg::types::errors::{BufferValidationError, SaveError};
use skg::types::misc::{ID, SkgConfig, SkgrepoName, TantivyIndex, members_of};
use skg::types::nodes::complete::Graphnode;
use skg::types::save::{NodeInstruction, ForkSpec, SaveNode};
use skg::types::views_state::{OpenViews, ViewforestId};

/// A foreign node N (title "N-original", contains [N1, N2]) lives under
/// an owned container P. The buffer makes N editable and edits its
/// title to "N-edited" -- a real change, so saving forks N. (N's
/// children stay write-protected foreign content.)
const FORK_BUFFER : &str = indoc! {"
  * (skg node (gnid P) (skgRepos (nodeRepo owned))) P-container
  ** (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-edited
  *** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  *** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  "};

/// A foreign node N opened as a bare root -- no owned ancestor to infer
/// a clone skgrepo from. Editing its title forks it; the clone's skgrepo
/// must then default to the user's first owned skgrepo.
const FORK_ROOT_BUFFER : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-edited
  ** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  ** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  "};

/// ('docs/forks.org', "Adding new content under a foreign node"): a bare
/// new headline (no metadata at all) appended under a foreign root.
/// The parse mints it an id and inherits the foreign skgrepo; that must
/// not be a foreign-creation error -- appending it edits N's contains,
/// which forks N, and the new node rides that fork, adopting the clone's
/// skgrepo.
const FORK_WITH_BARE_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-original
  ** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  ** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  ** Can I add to this?
  "};

/// The structural fork gesture: insert a bare new parent under foreign N,
/// then move old content N1 beneath it. The new node and its inherited
/// `contains N1` relationship must both adopt the clone's owned skgrepo.
const FORK_WITH_NEW_PARENT_FOR_OLD_CHILD_BUFFER : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-original
  ** New parent
  *** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  ** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  "};

/// Same shape, but the new node explicitly claims the foreign skgrepo:
/// a deliberate attempt to create a node in a write-protected skgrepo, which
/// must stay rejected.
const FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER : &str = indoc! {"
  * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-original
  ** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  ** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  ** (skg node (skgRepos (nodeRepo foreign))) Can I add to this?
  "};

/// The explicit 'skg-fork-node' gesture: an owned node (P) carries
/// (viewRequests fork). Unlike the implicit foreign fork, P keeps its
/// own save; saving adds a clone C that forks P, built from P's disk
/// snapshot.
const EXPLICIT_FORK_BUFFER : &str = indoc! {"
  * (skg node (gnid P) (skgRepos (nodeRepo owned)) (viewRequests fork)) P-container
  ** (skg node (gnid N) (skgRepos (nodeRepo foreign)) writeProtected) N-original
  "};

/// An explicit fork request on a brand-new (id-less) headline: the parse
/// mints a fresh pid that is not in the graph, so the fork is rejected.
const EXPLICIT_FORK_UNKNOWN_BUFFER : &str = indoc! {"
  * (skg node (skgRepos (nodeRepo owned)) (viewRequests fork)) Brand New
  "};

fn clone_forking_on_disk (
  config : &SkgConfig,
  target : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . find ( |node| node . forks . or_default () . iter ()
             . any ( |m| m . member == ID::from (target) ) )
    . ok_or_else ( || format! (
        "no clone (forks [{}]) on disk", target ) . into () ) }

async fn fork_specs_from (
  buffer : &str,
  config : &SkgConfig,
) -> Result<Vec<ForkSpec>, Box<dyn Error>> {
  Ok ( buffer_to_validated_saveplan ( buffer, config, &SkgrepoRestriction::unrestricted (config) )
        ? . 1 . fork_specs ) }

/// The single clone produced by saving the FORK_BUFFER -- the owned
/// node whose forks names N. (Its pid is a fresh uuid we
/// do not know in advance.)
fn clone_on_disk (
  config : &SkgConfig,
) -> Result<Graphnode, Box<dyn Error>> {
  read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . find ( |node|
      node . forks . or_default () . iter ()
        . any ( |m| m . member == ID::from ("N") ) )
    . ok_or_else ( || "no clone (forks [N]) on disk" . into () ) }

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
      s . reset ("fork_collateral_rerender_substitutes_too", fixtures) ?;
      fork_collateral_rerender_substitutes_too (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("forking_a_note_one_already_forks_is_refused", fixtures) ?;
      forking_a_note_one_already_forks_is_refused (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_repo_restricted", fixtures) ?;
      fork_skgrepo_restricted (
        &s . config ) . await ?;
      s . reset ("fork_confirmation_gates_skgsave_commit", fixtures) ?;
      fork_confirmation_gates_skgsave_commit (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_from_bare_new_child_plan", fixtures) ?;
      fork_from_bare_new_child_plan (
        &s . config ) . await ?;
      s . reset ("fork_new_parent_adopts_relationship_repos", fixtures) ?;
      fork_new_parent_adopts_relationship_skgrepos (
        &s . config ) . await ?;
      s . reset ("fork_from_bare_new_child_skgsave_commits", fixtures) ?;
      fork_from_bare_new_child_skgsave_commits (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("explicitly_foreign_new_child_still_rejected", fixtures) ?;
      explicitly_foreign_new_child_still_rejected (
        &s . config ) . await ?;
      let two_owned : &str = "tests/fork/fixtures-two-owned";
      s . reset ("fork_no_owned_ancestor_defaults", two_owned) ?;
      fork_no_owned_ancestor_defaults (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_user_set_skgrepo_wins", two_owned) ?;
      fork_user_set_skgrepo_wins (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("fork_default_prefers_unrestricted_owned_repo", two_owned) ?;
      fork_default_prefers_unrestricted_owned_skgrepo (
        &s . config ) . await ?;
      s . reset ("fork_user_set_repo_not_owned_rejected", two_owned) ?;
      fork_user_set_skgrepo_not_owned_rejected (
        &s . config ) . await ?;
      s . reset ("explicit_new_child_repo_confirms_clone_repo", two_owned) ?;
      explicit_new_child_skgrepo_confirms_clone_skgrepo (
        &s . config ) . await ?;
      s . reset ("disagreeing_new_child_repos_leave_clone_repo_unconfirmed", two_owned) ?;
      disagreeing_new_child_skgrepos_leave_clone_skgrepo_unconfirmed (
        &s . config ) . await ?;
      s . reset ("clone_skgrepo_inferred_from_a_moved_writeProtected_ancestor", two_owned) ?;
      clone_skgrepo_inferred_from_a_moved_writeProtected_ancestor (
        &s . config ) . await ?;
      s . reset ("explicit_fork_save_instruction", fixtures) ?;
      explicit_fork_save_instruction (
        &s . config ) . await ?;
      s . reset ("explicit_fork_on_unknown_node_errors", fixtures) ?;
      explicit_fork_on_unknown_node_errors (
        &s . config ) . await ?;
      s . reset ("explicit_fork_round_trip_and_refork_refusal", fixtures) ?;
      explicit_fork_round_trip_and_refork_refusal (
        &s . config, &mut s . tantivy ) . await ?;
      let mail : &str = "tests/fork/fixtures-mail";
      s . reset ("a_reply_under_an_unread_message_integrates_it", mail) ?;
      a_reply_under_an_unread_message_integrates_it (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_reply_deeper_in_an_unread_message_integrates_the_message", mail) ?;
      a_reply_deeper_in_an_unread_message_integrates_the_message (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_reply_under_ones_own_unfiled_fork_files_the_original", mail) ?;
      a_reply_under_ones_own_unfiled_fork_files_the_original (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_headline_typed_under_the_foreign_forkee_itself_is_refused", mail) ?;
      a_headline_typed_under_the_foreign_forkee_itself_is_refused (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_third_partys_reply_in_anothers_unread_list_warns",
                 "tests/fork/fixtures-mail-third-party") ?;
      a_third_partys_reply_in_anothers_unread_list_warns (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

/// The buffer with a headline 'title' typed as the first child of the
/// first line naming 'gnid'.
fn with_child_typed_under (
  buffer : &str,
  gnid   : &str,
  title  : &str,
) -> String {
  let needle : String = format! ("(gnid {})", gnid);
  let mut typed : bool = false;
  let mut lines : Vec<String> = Vec::new ();
  for line in buffer . lines () {
    lines . push (line . to_string ());
    if line . contains (&needle) && ! typed {
      typed = true;
      let depth : usize = line . chars () . take_while ( |c| *c == '*' ) . count ();
      lines . push ( format! ("{} {}", "*" . repeat (depth + 1), title )); }}
  lines . join ("\n") + "\n" }

/// The line of 'view' naming 'gnid', and the lines under it.
fn subtree_of<'a> (
  view : &'a str,
  gnid : &str,
) -> Vec<&'a str> {
  let needle : String = format! ("(gnid {})", gnid);
  let lines : Vec<&str> = view . lines () . collect ();
  let Some (start) : Option<usize> =
    lines . iter () . position ( |line| line . contains (&needle) )
    else { return Vec::new (); };
  let depth_of = |line : &str| -> usize {
    line . chars () . take_while ( |c| *c == '*' ) . count () };
  let depth : usize = depth_of (lines [start]);
  once (lines [start])
    . chain ( lines [start + 1 ..] . iter () . copied ()
              . take_while ( |line| depth_of (line) == 0 || depth_of (line) > depth ))
    . collect () }

/// The reader's view of 'f', its fork of the sender's 't', with 't'
/// drawn editable in the forks folder, as a fresh view draws a forkee
/// as such: the unread list. Returned as the saved view of an
/// unchanged save of it.
async fn reader_view_with_unread_list (
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  view_id     : &ViewforestId,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let ( f_view, pids, f_tree ) = single_root_view (
    config, Some (tantivy), &ID::from ("f") ) ?;
  views_state . open_views . register_view (
    &graph . load_full (), view_id . clone (), f_tree, &pids,
    &SkgrepoRestriction::unrestricted (config) );
  assert! ( f_view . lines () . any ( |line|
              line . contains ("(gnid t) (skgRepos (nodeRepo sender)) (view") ),
    "the fresh view must draw t editable:\n{}", f_view );
  let editable : String = f_view . clone ();
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &editable, config, tantivy, graph,
    &Ok (view_id . clone ()), views_state ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let unread : Vec<&str> = subtree_of (&response . saved_view, "t");
  assert! ( unread . iter () . any ( |line| line . contains ("(gnid m1)") )
            && unread . iter () . any ( |line|
                 line . contains ("(gnid m2r)") && line . contains ("(inPlaceOf m2)") )
            && first_says (&unread, "(unintegrated 2)"),
    "t, drawn editable, must list both messages as unintegrated, m2 as \
     the reader's earlier fork of it:\n{}",
    response . saved_view );
  Ok ( response . saved_view ) }

/// Whether the first of 'lines' contains 'text'.
fn first_says (
  lines : &[&str],
  text  : &str,
) -> bool {
  lines . first () . is_some_and ( |line| line . contains (text) ) }

/// The headlines of depth 2 in 'view': the content and folders of
/// its one root.
fn second_level (
  view : &str,
) -> Vec<&str> {
  view . lines () . filter ( |line| line . starts_with ("** ") ) . collect () }

/// Mail between two skgrepos: the sender's note 't' holds messages
/// 'm1' and 'm2'. The reader's 'f' forks 't' (and contains
/// only 'maybe done'), so 't' stands in f's
/// forks folder listing the messages f has neither filed nor
/// hidden: the reader's unread list. The reader types a reply under
/// 'm1' there, which forks 'm1', and saves, approving the fork.
/// The redrawn view must show the reply, under the reader's clone of
/// 'm1', and 'm1' must no longer count as unread: the save files it
/// in 'f', whose contains records the original, as every contains
/// list does, and draws the clone in its place.
async fn a_reply_under_an_unread_message_integrates_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let view_id : ViewforestId = ViewforestId::ContentView ("content-view:f" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let unread_shown : String = reader_view_with_unread_list (
    config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
  let replied : String = with_child_typed_under (&unread_shown, "m1", "my reply");
  { // The first round asks to approve the fork, and writes nothing.
    let (mut stream, _read_end) : (TcpStream, TcpStream) =
      mk_test_tcp_stream_pair ();
    let asked = update_from_and_rerender_buffer_with_fork_approval_test (
      &mut stream, &replied, config, tantivy, &graph,
      &Ok (view_id . clone ()), &mut views_state,
      /* approved_forks = */ false ) . await ?;
    assert! ( asked . fork_confirmation . is_some (),
      "the reply forks m1, which asks first: {:?}", asked . errors );
    assert_eq! ( members_of (& node_from_disk (config, "f") ? . contains),
                 vec! [ ID::from ("d") ], "an unapproved round files nothing" ); }
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &replied, config, tantivy, &graph,
    &Ok (view_id . clone ()), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert! ( response . warnings . is_empty (),
    "the reader's own view shows the fork, so nothing warns: {:?}", response . warnings );
  let clone : Graphnode = clone_forking_on_disk (config, "m1") ?;
  assert_eq! ( clone . home_skgrepo, SkgrepoName::from ("reader") );
  let f_disk : Graphnode = node_from_disk (config, "f") ?;
  assert_eq! ( members_of (& f_disk . contains),
               vec! [ ID::from ("d"), ID::from ("m1") ],
    "the reply must file m1 in f, by its original's ID" );
  assert! ( f_disk . hides . or_default () . is_empty (),
    "filing m1 hides nothing: {:?}", f_disk . hides );
  let redrawn : &str = &response . saved_view;
  assert! ( second_level (redrawn) . last () . is_some_and ( |line|
              line . contains (& format! ("(gnid {})", clone . pid . 0))
              && line . contains ("(inPlaceOf m1)")
              && ! line . contains ("writeProtected") ),
    "the redrawn view must end f's content with the clone, editable, \
     in m1's place:\n{}", redrawn );
  assert! ( subtree_of (redrawn, & clone . pid . 0) . iter ()
              . any ( |line| line . ends_with (" my reply") ),
    "the redrawn view must show the reply under the clone:\n{}", redrawn );
  let unread : Vec<&str> = subtree_of (redrawn, "t");
  let kept_elsewhere : Vec<&str> = subtree_of (redrawn, "m1");
  assert! ( first_says (&unread, "(unintegrated 1)")
            && unread . iter () . any ( |line| line . contains ("(inPlaceOf m2)") )
            && unread . iter () . filter ( |line| line . contains ("m1") ) . count () == 1
            && unread . iter () . any ( |line| line . contains ("integratedElsewhereFolder") )
            && first_says (&kept_elsewhere, "folded"),
    "the redrawn view must count one message unread, m2, and draw m1 there \
     only in the folder of what the user keeps elsewhere:\n{}",
    redrawn );
  // A fresh view draws t editable too, its count saying one message
  // is unread.
  let (fresh, _pids, _tree) = single_root_view (config, Some (tantivy), &ID::from ("f")) ?;
  assert! ( subtree_of (&fresh, & clone . pid . 0) . iter ()
              . any ( |line| line . ends_with (" my reply") )
            && first_says (&subtree_of (&fresh, "t"), "(unintegrated 1)"),
    "a fresh view must show the reply under the clone, and one message unread:\n{}",
    fresh );
  Ok (( )) }

/// The reader had forked 'm2' before, into 'm2r', without filing it,
/// so the unread list draws 'm2r' in m2's place. A reply typed under
/// it forks nothing ('m2r' is the reader's own) and files 'm2'.
async fn a_reply_under_ones_own_unfiled_fork_files_the_original (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let view_id : ViewforestId = ViewforestId::ContentView ("content-view:f" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let unread_shown : String = reader_view_with_unread_list (
    config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
  let replied : String = with_child_typed_under (&unread_shown, "m2r", "a later reply");
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, &replied, config, tantivy, &graph,
    &Ok (view_id . clone ()), &mut views_state,
    /* approved_forks = */ false ) . await ?;
  assert! ( response . errors . is_empty () && response . fork_confirmation . is_none (),
    "nothing forks, so nothing asks: {:?}", response . errors );
  assert_eq! ( members_of (& node_from_disk (config, "f") ? . contains),
               vec! [ ID::from ("d"), ID::from ("m2") ],
    "the reply must file m2, the original m2r stands for" );
  assert! ( second_level (&response . saved_view) . last () . is_some_and ( |line|
              line . contains ("(gnid m2r)") && line . contains ("(inPlaceOf m2)") )
            && subtree_of (&response . saved_view, "m2r") . iter ()
                 . any ( |line| line . ends_with (" a later reply") )
            && first_says (&subtree_of (&response . saved_view, "t"), "(unintegrated 1)"),
    "the redrawn view must end f's content with m2r and the reply, one message unread:\n{}",
    response . saved_view );
  Ok (( )) }

/// A headline typed straight under 't' in the unread list would join
/// t's content, but 't' is the sender's: the save is refused, saying
/// to make the node one's own first, and nothing is written, nor filed
/// in 'f'.
async fn a_headline_typed_under_the_foreign_forkee_itself_is_refused (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let view_id : ViewforestId = ViewforestId::ContentView ("content-view:f" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let unread_shown : String = reader_view_with_unread_list (
    config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
  let typed : String = with_child_typed_under (
    &unread_shown, "t", "(skg node (skgRepos (nodeRepo reader))) not a message" );
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let refusal : String = match update_from_and_rerender_buffer (
    &mut stream, &typed, config, tantivy, &graph,
    &Ok (view_id . clone ()), &mut views_state ) . await {
    Ok (response) => format! ("{:?}", response . errors),
    Err (error) => error . to_string (), };
  assert! ( refusal . contains ("Cannot add a child under t")
            && refusal . contains ("foreign node shown as a forkee"),
    "the save must be refused: {}", refusal );
  assert_eq! ( members_of (& node_from_disk (config, "f") ? . contains),
               vec! [ ID::from ("d") ],
    "nothing is filed" );
  assert_eq! ( members_of (& node_from_disk (config, "t") ? . contains),
               vec! [ ID::from ("m1"), ID::from ("m2") ],
    "t is as it was" );
  Ok (( )) }

/// As above, but the reader answers 'q1', a question inside 'm1':
/// the reply forks 'q1', and the message it stands in, 'm1', is the
/// one filed.
async fn a_reply_deeper_in_an_unread_message_integrates_the_message (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let view_id : ViewforestId = ViewforestId::ContentView ("content-view:f" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let unread_shown : String = reader_view_with_unread_list (
    config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
  let replied : String = with_child_typed_under (&unread_shown, "q1", "my answer");
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &replied, config, tantivy, &graph,
    &Ok (view_id . clone ()), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  let clone : Graphnode = clone_forking_on_disk (config, "q1") ?;
  assert_eq! ( members_of (& node_from_disk (config, "f") ? . contains),
               vec! [ ID::from ("d"), ID::from ("m1") ],
    "the answer must file m1, the message it is in, in f" );
  let from_fs_content : String = // from m1 as f's content on, past t's folder of what the user keeps elsewhere
    response . saved_view . lines ()
    . skip_while ( |line| ! ( line . starts_with ("** ") && line . contains ("(gnid m1)") ))
    . map ( |line| format! ("{}\n", line) )
    . collect ();
  let shown : Vec<&str> = subtree_of (&from_fs_content, "m1");
  assert! ( shown . iter () . any ( |line| line . contains (& format! ("(gnid {})", clone . pid . 0)) )
            && shown . iter () . any ( |line| line . ends_with (" my answer") ),
    "the redrawn view must show m1, filed, with the clone of q1 and the answer in it:\n{}",
    response . saved_view );
  assert! ( first_says (&subtree_of (&response . saved_view, "t"), "(unintegrated 1)"),
    "one message must remain unread:\n{}", response . saved_view );
  Ok (( )) }

/// The same mail, read by a third party, who owns neither 't' nor
/// 'f': its view of 'f' is drawn from the perspective of 'f''s home
/// skgrepo, 'reader', whose skgrepo list ranks the sender's skgrepo
/// (declared before the third party's) above the third party's. Its reply under
/// 'm1' forks 'm1' into its own skgrepo; the view goes on drawing the
/// original, without the reply, and 'f' cannot be written, so nothing
/// is filed. The save says so, and how to see the fork.
async fn a_third_partys_reply_in_anothers_unread_list_warns (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let view_id : ViewforestId = ViewforestId::ContentView ("content-view:f" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let unread_shown : String = reader_view_with_unread_list (
    config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
  let replied : String = with_child_typed_under (&unread_shown, "m1", "a third party's reply");
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &replied, config, tantivy, &graph,
    &Ok (view_id . clone ()), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (), "{:?}", response . errors );
  assert_eq! ( clone_forking_on_disk (config, "m1") ? . home_skgrepo,
               SkgrepoName::from ("third") );
  assert_eq! ( members_of (& node_from_disk (config, "f") ? . contains),
               vec! [ ID::from ("d") ], "f is foreign here: nothing is filed" );
  assert! ( subtree_of (&response . saved_view, "m1") . len () == 2
            && ! response . saved_view . contains ("a third party's reply"),
    "the view is drawn from the perspective of reader: it draws m1 raw, with its question only:\n{}",
    response . saved_view );
  assert! ( response . warnings . iter () . any ( |warning|
              warning . contains ("'message one' forked it into your skgrepo 'third'")
              && warning . contains ("from the perspective of reader") ),
    "the save must say where the reply went: {:?}", response . warnings );
  Ok (( )) }

/// The explicit fork of an owned node P: the SavePlan carries one
/// ForkSpec whose clone copies P's disk snapshot (title/contains),
/// forks=[P], in the first-declared owned
/// skgrepo. P itself keeps its own (owned) save -- it is not dropped like
/// a foreign fork's original.
async fn explicit_fork_save_instruction (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (EXPLICIT_FORK_BUFFER, config) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one explicit fork (of P): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_gnid, ID::from ("P") );
  let c : &Graphnode = &spec . clone . 0;
  assert_eq! ( c . title, "P-container",
    "clone copies P's disk title (not a buffer edit)" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N") ],
    "clone copies P's disk contains (shallow)" );
  assert_eq! ( members_of ( c . forks . or_default () ), vec! [ ID::from ("P") ],
    "clone forks P" );
  assert_eq! ( c . home_skgrepo, SkgrepoName::from ("owned"),
    "clone defaults to the first-declared owned repo; got {:?}",
    c . home_skgrepo );
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
    EXPLICIT_FORK_UNKNOWN_BUFFER, config, &SkgrepoRestriction::unrestricted (config) ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkRequestOnUnknownNode (_, _) )),
        "expected ForkRequestOnUnknownNode, got {:?}", errors ); }
    other => panic! (
      "expected ForkRequestOnUnknownNode, got {:?}", other ), }
  Ok (( )) }

/// The explicit fork skgsave-commits a clone forking the owned P (mirror
/// substitution then shows it in P's place), and a second explicit fork
/// of the now-forked P is rejected with 'ForkAlreadyExists'.
async fn explicit_fork_round_trip_and_refork_refusal (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, EXPLICIT_FORK_BUFFER, config, tantivy, &graph,
     &Err ( String::new () ), &mut views_state,
    /* approved_forks = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved explicit fork must skgsave-commit: {:?}", response . errors );
  let clone : Graphnode = clone_forking_on_disk (config, "P") ?;
  assert_eq! ( members_of ( clone . forks . or_default () ), vec! [ ID::from ("P") ],
    "the clone forks P" );
  assert! ( config . skgrepo_is_owned (& clone . home_skgrepo),
    "the clone lives in an owned repo" );
  // P is untouched (still owns its container role).
  assert_eq! ( members_of (& node_from_disk (config, "P") ? . contains),
               vec! [ ID::from ("N") ],
    "P keeps its own contains; the explicit fork does not rewrite it" );

  // Forking P again is rejected: the user already forks it.
  let result = buffer_to_validated_saveplan (
    EXPLICIT_FORK_BUFFER, config, &SkgrepoRestriction::unrestricted (config) ) ;
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

/// Under a restricted skgrepo-set, the clone-repo default must prefer an
/// owned skgrepo that is unrestricted. Here "owned" (alphabetically first owned)
/// is restricted and "owned2" is the only unrestricted owned skgrepo; a foreign
/// node with no owned ancestor must default to "owned2" and reach the
/// confirmation stage, not dead-end on ForkRepoRestricted.
async fn fork_default_prefers_unrestricted_owned_skgrepo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let restriction : SkgrepoRestriction = // shows owned2 and foreign
    SkgrepoRestriction::of_privacy_order_for_test (
      &["owned2", "foreign", "owned"], "foreign");
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    FORK_ROOT_BUFFER, config, &restriction )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "the fork must resolve, not dead-end on a restricted default repo" );
  assert_eq! ( save_plan . fork_specs[0] . clone . 0 . home_skgrepo,
               SkgrepoName::from ("owned2"),
    "with 'owned' restricted, the default must prefer the unrestricted owned 'owned2'; got {:?}",
    save_plan . fork_specs[0] . clone . 0 . home_skgrepo );
  Ok (( )) }

/// A user-set clone skgrepo that the user does not own (a configured but
/// foreign skgrepo, which the C-c s s prompt would let one type) is
/// rejected with ForkRepoNotOwned.
async fn fork_user_set_skgrepo_not_owned_rejected (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_skgrepos : HashMap<ID, SkgrepoName> =
    HashMap::from ([ ( ID::from ("N"), SkgrepoName::from ("foreign") ) ]);
  let result = buffer_to_validated_saveplan_with_fork_skgrepos (
    FORK_BUFFER, config, &SkgrepoRestriction::unrestricted (config), &fork_skgrepos ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkSkgrepoNotOwned (orig, skgrepo)
          if *orig == ID::from ("N")
             && *skgrepo == SkgrepoName::from ("foreign") )),
        "expected ForkRepoNotOwned(N, foreign), got {:?}", errors ); }
    other => panic! (
      "expected ForkRepoNotOwned rejecting a non-owned chosen repo, got {:?}",
      other ), }
  Ok (( )) }

/// A foreign node with no owned ancestor still forks: with no user-set
/// skgrepo and nothing to infer, the clone's skgrepo defaults to the
/// user's first owned skgrepo (alphabetically "owned", here). No more
/// 'ForkRepoUnresolved' hard-fail.
async fn fork_no_owned_ancestor_defaults (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_ROOT_BUFFER, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "a fork with no owned ancestor must complete: {:?}", response . errors );
  let c : Graphnode = clone_on_disk (config) ?;
  assert_eq! ( c . home_skgrepo, SkgrepoName::from ("owned"),
    "with no owned ancestor and no user-set repo, the clone defaults to \
     the first owned repo (alphabetically 'owned'); got {:?}", c . home_skgrepo );
  let root_line : &str = response . saved_view . lines () . next ()
    . ok_or ("the saved root view must not be empty") ?;
  assert! ( root_line . contains (&format! ("(gnid {})", c . pid . 0))
            && ! root_line . contains ("inPlaceOf"),
    "the buffer that forked root N must immediately be rooted at its clone, \
     which stands for nothing:\n{}",
    response . saved_view );
  assert! ( ! root_line . contains ("(gnid N)"),
    "the saved buffer must not leave the fork origin as its root:\n{}",
    response . saved_view );
  Ok (( )) }

/// The user-set clone skgrepo (the 'fork-repos' transport) wins
/// over the inferred one. FORK_BUFFER infers "owned" from the owned ancestor
/// P; passing N -> "owned2" lands the clone in "owned2" instead.
async fn fork_user_set_skgrepo_wins (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let fork_skgrepos : HashMap<ID, SkgrepoName> =
    HashMap::from ([ ( ID::from ("N"), SkgrepoName::from ("owned2") ) ]);
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_skgrepos_test (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, 
    &Err ( String::new () ), &mut views_state,
    /* approved_forks = */ true, &fork_skgrepos ) . await ?;
  assert! ( response . errors . is_empty (),
    "the user-set fork must skgsave-commit: {:?}", response . errors );
  let c : Graphnode = clone_on_disk (config) ?;
  assert_eq! ( c . home_skgrepo, SkgrepoName::from ("owned2"),
    "the user-set repo 'owned2' must win over the inferred 'owned'; \
     got {:?}", c . home_skgrepo );
  Ok (( )) }

/// ('docs/forks.org', "Adding new content under a foreign node"): appending
/// a new child that explicitly names an owned skgrepo specifies the clone's
/// skgrepo. The spec must resolve to that skgrepo, confirmed, and the
/// confirmation buffer must show it as settled -- no PICK-A-REPO, no
/// suggestion comment.
async fn explicit_new_child_skgrepo_confirms_clone_skgrepo (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-original
    ** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
    ** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
    ** (skg node (skgRepos (nodeRepo owned2))) Can I add to this?
    "};
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    buffer, config, &SkgrepoRestriction::unrestricted (config) )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1 );
  let spec : &ForkSpec = & save_plan . fork_specs[0];
  assert_eq! ( spec . clone . 0 . home_skgrepo, SkgrepoName::from ("owned2"),
    "the clone's repo must be the new child's explicit repo" );
  assert! ( spec . skgrepo_confirmed,
    "an explicitly-specified repo must be confirmed" );
  let confirmation : String =
    skg::from_text::fork::build_fork_confirmation_buffer (
      & save_plan . fork_specs );
  assert! ( confirmation . contains ("(skgRepos (nodeRepo owned2))"),
    "the buffer must show the specified repo as settled:\n{}",
    confirmation );
  assert! ( ! confirmation . contains ("(skgRepos (nodeRepo PICK-A-REPO))"),
    // (The instructions body may mention the placeholder; only the
    // metadata form matters.)
    "no placeholder repo when the repo was specified:\n{}",
    confirmation );
  Ok (( )) }

/// New children naming different owned skgrepos are ambiguous: the
/// clone's skgrepo falls back to inference/default, unconfirmed, so
/// the flow asks.
async fn disagreeing_new_child_skgrepos_leave_clone_skgrepo_unconfirmed (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-original
    ** (skg node (skgRepos (nodeRepo owned))) New thing one
    ** (skg node (skgRepos (nodeRepo owned2))) New thing two
    "};
  let ( _vf, save_plan, _w ) = buffer_to_validated_saveplan (
    buffer, config, &SkgrepoRestriction::unrestricted (config) )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1 );
  assert! ( ! save_plan . fork_specs[0] . skgrepo_confirmed,
    "disagreeing explicit child repos must not confirm a clone repo" );
  Ok (( )) }

/// N's nearest owned ancestor is a write-protected repeat of P, still
/// showing P's old repo 'owned', while P's editable occurrence moves it
/// to 'owned2' in the same save. The clone's inferred repo follows P's
/// ID to 'owned2'. (The repeat comes first: the first foreign
/// occurrence of N in preorder supplies the inference.)
async fn clone_skgrepo_inferred_from_a_moved_writeProtected_ancestor (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let buffer : &str = indoc! {"
    * (skg node (gnid P) (skgRepos (nodeRepo owned)) writeProtected) P-container
    ** (skg node (gnid N) (skgRepos (nodeRepo foreign)) noClaim) N-edited
    *** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
    *** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
    * (skg node (gnid P) (skgRepos (nodeRepo owned2))) P-container
    ** (skg node (gnid N) (skgRepos (nodeRepo foreign)) writeProtected) N-original
    "};
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (buffer, config) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one fork (of N): {:?}", fork_specs );
  assert_eq! ( fork_specs[0] . clone . 0 . home_skgrepo,
               SkgrepoName::from ("owned2"),
    "the clone's inferred repo must be P's after-save repo" );
  Ok (( )) }

/// The confirmation stage: a save that finds forks but is not approved
/// returns a fork-confirmation buffer and skgsave-commits nothing; re-issuing
/// the save approved then skgsave-commits.
async fn fork_confirmation_gates_skgsave_commit (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };

  // Unapproved save -> fork-confirmation, nothing skgsave-committed.
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_BUFFER, config, tantivy, &graph, 
    &Err ( String::new () ), &mut views_state,
    /* approved_forks = */ false ) . await ?;
  assert! ( response . fork_confirmation . is_some (),
    "an unapproved save with forks must return a fork-confirmation" );
  assert! ( response . saved_view . contains ("* Fork confirmation")
            && response . saved_view . contains ("(gnid N)"),
    "the confirmation buffer must list N:\n{}", response . saved_view );
  assert! ( clone_on_disk (config) . is_err (),
    "no clone may be skgsave-committed before approval" );
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original",
    "N must be untouched before approval" );

  // Approved re-issue -> skgsave-commits the clone.
  let (mut stream2, _read_end2) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response2 = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream2, FORK_BUFFER, config, tantivy, &graph, 
    &Err ( String::new () ), &mut views_state,
    /* approved_forks = */ true ) . await ?;
  assert! ( response2 . fork_confirmation . is_none (),
    "an approved save skgsave-commits and returns a normal save-result" );
  assert! ( clone_on_disk (config) . is_ok (),
    "the clone must be skgsave-committed after approval" );
  Ok (( )) }

/// ('docs/forks.org', "Adding new content under a foreign node"), at the
/// plan level: appending a bare (metadata-less) new headline under a
/// foreign root forks the root rather than dying with "Cannot create node
/// in foreign repo". The SavePlan must hold one ForkSpec for N, whose
/// clone's contains end with the new node's minted id, and a kept Save
/// nodeInstruction for the new node rewritten into the clone's skgrepo.
async fn fork_from_bare_new_child_plan (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let ( _vf, save_plan, _warnings ) = buffer_to_validated_saveplan (
    FORK_WITH_BARE_NEW_CHILD_BUFFER, config, &SkgrepoRestriction::unrestricted (config) )  ?;
  assert_eq! ( save_plan . fork_specs . len (), 1,
    "appending a bare new child must fork N: {:?}",
    save_plan . fork_specs );
  let clone : &Graphnode = & save_plan . fork_specs[0] . clone . 0;
  assert_eq! ( save_plan . fork_specs[0] . original_gnid, ID::from ("N") );
  assert_eq! ( clone . contains . len (), 3,
    "the clone's contains must be N1, N2 and the new node: {:?}",
    clone . contains );
  assert_eq! ( members_of (& clone . contains [..2]),
               vec! [ ID::from ("N1"), ID::from ("N2") ] );
  let new_node : &Graphnode =
    save_plan . node_instructions . iter ()
    . find_map ( |dn| match dn {
        NodeInstruction::Save ( SaveNode (n) )
          if n . title == "Can I add to this?" => Some (n),
        _ => None } )
    . expect ("the new node must survive as a Save instruction");
  assert_eq! ( new_node . home_skgrepo, clone . home_skgrepo,
    "the new node must adopt the clone's repo" );
  assert_eq! ( new_node . pid, clone . contains [2] . member,
    "the clone's last child must be the new node" );
  Ok (( )) }

async fn fork_new_parent_adopts_relationship_skgrepos (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let ( _vf, save_plan, _warnings ) = buffer_to_validated_saveplan (
    FORK_WITH_NEW_PARENT_FOR_OLD_CHILD_BUFFER, config, &SkgrepoRestriction::unrestricted (config) ) ?;
  let clone_skgrepo : SkgrepoName =
    save_plan . fork_specs . first ()
    . expect ("structural edit must fork N")
    . clone . 0 . home_skgrepo . clone ();
  let new_parent : &Graphnode =
    save_plan . node_instructions . iter ()
    . find_map ( |dn| match dn {
        NodeInstruction::Save (SaveNode (n)) if n . title == "New parent" => Some (n),
        _ => None } )
    . expect ("the new parent must survive as a Save instruction");
  assert_eq! (new_parent . home_skgrepo, clone_skgrepo,
    "the new parent must adopt the clone's repo");
  assert_eq! (members_of (&new_parent . contains), vec![ID::from ("N1")]);
  assert! (new_parent . contains . iter ()
           . all (|member| member . relRepo == clone_skgrepo),
    "the new parent's inherited relationships must adopt the clone repo: {:?}",
    new_parent . contains);
  Ok (( )) }

/// ('docs/forks.org', "Adding new content under a foreign node"),
/// committed: the approved save creates the clone and the new node, both in
/// the owned skgrepo; N's foreign .skg is untouched.
async fn fork_from_bare_new_child_skgsave_commits (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_approval_test (
    &mut stream, FORK_WITH_BARE_NEW_CHILD_BUFFER, config, tantivy,
    &graph,  &Err ( String::new () ), &mut views_state,
    /* approved_forks = */ true ) . await ?;
  assert! ( response . errors . is_empty (),
    "the approved bare-new-child fork must skgsave-commit: {:?}",
    response . errors );
  let clone : Graphnode = clone_on_disk (config) ?;
  let new_node : Graphnode =
    read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . find ( |node| node . title == "Can I add to this?" )
    . ok_or ("the new node must be on disk") ?;
  assert_eq! ( new_node . home_skgrepo, clone . home_skgrepo,
    "the new node must land in the clone's repo" );
  assert_eq! ( clone . home_skgrepo, SkgrepoName::from ("owned") );
  assert! ( clone . contains . iter () . any ( |m| m . member == new_node . pid ),
    "the clone must contain the new node: {:?}", clone . contains );
  assert_eq! ( members_of (& node_from_disk (config, "N") ? . contains),
               vec! [ ID::from ("N1"), ID::from ("N2") ],
    "N's own contains must be untouched" );
  Ok (( )) }

/// An explicitly foreign-repo new node stays a rejection: only an
/// inherited (guessed) foreign skgrepo rides the fork.
async fn explicitly_foreign_new_child_still_rejected (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let result = buffer_to_validated_saveplan (
    FORK_WITH_EXPLICIT_FOREIGN_NEW_CHILD_BUFFER, config, &SkgrepoRestriction::unrestricted (config) )
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

/// The fork operation's guard: forking N once creates a clone; forking
/// it again is rejected with 'ForkAlreadyExists', naming the clone and
/// saying how to fork it a second time deliberately.
async fn forking_a_note_one_already_forks_is_refused (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  let c1 : ID = clone_on_disk (config) ? . pid;

  // Save planning reloads the fixture graph and therefore sees the new clone.
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, &SkgrepoRestriction::unrestricted (config) ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      let refusal : &BufferValidationError = errors . iter ()
        . find ( |e| matches! ( e,
          BufferValidationError::ForkAlreadyExists (orig, existing)
            if *orig == ID::from ("N") && *existing == c1 ))
        . unwrap_or_else ( || panic! (
          "expected ForkAlreadyExists(N, {}), got {:?}", c1 . 0, errors ));
      let message : String = refusal . to_string ();
      assert! ( message . contains ("you already fork it; your fork is")
                && message . contains (&c1 . 0)
                && message . contains ("make another note of yours fork it"),
        "the refusal says what to do: {}", message ); }
    other => panic! (
      "expected ForkAlreadyExists rejecting the re-fork, got {:?}", other ), }
  Ok (( )) }

/// Skgrepo-set: a fork whose resolved owned skgrepo is restricted under the
/// skgrepo restriction is forbidden ('ForkRepoRestricted'), so an
/// invisible clone is never created silently. Here the skgrepo restriction is
/// {foreign} only, so the clone's inferred skgrepo "owned" is restricted.
async fn fork_skgrepo_restricted (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  // Save planning reloads the freshly reset fixture graph.
  let restriction : SkgrepoRestriction = // shows foreign alone
    SkgrepoRestriction::of_privacy_order_for_test (
      &["foreign", "owned", "owned2"], "foreign");
  let result = buffer_to_validated_saveplan (
    FORK_BUFFER, config, &restriction ) ;
  match result {
    Err ( SaveError::BufferValidationErrors { errors, .. } ) => {
      assert! ( errors . iter () . any ( |e| matches! ( e,
        BufferValidationError::ForkSkgrepoRestricted (orig, skgrepo)
          if *orig == ID::from ("N")
             && *skgrepo == SkgrepoName::from ("owned") )),
        "expected ForkRepoRestricted(N, owned), got {:?}", errors ); }
    other => panic! (
      "expected ForkRepoRestricted, got {:?}", other ), }
  Ok (( )) }

/// The SavePlan a foreign edit produces carries one ForkSpec whose
/// clone copies N's title/body/contains (shallow -- the child IDs only,
/// no descendants), forks=[N], and no
/// hides; in an owned skgrepo inferred from the owned ancestor P.
async fn fork_save_instruction (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  let fork_specs : Vec<ForkSpec> =
    fork_specs_from (FORK_BUFFER, config) . await ?;
  assert_eq! ( fork_specs . len (), 1,
    "exactly one fork (of N): {:?}", fork_specs );
  let spec : &ForkSpec = &fork_specs[0];
  assert_eq! ( spec . original_gnid, ID::from ("N") );
  let c : &Graphnode = &spec . clone . 0;
  assert_eq! ( c . title, "N-edited",
    "clone copies the edited title" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N1"), ID::from ("N2") ],
    "clone copies N's child IDs shallow (not descendants)" );
  assert_eq! ( members_of ( c . forks . or_default () ), vec! [ ID::from ("N") ],
    "clone forks N" );
  assert! ( c . hides . or_default () . is_empty (),
    "clone records no hides" );
  assert! ( config . skgrepo_is_owned (& c . home_skgrepo),
    "clone lives in an owned repo, got {:?}", c . home_skgrepo );
  assert_eq! ( c . home_skgrepo, SkgrepoName::from ("owned"),
    "clone's repo is inferred from the owned ancestor P" );
  assert_ne! ( c . pid, ID::from ("N"),
    "clone has a fresh pid, not N's" );
  Ok (( )) }

/// After saving the fork: a clone .skg appears in the owned skgrepo with
/// the four fields, and N's foreign .skg is byte-unchanged.
async fn fork_fixture_files (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let n_path : std::path::PathBuf =
    config . skgrepos . get (& SkgrepoName::from ("foreign")) . unwrap ()
    . path . join ("N.skg");
  let n_before : String = std::fs::read_to_string (&n_path) ?;

  // The save and its coherence check share this fixture-local handle.
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;
  assert! ( response . errors . is_empty (),
    "fork save must not error: {:?}", response . errors );

  let c : Graphnode = clone_on_disk (config) ?;
  assert_eq! ( c . title, "N-edited" );
  assert_eq! ( members_of (& c . contains), vec! [ ID::from ("N1"), ID::from ("N2") ] );
  assert_eq! ( members_of ( c . forks . or_default () ), vec! [ ID::from ("N") ] );
  assert_eq! ( c . home_skgrepo, SkgrepoName::from ("owned") );

  let n_after : String = std::fs::read_to_string (&n_path) ?;
  assert_eq! ( n_before, n_after,
    "N's foreign .skg must be byte-unchanged by the fork" );
  // And the in-memory disk node still says N-original.
  assert_eq! ( node_from_disk (config, "N") ? . title, "N-original" );
  Ok (( )) }

/// Round-trip: after the fork, reopening P shows the clone in N's
/// place, carrying (inPlaceOf N), with the default forksFolder
/// listing N. The load-bearing property:
/// P's stored
/// 'contains' is not rewritten to the clone -- it still lists N, so the
/// fact round-trips and a save never silently re-points containers at
/// the clone. (That the clone's forkee-as-such view of N starts
/// empty and fills as N gains children is the prerequisite rule,
/// exercised in hidden_from_forks; here C.contains ==
/// N.contains == [N1,N2] gives it nothing to show.)
async fn fork_round_trip (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle =
    ( graph_handle_from_config (config) ? );
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer (
    &mut stream, FORK_BUFFER, config, tantivy, &graph,
    &Err ( String::new () ), &mut views_state ) . await ?;

  let clone_gnid : ID = clone_on_disk (config) ? . pid;

  // Reopen P de-novo. N is P's content; mirror substitution shows the
  // clone in its place with (inPlaceOf N).
  let (p_view, _pids, _) =
    single_root_view (
      config, Some (tantivy), &ID::from ("P") ) ?;
  assert! ( p_view . contains ("(inPlaceOf N)"),
    "the clone must be shown in N's place with (inPlaceOf N):\n{}",
    p_view );
  assert! ( p_view . contains (& format! ("(gnid {})", clone_gnid . 0)),
    "the shown substitute must be the clone {}:\n{}", clone_gnid . 0, p_view );
  assert! ( p_view . contains ("forksFolder"),
    "the clone (a forker of N) shows a forksFolder:\n{}", p_view );

  // The load-bearing round-trip: P's stored contains was not rewritten
  // to the clone; it still lists N (the fact collected N, not C).
  let p_disk : Graphnode = node_from_disk (config, "P") ?;
  assert_eq! ( members_of (& p_disk . contains), vec! [ ID::from ("N") ],
    "P's contains must still point at N, not the clone {}", clone_gnid . 0 );
  Ok (( )) }

/// A collateral rerender substitutes the clone for N, as every refresh
/// of an editable occurrence's content does: the refresh decides from
/// the buffer and the graph as they are now, not from which view made
/// the fork. Editing P as well as forking N makes the second P
/// view collateral through the normal changed-PID path; no
/// fork-specific invalidation or rendering path participates.
async fn fork_collateral_rerender_substitutes_too (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let ( _text, pids, p_view ) = single_root_view (
    config, Some (tantivy), &ID::from ("P") ) ?;
  let saved_viewforest_id : ViewforestId = ViewforestId::ContentView ("saved-P" . to_string ());
  let collateral_viewforest_id : ViewforestId = ViewforestId::SearchView (
    "P before fork" . to_string ());
  let mut views_state : ViewsState = ViewsState {
    open_views     : OpenViews::new (),
    graph_straddle : None };
  let graph_snap = graph . load_full ();
  views_state . open_views . register_view (
    &graph_snap, saved_viewforest_id . clone (), p_view . clone (), &pids, &SkgrepoRestriction::unrestricted (config) );
  views_state . open_views . register_view (
    &graph_snap, collateral_viewforest_id . clone (), p_view, &pids, &SkgrepoRestriction::unrestricted (config) );

  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let buffer : String = FORK_BUFFER . replace ("P-container", "P-edited");
  let response = update_from_and_rerender_buffer (
    &mut stream, &buffer, config, tantivy, &graph,
    &Ok (saved_viewforest_id), &mut views_state ) . await ?;
  assert! ( response . saved_view . contains ("(inPlaceOf N)"),
    "the saved view must substitute the clone:\n{}", response . saved_view );

  let collateral = views_state . open_views
    . viewforest_id_to_view (&collateral_viewforest_id)
    .ok_or ("the collateral view must remain open") ?;
  let collateral_text : String = viewforest_to_string (collateral) ?;
  assert! ( collateral_text . contains ("(inPlaceOf N)")
            && ! collateral_text . contains ("(gnid N)"),
    "the collateral view must substitute the clone:\n{}", collateral_text );
  Ok (( )) }
