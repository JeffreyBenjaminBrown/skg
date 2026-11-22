// cargo nextest run --test grouped_forks -E 'test(fork_attribution::)'
//
// A fork's attribution ('docs/forks.org', "Attribution"): the fork
// operation records, in the clone's home section, the note it forked
// and that note's home skgrepo; the clone's herald then names both
// skgrepos wherever it is drawn; a fork of a fork records the note it
// actually forked; and 'stripAttribution' removes the record through
// the save. The git facts (remote hash, commit) are in
// 'tests/unit/attribution.rs'.

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::error::Error;
use std::net::TcpStream;

use indoc::indoc;

use std::collections::HashMap;

use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_skgrepos;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::test_utils::{graph_handle_from_config, run_with_shared_test_stores, single_root_view};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::update_from_and_rerender_buffer_with_fork_skgrepos_test;
use skg::types::config::SkgConfig;
use skg::types::misc::{ID, SkgrepoName, TantivyIndex};
use skg::types::nodes::attribution::Attribution;
use skg::types::nodes::complete::Graphnode;
use skg::types::views_state::OpenViews;

/// The foreign N, made editable under the owned P, retitled: saving
/// forks it into P's skgrepo, 'owned'.
const FORK_BUFFER : &str = indoc! {"
  * (skg node (gnid P) (skgRepos (nodeRepo owned))) P-container
  ** (skg node (gnid N) (skgRepos (nodeRepo foreign))) N-edited
  *** (skg node (gnid N1) (skgRepos (nodeRepo foreign)) writeProtected) N1
  *** (skg node (gnid N2) (skgRepos (nodeRepo foreign)) writeProtected) N2
  "};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/fork/fixtures-two-owned";
  run_with_shared_test_stores (
    "skg-test-fork-attribution",
    |s| Box::pin ( async move {
      s . reset ("a_fork_records_and_shows_its_attribution", fixtures) ?;
      a_fork_records_and_shows_its_attribution (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_fork_of_a_fork_records_the_note_it_forked", fixtures) ?;
      a_fork_of_a_fork_records_the_note_it_forked (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("a_plain_child_of_an_attributed_fork_says_its_home", fixtures) ?;
      a_plain_child_of_an_attributed_fork_says_its_home (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("stripping_an_attribution_removes_it_and_its_herald", fixtures) ?;
      stripping_an_attribution_removes_it_and_its_herald (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

fn fresh_views_state () -> ViewsState {
  ViewsState { open_views : OpenViews::new (), graph_straddle : None } }

/// The owned note that forks 'original', as on disk.
fn fork_of (
  config   : &SkgConfig,
  original : &str,
) -> Result<Graphnode, Box<dyn Error>> {
  read_all_skg_files_from_skgrepos (config) ?
    . into_iter ()
    . find ( |node| node . forks . or_default () . iter ()
             . any ( |m| m . member == ID::from (original) ) )
    . ok_or_else ( || format! ( "no note forks {}", original ) . into () ) }

/// Save FORK_BUFFER, approving the fork, and return the clone.
async fn fork_n (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
  graph   : &InRustGraphHandle,
) -> Result<Graphnode, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_skgrepos_test (
    &mut stream, FORK_BUFFER, config, tantivy, graph,
    &Err ( String::new () ), &mut fresh_views_state (),
    /* approved_forks = */ true, &HashMap::new () ) . await ?;
  assert! ( response . errors . is_empty (), "the fork must save: {:?}", response . errors );
  fork_of (config, "N") }

/// The line of 'view' whose metadata names 'gnid'.
fn line_naming<'a> (
  view : &'a str,
  gnid : &ID,
) -> &'a str {
  let needle : String = format! ( "(gnid {})", gnid . 0 );
  view . lines () . find ( |line| line . contains (&needle) )
    . unwrap_or_else ( || panic! ( "no line names {}:\n{}", gnid . 0, view )) }

/// Forking the foreign N into 'owned' records N and its skgrepo in the
/// clone's home section; the fixture's skgrepos are no gitrepos, so
/// no remote or commit. Drawn under P, of its own skgrepo, the clone
/// still stands at a skgrepo boundary, naming 'foreign'.
async fn a_fork_records_and_shows_its_attribution (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let clone : Graphnode = fork_n (config, tantivy, &graph) . await ?;
  assert_eq! ( clone . home_skgrepo, SkgrepoName::from ("owned") );
  assert_eq! ( clone . attribution, Some ( Attribution {
    original : ID::from ("N"), skgrepo : SkgrepoName::from ("foreign"),
    remote_hash : None, commit : None, uncommitted : false } ));
  let (p_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let clone_line : &str = line_naming (&p_view, &clone . pid);
  assert! ( clone_line . contains ("(attribution foreign) repoBoundary)"),
    "the clone, under a note of its own skgrepo, names the skgrepo it was forked from:\n{}",
    p_view );
  Ok (( )) }

/// Forking the clone C (in 'owned') into 'owned2' records C, and C's
/// skgrepo, not N's: the note actually forked.
async fn a_fork_of_a_fork_records_the_note_it_forked (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let clone : Graphnode = fork_n (config, tantivy, &graph) . await ?;
  let explicit_fork : String = format! (
    "* (skg node (gnid {}) (skgRepos (nodeRepo owned)) (viewRequests fork)) {}\n",
    clone . pid . 0, clone . title );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer_with_fork_skgrepos_test (
    &mut stream, &explicit_fork, config, tantivy, &graph,
    &Err ( String::new () ), &mut fresh_views_state (), true,
    &HashMap::from ([ ( clone . pid . clone (), SkgrepoName::from ("owned2") ) ]) ) . await ?;
  assert! ( response . errors . is_empty (), "the second fork must save: {:?}", response . errors );
  let second : Graphnode = fork_of (config, &clone . pid . 0) ?;
  assert_eq! ( second . home_skgrepo, SkgrepoName::from ("owned2") );
  let attribution : Attribution = second . attribution . expect ("the second fork is attributed");
  assert_eq! ( ( attribution . original, attribution . skgrepo ),
               ( clone . pid . clone (), SkgrepoName::from ("owned") ) );
  assert_eq! ( fork_of (config, "N") ? . attribution . map ( |a| a . skgrepo ),
               Some ( SkgrepoName::from ("foreign") ),
               "the first fork keeps its own attribution" );
  Ok (( )) }

/// A child typed under the clone, in the clone's skgrepo 'owned' but
/// with no attribution, speaks in another voice than its parent's
/// "⌂:<owned/foreign>": it shows its own "⌂:owned".
async fn a_plain_child_of_an_attributed_fork_says_its_home (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let clone : Graphnode = fork_n (config, tantivy, &graph) . await ?;
  let (p_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let clone_line : &str = line_naming (&p_view, &clone . pid);
  let buffer : String = p_view . replacen (
    clone_line, &format! ( "{}\n*** plain child", clone_line ), 1 );
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &buffer, config, tantivy, &graph,
    &Err ( String::new () ), &mut fresh_views_state () ) . await ?;
  assert! ( response . errors . is_empty (), "the child must save: {:?}", response . errors );
  let child_line : &str = response . saved_view . lines ()
    . find ( |line| line . ends_with (" plain child") )
    . unwrap_or_else ( || panic! ( "no child:\n{}", response . saved_view ));
  assert! ( child_line . contains ("(nodeRepo owned)") && child_line . contains ("repoBoundary"),
    "the plain child of the attributed clone stands at a boundary:\n{}", response . saved_view );
  assert! ( ! child_line . contains ("attribution"), "{}", child_line );
  Ok (( )) }

/// A view of the clone, its headline carrying 'stripAttribution', saved:
/// the clone keeps everything but its attribution, and the redrawn view
/// shows none.
async fn stripping_an_attribution_removes_it_and_its_herald (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let clone : Graphnode = fork_n (config, tantivy, &graph) . await ?;
  let (p_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let clone_line : &str = line_naming (&p_view, &clone . pid);
  let stripped_line : String = clone_line . replacen (
    "(skgRepos (nodeRepo owned))",
    "(skgRepos (nodeRepo owned)) (editRequest stripAttribution)", 1 );
  assert_ne! ( stripped_line, clone_line, "the request is written:\n{}", clone_line );
  let buffer : String = p_view . replacen (clone_line, &stripped_line, 1);
  let (mut stream, _read_end) : (TcpStream, TcpStream) = mk_test_tcp_stream_pair ();
  let response = update_from_and_rerender_buffer (
    &mut stream, &buffer, config, tantivy, &graph,
    &Err ( String::new () ), &mut fresh_views_state () ) . await ?;
  assert! ( response . errors . is_empty (), "the strip must save: {:?}", response . errors );
  let after : Graphnode = fork_of (config, "N") ?;
  assert_eq! ( after . attribution, None );
  assert_eq! ( ( &after . title, &after . contains, &after . forks ),
               ( &clone . title, &clone . contains, &clone . forks ),
               "only the attribution goes" );
  let (p_view, _, _) = single_root_view (config, Some (tantivy), &ID::from ("P")) ?;
  let clone_line : &str = line_naming (&p_view, &clone . pid);
  assert! ( ! clone_line . contains ("attribution") && ! clone_line . contains ("repoBoundary"),
    "the stripped clone, under a note of its own skgrepo, shows no herald:\n{}", p_view );
  Ok (( )) }
