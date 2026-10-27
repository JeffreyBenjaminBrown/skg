// cargo nextest run --test grouped_views -E 'test(containerward_chain::)'
//
// The containers folder and the containerward chain
// ('server/refresh/chain.rs'): a fresh content
// view puts a containers folder on each root, and a '(folder
// containers)' request draws one anywhere; under each member hangs
// the chain, write-protected, claiming nothing, up to the roots, a node met
// before on the walk shown once more as a leaf.
//
// Fixture: m contains r; b and c contain m; d contains b and c (a
// diamond above m). x and y contain each other (a cycle).

use crate::git_fixture_helpers::mk_test_tcp_stream_pair;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::metadata_canon::without_viewnode_ids;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::test_utils::{graph_handle_from_config, run_with_test_stores};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::test_utils::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::viewnode::Viewnode;
use skg::types::views_state::{OpenViews, ViewforestId};

use ego_tree::Tree;
use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

const FIXTURES : &str = "tests/containerward_chain/fixtures";

#[test]
fn containers_folders_and_their_chains (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-containerward-chain",
    FIXTURES,
    "/tmp/tantivy-test-containerward-chain",
    |config, tantivy| Box::pin ( async move {
      a_root_shows_its_containers_folder_with_a_diamond_in_the_chain (
        config, tantivy ) . await ?;
      a_cycle_in_the_chain_ends_in_a_leaf (config, tantivy) ?;
      a_folder_request_draws_the_chain_under_its_members (
        config, tantivy ) . await ?;
      a_request_on_a_chain_node_draws_a_containers_folder_under_it (
        config, tantivy ) . await ?;
      Ok (( )) } )) }

//
// Helpers
//

/// Open the view of 'roots' as a client's request does, registered
/// under 'view_name'. Returns its text.
fn open_view (
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  view_name   : &str,
  roots       : &[&str],
) -> Result<String, Box<dyn Error>> {
  let root_ids : Vec<ID> = roots . iter () . map ( |root| ID::from (*root) ) . collect ();
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (config, Some (tantivy), &root_ids) ?;
  views_state . open_views . register_view (
    &graph . load_full (), ViewforestId::ContentView (view_name . to_string ()),
    tree, &pids );
  Ok (text) }

/// Save 'buffer' as the view 'view_name'. Returns the saved view's text.
async fn save_view (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  view_name   : &str,
) -> Result<String, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok ( ViewforestId::ContentView (view_name . to_string ()) ),
    views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response . saved_view) }

fn fresh_views_state () -> ViewsState {
  ViewsState { open_views     : OpenViews::new (),
               graph_straddle : None } }

/// Each headline of 'buffer' as its stars and what it shows: a
/// containers folder, or a node's ID followed by 'noClaim' and
/// 'writeProtected' where it carries them.
fn outline (
  buffer : &str,
) -> String {
  buffer . lines ()
    . filter ( |line| line . starts_with ('*') )
    . map ( |line| {
      let stars : &str = line . split (' ') . next () . unwrap ();
      let metadata : String = without_viewnode_ids (line);
      let shown : String =
        if metadata . contains ("(skg containerFolder") {
          "containerFolder" . to_string ()
        } else {
          let gnid : &str = metadata . split ("(gnid ") . nth (1)
            . and_then ( |rest| rest . split (')') . next () )
            . unwrap_or ("?");
          let mut shown : String = gnid . to_string ();
          if metadata . contains ("noClaim")      { shown += " noClaim"; }
          if metadata . contains ("writeProtected") { shown += " writeProtected"; }
          shown };
      format! ("{} {}\n", stars, shown) } )
    . collect () }

/// 'buffer' with the view request 'request' on its one headline
/// whose metadata starts with 'headline_start'.
fn requesting_at (
  buffer         : &str,
  headline_start : &str,
  request        : &str,
) -> String {
  let lines : Vec<&str> = buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . starts_with (headline_start) ) . collect ();
  assert_eq! ( lines . len (), 1, "one headline should start {:?}:\n{}",
               headline_start, buffer );
  buffer . replacen (
    lines [0],
    &lines [0] . replacen ( "node ",
                            &format! ("node (viewRequests {}) ", request), 1 ),
    1 ) }

//
// The tests
//

/// The view of r: a containers folder holding m, write-protected;
/// under m its containers b and c, under each of those d. d is met
/// twice, the second time as a leaf; it has no container, so either
/// way nothing hangs under it. A save changes nothing: the chain is
/// not drawn again.
async fn a_root_shows_its_containers_folder_with_a_diamond_in_the_chain (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, "view of r", &["r"] ) ?;
  assert_eq! ( outline (&buffer), indoc! {"
    * r
    ** containerFolder
    *** m writeProtected
    **** b writeProtected
    ***** d writeProtected
    **** c writeProtected
    ***** d writeProtected
    "}, "{}", buffer );
  let saved : String = save_view (
    &buffer, config, tantivy, &graph, &mut views_state, "view of r" ) . await ?;
  assert_eq! ( saved, buffer, "a save changes nothing" );
  Ok (( )) }

/// The view of x: a containers folder holding y; under y its
/// container x, and under that x's container y, a leaf, since the
/// walk met y already. The content of x follows, as in any view.
fn a_cycle_in_the_chain_ends_in_a_leaf (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, "view of x", &["x"] ) ?;
  assert_eq! ( outline (&buffer), indoc! {"
    * x
    ** containerFolder
    *** y writeProtected
    **** x writeProtected
    ***** y writeProtected
    ** y
    *** x writeProtected
    "}, "{}", buffer );
  Ok (( )) }

/// The view of d shows m under b. A '(folder containers)' request on
/// it draws its containers folder, holding b and c, each with its
/// chain: d, under both.
async fn a_folder_request_draws_the_chain_under_its_members (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, "view of d", &["d"] ) ?;
  assert_eq! ( outline (&buffer), indoc! {"
    * d
    ** b
    *** m
    **** r
    ** c
    *** m writeProtected
    "}, "{}", buffer );
  let saved : String = save_view (
    &requesting_at (&buffer, "*** (skg node (gnid m) (skgRepos (nodeRepo main)) (view (rels",
                    "(folder containers)"),
    config, tantivy, &graph, &mut views_state, "view of d" ) . await ?;
  assert_eq! ( outline (&saved), indoc! {"
    * d
    ** b
    *** m
    **** containerFolder
    ***** b writeProtected
    ****** d writeProtected
    ***** c writeProtected
    ****** d writeProtected
    **** r
    ** c
    *** m writeProtected
    "}, "{}", saved );
  Ok (( )) }

/// In the view of r, a '(folder containers)' request on the chain
/// node b draws a containers folder under b, holding d, beside the
/// chain node d already there.
async fn a_request_on_a_chain_node_draws_a_containers_folder_under_it (
  config  : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
  let mut views_state : ViewsState = fresh_views_state ();
  let buffer : String = open_view (
    config, tantivy, &graph, &mut views_state, "view of r, again", &["r"] ) ?;
  let saved : String = save_view (
    &requesting_at (&buffer, "**** (skg node (gnid b) ", "(folder containers)"),
    config, tantivy, &graph, &mut views_state, "view of r, again" ) . await ?;
  assert_eq! ( outline (&saved), indoc! {"
    * r
    ** containerFolder
    *** m writeProtected
    **** b writeProtected
    ***** containerFolder
    ****** d writeProtected
    ***** d writeProtected
    **** c writeProtected
    ***** d writeProtected
    "}, "{}", saved );
  Ok (( )) }
