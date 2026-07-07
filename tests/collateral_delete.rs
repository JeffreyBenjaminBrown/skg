// cargo nextest run --test grouped_overrides -E 'test(collateral_delete::)'
//
// TODO/more.org, "BUG :: make-content cleanup -> collateral view not
// found": deleting a node whose viewnode is still present in ANOTHER
// open view must not make the save response complain about that
// collateral view. Fixture mirrors the reported shape: P contains the
// linking node L (whose title is one big textlink to X); a view of P
// is open; a second view of L alone is saved with (editRequest
// delete) on L.

use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};


use skg::dbs::in_rust_graph::{
  InRustGraphHandle, install_or_swap_global_handle};
use skg::test_utils::{
  run_with_test_db, graph_handle_from_config,
  read_all_lp_messages};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::{OpenViews, ViewUri};
use skg::types::misc::ID;

fn mk_pair () -> (TcpStream, TcpStream) {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr = listener . local_addr () . unwrap ();
  let write_end : TcpStream = TcpStream::connect (addr) . unwrap ();
  let (read_end, _) = listener . accept () . unwrap ();
  (write_end, read_end) }

#[test]
fn deleting_a_node_present_in_another_view_reports_no_errors
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-collateral-delete",
    "tests/collateral_delete/fixtures",
    "/tmp/tantivy-test-collateral-delete",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      install_or_swap_global_handle ( graph . clone () );
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let p_uri : ViewUri = ViewUri::ContentView ("collat-del-P" . into ());
      let l_uri : ViewUri = ViewUri::ContentView ("collat-del-L" . into ());

      // A view of P (showing L) stays open.
      let (p_view, p_pids, p_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("P"), false ) . await ?;
      assert! ( p_view . contains ("(id L)"),
        "P's view should show L:\n{}", p_view );
      views_state . open_views . register_view (
        p_uri . clone (), p_vf, &p_pids );

      { // A SECOND content view rooted at L, and a search view whose
        // result list holds L, both stay open too -- the deleted node
        // as a collateral view's ROOT, which is how the user usually
        // reached it (a search, or a goto-id view).
        let (_v, pids2, vf2) =
          single_root_view (
            driver, config, Some (tantivy), &ID::from ("L"), false ) . await ?;
        views_state . open_views . register_view (
          ViewUri::ContentView ("collat-del-L-2" . into ()), vf2, &pids2 );
        let (_v3, pids3, vf3) =
          single_root_view (
            driver, config, Some (tantivy), &ID::from ("L"), false ) . await ?;
        views_state . open_views . register_view (
          ViewUri::SearchView ("to X" . into ()), vf3, &pids3 ); }

      // A second view of L alone; register it, then save it with L
      // marked for deletion.
      let (_l_view, l_pids, l_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("L"), false ) . await ?;
      views_state . open_views . register_view (
        l_uri . clone (), l_vf, &l_pids );
      let delete_buffer : String =
        "* (skg (node (id L) (source main) (editRequest delete))) [[id:X][to X]]\n"
        . to_string ();
      let (mut stream, read_end) : (TcpStream, TcpStream) = mk_pair ();
      let response : SaveResponse =
        update_from_and_rerender_buffer (
          &mut stream, &delete_buffer, driver, config, tantivy, &graph,
          false, &Ok (l_uri . clone ()), &mut views_state ) . await ?;
      drop (stream);
      let msgs : Vec<String> = {
        let mut reader : BufReader<TcpStream> = BufReader::new (read_end);
        read_all_lp_messages (&mut reader) };
      assert! ( response . errors . is_empty (),
        "deleting L must not error, even with L visible in P's open view; got {:?}\ncollateral stream: {:?}",
        response . errors, msgs );
      Ok (( )) } )) }
