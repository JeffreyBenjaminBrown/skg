// cargo nextest run --test grouped_overrides -E 'test(collateral_subscribee_staleness::)'
//
// Regression for the collateral-rerender staleness flagged in
// subscribeecol-maybe-todo.org and fixed for forks (plan.org:
// "Collateral-rerender staleness fix"). A DEFINITIVE subscriber open in
// two views: changing its subscriptions in one view and saving must
// refresh the OTHER (collateral) view's subscribeeCol from the graph,
// not leave it showing the old subscription. Before the fix,
// reconcile_subscribee_col_children skipped a definitive subscriber
// outside diff mode (the `parent_indefinitive || source_diffs.is_some()`
// gate), so the collateral subscribeeCol kept stale members.
//
// Installs the process-global graph handle (the subscribee lookups read
// snapshot_global), so it lives among the grouped_overrides installers.

use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};
use std::sync::Arc;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle, install_or_swap_global_handle};
use skg::test_utils::{
  run_with_test_db, graph_handle_from_config,
  extract_string_field_from_sexp, read_all_lp_messages};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::{OpenViews, ViewUri};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use typedb_driver::TypeDBDriver;

fn mk_pair () -> (TcpStream, TcpStream) {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr = listener . local_addr () . unwrap ();
  let write_end : TcpStream = TcpStream::connect (addr) . unwrap ();
  let (read_end, _) = listener . accept () . unwrap ();
  (write_end, read_end) }

/// Save the buffer attributed to 'uri', returning the collateral views'
/// rendered contents.
async fn save_and_read_collateral (
  buffer      : &str,
  uri         : &ViewUri,
  driver      : &Arc<TypeDBDriver>,
  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(SaveResponse, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer (
      &mut stream, buffer, driver, config, tantivy, graph, false,
      &Ok (uri . clone ()), views_state ) . await ?;
  drop (stream);
  let mut reader : BufReader<TcpStream> = BufReader::new (read_end);
  let collateral_views : Vec<String> =
    read_all_lp_messages (&mut reader)
    . into_iter ()
    . filter ( |msg| msg . contains ("collateral-view") )
    . map ( |msg| extract_string_field_from_sexp (&msg, "content")
            . unwrap_or_else (
              || panic! ("no content field in collateral-view: {}", msg)) )
    . collect ();
  Ok ((response, collateral_views)) }

fn drop_member_line ( buf : &str, fragment : &str ) -> String {
  buf . lines ()
    . filter ( |l| ! l . contains (fragment) )
    . collect::<Vec<_>> ()
    . join ("\n") + "\n" }

#[test]
fn collateral_definitive_subscriber_subscribeeCol_refreshes
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-collateral-subscribee-staleness",
    "tests/collateral_subscribee_staleness/fixtures",
    "/tmp/tantivy-test-collateral-subscribee-staleness",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      install_or_swap_global_handle ( graph . clone () );
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let a_uri : ViewUri = ViewUri::ContentView ("S-a" . into ());
      let b_uri : ViewUri = ViewUri::ContentView ("S-b" . into ());

      // View A rooted at S: S is a definitive root, its subscribeeCol
      // shows both subscribees M and N.
      let (a_view, a_pids, a_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("S"), false ) . await ?;
      assert! ( a_view . contains ("subscribeeCol")
                && a_view . contains ("(id M)")
                && a_view . contains ("(id N)"),
        "view A should show M and N in S's subscribeeCol:\n{}", a_view );
      views_state . open_views . register_view (
        a_uri . clone (), a_vf, &a_pids );

      // View B rooted at S too -- the collateral view. S is definitive
      // here as well (it is the root).
      let (b_view, b_pids, b_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("S"), false ) . await ?;
      assert! ( b_view . contains ("(id M)")
                && b_view . contains ("(id N)"),
        "view B should show M and N in S's subscribeeCol:\n{}", b_view );
      views_state . open_views . register_view (
        b_uri . clone (), b_vf, &b_pids );

      // In view A, drop N from S's subscribeeCol (subscribes_to -> [M])
      // and save A.
      let edited_a : String = drop_member_line (&a_view, "(id N)");
      let (_response, collateral_views) =
        save_and_read_collateral (
          &edited_a, &a_uri, driver, config, tantivy, &graph,
          &mut views_state ) . await ?;

      // The collateral view B must refresh from the just-saved graph:
      // its subscribeeCol now shows M but NOT N, even though S is
      // DEFINITIVE there (the root) and diff mode is off. Before the
      // fix, B kept N stale.
      assert_eq! ( collateral_views . len (), 1,
        "exactly view B should be collateral: {:?}", collateral_views );
      let b_collateral : &str = &collateral_views[0];
      assert! ( b_collateral . contains ("(id M)"),
        "M must remain in B's subscribeeCol:\n{}", b_collateral );
      assert! ( ! b_collateral . contains ("(id N)"),
        "N must be gone from B's subscribeeCol collaterally \
         (definitive subscriber must refresh from the graph):\n{}",
        b_collateral );
      Ok (( )) } )) }
