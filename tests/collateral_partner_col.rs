// cargo nextest run --test grouped_overrides -E 'test(collateral_partner_col::)'
//
// Two cells of the relationship matrix
// (TODO/full-schema/13_test-rel-matrix.org) that need an open
// COLLATERAL view and the collateral rerender stream:
//
// 1. collateral col-membership update: a view of N shows a
//    subscriberCol with member S; a save of a SECOND view that drops
//    S's subscription to N must re-render N's view (collaterally) with
//    S gone from its subscriberCol. This exercises the
//    reconcile_partnerCol_children path shared by all eight cols.
//
// 2. saved-view-only warning scoping: a repair that happens in the
//    COLLATERAL view (a stale intruder under its subscriberCol) while
//    the saved buffer is clean must NOT add a repair warning to the
//    save response -- collateral rerenders pass no warning sink
//    (stage 8), so warnings stay scoped to the saved view.
//
// Its own target because it installs the process-global graph handle
// (PartnerCol scaffolds, esp. the inbound subscriberCol, are created
// from snapshot_global -- see TODO/problems.org). One test function,
// so the global handle is installed once.

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

/// Save the buffer attributed to 'uri', returning the saved view, the
/// full SaveResponse, and the collateral views' rendered contents.
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
fn collateral_partner_col_update_and_warning_scoping
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-collateral-partner-col",
    "tests/collateral_partner_col/fixtures",
    "/tmp/tantivy-test-collateral-partner-col",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      install_or_swap_global_handle ( graph . clone () );
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (), };
      let n_uri : ViewUri = ViewUri::ContentView ("collat-N" . into ());
      let s_uri : ViewUri = ViewUri::ContentView ("collat-S" . into ());

      // Render N (subscriberCol shows S) and register it as the view
      // that will be updated collaterally.
      let (n_view, n_pids, n_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("N"), false ) . await ?;
      assert! ( n_view . contains ("subscriberCol")
                && n_view . contains ("(id S)"),
        "N's view should show S in a subscriberCol:\n{}", n_view );
      views_state . open_views . register_view (
        n_uri . clone (), n_vf, &n_pids );

      // Render S (subscribeeCol shows N) and register it as the saved
      // view.
      let (s_view, s_pids, s_vf) =
        single_root_view (
          driver, config, Some (tantivy), &ID::from ("S"), false ) . await ?;
      assert! ( s_view . contains ("subscribeeCol")
                && s_view . contains ("(id N)"),
        "S's view should show N in a subscribeeCol:\n{}", s_view );
      views_state . open_views . register_view (
        s_uri . clone (), s_vf, &s_pids );

      // Drop N from S's subscribeeCol (emptying it = explicit empty
      // set) and save S's view.
      let edited_s : String = drop_member_line (&s_view, "(id N)");
      let (response, collateral_views) =
        save_and_read_collateral (
          &edited_s, &s_uri, driver, config, tantivy, &graph,
          &mut views_state ) . await ?;

      // CELL 1 -- collateral col-membership update: N's view is
      // re-rendered with S gone from its subscriberCol (the now-empty
      // col is dropped entirely).
      assert_eq! ( collateral_views . len (), 1,
        "exactly N's view should be collateral: {:?}", collateral_views );
      let n_collateral : &str = &collateral_views[0];
      assert! ( n_collateral . contains ("(id N)"),
        "the collateral view should be N's:\n{}", n_collateral );
      assert! ( ! n_collateral . contains ("(id S)"),
        "S must be gone from N's subscriberCol collaterally:\n{}",
        n_collateral );
      assert! ( ! n_collateral . contains ("subscriberCol"),
        "N's now-empty subscriberCol should be dropped:\n{}",
        n_collateral );

      // CELL 2 -- saved-view-only warning scoping: N's subscriberCol
      // changed in the collateral stream, but the change is not blamed
      // on the saver -- the save response carries no col-repair
      // warning. (Stage 8 passes no warning sink to collateral
      // rerenders; this pins the observable consequence. The stronger
      // form -- a collateral view holding an UNREPAIRED stale member --
      // is not reachable through register_view, whose viewforests are
      // always post-reconciliation; that form stays enforced by
      // construction. See progress.org.)
      assert! (
        ! response . warnings . iter () . any (
          |w| w . contains ("Repaired") ),
        "a collateral col change must not warn the saver: {:?}",
        response . warnings );
      Ok (( )) } )) }
