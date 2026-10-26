// cargo nextest run --test grouped_overrides -E 'test(collateral_partner_folder::)'
//
// Two cells of the relationship matrix ('docs/sharing-model.org', "The
// write-protected folders: who subscribes to, overrides, or hides this
// node") that need an open collateral view and the collateral rerender
// stream:
//
// 1. collateral folder-membership update: a view of N shows a
//    subscriberFolder with member S; a save of a second view that drops
//    S's subscription to N must re-render N's view (collaterally) with
//    S unwarranted in its subscriberFolder.
//
// 2. corrections are tagged with their view: whatever the refresh of
//    the collateral view corrects is reported under that view, never
//    under the saved one, and never as a warning.

use std::error::Error;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::test_utils::{
  run_with_test_stores, graph_handle_from_config,
  extract_string_field_from_sexp, read_all_lp_messages};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::to_org::render::content_view::single_root_view;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

fn mk_pair () -> (TcpStream, TcpStream) {
  let listener : TcpListener =
    TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr = listener . local_addr () . unwrap ();
  let write_end : TcpStream = TcpStream::connect (addr) . unwrap ();
  let (read_end, _) = listener . accept () . unwrap ();
  (write_end, read_end) }

/// Save the buffer attributed to 'viewforest_id', returning the saved view, the
/// full SaveResponse, and the collateral views' rendered contents.
async fn save_and_read_collateral (
  buffer      : &str,
  viewforest_id : &ViewforestId,

  config      : &SkgConfig,
  tantivy     : &mut TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
) -> Result<(SaveResponse, Vec<String>), Box<dyn Error>> {
  let (mut stream, read_end) : (TcpStream, TcpStream) = mk_pair ();
  let response : SaveResponse =
    update_from_and_rerender_buffer (
      &mut stream, buffer, config, tantivy, graph,
      &Ok (viewforest_id . clone ()), views_state ) . await ?;
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
fn collateral_partner_folder_update_and_warning_scoping
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-collateral-partner-folder",
    "tests/collateral_partner_folder/fixtures",
    "/tmp/tantivy-test-collateral-partner-folder",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views        : OpenViews::new (),
        graph_straddle    : None, };
      let n_viewforest_id : ViewforestId = ViewforestId::ContentView ("collat-N" . into ());
      let s_viewforest_id : ViewforestId = ViewforestId::ContentView ("collat-S" . into ());

      // Render N, explicitly request its subscription folders, and save it
      // under the view ID that will be updated collaterally.  subscriberFolder
      // is no longer part of the default presentation.
      let (n_view, _n_pids, _n_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("N") ) ?;
      let n_folder_request : String = n_view . replacen (
        "node ",
        "node (viewRequests (folder subscribesTo))", 1 );
      let (n_response, initial_collateral) =
        save_and_read_collateral (
          &n_folder_request, &n_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( n_response . errors . is_empty (),
        "N's folder request should save cleanly: {:?}", n_response . errors );
      assert! ( initial_collateral . is_empty (),
        "opening N should not update a collateral view: {:?}",
        initial_collateral );
      let n_view : String = n_response . saved_view;
      assert! ( n_view . contains ("subscriberFolder")
                && n_view . contains ("(gnid S)"),
        "N's view should show S in a subscriberFolder:\n{}", n_view );

      // Render S (subscribeeFolder shows N) and register it as the saved
      // view.
      let (s_view, s_pids, s_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("S") ) ?;
      assert! ( s_view . contains ("subscribeeFolder")
                && s_view . contains ("(gnid N)"),
        "S's view should show N in a subscribeeFolder:\n{}", s_view );
      views_state . open_views . register_view (
        &graph . load_full (), s_viewforest_id . clone (), s_vf, &s_pids );

      // Drop N from S's subscribeeFolder (emptying it = explicit empty
      // set) and save S's view.
      let edited_s : String = drop_member_line (&s_view, "(gnid N)");
      let (response, collateral_views) =
        save_and_read_collateral (
          &edited_s, &s_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;

      // CELL 1 -- collateral folder-membership update: N's view is
      // re-rendered with S, no longer a subscriber, kept in N's
      // subscriberFolder, unwarranted (the refresh keeps what the
      // truth no longer relates, and marks it).
      assert_eq! ( collateral_views . len (), 1,
        "exactly N's view should be collateral: {:?}", collateral_views );
      let n_collateral : &str = &collateral_views[0];
      assert! ( n_collateral . contains ("(gnid N)"),
        "the collateral view should be N's:\n{}", n_collateral );
      let s_line : &str = n_collateral . lines ()
        . find ( |line| line . contains ("(gnid S)") )
        . unwrap_or_else ( || panic! ( "S is kept:\n{}", n_collateral ));
      assert! ( s_line . starts_with ("*** ") && s_line . contains ("noWarrant"),
        "S must be unwarranted in N's subscriberFolder collaterally:\n{}",
        n_collateral );

      // CELL 2 -- corrections are tagged with their view: N's
      // subscriberFolder changed in the collateral stream, and nothing
      // of that is reported under the saved view, or as a warning.
      assert! (
        response . corrections . iter ()
        . all ( |view| view . viewforest_id != s_viewforest_id ),
        "the saved view corrected nothing: {:?}", response . corrections );
      assert! (
        response . warnings . is_empty (),
        "a collateral change must not warn the saver: {:?}",
        response . warnings );
      Ok (( )) } )) }

/// A member the folder reconciler adds at a later visit gets its
/// containerward chain. N's view shows S in its subscriberFolder; C
/// contains T. A save of T's view that makes T subscribe to N
/// rerenders N's view collaterally, its folder reconciler adding T,
/// under which hangs T's chain: C, write-protected and claiming nothing.
#[test]
fn a_member_added_at_a_later_visit_gets_its_chain
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-collateral-partner-folder-chain",
    "tests/collateral_partner_folder/fixtures-chain",
    "/tmp/tantivy-test-collateral-partner-folder-chain",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views        : OpenViews::new (),
        graph_straddle    : None, };
      let n_viewforest_id : ViewforestId = ViewforestId::ContentView ("chain-N" . into ());
      let t_viewforest_id : ViewforestId = ViewforestId::ContentView ("chain-T" . into ());
      let (n_view, _n_pids, _n_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("N") ) ?;
      let (n_response, _) =
        save_and_read_collateral (
          &n_view . replacen (
            "node ", "node (viewRequests (folder subscribesTo))", 1 ),
          &n_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( n_response . errors . is_empty ()
                && n_response . saved_view . contains ("(gnid S)")
                && ! n_response . saved_view . contains ("(gnid T)"),
        "N's subscriberFolder should hold S alone:\n{}",
        n_response . saved_view );
      let (response, collateral_views) =
        save_and_read_collateral (
          "* (skg node (gnid T) (skgRepos (nodeRepo main))) T\n\
           ** (skg subscribeeFolder)\n\
           *** (skg node (gnid N) (skgRepos (nodeRepo main)) writeProtected) N\n",
          &t_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (),
        "T's save should succeed: {:?}", response . errors );
      let n_collateral : &String = collateral_views . iter ()
        . find ( |view| view . contains ("subscriberFolder") )
        . unwrap_or_else ( || panic! (
          "N's view should be collateral: {:?}", collateral_views ));
      let lines : Vec<&str> = n_collateral . lines () . collect ();
      let t_index : usize = lines . iter ()
        . position ( |line| line . starts_with ("*** ") && line . contains ("(gnid T)") )
        . unwrap_or_else ( || panic! (
          "T should be a member of N's subscriberFolder:\n{}", n_collateral ));
      let chain_node : &str = lines . get (t_index + 1) . copied () . unwrap_or ("");
      assert! ( chain_node . starts_with ("**** ") && chain_node . contains ("(gnid C)")
                && ! chain_node . contains ("noClaim") // under a write-protected parent
                && chain_node . contains ("writeProtected"),
        "T should hold its chain, C:\n{}", n_collateral );
      Ok (( )) } )) }
