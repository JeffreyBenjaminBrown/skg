use std::error::Error;
use std::fs;
use std::io::BufReader;
use std::net::{TcpListener, TcpStream};

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::nodecomplete_from_graph;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::ClientViewSnapshot;
use skg::test_utils::{
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_test_stores,
  update_from_and_rerender_buffer_with_snapshots_test,
};
use skg::to_org::render::content_view::single_root_view;
use skg::types::misc::ID;
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{OpenViews, ViewUri};

fn mk_pair () -> (TcpStream, TcpStream) {
  let listener : TcpListener = TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let write_end : TcpStream = TcpStream::connect (
    listener . local_addr () . unwrap ()) . unwrap ();
  let (read_end, _) = listener . accept () . unwrap ();
  (write_end, read_end)
}

fn dirty_snapshot (
  uri      : &str,
  baseline : Option<&str>,
  current  : &str,
) -> ClientViewSnapshot {
  ClientViewSnapshot {
    uri      : ViewUri::ContentView (uri . into ()),
    dirty    : true,
    baseline : baseline . map (str::to_string),
    current  : Some (current . to_string ()),
  }
}

fn error_text (error : &(dyn Error + 'static)) -> String {
  error . downcast_ref::<SaveError> ()
    . map (format_save_error_as_org)
    . unwrap_or_else (|| error . to_string ())
}

#[test]
fn conflicting_dirty_view_refuses_before_mutation_and_disjoint_view_succeeds
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-save-dirty-conflicts",
    "tests/save_dirty_conflicts/fixtures",
    "/tmp/tantivy-test-save-dirty-conflicts",
    |config, tantivy| Box::pin (async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        diff_mode_enabled : false,
        open_views        : OpenViews::new (),
      };
      let saved_uri : ViewUri = ViewUri::ContentView ("saved-a" . into ());
      let (a_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("A"), false) ?;
      let edited_a : String = a_view . replace (
        "A links to [[id:X][X]]", "[[id:X][X]] [[id:A][A]]");
      let (b_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("B"), false) ?;
      let dirty_b : String = format! ("{}\nlocal edit", b_view);
      let a_path = config . sources . values () . next () . unwrap ()
        . path . join ("A.skg");
      let a_before : Vec<u8> = fs::read (&a_path) ?;

      let (mut stream, read_end) = mk_pair ();
      let result = update_from_and_rerender_buffer_with_snapshots_test (
        &mut stream, &edited_a, config, tantivy, &graph, false,
        &Ok (saved_uri . clone ()), &mut views_state,
        &[dirty_snapshot ("client-only-b", Some (&b_view), &dirty_b)])
        . await;
      drop (stream);
      let messages = read_all_lp_messages (&mut BufReader::new (read_end));
      let error = match result {
        Ok (_) => panic! (
          "B's shared link target X must conflict with A's neighborhood"),
        Err (error) => error,
      };
      let message : String = error_text (error . as_ref ());
      assert! (message . contains ("client-only-b")
               && message . contains ("X"), "{}", message);
      assert_eq! (fs::read (&a_path) ?, a_before,
                  "a refused save must not write its file");
      assert_eq! (
        nodecomplete_from_graph (&graph . load_full (), &ID::from ("A"))
          . unwrap () . title,
        "A links to [[id:X][X]]");
      assert! (messages . is_empty (),
               "no relaxation or collateral update precedes refusal: {:?}",
               messages);

      let (c_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("C"), false) ?;
      let dirty_c : String = format! ("{}\nindependent edit", c_view);
      // The collateral fixture contains only endpoint X as an active node.
      // Reaching it therefore proves the A -> X ordinary graph hop, rather
      // than shared ancestry supplied by a larger rendered view.
      let x_uri : ViewUri = ViewUri::ContentView ("endpoint-x" . into ());
      let (_, x_pids, x_forest) = single_root_view (
        config, Some (tantivy), &ID::from ("X"), false) ?;
      views_state . open_views . register_view (
        &graph . load_full (), x_uri, x_forest, &x_pids);
      let (mut stream, read_end) = mk_pair ();
      let response = update_from_and_rerender_buffer_with_snapshots_test (
        &mut stream, &edited_a, config, tantivy, &graph, false,
        &Ok (saved_uri), &mut views_state,
        &[dirty_snapshot ("client-only-c", Some (&c_view), &dirty_c)])
        . await ?;
      drop (stream);
      assert! (response . errors . is_empty ());
      let messages = read_all_lp_messages (&mut BufReader::new (read_end));
      assert_eq! (messages . len (), 2, "{:?}", messages);
      assert! (messages [0] . contains ("save-relax-lock")
               && messages [0] . contains ("client-only-c")
               && messages [0] . contains ("endpoint-x"),
               "dirty input remains in narrowed lock set: {:?}", messages);
      let collateral : &String = messages . iter ()
        . find (|message| message . contains ("collateral-view"))
        . expect ("X must receive a collateral view update");
      let collateral_text = extract_string_field_from_sexp (
        collateral, "content") . unwrap ();
      assert! (collateral_text . contains ("(interesting 1)"),
               "X's interesting-link herald must reflect A's new target: {}",
               collateral_text);
      assert_eq! (
        nodecomplete_from_graph (&graph . load_full (), &ID::from ("A"))
          . unwrap () . title,
        "[[id:X][X]] [[id:A][A]]");
      Ok (( ))
    }))
}

#[test]
fn missing_or_unparseable_dirty_baseline_refuses () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-save-dirty-invalid",
    "tests/save_dirty_conflicts/fixtures",
    "/tmp/tantivy-test-save-dirty-invalid",
    |config, tantivy| Box::pin (async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let (a_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("A"), false) ?;
      for snapshot in [
        dirty_snapshot ("missing-baseline", None, "* new node"),
        dirty_snapshot (
          "malformed-baseline", Some ("** starts too deep"), "* new node"),
      ] {
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views        : OpenViews::new (),
        };
        let (mut stream, _) = mk_pair ();
        let result = update_from_and_rerender_buffer_with_snapshots_test (
          &mut stream, &a_view, config, tantivy, &graph, false,
          &Ok (ViewUri::ContentView ("saved-a" . into ())),
          &mut views_state, &[snapshot]) . await;
        let error = match result {
          Ok (_) => panic! ("invalid dirty snapshot should refuse the save"),
          Err (error) => error,
        };
        let message : String = error_text (error . as_ref ());
        assert! (message . contains ("recovery command")
                 || message . contains (
                   "skg-show-unsaved-changes"), "{}", message); }
      Ok (( ))
    }))
}
