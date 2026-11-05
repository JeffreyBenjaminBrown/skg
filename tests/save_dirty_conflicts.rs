use std::error::Error;

use crate::tcp_stream_pair::mk_test_tcp_stream_pair;
use std::fs;
use std::io::BufReader;

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::dbs::node_lookup::graphnode_from_graph;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::ClientViewSnapshot;
use skg::test_utils::{
  extract_string_field_from_sexp, graph_handle_from_config,
  read_all_lp_messages, run_with_test_stores,
  update_from_and_rerender_buffer_with_snapshots_test,
};
use skg::test_utils::single_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::errors::SaveError;
use skg::types::save::format_save_error_as_org;
use skg::types::views_state::{OpenViews, ViewforestId};

fn dirty_buffer_snapshot (
  viewforest_id : &str,
  baseline : Option<&str>,
  current  : &str,
) -> ClientViewSnapshot {
  ClientViewSnapshot {
    viewforest_id : ViewforestId::ContentView (viewforest_id . into ()),
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
        open_views        : OpenViews::new (),
        graph_straddle    : None,
      };
      let saved_viewforest_id : ViewforestId = ViewforestId::ContentView ("saved-a" . into ());
      let (a_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("A")) ?;
      let edited_a : String = a_view . replace (
        "A links to [[id:X][X]]", "[[id:X][X]] [[id:A][A]]");
      let (x_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("X")) ?;
      let dirty_x : String = format! ("{}\nlocal edit", x_view);
      let a_path = config . skgrepos . values () . next () . unwrap ()
        . path . join ("A.skg");
      let a_before : Vec<u8> = fs::read (&a_path) ?;

      let (mut stream, read_end) = mk_test_tcp_stream_pair ();
      let result = update_from_and_rerender_buffer_with_snapshots_test (
        &mut stream, &edited_a, config, tantivy, &graph,
        &Ok (saved_viewforest_id . clone ()), &mut views_state,
        &[dirty_buffer_snapshot ("client-only-x", Some (&x_view), &dirty_x)])
        . await;
      drop (stream);
      let messages = read_all_lp_messages (&mut BufReader::new (read_end));
      let error = match result {
        Ok (_) => panic! (
          "a dirty view showing A's link target X must conflict with A's neighborhood"),
        Err (error) => error,
      };
      let message : String = error_text (error . as_ref ());
      assert! (message . contains ("client-only-x")
               && message . contains ("X"), "{}", message);
      assert_eq! (fs::read (&a_path) ?, a_before,
                  "a refused save must not write its file");
      assert_eq! (
        graphnode_from_graph (&graph . load_full (), &ID::from ("A"))
          . unwrap () . title,
        "A links to [[id:X][X]]");
      assert! (messages . is_empty (),
               "no relaxation or collateral update precedes refusal: {:?}",
               messages);

      let (c_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("C")) ?;
      let dirty_c : String = format! ("{}\nindependent edit", c_view);
      // The collateral fixture contains only endpoint X as an occurrence.
      // Reaching it therefore proves the A -> X ordinary graph hop, rather
      // than shared ancestry supplied by a larger rendered view.
      let x_viewforest_id : ViewforestId = ViewforestId::ContentView ("endpoint-x" . into ());
      let (_, x_pids, x_forest) = single_root_view (
        config, Some (tantivy), &ID::from ("X")) ?;
      views_state . open_views . register_view (
        &graph . load_full (), x_viewforest_id, x_forest, &x_pids, &SkgrepoRestriction::unrestricted (config));
      let (mut stream, read_end) = mk_test_tcp_stream_pair ();
      let response = update_from_and_rerender_buffer_with_snapshots_test (
        &mut stream, &edited_a, config, tantivy, &graph,
        &Ok (saved_viewforest_id), &mut views_state,
        &[dirty_buffer_snapshot ("client-only-c", Some (&c_view), &dirty_c)])
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
      assert! (collateral_text . contains ("(substantive 1)"),
               "X's interesting-link herald must reflect A's new target: {}",
               collateral_text);
      assert_eq! (
        graphnode_from_graph (&graph . load_full (), &ID::from ("A"))
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
        config, Some (tantivy), &ID::from ("A")) ?;
      for buffer_snapshot in [
        dirty_buffer_snapshot ("missing-baseline", None, "* new node"),
        dirty_buffer_snapshot (
          "malformed-baseline", Some ("** starts too deep"), "* new node"),
      ] {
        let mut views_state : ViewsState = ViewsState {
          open_views        : OpenViews::new (),
          graph_straddle    : None,
        };
        let (mut stream, _) = mk_test_tcp_stream_pair ();
        let result = update_from_and_rerender_buffer_with_snapshots_test (
          &mut stream, &a_view, config, tantivy, &graph,
          &Ok (ViewforestId::ContentView ("saved-a" . into ())),
          &mut views_state, &[buffer_snapshot]) . await;
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

/// Saves 'saved_text' as the view 'saved-view' while one other client
/// view, 'dirty_view', is dirty: its baseline is 'dirty_baseline' and its
/// unsaved text 'dirty_current'. No view is registered.
async fn save_beside_one_dirty_view (
  config         : &SkgConfig,
  tantivy        : &TantivyIndex,
  graph          : &InRustGraphHandle,
  saved_text     : &str,
  dirty_view     : &str,
  dirty_baseline : &str,
  dirty_current  : &str,
) -> Result<(), Box<dyn Error>> {
  let mut views_state : ViewsState = ViewsState {
    open_views        : OpenViews::new (),
    graph_straddle    : None,
  };
  let (mut stream, _) = mk_test_tcp_stream_pair ();
  update_from_and_rerender_buffer_with_snapshots_test (
    &mut stream, saved_text, config, tantivy, graph,
    &Ok (ViewforestId::ContentView ("saved-view" . into ())),
    &mut views_state,
    &[dirty_buffer_snapshot (dirty_view, Some (dirty_baseline), dirty_current)])
    . await ?;
  Ok (( ))
}

/// A dirty view of C whose only mention of B is a merge request on C
/// does not depend on B, so a save that retitles B succeeds.
#[test]
fn a_merge_request_target_is_not_a_dirty_view_dependency
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-save-dirty-merge-target",
    "tests/save_dirty_conflicts/fixtures",
    "/tmp/tantivy-test-save-dirty-merge-target",
    |config, tantivy| Box::pin (async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let (c_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("C")) ?;
      let c_requesting_merge : String = c_view . replacen (
        "(skg ", "(skg (editRequest (merge B)) ", 1);
      assert_ne! (c_requesting_merge, c_view);
      let (b_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("B")) ?;
      let retitled_b : String = b_view . replace (
        "B links to [[id:X][X]]", "B, retitled, links to [[id:X][X]]");
      save_beside_one_dirty_view (
        config, tantivy, &graph, &retitled_b,
        "merge-requesting-c", &c_view, &c_requesting_merge) . await
        . map_err (|error| error_text (error . as_ref ())) ?;
      assert_eq! (
        graphnode_from_graph (&graph . load_full (), &ID::from ("B"))
          . unwrap () . title,
        "B, retitled, links to [[id:X][X]]");
      Ok (( ))
    }))
}

/// A dirty view of C whose only mention of X is a link in C's unsaved
/// body does not depend on X, so a save that retitles X succeeds.
#[test]
fn a_link_target_is_not_a_dirty_view_dependency
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-save-dirty-link-target",
    "tests/save_dirty_conflicts/fixtures",
    "/tmp/tantivy-test-save-dirty-link-target",
    |config, tantivy| Box::pin (async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let (c_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("C")) ?;
      let c_linking_x : String = format! (
        "{}\nsee [[id:X][X]]", c_view . trim_end ());
      let (x_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("X")) ?;
      let retitled_x : String = x_view . replace (
        "shared X", "shared X, retitled");
      save_beside_one_dirty_view (
        config, tantivy, &graph, &retitled_x,
        "x-linking-c", &c_view, &c_linking_x) . await
        . map_err (|error| error_text (error . as_ref ())) ?;
      assert_eq! (
        graphnode_from_graph (&graph . load_full (), &ID::from ("X"))
          . unwrap () . title,
        "shared X, retitled");
      Ok (( ))
    }))
}

/// A dirty view that shows X still depends on X, so a save that
/// retitles X is refused, naming that view and X, and writes nothing.
#[test]
fn a_dirty_view_showing_a_graphnode_refuses_a_save_of_it
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-save-dirty-shown-target",
    "tests/save_dirty_conflicts/fixtures",
    "/tmp/tantivy-test-save-dirty-shown-target",
    |config, tantivy| Box::pin (async move {
      let graph : InRustGraphHandle = graph_handle_from_config (config) ?;
      let (x_view, _, _) = single_root_view (
        config, Some (tantivy), &ID::from ("X")) ?;
      let dirty_x : String = format! ("{}\nlocal edit", x_view . trim_end ());
      let retitled_x : String = x_view . replace (
        "shared X", "shared X, retitled");
      let x_path = config . skgrepos . values () . next () . unwrap ()
        . path . join ("X.skg");
      let x_before : Vec<u8> = fs::read (&x_path) ?;
      let message : String = match save_beside_one_dirty_view (
        config, tantivy, &graph, &retitled_x,
        "showing-x", &x_view, &dirty_x) . await
      { Ok (( )) => panic! ("a dirty view showing X must refuse a save of X"),
        Err (error) => error_text (error . as_ref ()), };
      assert! (message . contains ("showing-x [X]"), "{}", message);
      assert_eq! (fs::read (&x_path) ?, x_before,
                  "a refused save must not write its file");
      assert_eq! (
        graphnode_from_graph (&graph . load_full (), &ID::from ("X"))
          . unwrap () . title,
        "shared X");
      Ok (( ))
    }))
}
