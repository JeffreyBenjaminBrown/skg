// cargo nextest run --test grouped_overrides -E 'test(collateral_new_link::)'
//
// The collateral redraw of clean views matches through links
// ('SaveAffectedIds::matching_identity' reads the full update-relevant
// neighborhood), though the dirty-view check does not
// ('tests/save_dirty_conflicts.rs').

use std::error::Error;

use crate::git_fixture_helpers::save_and_read_collateral;

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::serve::ViewsState;
use skg::test_utils::{
  graph_handle_from_config, run_with_test_stores, single_root_view};
use skg::types::misc::ID;
use skg::types::views_state::{OpenViews, ViewforestId};

/// A clean view of A shows A's mentionerFolder, holding M. A save of
/// X's view whose only change is a link to A redraws A's view
/// collaterally, X joining M in the folder.
#[test]
fn a_clean_view_of_a_graphnode_is_redrawn_when_another_gains_a_link_to_it
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-collateral-new-link",
    "tests/collateral_new_link/fixtures",
    "/tmp/tantivy-test-collateral-new-link",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views        : OpenViews::new (),
        graph_straddle    : None, };
      let a_viewforest_id : ViewforestId =
        ViewforestId::ContentView ("new-link-A" . into ());
      let x_viewforest_id : ViewforestId =
        ViewforestId::ContentView ("new-link-X" . into ());
      let (a_view, _a_pids, _a_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("A") ) ?;
      let (a_response, _) =
        save_and_read_collateral (
          &a_view . replacen (
            "node ", "node (viewRequests (folder mentioners))", 1 ),
          &a_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( a_response . errors . is_empty ()
                && a_response . saved_view . contains ("mentionerFolder")
                && a_response . saved_view . contains ("(gnid M)")
                && ! a_response . saved_view . contains ("(gnid X)"),
        "A's mentionerFolder should hold M alone:\n{}",
        a_response . saved_view );
      let (x_view, _x_pids, _x_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("X") ) ?;
      let (response, collateral_views) =
        save_and_read_collateral (
          &format! ("{}\nsee [[id:A][A]]", x_view . trim_end ()),
          &x_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (),
        "X's save should succeed: {:?}", response . errors );
      assert_eq! ( collateral_views . len (), 1,
        "exactly A's view should be collateral: {:?}", collateral_views );
      let a_collateral : &str = &collateral_views[0];
      assert! ( a_collateral . contains ("mentionerFolder")
                && a_collateral . contains ("(gnid M)")
                && a_collateral . contains ("(gnid X)"),
        "A's redrawn mentionerFolder should hold M and X:\n{}",
        a_collateral );
      Ok (( )) } )) }
