// cargo nextest run --test grouped_overrides -E 'test(collateral_subscribee_staleness::)'
//
// Regression for a collateral-rerender staleness. A editable subscriber
// open in two views: changing its subscriptions in one view and saving must
// refresh the other (collateral) view's subscribeeFolder from the graph,
// not leave it claiming the old subscription.
//
// Installs the explicit graph handle (the subscribee lookups read
// the captured graph), so it lives among the grouped_overrides installers.

use std::error::Error;

use crate::git_fixture_helpers::{
  save_and_read_collateral, without_lines_containing};

use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::test_utils::{
  run_with_test_stores, graph_handle_from_config};
use skg::test_utils::single_root_view;
use skg::serve::ViewsState;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::types::misc::ID;

#[test]
fn collateral_editable_subscriber_subscribeeFolder_refreshes
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-collateral-subscribee-staleness",
    "tests/collateral_subscribee_staleness/fixtures",
    "/tmp/tantivy-test-collateral-subscribee-staleness",
    |config, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views        : OpenViews::new (),
        graph_straddle    : None, };
      let a_viewforest_id : ViewforestId = ViewforestId::ContentView ("S-a" . into ());
      let b_viewforest_id : ViewforestId = ViewforestId::ContentView ("S-b" . into ());

      // View A rooted at S: S is an editable root, its subscribeeFolder
      // shows both subscribees M and N.
      let (a_view, a_pids, a_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("S") ) ?;
      assert! ( a_view . contains ("subscribeeFolder")
                && a_view . contains ("(gnid M)")
                && a_view . contains ("(gnid N)"),
        "view A should show M and N in S's subscribeeFolder:\n{}", a_view );
      views_state . open_views . register_view (
        &graph . load_full (), a_viewforest_id . clone (), a_vf, &a_pids );

      // View B rooted at S too -- the collateral view. S is editable
      // here as well (it is the root).
      let (b_view, b_pids, b_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("S") ) ?;
      assert! ( b_view . contains ("(gnid M)")
                && b_view . contains ("(gnid N)"),
        "view B should show M and N in S's subscribeeFolder:\n{}", b_view );
      views_state . open_views . register_view (
        &graph . load_full (), b_viewforest_id . clone (), b_vf, &b_pids );

      // In view A, drop N from S's subscribeeFolder (subscribesTo -> [M])
      // and save A.
      let edited_a : String = without_lines_containing (&a_view, "(gnid N)");
      let (_response, collateral_views) =
        save_and_read_collateral (
          &edited_a, &a_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;

      // The collateral view B must refresh from the just-saved graph:
      // its subscribeeFolder still shows M, and N, no longer a
      // subscribee, is kept but claims nothing and is unwarranted, even
      // though S is editable there (the root).
      assert_eq! ( collateral_views . len (), 1,
        "exactly view B should be collateral: {:?}", collateral_views );
      let b_collateral : &str = &collateral_views[0];
      assert! ( b_collateral . contains ("(gnid M)"),
        "M must remain in B's subscribeeFolder:\n{}", b_collateral );
      let n_line : &str = b_collateral . lines ()
        . find ( |line| line . contains ("(gnid N)") )
        . unwrap_or_else ( || panic! ( "N is kept:\n{}", b_collateral ));
      assert! ( n_line . contains ("noClaim") && n_line . contains ("noWarrant"),
        "N must be a non-claiming, unwarranted child of B's subscribeeFolder \
         (editable subscriber must refresh from the graph):\n{}",
        b_collateral );
      Ok (( )) } )) }
