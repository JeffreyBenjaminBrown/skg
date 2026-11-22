// cargo nextest run --test grouped_forks -E 'test(collateral_partner_folder::)'
//
// Two cells of the relationship matrix ('docs/sharing-model.org', "The
// write-protected folders: who hides this node") that
// need an open collateral view and the collateral rerender
// stream:
//
// 1. collateral folder-membership update: a view of N shows a
//    forksFolder with member S; a save of a second view that drops
//    S's fork of N must re-render N's view (collaterally) with
//    S unwarranted in its forksFolder.
//
// 2. warning scoping: what the refresh of the collateral view changes
//    is never a warning to the saver.

use std::error::Error;

use crate::git_fixture_helpers::{
  save_and_read_collateral, without_subtrees_containing};

use skg::skgrepo_sets::SkgrepoRestriction;
use skg::dbs::in_rust_graph::{
  InRustGraphHandle};
use skg::test_utils::{
  empty_views_state,
  run_with_test_stores, graph_handle_from_config};
use skg::test_utils::single_root_view;
use skg::test_utils::single_root_view_text;
use skg::serve::ViewsState;
use skg::types::views_state::ViewforestId;
use skg::types::misc::ID;

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
      let mut views_state : ViewsState = empty_views_state ();
      let n_viewforest_id : ViewforestId = ViewforestId::ContentView ("collat-N" . into ());
      let s_viewforest_id : ViewforestId = ViewforestId::ContentView ("collat-S" . into ());

      // Render N, explicitly request its forks folder, and save it
      // under the view ID that will be updated collaterally.  forksFolder
      // is no longer part of the default presentation.
      let n_view : String =
        single_root_view_text (
          config, Some (tantivy), &ID::from ("N") ) ?;
      let n_folder_request : String = n_view . replacen (
        "node ",
        "node (viewRequests (folder forks))", 1 );
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
      assert! ( n_view . contains ("forksFolder")
                && n_view . contains ("(gnid S)"),
        "N's view should show S in a forksFolder:\n{}", n_view );

      // Render S (forksFolder shows N) and register it as the saved
      // view.
      let (s_view, s_pids, s_vf) =
        single_root_view (
          config, Some (tantivy), &ID::from ("S") ) ?;
      assert! ( s_view . contains ("forksFolder")
                && s_view . contains ("(gnid N)"),
        "S's view should show N in a forksFolder:\n{}", s_view );
      views_state . open_views . register_view (
        &graph . load_full (), s_viewforest_id . clone (), s_vf, &s_pids, &SkgrepoRestriction::unrestricted (config) );

      // Drop N from S's forksFolder (emptying it = explicit empty
      // set) and save S's view.
      let edited_s : String = without_subtrees_containing (&s_view, "(gnid N)");
      let (response, collateral_views) =
        save_and_read_collateral (
          &edited_s, &s_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;

      // CELL 1 -- collateral folder-membership update: N's view is
      // re-rendered with S, no longer a forker, kept in N's
      // forksFolder, unwarranted (the refresh keeps what the
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
        "S must be unwarranted in N's forksFolder collaterally:\n{}",
        n_collateral );

      // CELL 2 -- N's forksFolder changed in the collateral
      // stream, and nothing of that is a warning.
      assert! (
        response . warnings . is_empty (),
        "a collateral change must not warn the saver: {:?}",
        response . warnings );
      Ok (( )) } )) }

/// A member the refresh adds to a folder at a later visit gets its
/// containerward chain. N's view shows S in its forksFolder; C
/// contains T. A save of T's view that makes T fork N
/// rerenders N's view collaterally, its refresh adding T to the folder,
/// under which hangs T's chain, in a containers folder drawn folded (a
/// member of a forks folder's chain stands in one): C, write-protected,
/// claiming nothing, folded.
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
      let mut views_state : ViewsState = empty_views_state ();
      let n_viewforest_id : ViewforestId = ViewforestId::ContentView ("chain-N" . into ());
      let t_viewforest_id : ViewforestId = ViewforestId::ContentView ("chain-T" . into ());
      let n_view : String =
        single_root_view_text (
          config, Some (tantivy), &ID::from ("N") ) ?;
      let (n_response, _) =
        save_and_read_collateral (
          &n_view . replacen (
            "node ", "node (viewRequests (folder forks))", 1 ),
          &n_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( n_response . errors . is_empty ()
                && n_response . saved_view . contains ("(gnid S)")
                && ! n_response . saved_view . contains ("(gnid T)"),
        "N's forksFolder should hold S alone:\n{}",
        n_response . saved_view );
      let (response, collateral_views) =
        save_and_read_collateral (
          "* (skg node (gnid T) (skgRepos (nodeRepo main))) T\n\
           ** (skg forksFolder)\n\
           *** (skg node (gnid N) (skgRepos (nodeRepo main)) writeProtected) N\n",
          &t_viewforest_id, config, tantivy, &graph,
          &mut views_state ) . await ?;
      assert! ( response . errors . is_empty (),
        "T's save should succeed: {:?}", response . errors );
      let n_collateral : &String = collateral_views . iter ()
        . find ( |view| view . contains ("forksFolder") )
        . unwrap_or_else ( || panic! (
          "N's view should be collateral: {:?}", collateral_views ));
      let lines : Vec<&str> = n_collateral . lines () . collect ();
      let t_index : usize = lines . iter ()
        . position ( |line| line . starts_with ("*** ") && line . contains ("(gnid T)") )
        . unwrap_or_else ( || panic! (
          "T should be a member of N's forksFolder:\n{}", n_collateral ));
      let containers_folder : &str = lines . get (t_index + 1) . copied () . unwrap_or ("");
      let chain_node : &str = lines . get (t_index + 2) . copied () . unwrap_or ("");
      assert! ( containers_folder . starts_with ("**** ")
                && containers_folder . contains ("containerFolder")
                && chain_node . starts_with ("***** ") && chain_node . contains ("(gnid C)")
                && ! chain_node . contains ("noClaim") // under a write-protected parent
                && chain_node . contains ("writeProtected")
                && chain_node . contains ("folded"),
        "T should hold its chain, C, in a folded containers folder:\n{}", n_collateral );
      Ok (( )) } )) }
