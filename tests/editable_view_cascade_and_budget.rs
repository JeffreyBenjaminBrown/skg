// cargo nextest run --test grouped_views -E 'test(editable_view_cascade_and_budget::)'
//
// Tests for editable view request extension.
// These tests verify that when a node has a "Editable" view request,
// its content children are extended from disk using BFS with truncation.

use crate::git_fixture_helpers::mk_test_tcp_stream_pair;
use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle,
  };
use skg::assert_metadata_eq;
use skg::metadata_canon::{viewnode_id_atom_of, with_viewnode_id_atom, without_viewnode_ids};
use skg::test_utils::{run_with_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::org_to_text::viewforest_to_string;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::{OpenViews, ViewforestId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;



const FIXTURES : &str =
  "tests/editable_view_cascade_and_budget/fixtures";

#[test]
fn editable_view_ample_budget (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-ample-budget", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-ample-budget",
    |config, tantivy| Box::pin ( test_editable_view_ample_budget (config, tantivy) )) }

#[test]
fn editable_view_limit_5_or_6 (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-limit-5-or-6", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-limit-5-or-6",
    |config, tantivy| Box::pin ( test_editable_view_limit_5_or_6 (config, tantivy) )) }

#[test]
fn editable_view_limit_1_to_4 (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-limit-1-to-4", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-limit-1-to-4",
    |config, tantivy| Box::pin ( test_editable_view_limit_1_to_4 (config, tantivy) )) }

#[test]
fn editable_view_conflicting (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-conflicting", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-conflicting",
    |config, tantivy| Box::pin ( test_editable_view_conflicting (config, tantivy) )) }

#[test]
fn editable_view_with_cycle (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-with-cycle", "tests/editable_view_cascade_and_budget/fixtures-cycle",
    "/tmp/tantivy-test-evcb-editable-view-with-cycle",
    |config, tantivy| Box::pin ( test_editable_view_with_cycle (config, tantivy) )) }

#[test]
fn editable_view_with_repeat (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-with-repeat", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-with-repeat",
    |config, tantivy| Box::pin ( test_editable_view_with_repeat (config, tantivy) )) }

#[test]
fn editable_view_request_cleared (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-request-cleared", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-request-cleared",
    |config, tantivy| Box::pin ( test_editable_view_request_cleared (config, tantivy) )) }

#[test]
fn editable_view_request_on_an_editable_node (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-request-on-an-editable-node", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-request-on-an-editable-node",
    |config, tantivy| Box::pin ( test_editable_view_request_on_an_editable_node (config, tantivy) )) }

#[test]
fn editable_view_request_moves_editability_back (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-editable-view-request-moves-editability-back", FIXTURES,
    "/tmp/tantivy-test-evcb-editable-view-request-moves-editability-back",
    |config, tantivy| Box::pin ( test_editable_view_request_moves_editability_back (config, tantivy) )) }

#[test]
fn writeProtect_request_keeps_children_editable (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-writeProtect-request-keeps-children-editable", FIXTURES,
    "/tmp/tantivy-test-evcb-writeProtect-request-keeps-children-editable",
    |config, tantivy| Box::pin ( test_writeProtect_request_keeps_children_editable (config, tantivy) )) }

#[test]
fn writeProtect_subtree_request (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-writeProtect-subtree-request", FIXTURES,
    "/tmp/tantivy-test-evcb-writeProtect-subtree-request",
    |config, tantivy| Box::pin ( test_writeProtect_subtree_request (config, tantivy) )) }

#[test]
fn writeProtect_request_on_a_writeProtected_occurrence (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-writeProtect-request-on-a-writeProtected-occurrence", FIXTURES,
    "/tmp/tantivy-test-evcb-writeProtect-request-on-a-writeProtected-occurrence",
    |config, tantivy| Box::pin ( test_writeProtect_request_on_a_writeProtected_occurrence (config, tantivy) )) }

#[test]
fn writeProtect_and_editable_requests_move_editability_and_back (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-writeProtect-and-editable-requests-move-editability-and-back", FIXTURES,
    "/tmp/tantivy-test-evcb-writeProtect-and-editable-requests-move-editability-and-back",
    |config, tantivy| Box::pin ( test_writeProtect_and_editable_requests_move_editability_and_back (config, tantivy) )) }

#[test]
fn retained_content_node_is_shown_once (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-retained-content-node-is-shown-once", FIXTURES,
    "/tmp/tantivy-test-evcb-retained-content-node-is-shown-once",
    |config, tantivy| Box::pin ( test_retained_content_node_is_shown_once (config, tantivy) )) }

#[test]
fn a_request_claimed_elsewhere_leaves_no_member (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-a-request-claimed-elsewhere-leaves-no-member", FIXTURES,
    "/tmp/tantivy-test-evcb-a-request-claimed-elsewhere-leaves-no-member",
    |config, tantivy| Box::pin ( test_a_request_claimed_elsewhere_leaves_no_member (config, tantivy) )) }

#[test]
fn budget_aliasfolder_is_neutral (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-budget-aliasfolder-is-neutral", "tests/editable_view_cascade_and_budget/fixtures-aliases",
    "/tmp/tantivy-test-evcb-budget-aliasfolder-is-neutral",
    |config, tantivy| Box::pin ( test_budget_aliasfolder_is_neutral (config, tantivy) )) }

#[test]
fn budget_spent_on_a_node_with_members (
) -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-evcb-budget-spent-on-a-node-with-members", FIXTURES,
    "/tmp/tantivy-test-evcb-budget-spent-on-a-node-with-members",
    |config, tantivy| Box::pin ( test_budget_spent_on_a_node_with_members (config, tantivy) )) }

// ===================================================
// Test: Editable view with an ample budget -> everything extends.
// §5.5 budget counts vognode extensions (cost 1 each); the full subtree here is
// 12 extensions (1,2,11,12,13,121,122,123,124,1211,1212,1221), so a budget of
// 20 is ample and nothing is left write-protected.
// ===================================================
async fn test_editable_view_ample_budget (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
        ** (skg node (gnid 11)) 11
        ** (skg node (gnid 12) writeProtected (viewRequests editableView)) 12
        ** (skg node (gnid 13)) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 20;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with ample budget (20):\n{}", result);

      // With an ample budget, all children should be extended
      let expected = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        12 body
        *** (skg node (gnid 121) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1)))) 121
        121 body
        **** (skg node (gnid 1211) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1211
        1211 body
        **** (skg node (gnid 1212) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1212
        1212 body
        *** (skg node (gnid 122) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
        122 body
        **** (skg node (gnid 1221) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1221
        1221 body
        *** (skg node (gnid 123) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
        123 body
        *** (skg node (gnid 124) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
        124 body
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      assert_metadata_eq!(result, expected,
        "Editable view with an ample budget should extend all children");

      Ok (( )) }

// ===================================================
// Test: Editable view with limit=5 vs limit=6
// ===================================================
// §5.5 budget is granular: limit=5 spends its last unit on 1211 (1212 not
// created); limit=6 affords both 1211 and 1212. Nodes whose EVR is reached
// after the budget is spent are stripped to write-protected. (No sibling-group
// padding: the two limits now differ by one created node.)

async fn test_editable_view_limit_5_or_6 (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
        ** (skg node (gnid 11)) 11
        ** (skg node (gnid 12) writeProtected (viewRequests editableView)) 12
        ** (skg node (gnid 13)) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      let result_5 = {
        let mut config5 = config . clone();
        config5 . initial_node_limit = 5;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response_5 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config5, tantivy, &graph,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response_5 . saved_view };
      let result_6 = {
        let mut config6 = config . clone();
        config6 . initial_node_limit = 6;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response_6 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config6, tantivy, &graph,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response_6 . saved_view };

      println!("Result with limit=5:\n{}", result_5);
      println!("Result with limit=6:\n{}", result_6);

      // §5.5: the budget counts vognode extensions (cost 1 each), in BFS order.
      // The extension order here is 1, 2, 11, 12, 13, 121, 122, ... A folder fills
      // whole for free, so a node's whole content group is always shown -- the
      // budget only governs how many of those children then extend in turn; the
      // rest stay write-protected (visible, collapsed). Never a partial group.
      //
      // limit=5: extensions 1,2,11,12,13 spend the budget. 12 (the 4th) extended
      // and showed its whole group 121..124, but the budget hit 0 at 13, so when
      // 121..124 are visited they stay write-protected (none extends -> no gen-3).
      let expected_5 = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        12 body
        *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1)))) 121
        *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
        *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
        *** (skg node (gnid 124) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};
      // limit=6: one more extension than limit=5 -- 121 (the 6th) now extends and
      // shows its whole gen-3 group 1211,1212 (both then write-protected, budget
      // spent); 122..124 remain write-protected.
      let expected_6 = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        12 body
        *** (skg node (gnid 121) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1)))) 121
        121 body
        **** (skg node (gnid 1211) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1211
        **** (skg node (gnid 1212) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1212
        *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
        *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
        *** (skg node (gnid 124) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      assert_metadata_eq!(result_5, expected_5,
        "limit=5: 121 extends one gen-3 child (1211), rest writeProtected (§5.5)");
      assert_metadata_eq!(result_6, expected_6,
        "limit=6: 121 extends both gen-3 children, rest writeProtected (§5.5)");

      Ok (( )) }

// ===================================================
// Test: Editable view with limit=1 or limit=4
// ===================================================
// Gen 2 has 4 children (121, 122, 123, 124).
// Any limit from 1 to 4 hits the limit on the first generation,
// so the sibling group is completed, all are write-protected,
// and the grandchild generation is not visited at all.

async fn test_editable_view_limit_1_to_4 (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
        ** (skg node (gnid 11)) 11
        ** (skg node (gnid 12) writeProtected (viewRequests editableView)) 12
        ** (skg node (gnid 13)) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      let result_1 = {
        let mut config1 = config . clone();
        config1 . initial_node_limit = 1;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response_1 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config1, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response_1 . saved_view };
      let result_4 = {
        let mut config4 = config . clone();
        config4 . initial_node_limit = 4;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response_4 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config4, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response_4 . saved_view };

      println!("Result with limit=1:\n{}", result_1);
      println!("Result with limit=4:\n{}", result_4);

      // §5.5: the budget counts vognode extensions (cost 1 each), in BFS order:
      // 1, 2, 11, 12, 13, then 12's content 121..124, ... A view root is never
      // truncated (the user opened it), so roots 1 and 2 always extend; other
      // vognodes reached after the budget is spent are left write-protected.
      //
      // limit=1: root 1 extends (budget 1->0) and shows its whole group 11,12,13;
      // all three are then write-protected (budget spent), so 12 never extends and
      // none of 121.. is created. Root 2 still extends (root exemption).
      let expected_1 = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};
      // omittedBody marks only 12: the saved buffer showed 11 and 13
      // editable with no body text, wiping their bodies, while
      // write-protected 12 kept its body -- which this render hides.
      // limit=4: extensions 1, 2, 11, 12 spend the budget. 12 (the 4th) showed its
      // whole group 121..124, all write-protected (budget spent); 13 is reached
      // after the budget is gone, so it too is write-protected.
      let expected_4 = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        12 body
        *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1)))) 121
        *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
        *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
        *** (skg node (gnid 124) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
        * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
      "};

      assert_metadata_eq!(result_1, expected_1,
        "limit=1 creates only the first gen-2 child, writeProtected (§5.5)");
      assert_metadata_eq!(result_4, expected_4,
        "limit=4 creates all four gen-2 children, all writeProtected (§5.5)");

      Ok (( )) }

// ===================================================
// Test: Editable view on node that conflicts with existing editable
// ===================================================
// If node X is already editable in the tree, and we request
// editable view on a second occurrence of X, the first should become
// write-protected.

async fn test_editable_view_conflicting (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Node 12 appears twice:
      // - First as a regular child of 1 (will be editable after completion)
      // - Second with an editable view request
      let input_org_text = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
        ** (skg node (gnid 12)) 12
        *** (skg node (gnid 122) writeProtected omittedBody) 122
        * (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with conflict:\n{}", result);

      let expected = indoc! {
        // The first 12 (child of 1) should become write-protected,
        // its child 122 no longer a member.
        // The second 12 (root with request) should be extended.
        // NOTE: The first 12 redefines the children of 12 as [122]
        // rather than [121,122,123,124].
        "* (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 1)))) 1
         ** (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 12
         *** (skg node (gnid 122) (skgRepos (nodeRepo main)) noClaim writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)))) 122
         * (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1) (out 1)))) 12
         ** (skg node (gnid 122) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
         122 body
         *** (skg node (gnid 1221) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1221
         1221 body
      "};

      assert_metadata_eq!(result, expected,
        "First editable occurrence should become write-protected when second requests editable");

      Ok (( )) }

// ===================================================
// Test: Editable view extension detects cycles
// ===================================================
// Create fixtures for a cycle: a contains b contains a

async fn test_editable_view_with_cycle (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Node a has editable request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg node (gnid cyc-a) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) cyc-a
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with cycle:\n{}", result);

      // a should extend to show b, and b's child a should be marked as cycle
      let expected = indoc! {"
        * (skg node (gnid cyc-a) (skgRepos (nodeRepo main)) (rels (contains (in 1) (out 1)))) cyc-a
        cyc-a body
        ** (skg node (gnid cyc-b) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 1 (ancestors 1))) (birth (contains in 1)))) cyc-b
        cyc-b body
        *** (skg node (gnid cyc-a) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1 (ancestors 1))) (birth (contains in 1))) (viewStats cycle)) cyc-a
      "};

      assert_metadata_eq!(result, expected,
        "Editable view should detect cycles and mark them");

      Ok (( )) }

// ===================================================
// Test: Editable view extension detects repeats
// ===================================================
// If a node is already visited (editable elsewhere), it should be
// marked write-protected when encountered again during extension.

async fn test_editable_view_with_repeat (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid 121) (skgRepos (nodeRepo main))) 121
        * (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with repeat:\n{}", result);

      let expected = indoc! {
        // Upon saving, the write-protected view of node 12 had no effect, but the
        // editable view of 121 (editable in the *buffer*) deleted its
        // children at extraction. In the rerender, node 12's editableView
        // cascades (§5.3) a EVR onto its content child 121, which is Final
        // and so clobbers the Tentative bare root 121 (§5.2). So the bare
        // root 121 is now write-protected and 12's child 121 is the editable
        // occurrence (childless, since the save emptied 121's contains).
        "* (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected (rels (contains (in 1)))) 121
         * (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1) (out 4)))) 12
         12 body
         ** (skg node (gnid 121) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 121
         ** (skg node (gnid 122) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
         122 body
         *** (skg node (gnid 1221) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 1221
         1221 body
         ** (skg node (gnid 123) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
         123 body
         ** (skg node (gnid 124) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
         124 body
      "};

      assert_metadata_eq!(result, expected,
        "Editable view should mark repeated nodes as writeProtected");

      Ok (( )) }

// =====================================================
// Test: Editable view request clears after processing
// =====================================================

async fn test_editable_view_request_cleared (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result:\n{}", result);

      // The viewRequests should be cleared after processing
      // So the output should not contain "viewRequests"
      assert!(!result . contains ("viewRequests"),
        "viewRequests should be cleared after processing");

      Ok (( )) }

/// Open a content view of 'roots', rendered with 'config', registered
/// in 'views_state' so that a save of it is checked against it.
/// Returns the view's ID and text.
fn open_content_view (
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  roots       : &[&str],
) -> Result<(ViewforestId, String), Box<dyn Error>> {
  let viewforest_id : ViewforestId =
    ViewforestId::ContentView ( "content-view".to_string () );
  let root_ids : Vec<ID> =
    roots . iter () . map ( |root| ID::from (*root) ) . collect ();
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (config, Some (tantivy), &root_ids) ?;
  views_state . open_views . register_view (
    &graph . load_full (), viewforest_id . clone (), tree, &pids );
  Ok (( viewforest_id, text )) }

/// 'buffer' with an editable-view request added to its first line
/// containing 'metadata', just after it.
fn requesting_editable_at (
  buffer   : &str,
  metadata : &str,
) -> String {
  requesting_at ( buffer, metadata, "editableView" ) }

/// 'buffer' with the view request 'request' added to its first line
/// containing 'metadata' (starting with it, if 'metadata' starts with
/// a newline), viewnode ID aside, just after it.
fn requesting_at (
  buffer   : &str,
  metadata : &str,
  request  : &str,
) -> String {
  let (at_line_start, metadata) : (bool, &str) =
    match metadata . strip_prefix ('\n') {
      Some (rest) => (true, rest),
      None        => (false, metadata), };
  let mut requested : bool = false;
  let lines : Vec<String> = buffer . lines () . map ( |line| {
    let plain : String = without_viewnode_ids (line);
    let holds_it : bool =
      if at_line_start { plain . starts_with (metadata) }
      else { plain . contains (metadata) };
    if requested || ! holds_it { return line . to_string (); }
    requested = true;
    let with_request : String = plain . replacen (
      metadata, &format! ( "{} (viewRequests {})", metadata, request ), 1 );
    match viewnode_id_atom_of (line) {
      Some (atom) => with_viewnode_id_atom (&with_request, atom),
      None        => with_request, }} ) . collect ();
  assert! ( requested, "no line contains {:?}:\n{}", metadata, buffer );
  lines . join ("\n") + "\n" }

/// The headlines of 'buffer' that start with 'prefix'.
fn lines_starting<'a> (
  buffer : &'a str,
  prefix : &str,
) -> Vec<&'a str> {
  buffer . lines ()
    . filter ( |line| without_viewnode_ids (line) . starts_with (prefix) )
    . collect () }

/// The headlines of the subtree of 'buffer' whose root is the first
/// line starting with 'root', the root excluded.
fn subtree_headlines<'a> (
  buffer : &'a str,
  root   : &str,
) -> Vec<&'a str> {
  let depth : usize = root . find (' ') . unwrap_or (0);
  buffer . lines ()
    . skip_while ( |line| ! without_viewnode_ids (line) . starts_with (root) ) . skip (1)
    . filter ( |line| line . starts_with ('*') )
    . take_while ( |line| line . find (' ') . unwrap_or (0) > depth )
    . collect () }

// ===================================================
// Test: a request on an editable node extends its content
// ===================================================
// Opened with a budget of 1, root 1 shows 11, 12 and 13
// write-protected. A request on 1, already editable, passes to them
// (the cascade). With a budget of 3, the extensions of 1, 11 and 12
// spend it: 11 and 12 become editable, 12 shows its whole group
// 121..124 write-protected, and 13 stays write-protected.
async fn test_editable_view_request_on_an_editable_node (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) = {
        let mut config : SkgConfig = config . clone ();
        config . initial_node_limit = 1;
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1"] ) ? };
      let requested : String =
        requesting_editable_at (&opened, "node (gnid 1) (skgRepos (nodeRepo main))");
      let result : String = {
        let mut config : SkgConfig = config . clone ();
        config . initial_node_limit = 3;
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response : SaveResponse = update_from_and_rerender_buffer (
          &mut stream, &requested, &config, tantivy, &graph,
          &Ok (viewforest_id), &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (),
                  "the request must save, got {:?}", response . errors );
        response . saved_view };
      println! ("Result of a request on an editable node:\n{}", result);
      let expected = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (rels (contains (out 3)))) 1
        1 body
        ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 11
        11 body
        ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1)))) 12
        12 body
        *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1)))) 121
        *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) 122
        *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 123
        *** (skg node (gnid 124) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 124
        ** (skg node (gnid 13) (skgRepos (nodeRepo main)) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) 13
      "};
      assert_metadata_eq!(result, expected,
        "a request on editable 1 extends its content within the budget");
      Ok (( )) }

// ===================================================
// Test: a request on a write-protected occurrence moves editability
// ===================================================
// A view of 1 and 12 shows root 12 editable, with its content, and
// the 12 under 1 as a write-protected member without its content. A request on it
// moves editability to it; root 12 is write-protected and keeps its
// shown content, no longer as members. A request on root 12, whose
// shown children are unchanged, moves editability back: root 12 is
// editable again with the same content, adopted as members again,
// and the 12 under 1 is write-protected. A further
// save changes nothing.
async fn test_editable_view_request_moves_editability_back (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let mut save = async | buffer : &str | -> Result<String, Box<dyn Error>> {
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response : SaveResponse = update_from_and_rerender_buffer (
          &mut stream, buffer, &config, tantivy, &graph,
          &Ok (viewforest_id . clone ()), &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (),
                  "the buffer must save, got {:?}\n\nbuffer:\n{}",
                  response . errors, buffer );
        Ok (response . saved_view) };
      let root_12 : &str = "* (skg node (gnid 12) (skgRepos (nodeRepo main))";
      let under_1 : &str = "** (skg node (gnid 12) (skgRepos (nodeRepo main))";
      let line_starting = |buffer : &str, prefix : &str| -> String {
        buffer . lines () . find ( |line| without_viewnode_ids (line) . starts_with (prefix) )
          . unwrap_or_else ( || panic! ( "no line starts {:?}:\n{}", prefix, buffer ))
          . to_string () };
      let root_12_content = |buffer : &str| -> Vec<String> {
        buffer . lines ()
          . skip_while ( |line| ! without_viewnode_ids (line) . starts_with (root_12) ) . skip (1)
          . filter ( |line| line . starts_with ("*") )
          . map ( |line| line . replace ("writeProtected ", "")
                              . replace ("omittedBody ", "")
                              // write-protected, the content stops being members
                              . replace ("noClaim ", "")
                              . replace (" (birth (contains in 1))", "") )
          . collect () };
      let moved : String = save (
        &requesting_editable_at (&opened, "node (gnid 12) (skgRepos (nodeRepo main)) writeProtected")
      ) . await ?;
      assert! ( ! line_starting (&moved, under_1) . contains ("writeProtected")
                && line_starting (&moved, root_12) . contains ("writeProtected"),
                "editability must move to the 12 under 1:\n{}", moved );
      assert! ( ! root_12_content (&moved) . is_empty (),
                "root 12 keeps its shown content:\n{}", moved );
      let moved_back : String = save (
        &requesting_editable_at (
          &moved, "* (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected")
      ) . await ?;
      println! ("Result of moving editability back:\n{}", moved_back);
      assert! ( line_starting (&moved_back, under_1) . contains ("writeProtected")
                && ! line_starting (&moved_back, root_12) . contains ("writeProtected"),
                "editability must move back to root 12:\n{}", moved_back );
      assert_eq! ( root_12_content (&moved_back), root_12_content (&moved),
                   "root 12's shown content is kept:\n{}", moved_back );
      let again : String = save (&moved_back) . await ?;
      assert_eq! ( again, moved_back, "a further save changes nothing" );
      Ok (( )) }

// ===================================================
// Test: an AliasFolder fills whole and is budget-neutral (§5.5).
// ===================================================
// Companion to initial_view_bfs::test_budget_content_beats_subscribers (which
// pins the same neutrality for the auto-rendered SubscribeeFolder). An AliasFolder,
// unlike a SubscribeeFolder, is not auto-rendered de-novo -- it appears only on an
// explicit `aliases` view request -- so this exercises the request path through
// the rerender (build_and_integrate_aliases / reconcile_aliasFolder_children),
// neither of which takes node_budget.
//
// Fixture: root r contains the chain c1 -> c2, and r has two aliases. The budget
// counts vognode extensions (cost 1), so the whole content here is exactly 3
// extensions: r, c1, c2. With limit=3 the chain fully extends (c2 editable)
// and the AliasFolder shows both aliases. The alias members are non-vognodes, so they
// cost nothing: were they charged, the 3 units could not also cover c2, which
// would then be left write-protected (verified: at limit=2 c2 *is* write-protected
// while the AliasFolder stays whole). That c2 is editable here is the guarantee
// this test pins.
async fn test_budget_aliasfolder_is_neutral (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg node (gnid r) (skgRepos (nodeRepo main)) (viewRequests (folder aliases))) r
        ** (skg node (gnid c1) (skgRepos (nodeRepo main))) c1
        *** (skg node (gnid c2) (skgRepos (nodeRepo main))) c2
      "};
      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 3;
        let graph : InRustGraphHandle =
          (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          open_views            : OpenViews::new (),
          graph_straddle        : None,};
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("alias-budget (budget 3):\n{}", result);

      let expected = indoc! {"
        * (skg node (gnid r) (skgRepos (nodeRepo main)) (rels (contains (out 1)) (aliases 2))) r
        ** (skg aliasFolder)
        *** (skg alias) first alias
        *** (skg alias) second alias
        ** (skg node (gnid c1) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1)))) c1
        *** (skg node (gnid c2) (skgRepos (nodeRepo main)) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1)))) c2
      "};
      assert_metadata_eq!(result, expected,
        "budget 3 extends the whole content chain; the AliasFolder is whole + budget-neutral");

      Ok (( )) }

/// Save 'buffer' as the view 'viewforest_id', asserting that it saves.
async fn save_view (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  viewforest_id : &ViewforestId,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let (mut stream, _read_end) : (TcpStream, TcpStream) =
    mk_test_tcp_stream_pair ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (viewforest_id . clone ()), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response . saved_view) }

// ===================================================
// Test: a write-protect request keeps the children editable
// ===================================================
// A view of 12 shows it editable, with 121..124 editable under it
// (and its containers folder, holding 1).
// A write-protect request on root 12, in a save that also retitles
// it, saves the title first; the rerender write-protects root 12 and
// makes 121..124 non-members, still editable, with their own
// content (1211 under 121) still members. A further save changes
// nothing.
async fn test_writeProtect_request_keeps_children_editable (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["12"] ) ?;
      let requested : String = requesting_at (
        &opened . replacen ( ")) 12\n", ")) twelve\n", 1 ),
        "node (gnid 12) (skgRepos (nodeRepo main))", "writeProtectedView" );
      let saved : String = save_view (
        &requested, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      println! ("Result of a write-protect request:\n{}", saved);
      let root : &str = saved . lines () . next () . unwrap ();
      assert! ( root . contains ("writeProtected") && root . ends_with (" twelve"),
                "root 12 must be write-protected, with the saved title:\n{}", saved );
      assert! ( ! saved . contains ("viewRequests"), "{}", saved );
      let children : Vec<&str> = // not the root's containers folder
        lines_starting (&saved, "** (skg node (gnid 12");
      assert_eq! ( children . len (), 4, "{}", saved );
      assert! ( children . iter () . all ( |line|
                  line . contains ("noClaim")
                  && ! line . contains ("writeProtected") ),
                "121..124 must be editable non-members:\n{}", saved );
      let grandchildren : Vec<&str> = // not the 1 in the containers folder
        lines_starting (&saved, "*** (skg node (gnid 12");
      assert_eq! ( grandchildren . len (), 3, "{}", saved );
      assert! ( grandchildren . iter () . all ( |line|
                  ! line . contains ("noClaim")
                  && ! line . contains ("writeProtected") ),
                "their content must stay editable members:\n{}", saved );
      let again : String = save_view (
        &saved, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      assert_eq! ( again, saved, "a further save changes nothing" );
      Ok (( )) }

// ===================================================
// Test: the subtree form write-protects the whole content subtree
// ===================================================
async fn test_writeProtect_subtree_request (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["12"] ) ?;
      let saved : String = save_view (
        &requesting_at (&opened, "node (gnid 12) (skgRepos (nodeRepo main))",
                        "writeProtectedSubtreeView"),
        &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      println! ("Result of a write-protect-subtree request:\n{}", saved);
      let headlines : Vec<&str> = // 12's content subtree, not its containers folder
        lines_starting (&saved, "*") . into_iter ()
        . filter ( |line| line . contains ("node (gnid 12") )
        . collect ();
      assert_eq! ( headlines . len (), 8, "{}", saved );
      assert! ( headlines . iter () . all ( |line| line . contains ("writeProtected") ),
                "everything must be write-protected:\n{}", saved );
      assert! ( headlines [1 ..] . iter () . all ( |line|
                  line . contains ("noClaim") ),
                "every child must be a non-member:\n{}", saved );
      assert! ( ! saved . contains ("viewRequests")
                && ! saved . contains ("body\n"),
                "no request, and no body, is left:\n{}", saved );
      Ok (( )) }

// ===================================================
// Test: a write-protect request on a write-protected occurrence is a no-op
// ===================================================
// A view of 1 and 12 shows the 12 under 1 as a write-protected member
// without its content.
async fn test_writeProtect_request_on_a_writeProtected_occurrence (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let plain : String = save_view (
        &opened, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      for request in [ "writeProtectedView", "writeProtectedSubtreeView" ] {
        let requested : String = save_view (
          &requesting_at (&plain, "node (gnid 12) (skgRepos (nodeRepo main)) writeProtected", request),
          &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
        assert_eq! ( requested, plain, "{} on a write-protected occurrence", request ); }
      Ok (( )) }

// ===================================================
// Test: write-protecting one occurrence while requesting another moves
// editability, and a request moves it back
// ===================================================
// A view of 1 and 12 shows root 12 editable, with 121..124, and the
// 12 under 1 write-protected. One save requests root 12
// write-protected and the 12 under 1 editable, and folds the 121
// under root 12. 121..124 are then shown once under root 12, as
// write-protected non-members, and editable under the 12 under 1.
// An editable-view request on root 12 moves editability back: root
// 12 adopts 121..124 as its members again, keeping the fold.
async fn test_writeProtect_and_editable_requests_move_editability_and_back (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let root_12 : &str = "* (skg node (gnid 12) (skgRepos (nodeRepo main))";
      let under_1 : &str = "** (skg node (gnid 12) (skgRepos (nodeRepo main))";
      let folded_121 : &str = "** (skg folded node (gnid 121) ";
      let content_under = |buffer : &str, root : &str| -> Vec<String> {
        // 12's content subtree, not root 12's containers folder (1)
        subtree_headlines (buffer, root) . iter ()
          . filter ( |line| line . contains ("(gnid 12") )
          . map ( |line| line . to_string () )
          . collect () };
      let ids_under = |buffer : &str, root : &str| -> Vec<String> {
        content_under (buffer, root) . iter ()
          . filter ( |line| line . len () - line . trim_start_matches ('*') . len ()
                            == root . find (' ') . unwrap () + 1 )
          . map ( |line| line . split ("(gnid ") . nth (1) . unwrap ()
                              . split (')') . next () . unwrap () . to_string () )
          . collect () };
      let edited : String = requesting_editable_at (
        &requesting_at ( &opened, "\n* (skg node (gnid 12) (skgRepos (nodeRepo main))",
                         "writeProtectedView" ),
        "node (gnid 12) (skgRepos (nodeRepo main)) writeProtected" )
        . replacen ( "node (gnid 121) ", "folded node (gnid 121) ", 1 );
      let moved : String = save_view (
        &edited, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      println! ("Result of moving editability:\n{}", moved);
      assert! ( lines_starting (&moved, root_12) [0] . contains ("writeProtected")
                && ! lines_starting (&moved, under_1) [0] . contains ("writeProtected"),
                "editability must move to the 12 under 1:\n{}", moved );
      assert_eq! ( ids_under (&moved, root_12), [ "121", "122", "123", "124" ],
                   "root 12 shows its content once:\n{}", moved );
      assert! ( content_under (&moved, root_12) . iter ()
                  . all ( |line| line . contains ("writeProtected")
                                 && line . contains ("noClaim") ),
                "under root 12, write-protected non-members:\n{}", moved );
      assert_eq! ( ids_under (&moved, under_1), [ "121", "122", "123", "124" ],
                   "{}", moved );
      assert! ( content_under (&moved, under_1) . iter ()
                  . all ( |line| ! line . contains ("writeProtected")
                                 && ! line . contains ("noClaim") ),
                "under the 12 under 1, editable members:\n{}", moved );
      let moved_back : String = save_view (
        &requesting_editable_at (
          &moved, "node (gnid 12) (skgRepos (nodeRepo main)) writeProtected" ),
        &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      println! ("Result of moving editability back:\n{}", moved_back);
      assert! ( ! lines_starting (&moved_back, root_12) [0] . contains ("writeProtected")
                && lines_starting (&moved_back, under_1) [0] . contains ("writeProtected"),
                "editability must move back to root 12:\n{}", moved_back );
      assert_eq! ( ids_under (&moved_back, root_12), [ "121", "122", "123", "124" ],
                   "{}", moved_back );
      assert! ( content_under (&moved_back, root_12) . iter ()
                  . all ( |line| ! line . contains ("writeProtected")
                                 && ! line . contains ("noClaim") ),
                "root 12's content must be editable members again:\n{}", moved_back );
      assert! ( without_viewnode_ids (&content_under (&moved_back, root_12) [0])
                . starts_with (folded_121),
                "the adopted 121 keeps its fold:\n{}", moved_back );
      Ok (( )) }

// ===================================================
// Test: a retained content node is shown once
// ===================================================
// In a view of 1, 13 is marked as not affecting its parent and saved:
// 1 no longer contains 13, which stays under 1 as a non-member. A
// save of another view of 1 puts 13 back in 1's content. The first
// view, rerendered as collateral, adopts the retained 13 as 1's member
// instead of showing a second 13 beside it.
async fn test_retained_content_node_is_shown_once (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1"] ) ?;
      let retained : String = save_view (
        &opened . replacen ( "node (gnid 13) (skgRepos (nodeRepo main)) ",
                             "node (gnid 13) (skgRepos (nodeRepo main)) noClaim ", 1 ),
        &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      assert! ( lines_starting (&retained, "** (skg node (gnid 13) ") . len () == 1
                && retained . contains ("noClaim"),
                "13 stays under 1 as a non-member:\n{}", retained );
      let other_viewforest_id : ViewforestId = ViewforestId::ContentView ("other-view-of-1" . to_string ());
      let other : String = {
        let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
          multi_root_view (&config, Some (tantivy), &[ ID::from ("1") ]) ?;
        views_state . open_views . register_view (
          &graph . load_full (), other_viewforest_id . clone (), tree, &pids );
        text };
      assert! ( ! other . contains ("(gnid 13)"), "1 no longer contains 13:\n{}", other );
      save_view (
        &format! ( "{}** (skg node (gnid 13) (skgRepos (nodeRepo main)) writeProtected) 13\n", other ),
        &config, tantivy, &graph, &other_viewforest_id, &mut views_state ) . await ?;
      let collateral : String = {
        let viewforest : &ViewForest =
          views_state . open_views . viewforest_id_to_view (&viewforest_id)
          . expect ("the first view is registered");
        viewforest_to_string (viewforest, &config) ? };
      println! ("The first view after the other's save:\n{}", collateral);
      let thirteens : Vec<&str> = lines_starting (&collateral, "** (skg node (gnid 13) ");
      assert! ( thirteens . len () == 1
                && ! thirteens [0] . contains ("noClaim"),
                "13 is shown once, as 1's member:\n{}", collateral );
      Ok (( )) }

// ===================================================
// Test: the budget runs out at a node that shows members
// ===================================================
// A view of 1, opened with an ample budget, shows everything
// editable, 12 with its members 121..124. Saved with a budget of 2,
// the extensions of 1 and 11 spend it, so the rerender leaves 12
// write-protected. A write-protected occurrence holds no member, so
// 121..124 become non-members, keeping their own editability, and a
// further save of the rerendered buffer is not refused.
async fn test_budget_spent_on_a_node_with_members (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let (viewforest_id, opened) : (ViewforestId, String) = {
        let mut config : SkgConfig = config . clone ();
        config . initial_node_limit = 100;
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1"] ) ? };
      assert! ( ! opened . contains ("writeProtected"),
                "an ample budget leaves everything editable:\n{}", opened );
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 2;
      let saved : String = save_view (
        &opened, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      println! ("Result of a save that spends the budget at 12:\n{}", saved);
      let twelve : Vec<&str> = lines_starting (&saved, "** (skg node (gnid 12) ");
      assert! ( twelve . len () == 1 && twelve [0] . contains ("writeProtected"),
                "the budget runs out at 12:\n{}", saved );
      let members_of_twelve : Vec<&str> =
        subtree_headlines (&saved, "** (skg node (gnid 12) ")
        . into_iter ()
        . filter ( |line| line . starts_with ("*** ") )
        . collect ();
      assert_eq! ( members_of_twelve . len (), 4, "{}", saved );
      assert! ( members_of_twelve . iter () . all ( |line|
                  line . contains ("noClaim") ),
                "121..124 must be non-members under write-protected 12:\n{}", saved );
      save_view (
        &saved, &config, tantivy, &graph, &viewforest_id, &mut views_state ) . await ?;
      Ok (( )) }

// ===================================================
// Test: a request claimed elsewhere leaves no member
// ===================================================
// Both roots carry an editable-view request, and the 12 under 1 shows
// 122. Root 12 is made Final at its visit; root 1's cascade hands a
// request to the 12 under 1, which, met later, yields: it is
// write-protected and its member 122 becomes a non-member, so the
// rerendered buffer saves unchanged.
async fn test_a_request_claimed_elsewhere_leaves_no_member (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut config : SkgConfig = config . clone ();
      config . initial_node_limit = 100;
      let graph : InRustGraphHandle =
        graph_handle_from_config (&config) ?;
      let mut views_state : ViewsState = ViewsState {
        open_views     : OpenViews::new (),
        graph_straddle : None,};
      let input : &str = indoc! {"
        * (skg node (gnid 1) (skgRepos (nodeRepo main)) (viewRequests editableView)) 1
        ** (skg node (gnid 12)) 12
        *** (skg node (gnid 122) writeProtected omittedBody) 122
        * (skg node (gnid 12) (skgRepos (nodeRepo main)) writeProtected (viewRequests editableView)) 12
      "};
      let mut saved : Vec<String> = Vec::new ();
      for buffer_index in 0 .. 2 {
        let buffer : &str = if buffer_index == 0 { input } else { &saved [0] };
        let (mut stream, _read_end) : (TcpStream, TcpStream) =
          mk_test_tcp_stream_pair ();
        let response : SaveResponse = update_from_and_rerender_buffer (
          &mut stream, buffer, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (),
                  "the buffer must save, got {:?}\n\nbuffer:\n{}",
                  response . errors, buffer );
        saved . push (response . saved_view); }
      let rerendered : &str = &saved [0];
      println! ("The rerendered buffer:\n{}", rerendered);
      let twelve_under_one : Vec<&str> = lines_starting (rerendered, "** (skg node (gnid 12) ");
      assert! ( twelve_under_one . len () == 1
                && twelve_under_one [0] . contains ("writeProtected"),
                "the 12 under 1 yields the claim:\n{}", rerendered );
      let under_twelve : Vec<&str> =
        subtree_headlines (rerendered, "** (skg node (gnid 12) ");
      assert! ( ! under_twelve . is_empty ()
                && under_twelve . iter () . all ( |line| line . contains ("noClaim") ),
                "a write-protected occurrence holds no member:\n{}", rerendered );
      Ok (( )) }
