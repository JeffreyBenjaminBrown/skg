// cargo test --test definitive_view_cascade_and_budget -- --nocapture
//
// Tests for definitive view request expansion.
// These tests verify that when a node has a "Definitive" view request,
// its content children are expanded from disk using BFS with truncation.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle, new_handle};
use skg::test_utils::run_with_test_db;
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::views_state::OpenViews;


fn mk_test_tcp_stream ()
  -> TcpStream
{ let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr =
    listener . local_addr () . unwrap ();
  TcpStream::connect (addr) . unwrap () }

// ===================================================
// Test: Definitive view with an ample budget -> everything expands.
// §5.5 budget counts vognode EXPANSIONS (cost 1 each); the full subtree here is
// 12 expansions (1,2,11,12,13,121,122,123,124,1211,1212,1221), so a budget of
// 20 is ample and nothing is left indefinitive.
// ===================================================
#[test]
fn test_definitive_view_ample_budget
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-ample-budget",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-ample-budget",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indef (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 20;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with ample budget (20):\n{}", result);

      // With an ample budget, all children should be expanded
      let expected = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (graphStats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) (graphStats (contents 2)))) 121
        121 body
        **** (skg (node (id 1211) (source main))) 1211
        1211 body
        **** (skg (node (id 1212) (source main))) 1212
        1212 body
        *** (skg (node (id 122) (source main) (graphStats (contents 1)))) 122
        122 body
        **** (skg (node (id 1221) (source main))) 1221
        1221 body
        *** (skg (node (id 123) (source main))) 123
        123 body
        *** (skg (node (id 124) (source main))) 124
        124 body
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result, expected,
        "Definitive view with an ample budget should expand all children");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view with limit=5 vs limit=6
// ===================================================
// §5.5 budget is granular: limit=5 spends its last unit on 1211 (1212 not
// created); limit=6 affords both 1211 and 1212. Nodes whose DVR is reached
// after the budget is spent are stripped to indefinitive. (No sibling-group
// padding: the two limits now differ by one created node.)

#[test]
fn test_definitive_view_limit_5_or_6
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-limit-5-or-6",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-limit-5-or-6",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indef (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result_5 = {
        let mut config5 = config . clone();
        config5 . initial_node_limit = 5;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_5 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config5, tantivy, &graph, false,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response_5 . saved_view };
      let result_6 = {
        let mut config6 = config . clone();
        config6 . initial_node_limit = 6;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_6 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config6, tantivy, &graph, false,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response_6 . saved_view };

      println!("Result with limit=5:\n{}", result_5);
      println!("Result with limit=6:\n{}", result_6);

      // §5.5: the budget counts vognode EXPANSIONS (cost 1 each), in BFS order.
      // The expansion order here is 1, 2, 11, 12, 13, 121, 122, ... A col fills
      // WHOLE for free, so a node's whole content group is always drawn -- the
      // budget only governs how many of those children then EXPAND in turn; the
      // rest stay indefinitive (visible, collapsed). Never a partial group.
      //
      // limit=5: expansions 1,2,11,12,13 spend the budget. 12 (the 4th) expanded
      // and drew its WHOLE group 121..124, but the budget hit 0 at 13, so when
      // 121..124 are visited they stay indefinitive (none expands -> no gen-3).
      let expected_5 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (graphStats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) indef (graphStats (contents 2)))) 121
        *** (skg (node (id 122) (source main) indef (graphStats (contents 1)))) 122
        *** (skg (node (id 123) (source main) indef)) 123
        *** (skg (node (id 124) (source main) indef)) 124
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};
      // limit=6: one more expansion than limit=5 -- 121 (the 6th) now expands and
      // draws its whole gen-3 group 1211,1212 (both then indefinitive, budget
      // spent); 122..124 remain indefinitive.
      let expected_6 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (graphStats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) (graphStats (contents 2)))) 121
        121 body
        **** (skg (node (id 1211) (source main) indef)) 1211
        **** (skg (node (id 1212) (source main) indef)) 1212
        *** (skg (node (id 122) (source main) indef (graphStats (contents 1)))) 122
        *** (skg (node (id 123) (source main) indef)) 123
        *** (skg (node (id 124) (source main) indef)) 124
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result_5, expected_5,
        "limit=5: 121 expands one gen-3 child (1211), rest indefinitive (§5.5)");
      assert_eq!(result_6, expected_6,
        "limit=6: 121 expands both gen-3 children, rest indefinitive (§5.5)");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view with limit=1 or limit=4
// ===================================================
// Gen 2 has 4 children (121, 122, 123, 124).
// Any limit from 1 to 4 hits the limit on the first generation,
// so the sibling group is completed, all are indefinitive,
// and the grandchild generation is not visited at all.

#[test]
fn test_definitive_view_limit_1_to_4
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-limit-1-to-4",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-limit-1-to-4",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indef (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result_1 = {
        let mut config1 = config . clone();
        config1 . initial_node_limit = 1;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_1 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config1, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response_1 . saved_view };
      let result_4 = {
        let mut config4 = config . clone();
        config4 . initial_node_limit = 4;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_4 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config4, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response_4 . saved_view };

      println!("Result with limit=1:\n{}", result_1);
      println!("Result with limit=4:\n{}", result_4);

      // §5.5: the budget counts vognode EXPANSIONS (cost 1 each), in BFS order:
      // 1, 2, 11, 12, 13, then 12's content 121..124, ... A view ROOT is never
      // truncated (the user opened it), so roots 1 and 2 always expand; other
      // vognodes reached after the budget is spent are left indefinitive.
      //
      // limit=1: root 1 expands (budget 1->0) and draws its WHOLE group 11,12,13;
      // all three are then indefinitive (budget spent), so 12 never expands and
      // none of 121.. is created. Root 2 still expands (root exemption).
      let expected_1 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
        ** (skg (node (id 11) (source main) indef)) 11
        ** (skg (node (id 12) (source main) indef (graphStats (contents 4)))) 12
        ** (skg (node (id 13) (source main) indef)) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};
      // limit=4: expansions 1, 2, 11, 12 spend the budget. 12 (the 4th) drew its
      // whole group 121..124, all indefinitive (budget spent); 13 is reached
      // after the budget is gone, so it too is indefinitive.
      let expected_4 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (graphStats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) indef (graphStats (contents 2)))) 121
        *** (skg (node (id 122) (source main) indef (graphStats (contents 1)))) 122
        *** (skg (node (id 123) (source main) indef)) 123
        *** (skg (node (id 124) (source main) indef)) 124
        ** (skg (node (id 13) (source main) indef)) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result_1, expected_1,
        "limit=1 creates only the first gen-2 child, indefinitive (§5.5)");
      assert_eq!(result_4, expected_4,
        "limit=4 creates all four gen-2 children, all indefinitive (§5.5)");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view on node that conflicts with existing definitive
// ===================================================
// If node X is already definitive in the tree, and we request
// definitive view on a second instance of X, the first should become
// indefinitive.

#[test]
fn test_definitive_view_conflicting
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-conflict",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-conflict",
    |config, driver, tantivy| Box::pin ( async move {
      // Node 12 appears twice:
      // - First as a regular child of 1 (will be definitive after completion)
      // - Second with a definitive view request
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 12))) 12
        *** (skg (node (id 122) indef)) 122
        * (skg (node (id 12) (source main) indef (viewRequests definitiveView))) 12 copy
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with conflict:\n{}", result);

      let expected = indoc! {
        // The first 12 (child of 1) should become indefinitive.
        // The second 12 (root with request) should be expanded.
        // NOTE: The first 12 redefines the children of 12 as [122]
        // rather than [121,122,123,124].
        "* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 1)))) 1
         ** (skg (node (id 12) (source main) indef (graphStats (contents 1)))) 12
         *** (skg (node (id 122) (source main) indef (graphStats (contents 1)))) 122
         * (skg (node (id 12) (source main) (parentIs absent) (graphStats (containers 1) (contents 1)))) 12
         ** (skg (node (id 122) (source main) (graphStats (contents 1)))) 122
         122 body
         *** (skg (node (id 1221) (source main))) 1221
         1221 body
      "};

      assert_eq!(result, expected,
        "First definitive instance should become indef when second requests definitive");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view expansion detects cycles
// ===================================================
// Create fixtures for a cycle: a contains b contains a

#[test]
fn test_definitive_view_with_cycle
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-cycle",
    "tests/definitive_view_cascade_and_budget/fixtures-cycle",
    "/tmp/tantivy-test-definitive-view-cycle",
    |config, driver, tantivy| Box::pin ( async move {
      // Node a has definitive request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg (node (id cyc-a) (source main) indef (viewRequests definitiveView))) cyc-a
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with cycle:\n{}", result);

      // a should expand to show b, and b's child a should be marked as cycle
      let expected = indoc! {"
        * (skg (node (id cyc-a) (source main) (parentIs absent) (graphStats (containers 1) (contents 1)))) cyc-a
        cyc-a body
        ** (skg (node (id cyc-b) (source main) (graphStats (contents 1)) (viewStats containsParent))) cyc-b
        cyc-b body
        *** (skg (node (id cyc-a) (source main) indef (graphStats (contents 1)) (viewStats cycle containsParent))) cyc-a
      "};

      assert_eq!(result, expected,
        "Definitive view should detect cycles and mark them");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view expansion detects repeats
// ===================================================
// If a node is already visited (definitive elsewhere), it should be
// marked indef when encountered again during expansion.

#[test]
fn test_definitive_view_with_repeat
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-repeat",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-repeat",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 121) (source main))) 121
        * (skg (node (id 12) (source main) indef (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with repeat:\n{}", result);

      let expected = indoc! {
        // Upon saving, the indef view of node 12 had no effect,
        // but the definitive view of 121 (definitive in the *buffer*)
        // deleted its children at extraction. In the rerender, node 12's
        // definitiveView cascades (§5.3) a DVR onto its content child 121,
        // which is Final and so clobbers the Tentative bare root 121 (§5.2:
        // "the explicit request wins; the origin is demoted"). So the bare
        // root 121 is now indefinitive and 12's child 121 is the definitive
        // occurrence (childless, since the save emptied 121's contains).
        "* (skg (node (id 121) (source main) (parentIs absent) indef (graphStats (containers 1)))) 121
         * (skg (node (id 12) (source main) (parentIs absent) (graphStats (containers 1) (contents 4)))) 12
         12 body
         ** (skg (node (id 121) (source main))) 121
         ** (skg (node (id 122) (source main) (graphStats (contents 1)))) 122
         122 body
         *** (skg (node (id 1221) (source main))) 1221
         1221 body
         ** (skg (node (id 123) (source main))) 123
         123 body
         ** (skg (node (id 124) (source main))) 124
         124 body
      "};

      assert_eq!(result, expected,
        "Definitive view should mark repeated nodes as indefinitive");

      Ok (( )) } )) }

// =====================================================
// Test: Definitive view request clears after processing
// =====================================================

#[test]
fn test_definitive_view_request_cleared
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-cleared",
    "tests/definitive_view_cascade_and_budget/fixtures",
    "/tmp/tantivy-test-definitive-view-cleared",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 12) (source main) indef (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          new_handle (InRustGraph::new ());
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result:\n{}", result);

      // The viewRequests should be cleared after processing
      // So the output should NOT contain "viewRequests"
      assert!(!result . contains ("viewRequests"),
        "viewRequests should be cleared after processing");

      Ok (( )) } )) }
