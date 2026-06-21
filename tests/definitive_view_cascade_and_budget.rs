// cargo nextest run --test grouped_views -E 'test(definitive_view_cascade_and_budget::)'
//
// Tests for definitive view request expansion.
// These tests verify that when a node has a "Definitive" view request,
// its content children are expanded from disk using BFS with truncation.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle,
  install_or_swap_global_handle };
use skg::test_utils::{run_with_shared_test_db, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::types::misc::{SkgConfig, TantivyIndex};
use skg::types::views_state::OpenViews;

use std::sync::Arc;
use typedb_driver::TypeDBDriver;


fn mk_test_tcp_stream ()
  -> TcpStream
{ let listener : std::net::TcpListener =
    std::net::TcpListener::bind ("127.0.0.1:0") . unwrap ();
  let addr : std::net::SocketAddr =
    listener . local_addr () . unwrap ();
  TcpStream::connect (addr) . unwrap () }

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str =
    "tests/definitive_view_cascade_and_budget/fixtures";
  run_with_shared_test_db (
    "skg-test-definitive-view-cascade-and-budget",
    |s| Box::pin ( async move {
      s . reset ("test_definitive_view_ample_budget", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_ample_budget (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_limit_5_or_6", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_limit_5_or_6 (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_limit_1_to_4", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_limit_1_to_4 (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_conflicting", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_conflicting (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_with_cycle",
                 "tests/definitive_view_cascade_and_budget/fixtures-cycle") . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_with_cycle (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_with_repeat", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_with_repeat (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_definitive_view_request_cleared", fixtures) . await ?;
      s . install_graph_handle () ?;
      test_definitive_view_request_cleared (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_budget_aliascol_is_neutral",
                 "tests/definitive_view_cascade_and_budget/fixtures-aliases") . await ?;
      s . install_graph_handle () ?;
      test_budget_aliascol_is_neutral (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

// ===================================================
// Test: Definitive view with an ample budget -> everything expands.
// §5.5 budget counts vognode EXPANSIONS (cost 1 each); the full subtree here is
// 12 expansions (1,2,11,12,13,121,122,123,124,1211,1212,1221), so a budget of
// 20 is ample and nothing is left indefinitive.
// ===================================================
async fn test_definitive_view_ample_budget (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        * (skg (node (id 1) (source main) (parentIs absent) (rels \"C3\"))) 1
        ** (skg (node (id 11) (source main) (birthHerald \"aC\"))) 11
        ** (skg (node (id 12) (source main) (birthHerald \"aC4\"))) 12
        12 body
        *** (skg (node (id 121) (source main) (birthHerald \"aC2\"))) 121
        121 body
        **** (skg (node (id 1211) (source main) (birthHerald \"aC\"))) 1211
        1211 body
        **** (skg (node (id 1212) (source main) (birthHerald \"aC\"))) 1212
        1212 body
        *** (skg (node (id 122) (source main) (birthHerald \"aC1\"))) 122
        122 body
        **** (skg (node (id 1221) (source main) (birthHerald \"aC\"))) 1221
        1221 body
        *** (skg (node (id 123) (source main) (birthHerald \"aC\"))) 123
        123 body
        *** (skg (node (id 124) (source main) (birthHerald \"aC\"))) 124
        124 body
        ** (skg (node (id 13) (source main) (birthHerald \"aC\"))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result, expected,
        "Definitive view with an ample budget should expand all children");

      Ok (( )) }

// ===================================================
// Test: Definitive view with limit=5 vs limit=6
// ===================================================
// §5.5 budget is granular: limit=5 spends its last unit on 1211 (1212 not
// created); limit=6 affords both 1211 and 1212. Nodes whose DVR is reached
// after the budget is spent are stripped to indefinitive. (No sibling-group
// padding: the two limits now differ by one created node.)

async fn test_definitive_view_limit_5_or_6 (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        * (skg (node (id 1) (source main) (parentIs absent) (rels \"C3\"))) 1
        ** (skg (node (id 11) (source main) (birthHerald \"aC\"))) 11
        ** (skg (node (id 12) (source main) (birthHerald \"aC4\"))) 12
        12 body
        *** (skg (node (id 121) (source main) indef (birthHerald \"aC2\"))) 121
        *** (skg (node (id 122) (source main) indef (birthHerald \"aC1\"))) 122
        *** (skg (node (id 123) (source main) indef (birthHerald \"aC\"))) 123
        *** (skg (node (id 124) (source main) indef (birthHerald \"aC\"))) 124
        ** (skg (node (id 13) (source main) (birthHerald \"aC\"))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};
      // limit=6: one more expansion than limit=5 -- 121 (the 6th) now expands and
      // draws its whole gen-3 group 1211,1212 (both then indefinitive, budget
      // spent); 122..124 remain indefinitive.
      let expected_6 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (rels \"C3\"))) 1
        ** (skg (node (id 11) (source main) (birthHerald \"aC\"))) 11
        ** (skg (node (id 12) (source main) (birthHerald \"aC4\"))) 12
        12 body
        *** (skg (node (id 121) (source main) (birthHerald \"aC2\"))) 121
        121 body
        **** (skg (node (id 1211) (source main) indef (birthHerald \"aC\"))) 1211
        **** (skg (node (id 1212) (source main) indef (birthHerald \"aC\"))) 1212
        *** (skg (node (id 122) (source main) indef (birthHerald \"aC1\"))) 122
        *** (skg (node (id 123) (source main) indef (birthHerald \"aC\"))) 123
        *** (skg (node (id 124) (source main) indef (birthHerald \"aC\"))) 124
        ** (skg (node (id 13) (source main) (birthHerald \"aC\"))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result_5, expected_5,
        "limit=5: 121 expands one gen-3 child (1211), rest indefinitive (§5.5)");
      assert_eq!(result_6, expected_6,
        "limit=6: 121 expands both gen-3 children, rest indefinitive (§5.5)");

      Ok (( )) }

// ===================================================
// Test: Definitive view with limit=1 or limit=4
// ===================================================
// Gen 2 has 4 children (121, 122, 123, 124).
// Any limit from 1 to 4 hits the limit on the first generation,
// so the sibling group is completed, all are indefinitive,
// and the grandchild generation is not visited at all.

async fn test_definitive_view_limit_1_to_4 (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        * (skg (node (id 1) (source main) (parentIs absent) (rels \"C3\"))) 1
        ** (skg (node (id 11) (source main) indef (birthHerald \"aC\"))) 11
        ** (skg (node (id 12) (source main) indef (birthHerald \"aC4\"))) 12
        ** (skg (node (id 13) (source main) indef (birthHerald \"aC\"))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};
      // limit=4: expansions 1, 2, 11, 12 spend the budget. 12 (the 4th) drew its
      // whole group 121..124, all indefinitive (budget spent); 13 is reached
      // after the budget is gone, so it too is indefinitive.
      let expected_4 = indoc! {"
        * (skg (node (id 1) (source main) (parentIs absent) (rels \"C3\"))) 1
        ** (skg (node (id 11) (source main) (birthHerald \"aC\"))) 11
        ** (skg (node (id 12) (source main) (birthHerald \"aC4\"))) 12
        12 body
        *** (skg (node (id 121) (source main) indef (birthHerald \"aC2\"))) 121
        *** (skg (node (id 122) (source main) indef (birthHerald \"aC1\"))) 122
        *** (skg (node (id 123) (source main) indef (birthHerald \"aC\"))) 123
        *** (skg (node (id 124) (source main) indef (birthHerald \"aC\"))) 124
        ** (skg (node (id 13) (source main) indef (birthHerald \"aC\"))) 13
        * (skg (node (id 2) (source main) (parentIs absent))) 2
      "};

      assert_eq!(result_1, expected_1,
        "limit=1 creates only the first gen-2 child, indefinitive (§5.5)");
      assert_eq!(result_4, expected_4,
        "limit=4 creates all four gen-2 children, all indefinitive (§5.5)");

      Ok (( )) }

// ===================================================
// Test: Definitive view on node that conflicts with existing definitive
// ===================================================
// If node X is already definitive in the tree, and we request
// definitive view on a second instance of X, the first should become
// indefinitive.

async fn test_definitive_view_conflicting (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        "* (skg (node (id 1) (source main) (parentIs absent) (rels \"C1\"))) 1
         ** (skg (node (id 12) (source main) indef (birthHerald \"aC1\"))) 12
         *** (skg (node (id 122) (source main) indef (birthHerald \"aC1\"))) 122
         * (skg (node (id 12) (source main) (parentIs absent) (rels \"1C1\"))) 12
         ** (skg (node (id 122) (source main) (birthHerald \"aC1\"))) 122
         122 body
         *** (skg (node (id 1221) (source main) (birthHerald \"aC\"))) 1221
         1221 body
      "};

      assert_eq!(result, expected,
        "First definitive instance should become indef when second requests definitive");

      Ok (( )) }

// ===================================================
// Test: Definitive view expansion detects cycles
// ===================================================
// Create fixtures for a cycle: a contains b contains a

async fn test_definitive_view_with_cycle (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Node a has definitive request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg (node (id cyc-a) (source main) indef (viewRequests definitiveView))) cyc-a
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        * (skg (node (id cyc-a) (source main) (parentIs absent) (rels \"1C1\"))) cyc-a
        cyc-a body
        ** (skg (node (id cyc-b) (source main) (birthHerald \"aCa\"))) cyc-b
        cyc-b body
        *** (skg (node (id cyc-a) (source main) indef (birthHerald \"aCa\") (viewStats cycle))) cyc-a
      "};

      assert_eq!(result, expected,
        "Definitive view should detect cycles and mark them");

      Ok (( )) }

// ===================================================
// Test: Definitive view expansion detects repeats
// ===================================================
// If a node is already visited (definitive elsewhere), it should be
// marked indef when encountered again during expansion.

async fn test_definitive_view_with_repeat (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id 121) (source main))) 121
        * (skg (node (id 12) (source main) indef (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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
        // Upon saving, the indef view of node 12 had no effect, but the
        // definitive view of 121 (definitive in the *buffer*) deleted its
        // children at extraction. In the rerender, node 12's definitiveView
        // cascades (§5.3) a DVR onto its content child 121, which is Final
        // and so clobbers the Tentative bare root 121 (§5.2). So the bare
        // root 121 is now indefinitive and 12's child 121 is the definitive
        // occurrence (childless, since the save emptied 121's contains).
        "* (skg (node (id 121) (source main) (parentIs absent) indef (rels \"1C\"))) 121
         * (skg (node (id 12) (source main) (parentIs absent) (rels \"1C4\"))) 12
         12 body
         ** (skg (node (id 121) (source main) (birthHerald \"aC\"))) 121
         ** (skg (node (id 122) (source main) (birthHerald \"aC1\"))) 122
         122 body
         *** (skg (node (id 1221) (source main) (birthHerald \"aC\"))) 1221
         1221 body
         ** (skg (node (id 123) (source main) (birthHerald \"aC\"))) 123
         123 body
         ** (skg (node (id 124) (source main) (birthHerald \"aC\"))) 124
         124 body
      "};

      assert_eq!(result, expected,
        "Definitive view should mark repeated nodes as indefinitive");

      Ok (( )) }

// =====================================================
// Test: Definitive view request clears after processing
// =====================================================

async fn test_definitive_view_request_cleared (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id 12) (source main) indef (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 100;
        let graph : InRustGraphHandle =
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
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

      Ok (( )) }

// ===================================================
// Test: an AliasCol fills WHOLE and is budget-NEUTRAL (§5.5).
// ===================================================
// Companion to initial_view_bfs::test_budget_content_beats_subscribers (which
// pins the same neutrality for the auto-rendered SubscribeeCol). An AliasCol,
// unlike a SubscribeeCol, is NOT auto-rendered de-novo -- it appears only on an
// explicit `aliases` view request -- so this exercises the request path through
// the rerender (build_and_integrate_aliases / reconcile_alias_col_children),
// neither of which takes node_budget.
//
// Fixture: root r contains the chain c1 -> c2, and r has two aliases. The budget
// counts vognode EXPANSIONS (cost 1), so the whole content here is exactly 3
// expansions: r, c1, c2. With limit=3 the chain fully expands (c2 definitive)
// AND the AliasCol shows BOTH aliases. The alias members are scaffolds, so they
// cost nothing: were they charged, the 3 units could not also cover c2, which
// would then be left indefinitive (verified: at limit=2 c2 *is* indefinitive
// while the AliasCol stays whole). That c2 is definitive here is the guarantee
// this test pins.
async fn test_budget_aliascol_is_neutral (
  config  : &SkgConfig,
  driver  : &Arc<TypeDBDriver>,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id r) (source main) (viewRequests (col aliases)))) r
        ** (skg (node (id c1) (source main))) c1
        *** (skg (node (id c2) (source main))) c2
      "};
      let result = {
        let mut config = config . clone();
        config . initial_node_limit = 3;
        let graph : InRustGraphHandle =
          install_or_swap_global_handle (
            graph_handle_from_config (&config) ?);
        let mut views_state : ViewsState = ViewsState {
          diff_mode_enabled : false,
          open_views            : OpenViews::new (),};
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, driver, &config, tantivy, &graph, false,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("alias-budget (budget 3):\n{}", result);

      let expected = indoc! {"
        * (skg (node (id r) (source main) (parentIs absent) (rels \"C1 A2\"))) r
        ** (skg aliasCol)
        *** (skg alias) first alias
        *** (skg alias) second alias
        ** (skg (node (id c1) (source main) (birthHerald \"aC1\"))) c1
        *** (skg (node (id c2) (source main) (birthHerald \"aC\"))) c2
      "};
      assert_eq!(result, expected,
        "budget 3 expands the whole content chain; the AliasCol is whole + budget-neutral");

      Ok (( )) }
