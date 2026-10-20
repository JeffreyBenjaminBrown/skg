// cargo nextest run --test grouped_views -E 'test(editable_view_cascade_and_budget::)'
//
// Tests for editable view request expansion.
// These tests verify that when a node has a "Editable" view request,
// its content children are expanded from disk using BFS with truncation.

use indoc::indoc;
use std::error::Error;
use std::net::TcpStream;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle,
  };
use skg::assert_metadata_eq;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::test_utils::update_from_and_rerender_buffer_test as update_from_and_rerender_buffer;
use skg::serve::ViewsState;
use skg::serve::handlers::save_buffer::SaveResponse;
use skg::org_to_text::viewforest_to_string;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::tree::forest::ViewForest;
use skg::types::views_state::{OpenViews, ViewId};
use skg::types::viewnode::Viewnode;

use ego_tree::Tree;



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
    "tests/editable_view_cascade_and_budget/fixtures";
  run_with_shared_test_stores (
    "skg-test-editable-view-cascade-and-budget",
    |s| Box::pin ( async move {
      s . reset ("test_editable_view_ample_budget", fixtures) ?;
      test_editable_view_ample_budget (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_limit_5_or_6", fixtures) ?;
      test_editable_view_limit_5_or_6 (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_limit_1_to_4", fixtures) ?;
      test_editable_view_limit_1_to_4 (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_conflicting", fixtures) ?;
      test_editable_view_conflicting (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_with_cycle",
                 "tests/editable_view_cascade_and_budget/fixtures-cycle") ?;
      test_editable_view_with_cycle (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_with_repeat", fixtures) ?;
      test_editable_view_with_repeat (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_request_cleared", fixtures) ?;
      test_editable_view_request_cleared (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_request_on_an_editable_node", fixtures) ?;
      test_editable_view_request_on_an_editable_node (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_editable_view_request_moves_editability_back", fixtures) ?;
      test_editable_view_request_moves_editability_back (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_writeProtect_request_keeps_children_editable", fixtures) ?;
      test_writeProtect_request_keeps_children_editable (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_writeProtect_subtree_request", fixtures) ?;
      test_writeProtect_subtree_request (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_writeProtect_request_on_a_writeProtected_occurrence", fixtures) ?;
      test_writeProtect_request_on_a_writeProtected_occurrence (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_writeProtect_and_editable_requests_move_editability_and_back", fixtures) ?;
      test_writeProtect_and_editable_requests_move_editability_and_back (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_parked_content_node_is_drawn_once", fixtures) ?;
      test_parked_content_node_is_drawn_once (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_budget_aliasfolder_is_neutral",
                 "tests/editable_view_cascade_and_budget/fixtures-aliases") ?;
      test_budget_aliasfolder_is_neutral (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

// ===================================================
// Test: Editable view with an ample budget -> everything expands.
// §5.5 budget counts vognode EXPANSIONS (cost 1 each); the full subtree here is
// 12 expansions (1,2,11,12,13,121,122,123,124,1211,1212,1221), so a budget of
// 20 is ample and nothing is left write-protected.
// ===================================================
async fn test_editable_view_ample_budget (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (repo main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) writeProtected (viewRequests editableView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (repo main))) 2
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with ample budget (20):\n{}", result);

      // With an ample budget, all children should be expanded
      let expected = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        ** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        ** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        12 body
        *** (skg (node (id 121) (repo main) (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1))))) 121
        121 body
        **** (skg (node (id 1211) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1211
        1211 body
        **** (skg (node (id 1212) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1212
        1212 body
        *** (skg (node (id 122) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
        122 body
        **** (skg (node (id 1221) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1221
        1221 body
        *** (skg (node (id 123) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
        123 body
        *** (skg (node (id 124) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
        124 body
        ** (skg (node (id 13) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
        * (skg (node (id 2) (repo main) (affectsParent na))) 2
      "};

      assert_metadata_eq!(result, expected,
        "Editable view with an ample budget should expand all children");

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
        * (skg (node (id 1) (repo main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) writeProtected (viewRequests editableView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (repo main))) 2
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_6 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config6, tantivy, &graph,

          &Err ( String::new () ), &mut views_state ) . await ?;
        response_6 . saved_view };

      println!("Result with limit=5:\n{}", result_5);
      println!("Result with limit=6:\n{}", result_6);

      // §5.5: the budget counts vognode EXPANSIONS (cost 1 each), in BFS order.
      // The expansion order here is 1, 2, 11, 12, 13, 121, 122, ... A folder fills
      // WHOLE for free, so a node's whole content group is always drawn -- the
      // budget only governs how many of those children then EXPAND in turn; the
      // rest stay write-protected (visible, collapsed). Never a partial group.
      //
      // limit=5: expansions 1,2,11,12,13 spend the budget. 12 (the 4th) expanded
      // and drew its WHOLE group 121..124, but the budget hit 0 at 13, so when
      // 121..124 are visited they stay write-protected (none expands -> no gen-3).
      let expected_5 = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        ** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        ** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        12 body
        *** (skg (node (id 121) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1))))) 121
        *** (skg (node (id 122) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
        *** (skg (node (id 123) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
        *** (skg (node (id 124) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
        ** (skg (node (id 13) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
        * (skg (node (id 2) (repo main) (affectsParent na))) 2
      "};
      // limit=6: one more expansion than limit=5 -- 121 (the 6th) now expands and
      // draws its whole gen-3 group 1211,1212 (both then write-protected, budget
      // spent); 122..124 remain write-protected.
      let expected_6 = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        ** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        ** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        12 body
        *** (skg (node (id 121) (repo main) (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1))))) 121
        121 body
        **** (skg (node (id 1211) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1211
        **** (skg (node (id 1212) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1212
        *** (skg (node (id 122) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
        *** (skg (node (id 123) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
        *** (skg (node (id 124) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
        ** (skg (node (id 13) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
        * (skg (node (id 2) (repo main) (affectsParent na))) 2
      "};

      assert_metadata_eq!(result_5, expected_5,
        "limit=5: 121 expands one gen-3 child (1211), rest writeProtected (§5.5)");
      assert_metadata_eq!(result_6, expected_6,
        "limit=6: 121 expands both gen-3 children, rest writeProtected (§5.5)");

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
        * (skg (node (id 1) (repo main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) writeProtected (viewRequests editableView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (repo main))) 2
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response_4 = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config4, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response_4 . saved_view };

      println!("Result with limit=1:\n{}", result_1);
      println!("Result with limit=4:\n{}", result_4);

      // §5.5: the budget counts vognode EXPANSIONS (cost 1 each), in BFS order:
      // 1, 2, 11, 12, 13, then 12's content 121..124, ... A view ROOT is never
      // truncated (the user opened it), so roots 1 and 2 always expand; other
      // vognodes reached after the budget is spent are left write-protected.
      //
      // limit=1: root 1 expands (budget 1->0) and draws its WHOLE group 11,12,13;
      // all three are then write-protected (budget spent), so 12 never expands and
      // none of 121.. is created. Root 2 still expands (root exemption).
      let expected_1 = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        ** (skg (node (id 11) (repo main) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        ** (skg (node (id 12) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        ** (skg (node (id 13) (repo main) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
        * (skg (node (id 2) (repo main) (affectsParent na))) 2
      "};
      // omittedBody marks only 12: the saved buffer drew 11 and 13
      // editable with no body text, WIPING their bodies, while
      // write-protected 12 kept its body -- which this render hides.
      // limit=4: expansions 1, 2, 11, 12 spend the budget. 12 (the 4th) drew its
      // whole group 121..124, all write-protected (budget spent); 13 is reached
      // after the budget is gone, so it too is write-protected.
      let expected_4 = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        ** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        ** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        12 body
        *** (skg (node (id 121) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1))))) 121
        *** (skg (node (id 122) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
        *** (skg (node (id 123) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
        *** (skg (node (id 124) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
        ** (skg (node (id 13) (repo main) writeProtected (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
        * (skg (node (id 2) (repo main) (affectsParent na))) 2
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
        * (skg (node (id 1) (repo main))) 1
        ** (skg (node (id 12))) 12
        *** (skg (node (id 122) writeProtected omittedBody)) 122
        * (skg (node (id 12) (repo main) writeProtected (viewRequests editableView))) 12
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with conflict:\n{}", result);

      let expected = indoc! {
        // The first 12 (child of 1) should become write-protected,
        // its child 122 no longer a member.
        // The second 12 (root with request) should be expanded.
        // NOTE: The first 12 redefines the children of 12 as [122]
        // rather than [121,122,123,124].
        "* (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 1))))) 1
         ** (skg (node (id 12) (repo main) writeProtected (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 12
         *** (skg (node (id 122) (repo main) (affectsParent false) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1))))) 122
         * (skg (node (id 12) (repo main) (affectsParent na) (rels (contains (in 1) (out 1))))) 12
         ** (skg (node (id 122) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
         122 body
         *** (skg (node (id 1221) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1221
         1221 body
      "};

      assert_metadata_eq!(result, expected,
        "First editable occurrence should become write-protected when second requests editable");

      Ok (( )) }

// ===================================================
// Test: Editable view expansion detects cycles
// ===================================================
// Create fixtures for a cycle: a contains b contains a

async fn test_editable_view_with_cycle (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // Node a has editable request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg (node (id cyc-a) (repo main) writeProtected (viewRequests editableView))) cyc-a
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result with cycle:\n{}", result);

      // a should expand to show b, and b's child a should be marked as cycle
      let expected = indoc! {"
        * (skg (node (id cyc-a) (repo main) (affectsParent na) (rels (contains (in 1) (out 1))))) cyc-a
        cyc-a body
        ** (skg (node (id cyc-b) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1 (ancestors 1))) (birth (contains in 1))))) cyc-b
        cyc-b body
        *** (skg (node (id cyc-a) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1 (ancestors 1))) (birth (contains in 1))) (viewStats cycle))) cyc-a
      "};

      assert_metadata_eq!(result, expected,
        "Editable view should detect cycles and mark them");

      Ok (( )) }

// ===================================================
// Test: Editable view expansion detects repeats
// ===================================================
// If a node is already visited (editable elsewhere), it should be
// marked write-protected when encountered again during expansion.

async fn test_editable_view_with_repeat (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id 121) (repo main))) 121
        * (skg (node (id 12) (repo main) writeProtected (viewRequests editableView))) 12
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
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
        "* (skg (node (id 121) (repo main) (affectsParent na) writeProtected (rels (contains (in 1))))) 121
         * (skg (node (id 12) (repo main) (affectsParent na) (rels (contains (in 1) (out 4))))) 12
         12 body
         ** (skg (node (id 121) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 121
         ** (skg (node (id 122) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
         122 body
         *** (skg (node (id 1221) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 1221
         1221 body
         ** (skg (node (id 123) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
         123 body
         ** (skg (node (id 124) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
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
        * (skg (node (id 12) (repo main) writeProtected (viewRequests editableView))) 12
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("Result:\n{}", result);

      // The viewRequests should be cleared after processing
      // So the output should NOT contain "viewRequests"
      assert!(!result . contains ("viewRequests"),
        "viewRequests should be cleared after processing");

      Ok (( )) }

/// Open a content view of 'roots', drawn with 'config', registered
/// in 'views_state' so that a save of it is checked against it.
/// Returns the view's ID and text.
fn open_content_view (
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  views_state : &mut ViewsState,
  roots       : &[&str],
) -> Result<(ViewId, String), Box<dyn Error>> {
  let view_id : ViewId =
    ViewId::ContentView ( "content-view".to_string () );
  let root_ids : Vec<ID> =
    roots . iter () . map ( |root| ID::from (*root) ) . collect ();
  let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
    multi_root_view (config, Some (tantivy), &root_ids) ?;
  views_state . open_views . register_view (
    &graph . load_full (), view_id . clone (), tree, &pids );
  Ok (( view_id, text )) }

/// 'buffer' with an editable-view request added to its first line
/// containing 'metadata', just after it.
fn requesting_editable_at (
  buffer   : &str,
  metadata : &str,
) -> String {
  requesting_at ( buffer, metadata, "editableView" ) }

/// 'buffer' with the view request 'request' added to its first line
/// containing 'metadata', just after it.
fn requesting_at (
  buffer   : &str,
  metadata : &str,
  request  : &str,
) -> String {
  assert! ( buffer . contains (metadata),
            "no line contains {:?}:\n{}", metadata, buffer );
  buffer . replacen (
    metadata,
    &format! ( "{} (viewRequests {})", metadata, request ),
    1 ) }

/// The headlines of 'buffer' that start with 'prefix'.
fn lines_starting<'a> (
  buffer : &'a str,
  prefix : &str,
) -> Vec<&'a str> {
  buffer . lines ()
    . filter ( |line| line . starts_with (prefix) )
    . collect () }

/// The headlines of the subtree of 'buffer' whose root is the first
/// line starting with 'root', the root excluded.
fn subtree_headlines<'a> (
  buffer : &'a str,
  root   : &str,
) -> Vec<&'a str> {
  let depth : usize = root . find (' ') . unwrap_or (0);
  buffer . lines ()
    . skip_while ( |line| ! line . starts_with (root) ) . skip (1)
    . filter ( |line| line . starts_with ('*') )
    . take_while ( |line| line . find (' ') . unwrap_or (0) > depth )
    . collect () }

// ===================================================
// Test: a request on an editable node unfolds its content
// ===================================================
// Opened with a budget of 1, root 1 draws 11, 12 and 13
// write-protected. A request on 1, already editable, passes to them
// (the cascade). With a budget of 3, the expansions of 1, 11 and 12
// spend it: 11 and 12 become editable, 12 draws its whole group
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
      let (view_id, opened) : (ViewId, String) = {
        let mut config : SkgConfig = config . clone ();
        config . initial_node_limit = 1;
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1"] ) ? };
      let requested : String =
        requesting_editable_at (&opened, "(node (id 1) (repo main)");
      let result : String = {
        let mut config : SkgConfig = config . clone ();
        config . initial_node_limit = 3;
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response : SaveResponse = update_from_and_rerender_buffer (
          &mut stream, &requested, &config, tantivy, &graph,
          &Ok (view_id), &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (),
                  "the request must save, got {:?}", response . errors );
        response . saved_view };
      println! ("Result of a request on an editable node:\n{}", result);
      let expected = indoc! {"
        * (skg (node (id 1) (repo main) (affectsParent na) (rels (contains (out 3))))) 1
        1 body
        ** (skg (node (id 11) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 11
        11 body
        ** (skg (node (id 12) (repo main) (rels (contains (in 1 (ancestors 1)) (out 4)) (birth (contains in 1))))) 12
        12 body
        *** (skg (node (id 121) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 2)) (birth (contains in 1))))) 121
        *** (skg (node (id 122) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) 122
        *** (skg (node (id 123) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 123
        *** (skg (node (id 124) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 124
        ** (skg (node (id 13) (repo main) writeProtected omittedBody (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) 13
      "};
      assert_metadata_eq!(result, expected,
        "a request on editable 1 unfolds its content within the budget");
      Ok (( )) }

// ===================================================
// Test: a request on a write-protected occurrence moves editability
// ===================================================
// A view of 1 and 12 draws root 12 editable, with its content, and
// the 12 under 1 as a write-protected leaf. A request on that leaf
// moves editability to it; root 12 is write-protected and keeps its
// drawn content, no longer as members. A request on root 12, whose
// drawn children are unchanged, moves editability back: root 12 is
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let mut save = async | buffer : &str | -> Result<String, Box<dyn Error>> {
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response : SaveResponse = update_from_and_rerender_buffer (
          &mut stream, buffer, &config, tantivy, &graph,
          &Ok (view_id . clone ()), &mut views_state ) . await ?;
        assert! ( response . errors . is_empty (),
                  "the buffer must save, got {:?}\n\nbuffer:\n{}",
                  response . errors, buffer );
        Ok (response . saved_view) };
      let root_12 : &str = "* (skg (node (id 12) (repo main)";
      let under_1 : &str = "** (skg (node (id 12) (repo main)";
      let line_starting = |buffer : &str, prefix : &str| -> String {
        buffer . lines () . find ( |line| line . starts_with (prefix) )
          . unwrap_or_else ( || panic! ( "no line starts {:?}:\n{}", prefix, buffer ))
          . to_string () };
      let root_12_content = |buffer : &str| -> Vec<String> {
        buffer . lines ()
          . skip_while ( |line| ! line . starts_with (root_12) ) . skip (1)
          . filter ( |line| line . starts_with ("*") )
          . map ( |line| line . replace ("writeProtected ", "")
                              . replace ("omittedBody ", "")
                              // write-protected, the content stops being members
                              . replace ("(affectsParent false) ", "")
                              . replace (" (birth (contains in 1))", "") )
          . collect () };
      let moved : String = save (
        &requesting_editable_at (&opened, "(node (id 12) (repo main) writeProtected")
      ) . await ?;
      assert! ( ! line_starting (&moved, under_1) . contains ("writeProtected")
                && line_starting (&moved, root_12) . contains ("writeProtected"),
                "editability must move to the 12 under 1:\n{}", moved );
      assert! ( ! root_12_content (&moved) . is_empty (),
                "root 12 keeps its drawn content:\n{}", moved );
      let moved_back : String = save (
        &requesting_editable_at (
          &moved, "* (skg (node (id 12) (repo main) (affectsParent na) writeProtected")
      ) . await ?;
      println! ("Result of moving editability back:\n{}", moved_back);
      assert! ( line_starting (&moved_back, under_1) . contains ("writeProtected")
                && ! line_starting (&moved_back, root_12) . contains ("writeProtected"),
                "editability must move back to root 12:\n{}", moved_back );
      assert_eq! ( root_12_content (&moved_back), root_12_content (&moved),
                   "root 12's drawn content is kept:\n{}", moved_back );
      let again : String = save (&moved_back) . await ?;
      assert_eq! ( again, moved_back, "a further save changes nothing" );
      Ok (( )) }

// ===================================================
// Test: an AliasFolder fills WHOLE and is budget-NEUTRAL (§5.5).
// ===================================================
// Companion to initial_view_bfs::test_budget_content_beats_subscribers (which
// pins the same neutrality for the auto-rendered SubscribeeFolder). An AliasFolder,
// unlike a SubscribeeFolder, is NOT auto-rendered de-novo -- it appears only on an
// explicit `aliases` view request -- so this exercises the request path through
// the rerender (build_and_integrate_aliases / reconcile_aliasFolder_children),
// neither of which takes node_budget.
//
// Fixture: root r contains the chain c1 -> c2, and r has two aliases. The budget
// counts vognode EXPANSIONS (cost 1), so the whole content here is exactly 3
// expansions: r, c1, c2. With limit=3 the chain fully expands (c2 editable)
// AND the AliasFolder shows BOTH aliases. The alias members are non-vognodes, so they
// cost nothing: were they charged, the 3 units could not also cover c2, which
// would then be left write-protected (verified: at limit=2 c2 *is* write-protected
// while the AliasFolder stays whole). That c2 is editable here is the guarantee
// this test pins.
async fn test_budget_aliasfolder_is_neutral (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let input_org_text = indoc! {"
        * (skg (node (id r) (repo main) (viewRequests (folder aliases)))) r
        ** (skg (node (id c1) (repo main))) c1
        *** (skg (node (id c2) (repo main))) c2
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
        let mut stream : TcpStream = mk_test_tcp_stream ();
        let response = update_from_and_rerender_buffer (
          &mut stream,
          input_org_text, &config, tantivy, &graph,
          &Err ( String::new () ), &mut views_state ) . await ?;
        response . saved_view };

      println!("alias-budget (budget 3):\n{}", result);

      let expected = indoc! {"
        * (skg (node (id r) (repo main) (affectsParent na) (rels (contains (out 1)) (aliases 2)))) r
        ** (skg aliasFolder)
        *** (skg alias) first alias
        *** (skg alias) second alias
        ** (skg (node (id c1) (repo main) (rels (contains (in 1 (ancestors 1)) (out 1)) (birth (contains in 1))))) c1
        *** (skg (node (id c2) (repo main) (rels (contains (in 1 (ancestors 1))) (birth (contains in 1))))) c2
      "};
      assert_metadata_eq!(result, expected,
        "budget 3 expands the whole content chain; the AliasFolder is whole + budget-neutral");

      Ok (( )) }

/// Save 'buffer' as the view 'view_id', asserting that it saves.
async fn save_view (
  buffer      : &str,
  config      : &SkgConfig,
  tantivy     : &TantivyIndex,
  graph       : &InRustGraphHandle,
  view_id     : &ViewId,
  views_state : &mut ViewsState,
) -> Result<String, Box<dyn Error>> {
  let mut stream : TcpStream = mk_test_tcp_stream ();
  let response : SaveResponse = update_from_and_rerender_buffer (
    &mut stream, buffer, config, tantivy, graph,
    &Ok (view_id . clone ()), views_state ) . await ?;
  assert! ( response . errors . is_empty (),
            "the buffer must save, got {:?}\n\nbuffer:\n{}",
            response . errors, buffer );
  Ok (response . saved_view) }

// ===================================================
// Test: a write-protect request keeps the children editable
// ===================================================
// A view of 12 draws it editable, with 121..124 editable under it
// (and its containerward branch, 1).
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["12"] ) ?;
      let requested : String = requesting_at (
        &opened . replacen ( ")))) 12\n", ")))) twelve\n", 1 ),
        "(node (id 12) (repo main)", "writeProtectedView" );
      let saved : String = save_view (
        &requested, &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      println! ("Result of a write-protect request:\n{}", saved);
      let root : &str = saved . lines () . next () . unwrap ();
      assert! ( root . contains ("writeProtected") && root . ends_with (" twelve"),
                "root 12 must be write-protected, with the saved title:\n{}", saved );
      assert! ( ! saved . contains ("viewRequests"), "{}", saved );
      let children : Vec<&str> = // not the 1 the root's containerward branch draws
        lines_starting (&saved, "** (skg (node (id 12");
      assert_eq! ( children . len (), 4, "{}", saved );
      assert! ( children . iter () . all ( |line|
                  line . contains ("(affectsParent false)")
                  && ! line . contains ("writeProtected") ),
                "121..124 must be editable non-members:\n{}", saved );
      let grandchildren : Vec<&str> = lines_starting (&saved, "*** ");
      assert_eq! ( grandchildren . len (), 3, "{}", saved );
      assert! ( grandchildren . iter () . all ( |line|
                  ! line . contains ("affectsParent")
                  && ! line . contains ("writeProtected") ),
                "their content must stay editable members:\n{}", saved );
      let again : String = save_view (
        &saved, &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["12"] ) ?;
      let saved : String = save_view (
        &requesting_at (&opened, "(node (id 12) (repo main)",
                        "writeProtectedSubtreeView"),
        &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      println! ("Result of a write-protect-subtree request:\n{}", saved);
      let headlines : Vec<&str> = lines_starting (&saved, "*");
      assert_eq! ( headlines . len (), 9, // with the 1 of the root's containerward branch
                   "{}", saved );
      assert! ( headlines . iter () . all ( |line| line . contains ("writeProtected") ),
                "everything must be write-protected:\n{}", saved );
      assert! ( headlines [1 ..] . iter () . all ( |line|
                  line . contains ("(affectsParent false)") ),
                "every child must be a non-member:\n{}", saved );
      assert! ( ! saved . contains ("viewRequests")
                && ! saved . contains ("body\n"),
                "no request, and no body, is left:\n{}", saved );
      Ok (( )) }

// ===================================================
// Test: a write-protect request on a write-protected occurrence is a no-op
// ===================================================
// A view of 1 and 12 draws the 12 under 1 as a write-protected leaf.
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let plain : String = save_view (
        &opened, &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      for request in [ "writeProtectedView", "writeProtectedSubtreeView" ] {
        let requested : String = save_view (
          &requesting_at (&plain, "(node (id 12) (repo main) writeProtected", request),
          &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
        assert_eq! ( requested, plain, "{} on a write-protected occurrence", request ); }
      Ok (( )) }

// ===================================================
// Test: write-protecting one occurrence while requesting another moves
// editability, and a request moves it back
// ===================================================
// A view of 1 and 12 draws root 12 editable, with 121..124, and the
// 12 under 1 write-protected. One save requests root 12
// write-protected and the 12 under 1 editable, and folds the 121
// under root 12. 121..124 are then drawn once under root 12, as
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1", "12"] ) ?;
      let root_12 : &str = "* (skg (node (id 12) (repo main)";
      let under_1 : &str = "** (skg (node (id 12) (repo main)";
      let folded_121 : &str = "** (skg folded (node (id 121) ";
      let content_under = |buffer : &str, root : &str| -> Vec<String> {
        // 12's content subtree, not root 12's containerward branch (1)
        subtree_headlines (buffer, root) . iter ()
          . filter ( |line| line . contains ("(id 12") )
          . map ( |line| line . to_string () )
          . collect () };
      let ids_under = |buffer : &str, root : &str| -> Vec<String> {
        content_under (buffer, root) . iter ()
          . filter ( |line| line . len () - line . trim_start_matches ('*') . len ()
                            == root . find (' ') . unwrap () + 1 )
          . map ( |line| line . split ("(id ") . nth (1) . unwrap ()
                              . split (')') . next () . unwrap () . to_string () )
          . collect () };
      let edited : String = requesting_editable_at (
        &requesting_at ( &opened, "(node (id 12) (repo main) (affectsParent na)",
                         "writeProtectedView" ),
        "(node (id 12) (repo main) writeProtected" )
        . replacen ( "** (skg (node (id 121) ", folded_121, 1 );
      let moved : String = save_view (
        &edited, &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      println! ("Result of moving editability:\n{}", moved);
      assert! ( lines_starting (&moved, root_12) [0] . contains ("writeProtected")
                && ! lines_starting (&moved, under_1) [0] . contains ("writeProtected"),
                "editability must move to the 12 under 1:\n{}", moved );
      assert_eq! ( ids_under (&moved, root_12), [ "121", "122", "123", "124" ],
                   "root 12 draws its content once:\n{}", moved );
      assert! ( content_under (&moved, root_12) . iter ()
                  . all ( |line| line . contains ("writeProtected")
                                 && line . contains ("(affectsParent false)") ),
                "under root 12, write-protected non-members:\n{}", moved );
      assert_eq! ( ids_under (&moved, under_1), [ "121", "122", "123", "124" ],
                   "{}", moved );
      assert! ( content_under (&moved, under_1) . iter ()
                  . all ( |line| ! line . contains ("writeProtected")
                                 && ! line . contains ("affectsParent") ),
                "under the 12 under 1, editable members:\n{}", moved );
      let moved_back : String = save_view (
        &requesting_editable_at (
          &moved, "(node (id 12) (repo main) (affectsParent na) writeProtected" ),
        &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      println! ("Result of moving editability back:\n{}", moved_back);
      assert! ( ! lines_starting (&moved_back, root_12) [0] . contains ("writeProtected")
                && lines_starting (&moved_back, under_1) [0] . contains ("writeProtected"),
                "editability must move back to root 12:\n{}", moved_back );
      assert_eq! ( ids_under (&moved_back, root_12), [ "121", "122", "123", "124" ],
                   "{}", moved_back );
      assert! ( content_under (&moved_back, root_12) . iter ()
                  . all ( |line| ! line . contains ("writeProtected")
                                 && ! line . contains ("affectsParent") ),
                "root 12's content must be editable members again:\n{}", moved_back );
      assert! ( content_under (&moved_back, root_12) [0] . starts_with (folded_121),
                "the adopted 121 keeps its fold:\n{}", moved_back );
      Ok (( )) }

// ===================================================
// Test: a parked content node is drawn once
// ===================================================
// In a view of 1, 13 is marked as not affecting its parent and saved:
// 1 no longer contains 13, which stays under 1 as a non-member. A
// save of another view of 1 puts 13 back in 1's content. The first
// view, rerendered as collateral, adopts the parked 13 as 1's member
// instead of drawing a second 13 beside it.
async fn test_parked_content_node_is_drawn_once (
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
      let (view_id, opened) : (ViewId, String) =
        open_content_view (
          &config, tantivy, &graph, &mut views_state, &["1"] ) ?;
      let parked : String = save_view (
        &opened . replacen ( "** (skg (node (id 13) (repo main) ",
                             "** (skg (node (id 13) (repo main) (affectsParent false) ", 1 ),
        &config, tantivy, &graph, &view_id, &mut views_state ) . await ?;
      assert! ( lines_starting (&parked, "** (skg (node (id 13) ") . len () == 1
                && parked . contains ("(affectsParent false)"),
                "13 stays under 1 as a non-member:\n{}", parked );
      let other_view_id : ViewId = ViewId::ContentView ("other-view-of-1" . to_string ());
      let other : String = {
        let (text, pids, tree) : (String, Vec<ID>, Tree<Viewnode>) =
          multi_root_view (&config, Some (tantivy), &[ ID::from ("1") ]) ?;
        views_state . open_views . register_view (
          &graph . load_full (), other_view_id . clone (), tree, &pids );
        text };
      assert! ( ! other . contains ("(id 13)"), "1 no longer contains 13:\n{}", other );
      save_view (
        &format! ( "{}** (skg (node (id 13) (repo main) writeProtected)) 13\n", other ),
        &config, tantivy, &graph, &other_view_id, &mut views_state ) . await ?;
      let collateral : String = {
        let viewforest : &ViewForest =
          views_state . open_views . viewid_to_view (&view_id)
          . expect ("the first view is registered");
        viewforest_to_string (viewforest, &config) ? };
      println! ("The first view after the other's save:\n{}", collateral);
      let thirteens : Vec<&str> = lines_starting (&collateral, "** (skg (node (id 13) ");
      assert! ( thirteens . len () == 1
                && ! thirteens [0] . contains ("affectsParent"),
                "13 is drawn once, as 1's member:\n{}", collateral );
      Ok (( )) }
