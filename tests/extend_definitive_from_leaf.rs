// cargo test --test extend_definitive_from_leaf -- --nocapture
//
// Tests for definitive view request expansion.
// These tests verify that when a node has a "Definitive" view request,
// its content children are expanded from disk using BFS with truncation.

use indoc::indoc;
use std::error::Error;

use skg::test_utils::run_with_test_db;
use skg::serve::handlers::save_buffer::update_from_and_rerender_buffer;

// ===================================================
// Test: Definitive view with limit=10
// Gives room for all 7 descendents.
// ===================================================
#[test]
fn test_definitive_view_limit_10
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-limit-10",
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-limit-10",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indefinitive (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result = {
        let mut config = config.clone();
        config.initial_node_limit = 10;
        let response = update_from_and_rerender_buffer (
          input_org_text, driver, &config, tantivy ) . await ?;
        response.buffer_content };

      println!("Result with limit=10:\n{}", result);

      // With limit=10, all children should be expanded
      let expected = indoc! {"
        * (skg (node (id 1) (source main) (stats (containers 0) (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (stats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) (stats (contents 2)))) 121
        121 body
        **** (skg (node (id 1211) (source main))) 1211
        1211 body
        **** (skg (node (id 1212) (source main))) 1212
        1212 body
        *** (skg (node (id 122) (source main) (stats (contents 1)))) 122
        122 body
        **** (skg (node (id 1221) (source main))) 1221
        1221 body
        *** (skg (node (id 123) (source main))) 123
        123 body
        *** (skg (node (id 124) (source main))) 124
        124 body
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (stats (containers 0)))) 2
      "};

      assert_eq!(result, expected,
        "Definitive view with limit=10 should expand all children");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view with limit=5 or limit=6
// ===================================================
// - Hits limit at 1211 or 1212
// - Completes 1211's sibling group: 1211, 1212 both indefinitive
// - Truncates (renders indef) nodes after 121 in gen 2: 122, 123, 124

#[test]
fn test_definitive_view_limit_5_or_6
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-limit-5-or-6",
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-limit-5-or-6",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indefinitive (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result_5 = {
        let mut config5 = config.clone();
        config5.initial_node_limit = 5;
        let response_5 = update_from_and_rerender_buffer (
          input_org_text, driver, &config5, tantivy ) . await ?;
        response_5.buffer_content };
      let result_6 = {
        let mut config6 = config.clone();
        config6.initial_node_limit = 6;
        let response_6 = update_from_and_rerender_buffer (
          input_org_text, driver, &config6, tantivy ) . await ?;
        response_6.buffer_content };

      println!("Result with limit=5:\n{}", result_5);
      println!("Result with limit=6:\n{}", result_6);

      // With limit=5 or 6:
      // - 121, 122, 123, 124 are added (4 nodes)
      // - Then 1211 is the 5th node, limit hit (or 6th slot available)
      // - Complete sibling group: 1211, 1212 both indefinitive
      // - Truncate after 121 in parent gen: 122, 123, 124 indefinitive
      // Both limits produce the same result because truncation
      // completes the sibling group.
      let expected = indoc! {"
        * (skg (node (id 1) (source main) (stats (containers 0) (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (stats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) (stats (contents 2)))) 121
        121 body
        **** (skg (node (id 1211) (source main) indefinitive)) 1211
        **** (skg (node (id 1212) (source main) indefinitive)) 1212
        *** (skg (node (id 122) (source main) indefinitive (stats (contents 1)))) 122
        *** (skg (node (id 123) (source main) indefinitive)) 123
        *** (skg (node (id 124) (source main) indefinitive)) 124
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (stats (containers 0)))) 2
      "};

      assert_eq!(result_5, expected,
        "Definitive view with limit=5 should truncate at gen 3, complete sibling group");
      assert_eq!(result_6, expected,
        "Definitive view with limit=6 should produce the same result as limit=5");

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
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-limit-1-to-4",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 11))) 11
        ** (skg (node (id 12) indefinitive (viewRequests definitiveView))) 12
        ** (skg (node (id 13))) 13
        * (skg (node (id 2) (source main))) 2
      "};

      let result_1 = {
        let mut config1 = config.clone();
        config1.initial_node_limit = 1;
        let response_1 = update_from_and_rerender_buffer (
          input_org_text, driver, &config1, tantivy ) . await ?;
        response_1.buffer_content };
      let result_4 = {
        let mut config4 = config.clone();
        config4.initial_node_limit = 4;
        let response_4 = update_from_and_rerender_buffer (
          input_org_text, driver, &config4, tantivy ) . await ?;
        response_4.buffer_content };

      println!("Result with limit=1:\n{}", result_1);
      println!("Result with limit=4:\n{}", result_4);

      // With limit=1 or limit=4:
      // - Node 12 has 4 children (121, 122, 123, 124)
      // - Limit is hit on the first generation
      // - Sibling group is completed, all are indefinitive
      let expected = indoc! {"
        * (skg (node (id 1) (source main) (stats (containers 0) (contents 3)))) 1
        ** (skg (node (id 11) (source main))) 11
        ** (skg (node (id 12) (source main) (stats (contents 4)))) 12
        12 body
        *** (skg (node (id 121) (source main) indefinitive (stats (contents 2)))) 121
        *** (skg (node (id 122) (source main) indefinitive (stats (contents 1)))) 122
        *** (skg (node (id 123) (source main) indefinitive)) 123
        *** (skg (node (id 124) (source main) indefinitive)) 124
        ** (skg (node (id 13) (source main))) 13
        * (skg (node (id 2) (source main) (stats (containers 0)))) 2
      "};

      assert_eq!(result_1, expected,
        "Definitive view with limit=1 should mark all gen 2 children indefinitive");
      assert_eq!(result_4, expected,
        "Definitive view with limit=4 should produce the same result as limit=1");

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
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-conflict",
    |config, driver, tantivy| Box::pin ( async move {
      // Node 12 appears twice:
      // - First as a regular child of 1 (will be definitive after completion)
      // - Second with a definitive view request
      let input_org_text = indoc! {"
        * (skg (node (id 1) (source main))) 1
        ** (skg (node (id 12))) 12
        *** (skg (node (id 122) indefinitive)) 122
        * (skg (node (id 12) (source main) indefinitive (viewRequests definitiveView))) 12 copy
      "};

      let result = {
        let mut config = config.clone();
        config.initial_node_limit = 100;
        let response = update_from_and_rerender_buffer (
          input_org_text, driver, &config, tantivy ) . await ?;
        response.buffer_content };

      println!("Result with conflict:\n{}", result);

      let expected = indoc! {
        // The first 12 (child of 1) should become indefinitive.
        // The second 12 (root with request) should be expanded.
        // NOTE: The first 12 redefines the children of 12 as [122]
        // rather than [121,122,123,124].
        "* (skg (node (id 1) (source main) (stats (containers 0) (contents 1)))) 1
         ** (skg (node (id 12) (source main) indefinitive (stats (contents 1)))) 12
         *** (skg (node (id 122) (source main) indefinitive (stats (contents 1)))) 122
         * (skg (node (id 12) (source main) (stats (contents 1)))) 12
         ** (skg (node (id 122) (source main) (stats (contents 1)))) 122
         122 body
         *** (skg (node (id 1221) (source main))) 1221
         1221 body
      "};

      assert_eq!(result, expected,
        "First definitive instance should become indefinitive when second requests definitive");

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
    "tests/extend_definitive_from_leaf/fixtures-cycle",
    "/tmp/tantivy-test-definitive-view-cycle",
    |config, driver, tantivy| Box::pin ( async move {
      // Node a has definitive request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg (node (id cyc-a) (source main) indefinitive (viewRequests definitiveView))) cyc-a
      "};

      let result = {
        let mut config = config.clone();
        config.initial_node_limit = 100;
        let response = update_from_and_rerender_buffer (
          input_org_text, driver, &config, tantivy ) . await ?;
        response.buffer_content };

      println!("Result with cycle:\n{}", result);

      // a should expand to show b, and b's child a should be marked as cycle
      let expected = indoc! {"
        * (skg (node (id cyc-a) (source main) (stats (contents 1)))) cyc-a
        cyc-a body
        ** (skg (node (id cyc-b) (source main) (stats containsParent (contents 1)))) cyc-b
        cyc-b body
        *** (skg (node (id cyc-a) (source main) indefinitive cycle (stats containsParent (contents 1)))) cyc-a
      "};

      assert_eq!(result, expected,
        "Definitive view should detect cycles and mark them");

      Ok (( )) } )) }

// ===================================================
// Test: Definitive view expansion detects repeats
// ===================================================
// If a node is already visited (definitive elsewhere), it should be
// marked indefinitive when encountered again during expansion.

#[test]
fn test_definitive_view_with_repeat
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-definitive-view-repeat",
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-repeat",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 121) (source main))) 121
        * (skg (node (id 12) (source main) indefinitive (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config.clone();
        config.initial_node_limit = 100;
        let response = update_from_and_rerender_buffer (
          input_org_text, driver, &config, tantivy ) . await ?;
        response.buffer_content };

      println!("Result with repeat:\n{}", result);

      let expected = indoc! {
        // Upon saving, the indefinitive view of node 12 had no effect,
        // but the definitive view of 121 deleted its children.
        // The new 121 under 12 is rendered indefinitive,
        // because it was already rendered as a sibling of 12.
        "* (skg (node (id 121) (source main))) 121
         * (skg (node (id 12) (source main) (stats (contents 4)))) 12
         12 body
         ** (skg (node (id 121) (source main) indefinitive)) 121
         ** (skg (node (id 122) (source main) (stats (contents 1)))) 122
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
    "tests/extend_definitive_from_leaf/fixtures",
    "/tmp/tantivy-test-definitive-view-cleared",
    |config, driver, tantivy| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (node (id 12) (source main) indefinitive (viewRequests definitiveView))) 12
      "};

      let result = {
        let mut config = config.clone();
        config.initial_node_limit = 100;
        let response = update_from_and_rerender_buffer (
          input_org_text, driver, &config, tantivy ) . await ?;
        response.buffer_content };

      println!("Result:\n{}", result);

      // The viewRequests should be cleared after processing
      // So the output should NOT contain "viewRequests"
      assert!(!result.contains("viewRequests"),
        "viewRequests should be cleared after processing");

      Ok (( )) } )) }
