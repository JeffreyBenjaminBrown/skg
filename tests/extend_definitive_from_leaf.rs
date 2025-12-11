// cargo test --test extend_definitive_from_leaf -- --nocapture
//
// Tests for definitive view request expansion.
// These tests verify that when a node has a "Definitive" view request,
// its content children are expanded from disk using BFS with truncation.

use indoc::indoc;
use std::error::Error;

use skg::to_org::complete::contents::completeOrgnodeForest_collectingDefinitiveRequests;
use skg::to_org::expand::definitive::execute_definitive_view_requests;
use skg::org_to_text::orgnode_forest_to_string;
use skg::media::tree::pair_forest_with_save_instructions;
use skg::from_text::buffer_to_save_instructions;
use skg::test_utils::run_with_test_db;
use skg::types::SkgConfig;
use skg::types::trees::PairTree;


/// Helper to run the full completion + definitive expansion pipeline.
/// Returns the rendered org text.
async fn complete_and_expand_definitive (
  input_org_text : &str,
  config         : &SkgConfig,
  driver         : &typedb_driver::TypeDBDriver,
  node_limit     : usize,
) -> Result < String, Box<dyn Error> > {
  let mut config = config.clone();
  config.initial_node_limit = node_limit;
  let (orgnode_forest, save_instructions, _merge_instructions) =
    buffer_to_save_instructions ( input_org_text, &config, driver )
    . await ?;
  let mut errors : Vec < String > = Vec::new ();
  let mut paired_forest : Vec < PairTree > =
    pair_forest_with_save_instructions (
      orgnode_forest,
      &save_instructions );
  let (mut visited, definitive_requests) =
    completeOrgnodeForest_collectingDefinitiveRequests (
      &mut paired_forest,
      &config,
      driver,
      &mut errors ) . await ?;
  execute_definitive_view_requests (
    &mut paired_forest,
    definitive_requests,
    &config,
    driver,
    &mut visited,
    &mut errors ) . await ?;
  Ok ( orgnode_forest_to_string ( & paired_forest ) ) }

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
    |config, driver| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 11)) 11
        ** (skg (id 12) (code indefinitive (viewRequests definitiveView))) 12
        ** (skg (id 13)) 13
        * (skg (id 2) (source main)) 2
      "};

      let result = complete_and_expand_definitive (
        input_org_text, config, driver, 10 ) . await ?;

      println!("Result with limit=10:\n{}", result);

      // With limit=10, all children should be expanded
      let expected = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 11) (source main)) 11
        ** (skg (id 12) (source main)) 12
        12 body
        *** (skg (id 121) (source main)) 121
        121 body
        **** (skg (id 1211) (source main)) 1211
        1211 body
        **** (skg (id 1212) (source main)) 1212
        1212 body
        *** (skg (id 122) (source main)) 122
        122 body
        **** (skg (id 1221) (source main)) 1221
        1221 body
        *** (skg (id 123) (source main)) 123
        123 body
        *** (skg (id 124) (source main)) 124
        124 body
        ** (skg (id 13) (source main)) 13
        * (skg (id 2) (source main)) 2
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
    |config, driver| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 11)) 11
        ** (skg (id 12) (code indefinitive (viewRequests definitiveView))) 12
        ** (skg (id 13)) 13
        * (skg (id 2) (source main)) 2
      "};

      let result_5 = complete_and_expand_definitive (
        input_org_text, config, driver, 5 ) . await ?;
      let result_6 = complete_and_expand_definitive (
        input_org_text, config, driver, 6 ) . await ?;

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
        * (skg (id 1) (source main)) 1
        ** (skg (id 11) (source main)) 11
        ** (skg (id 12) (source main)) 12
        12 body
        *** (skg (id 121) (source main)) 121
        121 body
        **** (skg (id 1211) (source main) (code indefinitive)) 1211
        **** (skg (id 1212) (source main) (code indefinitive)) 1212
        *** (skg (id 122) (source main) (code indefinitive)) 122
        *** (skg (id 123) (source main) (code indefinitive)) 123
        *** (skg (id 124) (source main) (code indefinitive)) 124
        ** (skg (id 13) (source main)) 13
        * (skg (id 2) (source main)) 2
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
    |config, driver| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 11)) 11
        ** (skg (id 12) (code indefinitive (viewRequests definitiveView))) 12
        ** (skg (id 13)) 13
        * (skg (id 2) (source main)) 2
      "};

      let result_1 = complete_and_expand_definitive (
        input_org_text, config, driver, 1 ) . await ?;
      let result_4 = complete_and_expand_definitive (
        input_org_text, config, driver, 4 ) . await ?;

      println!("Result with limit=1:\n{}", result_1);
      println!("Result with limit=4:\n{}", result_4);

      // With limit=1 or limit=4:
      // - Node 12 has 4 children (121, 122, 123, 124)
      // - Limit is hit on the first generation
      // - Sibling group is completed, all are indefinitive
      let expected = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 11) (source main)) 11
        ** (skg (id 12) (source main)) 12
        12 body
        *** (skg (id 121) (source main) (code indefinitive)) 121
        *** (skg (id 122) (source main) (code indefinitive)) 122
        *** (skg (id 123) (source main) (code indefinitive)) 123
        *** (skg (id 124) (source main) (code indefinitive)) 124
        ** (skg (id 13) (source main)) 13
        * (skg (id 2) (source main)) 2
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
    |config, driver| Box::pin ( async move {
      // Node 12 appears twice:
      // - First as a regular child of 1 (will be definitive after completion)
      // - Second with a definitive view request
      // The first instance should become indefinitive
      let input_org_text = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 12)) 12
        * (skg (id 12) (source main) (code indefinitive (viewRequests definitiveView))) 12 copy
      "};

      let result = complete_and_expand_definitive (
        input_org_text, config, driver, 100 ) . await ?;

      println!("Result with conflict:\n{}", result);

      // The first 12 (child of 1) should become indefinitive
      // The second 12 (root with request) should be expanded
      // NOTE: Title comes from disk ("12"), not from buffer ("12 copy"),
      // because rebuild_node_as_definitive fetches canonical data from disk.
      let expected = indoc! {"
        * (skg (id 1) (source main)) 1
        ** (skg (id 12) (source main) (code indefinitive)) 12
        * (skg (id 12) (source main)) 12
        12 body
        ** (skg (id 121) (source main)) 121
        121 body
        *** (skg (id 1211) (source main)) 1211
        1211 body
        *** (skg (id 1212) (source main)) 1212
        1212 body
        ** (skg (id 122) (source main)) 122
        122 body
        *** (skg (id 1221) (source main)) 1221
        1221 body
        ** (skg (id 123) (source main)) 123
        123 body
        ** (skg (id 124) (source main)) 124
        124 body
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
    |config, driver| Box::pin ( async move {
      // Node a has definitive request
      // a contains b contains a (cycle)
      let input_org_text = indoc! {"
        * (skg (id cyc-a) (source main) (code indefinitive (viewRequests definitiveView))) cyc-a
      "};

      let result = complete_and_expand_definitive (
        input_org_text, config, driver, 100 ) . await ?;

      println!("Result with cycle:\n{}", result);

      // a should expand to show b, and b's child a should be marked as cycle
      let expected = indoc! {"
        * (skg (id cyc-a) (source main)) cyc-a
        cyc-a body
        ** (skg (id cyc-b) (source main)) cyc-b
        cyc-b body
        *** (skg (id cyc-a) (source main) (view cycle) (code indefinitive)) cyc-a
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
    |config, driver| Box::pin ( async move {
      // Node 121 appears first as a root (definitive)
      // Then 12 has a definitive view request, which contains 121
      // When 121 is encountered during expansion of 12, it should be indefinitive
      let input_org_text = indoc! {"
        * (skg (id 121) (source main)) 121
        * (skg (id 12) (source main) (code indefinitive (viewRequests definitiveView))) 12
      "};

      let result = complete_and_expand_definitive (
        input_org_text, config, driver, 100 ) . await ?;

      println!("Result with repeat:\n{}", result);

      // 121 is definitive as first root
      // When 12 expands, its child 121 should be indefinitive (repeat)
      // NOTE: Node 121 from buffer doesn't get body populated because
      // completeDefinitiveOrgnode doesn't set body from SkgNode.
      // This is a separate issue from definitive view requests.
      let expected = indoc! {"
        * (skg (id 121) (source main)) 121
        * (skg (id 12) (source main)) 12
        12 body
        ** (skg (id 121) (source main) (code indefinitive)) 121
        ** (skg (id 122) (source main)) 122
        122 body
        *** (skg (id 1221) (source main)) 1221
        1221 body
        ** (skg (id 123) (source main)) 123
        123 body
        ** (skg (id 124) (source main)) 124
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
    |config, driver| Box::pin ( async move {
      let input_org_text = indoc! {"
        * (skg (id 12) (source main) (code indefinitive (viewRequests definitiveView))) 12
      "};

      let result = complete_and_expand_definitive (
        input_org_text, config, driver, 100 ) . await ?;

      println!("Result:\n{}", result);

      // The viewRequests should be cleared after processing
      // So the output should NOT contain "viewRequests"
      assert!(!result.contains("viewRequests"),
        "viewRequests should be cleared after processing");

      Ok (( )) } )) }
