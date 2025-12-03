// cargo test --test content_view -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::content_view::{multi_root_view, single_root_view};
use skg::test_utils::run_with_test_db;
use skg::media::typedb::search::{
  climb_containerward_and_fetch_rootish_context,
  path_containerward_to_end_cycle_and_or_branches, };
use skg::types::ID;

#[test]
fn test_a_mess_of_stuff
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-content-view",
    "tests/content_view/fixtures",
    "/tmp/tantivy-test-content-view",
    |config, driver| Box::pin ( async move {
      run_path_and_root_tests ( config, driver ) . await
    } )) }

#[test]
fn test_multi_root_view
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-multi-root-view",
    "tests/content_view/fixtures-2",
    "/tmp/tantivy-test-multi-root-view",
    |config, driver| Box::pin ( async move {
      test_multi_root_view_logic ( config, driver ) . await
    } )) }

async fn run_path_and_root_tests (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver
) -> Result<(), Box<dyn std::error::Error>> {

  // Test the path from node "4" to the root container
  match path_containerward_to_end_cycle_and_or_branches (
    & config . db_name,
    & driver,
    & ID("4".to_string() )
  ).await {
    Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
      path,
      vec![
        ID ( "4".to_string() ),
        ID ( "2".to_string() ),
        ID ( "1".to_string() ) ],
      "Unexpected path to root container from node '4'.");
    }, Err(e) => {
      panic!("Error finding path to root container: {}", e); } }

  match climb_containerward_and_fetch_rootish_context (
    & config . db_name,
    & driver,
    & ID("4".to_string() )
  ).await {
    Ok(root) => { assert_eq!(
      root,
      ID ( "1".to_string() ),
      "Root of node '4' should be 1." ) },
    Err(e) => { panic!(
      "Error finding root container from id {}", e); } }

  // Test the path "to root" from node "cycle-3".
  // (1 contains 2 contains 3 contains 1.)
  match path_containerward_to_end_cycle_and_or_branches (
    & config . db_name,
    & driver,
    & ID("cycle-3".to_string() )
  ).await {
    Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
      path,
      vec![
        ID ( "cycle-3".to_string() ),
        ID ( "cycle-2".to_string() ),
        ID ( "cycle-1".to_string() ) ],
      "Unexpected path to \"root container\" from node 'cycle-3'.");
    }, Err(e) => {
      panic!("Error finding path to root container: {}", e); } }

  // Test the path "to root" from node "cycle-1".
  // (1 contains 2 contains 3 contains 1.)
  match path_containerward_to_end_cycle_and_or_branches (
    & config . db_name,
    & driver,
    & ID("cycle-1".to_string() )
  ).await {
    Ok((path, _cycle_node, _multi_containers)) => { assert_eq!(
      path,
      vec![
        ID ( "cycle-1".to_string() ),
        ID ( "cycle-3".to_string() ),
        ID ( "cycle-2".to_string() ),
      ],
      "Unexpected path to \"root container\" from node 'cycle-1'.");
    }, Err(e) => {
      panic!("Error finding path to root container: {}", e); } }

  Ok (( )) }

async fn test_multi_root_view_logic (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver
) -> Result<(), Box<dyn std::error::Error>> {

  let focii : Vec<ID> = vec![
    ID("1".to_string()),
    ID("2".to_string()),
    ID("1".to_string())
  ];
  let result : String = multi_root_view (
    & driver,
    & config,
    & focii
  ) . await ?;

  println!("Multi-root view result:\n{}", result);

  let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0)))) 1
                          1 has a body
                          * (skg (id 2) (source main) (view (rels (containers 0)))) 2
                          * (skg (id 1) (source main) (view (rels (containers 0))) (code indefinitive)) 1
                          "};
  assert_eq!(result, expected,
             "Multi-root view should produce exact expected output");

  Ok (( )) }

#[test]
fn test_single_root_view_with_cycle
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-single-root-view-cycle",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-single-root-view-cycle",
    |config, driver| Box::pin ( async move {
      // Test with node "a" which has a cycle (a -> b -> c -> b)
      let result : String = single_root_view (
        driver,
        config,
        &ID ( "a".to_string () )
      ) . await ?;

      println!("Single root view with cycle result:\n{}", result);

      let expected = indoc! {"* (skg (id a) (source main) (view (rels (containers 0) (contents 1)))) a
                              ** (skg (id b) (source main) (view (rels (containers 2) (contents 1)))) b
                              b has a body
                              *** (skg (id c) (source main) (view (rels containsParent (contents 1)))) c
                              **** (skg (id b) (source main) (view cycle (rels containsParent (containers 2) (contents 1))) (code indefinitive)) b
                              "};
      assert_eq!(result, expected,
                 "Single root view should detect cycle and mark repeated node");

      Ok (( )) } )) }

#[test]
fn test_multi_root_view_with_shared_nodes
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-multi-root-view-shared",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-multi-root-view-shared",
    |config, driver| Box::pin ( async move {
      // Test with multiple roots that share a node
      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () )
      ];
      let result : String = multi_root_view (
        driver,
        config,
        & focii
      ) . await ?;

      println!("Multi root view with shared nodes result:\n{}", result);

      // BFS processes all roots (generation 1) before children (generation 2),
      // so node 2 appears first as a root, then as a child (marked indefinitive)
      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 2)))) title 1
                              This one string could span pages,
                              and it can include newlines, no problem.
                              ** (skg (id 2) (source main) (view (rels (linksIn 1))) (code indefinitive)) title 2
                              ** (skg (id 3) (source main) (view (rels (linksIn 1)))) title 3
                              this one string could span pages
                              * (skg (id 2) (source main) (view (rels (linksIn 1)))) title 2
                              this one string could span pages
                              "};
      assert_eq!(result, expected,
                 "Multi root view should detect cross-tree duplicates");

      Ok (( )) } )) }

#[test]
fn test_multi_root_view_with_node_limit
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-multi-root-view-limit",
    "tests/typedb/fixtures",
    "/tmp/tantivy-test-multi-root-view-limit",
    |config, driver| Box::pin ( async move {
      // Test with two roots that share a node, with node limit
      // Tree structure: 1 -> (2, 3), 2 (standalone root)
      // Generations: 1: [1, 2], 2: [2 (repeated), 3]
      // With limit=3, should render 1, 2, then 2 and 3 are both truncated (sibling group)
      let mut test_config = config.clone();
      test_config.initial_node_limit = 3;

      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () )
      ];
      let result : String = multi_root_view (
        driver,
        &test_config,
        & focii
      ) . await ?;

      println!("Multi root view with limit=3 result:\n{}", result);

      // Expected: roots 1 and 2 (gen 1), then children 2 and 3 truncated (gen 2)
      // Generation 2 has 2 nodes, so 2+2=4 > 3, apply truncation
      // Limit node is at index 1 (the 4th node overall, 2nd in gen 2)
      // That's node 3, child of node 1. Its parent is node 1.
      // Complete sibling group: truncate both node 2 (repeated) and node 3
      // Then truncate everything after node 1 in gen 1 - which includes root node 2
      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 2)))) title 1
                              This one string could span pages,
                              and it can include newlines, no problem.
                              ** (skg (id 2) (source main) (view (rels (linksIn 1))) (code indefinitive)) title 2
                              ** (skg (id 3) (source main) (view (rels (linksIn 1))) (code indefinitive)) title 3
                              * (skg (id 2) (source main) (view (rels (linksIn 1))) (code indefinitive)) title 2
                              "};
      assert_eq!(result, expected,
                 "Multi root view with limit=3 should truncate generation 2 sibling group");

      Ok (( )) } )) }

#[test]
fn test_limit_with_multiple_sibling_groups
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-multiple-sibling-groups",
    "tests/content_view/fixtures-3",
    "/tmp/tantivy-test-multiple-sibling-groups",
    |config, driver| Box::pin ( async move {
      // Test that truncation correctly stops at sibling group boundaries
      // Tree structure:
      //   1 (gen 1)
      //   ├─ 11 (gen 2)
      //   │  ├─ 111 (gen 3)
      //   │  └─ 112 (gen 3)
      //   └─ 12 (gen 2)
      //      └─ 121 (gen 3)
      //
      // With limit=4: 1 (1), 11 (2), 12 (3), 111 (4)
      // Generation 3 has 3 nodes, so 4+3=7 > 4, apply truncation
      // Limit node is 111 (index 0 in gen 3)
      // Complete its sibling group: truncate both 111 and 112
      // Stop there - do NOT truncate 121 (different parent)
      // Then truncate 12 (everything after 11 in gen 2)

      let mut test_config = config.clone();
      test_config.initial_node_limit = 4;

      let result : String = single_root_view (
        driver,
        &test_config,
        &ID ( "1".to_string () )
      ) . await ?;

      println!("Result with multiple sibling groups:\n{}", result);

      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 2)))) 1
                              1 body
                              ** (skg (id 11) (source main) (view (rels (contents 2)))) 11
                              11 body
                              *** (skg (id 111) (source main) (code indefinitive)) 111
                              *** (skg (id 112) (source main) (code indefinitive)) 112
                              ** (skg (id 12) (source main) (view (rels (contents 1))) (code indefinitive)) 12
                              "};
      assert_eq!(result, expected,
                 "Truncated nodes should have no children (121 removed)");

      Ok (( )) } )) }
