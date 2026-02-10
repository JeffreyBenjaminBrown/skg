// cargo test --test content_view -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::render::content_view::{multi_root_view, single_root_view};
use skg::test_utils::run_with_test_db;
use skg::dbs::typedb::search::climb_containerward_and_fetch_rootish_context;
use skg::dbs::typedb::search::path_containerward_to_end_cycle_and_or_branches;
use skg::types::misc::{ID, SkgConfig};

#[test]
fn test_a_mess_of_stuff
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-content-view",
    "tests/content_view/fixtures",
    "/tmp/tantivy-test-content-view",
    |config, driver, _tantivy| Box::pin ( async move {
      run_path_and_root_tests ( config, driver ) . await
    } )) }

#[test]
fn test_multi_root_view
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-multi-root-view",
    "tests/content_view/fixtures-2",
    "/tmp/tantivy-test-multi-root-view",
    |config, driver, _tantivy| Box::pin ( async move {
      test_multi_root_view_logic ( config, driver ) . await
    } )) }

async fn run_path_and_root_tests (
  config : &SkgConfig,
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
  config : &SkgConfig,
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
    & focii,
    false
  ) . await ?;

  println!("Multi-root view result:\n{}", result);

  let expected = indoc! {"* (skg (node (id 1) (source main) (graphStats (containers 0)))) 1
                          1 has a body
                          * (skg (node (id 2) (source main) (graphStats (containers 0)))) 2
                          * (skg (node (id 1) (source main) indefinitive (graphStats (containers 0)))) 1
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
    |config, driver, _tantivy| Box::pin ( async move {
      // Test with node "a" which has a cycle (a -> b -> c -> b)
      let result : String = single_root_view (
        driver,
        config,
        &ID ( "a".to_string () ),
        false
      ) . await ?;

      println!("Single root view with cycle result:\n{}", result);

      let expected = indoc! {"* (skg (node (id a) (source main) (graphStats (containers 0) (contents 1)))) a
                              ** (skg (node (id b) (source main) (graphStats (containers 2) (contents 1)))) b
                              b has a body
                              *** (skg (node (id c) (source main) (graphStats (contents 1)) (viewStats containsParent))) c
                              **** (skg (node (id b) (source main) indefinitive (graphStats (containers 2) (contents 1)) (viewStats cycle containsParent))) b
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
    |config, driver, _tantivy| Box::pin ( async move {
      // Test with multiple roots that share a node
      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () )
      ];
      let result : String = multi_root_view (
        driver,
        config,
        & focii,
        false
      ) . await ?;

      println!("Multi root view with shared nodes result:\n{}", result);

      // BFS processes all roots (generation 1) before children (generation 2),
      // so node 2 appears first as a root, then as a child (marked indefinitive).
      // Definitive nodes with subscriptions get SubscribeeCol children,
      // and each SubscribeeCol has Subscribee children with the subscribed IDs.
      let expected = indoc! {
        "* (skg (node (id 1) (source main) (graphStats (containers 0) (contents 2)))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg (node (id 2) (source main) indefinitive (graphStats (linksIn 1) extraIDs subscribing))) title 2
         ** (skg (node (id 3) (source main) (graphStats (linksIn 1) extraIDs overriding subscribing))) title 3
         this one string could span pages
         *** (skg subscribeeCol) it subscribes to these
         **** (skg (node (id 4) (source main) indefinitive (graphStats (containers 0) extraIDs overriding subscribing))) This is a [[id:shgulasdghu][test]] of a second kind.
         **** (skg (node (id 5) (source main) indefinitive (graphStats (containers 0) (linksIn 1) extraIDs overriding subscribing))) this title includes a [[id:22][textlink to another file]]
         * (skg (node (id 2) (source main) (graphStats (linksIn 1) extraIDs subscribing))) title 2
         this one string could span pages
         ** (skg subscribeeCol) it subscribes to these
         *** (skg (node (id 4) (source main) indefinitive (graphStats (containers 0) extraIDs overriding subscribing))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg (node (id 5) (source main) indefinitive (graphStats (containers 0) (linksIn 1) extraIDs overriding subscribing))) this title includes a [[id:22][textlink to another file]]
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
    |config, driver, _tantivy| Box::pin ( async move {
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
        & focii,
        false
      ) . await ?;

      println!("Multi root view with limit=3 result:\n{}", result);

      // Expected: roots 1 and 2 (gen 1), then children 2 and 3 truncated (gen 2)
      // Generation 2 has 2 nodes, so 2+2=4 > 3, apply truncation
      // Limit node is at index 1 (the 4th node overall, 2nd in gen 2)
      // That's node 3, child of node 1. Its parent is node 1.
      // Complete sibling group: truncate both node 2 (repeated) and node 3
      // Then truncate everything after node 1 in gen 1 - which includes root node 2
      // Note: Root node 2 has SubscribeeCol added before truncation (when it was definitive),
      // but since it becomes indefinitive after truncation, this is somewhat inconsistent.
      // The SubscribeeCol includes Subscribee children created when the node was definitive.
      // For now we accept this behavior.
      let expected = indoc! {
        "* (skg (node (id 1) (source main) (graphStats (containers 0) (contents 2)))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg (node (id 2) (source main) indefinitive (graphStats (linksIn 1) extraIDs subscribing))) title 2
         ** (skg (node (id 3) (source main) indefinitive (graphStats (linksIn 1) extraIDs overriding subscribing))) title 3
         * (skg (node (id 2) (source main) indefinitive (graphStats (linksIn 1) extraIDs subscribing))) title 2
         ** (skg subscribeeCol) it subscribes to these
         *** (skg (node (id 4) (source main) indefinitive (graphStats (containers 0) extraIDs overriding subscribing))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg (node (id 5) (source main) indefinitive (graphStats (containers 0) (linksIn 1) extraIDs overriding subscribing))) this title includes a [[id:22][textlink to another file]]
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
    |config, driver, _tantivy| Box::pin ( async move {
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
        &ID ( "1".to_string () ),
        false
      ) . await ?;

      println!("Result with multiple sibling groups:\n{}", result);

      let expected = indoc! {"* (skg (node (id 1) (source main) (graphStats (containers 0) (contents 2)))) 1
                              1 body
                              ** (skg (node (id 11) (source main) (graphStats (contents 2)))) 11
                              11 body
                              *** (skg (node (id 111) (source main) indefinitive)) 111
                              *** (skg (node (id 112) (source main) indefinitive)) 112
                              ** (skg (node (id 12) (source main) indefinitive (graphStats (contents 1)))) 12
                              "};
      assert_eq!(result, expected,
                 "Truncated nodes should have no children (121 removed)");

      Ok (( )) } )) }
