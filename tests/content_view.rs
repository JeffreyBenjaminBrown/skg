// cargo test --test content_view -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::mk_org_text::{multi_root_view, single_root_view};
use skg::test_utils::run_with_test_db;
use skg::typedb::search::{
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

  let expected = indoc! {"* (skg (id 1) (rels (containers 0))) 1
                          1 has a body
                          * (skg (id 2) (rels (containers 0))) 2
                          * (skg (id 1) repeated (rels (containers 0))) 1
                          Repeated, probably above. Edit there, not here.
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

      let expected = indoc! {"* (skg (id a) (rels (containers 0) (contents 1))) a
                              ** (skg (id b) (rels (containers 2) (contents 1))) b
                              b has a body
                              *** (skg (id c) (rels containsParent (contents 1))) c
                              **** (skg (id b) repeated (rels containsParent (containers 2) (contents 1))) b
                              Repeated, probably above. Edit there, not here.
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

      let expected = indoc! {"* (skg (id 1) (rels (containers 0) (contents 2))) title 1
                              This one string could span pages,
                              and it can include newlines, no problem.
                              ** (skg (id 2) (rels (linksIn 1))) title 2
                              this one string could span pages
                              ** (skg (id 3) (rels (linksIn 1))) title 3
                              this one string could span pages
                              * (skg (id 2) repeated (rels (linksIn 1))) title 2
                              Repeated, probably above. Edit there, not here.
                              "};
      assert_eq!(result, expected,
                 "Multi root view should detect cross-tree duplicates");

      Ok (( )) } )) }
