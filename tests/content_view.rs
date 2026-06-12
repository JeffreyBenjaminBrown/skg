// cargo nextest run --test grouped_views -E 'test(content_view::)'

use indoc::indoc;
use std::error::Error;

use skg::to_org::render::content_view::{multi_root_view, single_root_view};
use skg::test_utils::run_with_shared_test_db;
use skg::dbs::typedb::paths::path_containerward_to_first_nonlinearity;
use skg::types::misc::{ID, SkgConfig};

use std::sync::Arc;
use typedb_driver::TypeDBDriver;

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_db (
    "skg-test-content-view",
    |s| Box::pin ( async move {
      s . reset ("run_path_and_root_tests (a mess of stuff)",
                 "tests/content_view/fixtures") . await ?;
      run_path_and_root_tests (
        &s . config, &s . driver ) . await ?;
      s . reset ("test_multi_root_view_logic",
                 "tests/content_view/fixtures-2") . await ?;
      test_multi_root_view_logic (
        &s . config, &s . driver ) . await ?;
      s . reset ("test_single_root_view_with_cycle",
                 "tests/typedb/fixtures") . await ?;
      test_single_root_view_with_cycle (
        &s . config, &s . driver ) . await ?;
      s . reset ("test_multi_root_view_with_shared_nodes",
                 "tests/typedb/fixtures") . await ?;
      test_multi_root_view_with_shared_nodes (
        &s . config, &s . driver ) . await ?;
      s . reset ("test_multi_root_view_with_node_limit",
                 "tests/typedb/fixtures") . await ?;
      test_multi_root_view_with_node_limit (
        &s . config, &s . driver ) . await ?;
      s . reset ("test_limit_with_multiple_sibling_groups",
                 "tests/content_view/fixtures-3") . await ?;
      test_limit_with_multiple_sibling_groups (
        &s . config, &s . driver ) . await ?;
      Ok (( )) } )) }

async fn run_path_and_root_tests (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver
) -> Result<(), Box<dyn std::error::Error>> {

  // Test the path from node "4" to the root container
  match path_containerward_to_first_nonlinearity (
    & config . db_name,
    & driver,
    & ID("4" . to_string() )
  ) . await {
    Ok(r) => { assert_eq!(
      r.path,
      vec![
        ID ( "2" . to_string() ),
        ID ( "1" . to_string() ) ],
      "Unexpected path to root container from node '4'.");
    }, Err (e) => {
      panic!("Error finding path to root container: {}", e); } }

  match path_containerward_to_first_nonlinearity (
    & config . db_name,
    & driver,
    & ID("4" . to_string() )
  ) . await {
    Ok(r) => { assert_eq!(
      r.path . last () . cloned (),
      Some ( ID ( "1" . to_string() ) ),
      "Root of node '4' should be 1." ) },
    Err (e) => { panic!(
      "Error finding root container from id {}", e); } }

  // Test the path "to root" from node "cycle-3".
  // (1 contains 2 contains 3 contains 1.)
  match path_containerward_to_first_nonlinearity (
    & config . db_name,
    & driver,
    & ID("cycle-3" . to_string() )
  ) . await {
    Ok(r) => { assert_eq!(
      r.path,
      vec![
        ID ( "cycle-2" . to_string() ),
        ID ( "cycle-1" . to_string() ) ],
      "Unexpected path to \"root container\" from node 'cycle-3'.");
    }, Err (e) => {
      panic!("Error finding path to root container: {}", e); } }

  // Test the path "to root" from node "cycle-1".
  // (1 contains 2 contains 3 contains 1.)
  match path_containerward_to_first_nonlinearity (
    & config . db_name,
    & driver,
    & ID("cycle-1" . to_string() )
  ) . await {
    Ok(r) => { assert_eq!(
      r.path,
      vec![
        ID ( "cycle-3" . to_string() ),
        ID ( "cycle-2" . to_string() ),
      ],
      "Unexpected path to \"root container\" from node 'cycle-1'.");
    }, Err (e) => {
      panic!("Error finding path to root container: {}", e); } }

  Ok (( )) }

async fn test_multi_root_view_logic (
  config : &SkgConfig,
  driver : &std::sync::Arc<typedb_driver::TypeDBDriver>
) -> Result<(), Box<dyn std::error::Error>> {

  let focii : Vec<ID> = vec![
    ID("1" . to_string()),
    ID("2" . to_string()),
    ID("1" . to_string())
  ];
  let (result, _pids, _)
    : (String, Vec<ID>, _)
    = multi_root_view ( driver, & config, None, & focii, false
                      ) . await ?;

  println!("Multi-root view result:\n{}", result);

  let expected = indoc! {"* (skg (node (id 1) (source main) (parentIs absent))) 1
                          1 has a body
                          * (skg (node (id 2) (source main) (parentIs absent))) 2
                          * (skg (node (id 1) (source main) (parentIs absent) indef)) 1
                          "};
  assert_eq!(result, expected,
             "Multi-root view should produce exact expected output");

  Ok (( )) }

async fn test_single_root_view_with_cycle (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  {
      // Test with node "a" which has a cycle (a -> b -> c -> b)
      let (result, _pids, _)
        : (String, Vec<ID>, _)
        = single_root_view ( driver, config, None,
                             &ID ( "a" . to_string () ),
                             false
                           ) . await ?;

      println!("Single root view with cycle result:\n{}", result);

      let expected = indoc! {"* (skg (node (id a) (source main) (parentIs absent) (graphStats (contents 1)))) a
                              ** (skg (node (id b) (source main) (graphStats (containers 2) (contents 1)))) b
                              b has a body
                              *** (skg (node (id c) (source main) (graphStats (contents 1)) (viewStats containsParent))) c
                              **** (skg (node (id b) (source main) indef (graphStats (containers 2) (contents 1)) (viewStats cycle containsParent))) b
                              "};
      assert_eq!(result, expected,
                 "Single root view should detect cycle and mark repeated node");

      Ok (( )) }}

async fn test_multi_root_view_with_shared_nodes (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  {
      // Test with multiple roots that share a node
      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () )
      ];
      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, config, None, & focii, false
                        ) . await ?;

      println!("Multi root view with shared nodes result:\n{}", result);

      // BFS processes all roots (generation 1) before children (generation 2),
      // so node 2 appears first as a root, then as a child (marked indef).
      // Definitive nodes with subscriptions get SubscribeeCol children,
      // and each SubscribeeCol has Subscribee children with the subscribed IDs.
      // Because node 2 has node 1 as a container, the multi_root_view
      // pipeline prepends node 1's ancestry (here just node 1 itself,
      // indef Content) as the first child of the level-1 view of
      // node 2.
      let expected = indoc! {
        "* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 2) hiding))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg (node (id 2) (source main) indef (graphStats (linksInFromLeaves 1) extraIDs subscribing))) title 2
         ** (skg (node (id 5) (source main) (graphStats (containers 0) (linksInFromLeaves 1) extraIDs overriding subscribing hiding) (viewStats (overridesHere 3)))) this title includes a [[id:22][textlink to another file]]
         this body includes more textlinks:  [[id:33][to the third]] and [[id:55][even to itself]]
         * (skg (node (id 2) (source main) (parentIs absent) (graphStats (containers 1) (linksInFromLeaves 1) extraIDs subscribing))) title 2
         this one string could span pages
         ** (skg (node (id 1) (source main) (parentIs independent) (birth containsParent) indef (graphStats (containers 0) (contents 2) hiding) (viewStats containsParent))) title 1
         ** (skg subscribeeCol)
         *** (skg (node (id 4) (source main) indef (graphStats (containers 0) extraIDs overriding subscribing hiding))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg (node (id 5) (source main) indef (graphStats (containers 0) (linksInFromLeaves 1) extraIDs overriding subscribing hiding))) this title includes a [[id:22][textlink to another file]]
         "};
      assert_eq!(result, expected,
                 "Multi root view should detect cross-tree duplicates");

      Ok (( )) }}

async fn test_multi_root_view_with_node_limit (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  {
      // Test with two roots that share a node, with node limit
      // Tree structure: 1 -> (2, 3), 2 (standalone root)
      // Generations: 1: [1, 2], 2: [2 (repeated), 3]
      // With limit=3, should render 1, 2, then 2 and 3 are both truncated (sibling group)
      let mut test_config = config . clone();
      test_config . initial_node_limit = 3;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () )
      ];
      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, &test_config, None, & focii, false
                        ) . await ?;

      println!("Multi root view with limit=3 result:\n{}", result);

      // §5.5 node budget, limit=3: level-order creation spends the budget on the
      // content children (2, and node 5 drawn in place of 3, under root 1; and
      // root 2's independent containerward node 1). Those nodes are created
      // DEFINITIVELY (no indef placeholders), so the drawn 5 and root 2 expand
      // their bodies + cols (cols are not budget-bound); subscribee members
      // are indef.
      let expected = indoc! {
        "* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 2) hiding))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg (node (id 2) (source main) indef (graphStats (linksInFromLeaves 1) extraIDs subscribing))) title 2
         ** (skg (node (id 5) (source main) (graphStats (containers 0) (linksInFromLeaves 1) extraIDs overriding subscribing hiding) (viewStats (overridesHere 3)))) this title includes a [[id:22][textlink to another file]]
         this body includes more textlinks:  [[id:33][to the third]] and [[id:55][even to itself]]
         * (skg (node (id 2) (source main) (parentIs absent) (graphStats (containers 1) (linksInFromLeaves 1) extraIDs subscribing))) title 2
         this one string could span pages
         ** (skg (node (id 1) (source main) (parentIs independent) (birth containsParent) indef (graphStats (containers 0) (contents 2) hiding) (viewStats containsParent))) title 1
         ** (skg subscribeeCol)
         *** (skg (node (id 4) (source main) indef (graphStats (containers 0) extraIDs overriding subscribing hiding))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg (node (id 5) (source main) indef (graphStats (containers 0) (linksInFromLeaves 1) extraIDs overriding subscribing hiding))) this title includes a [[id:22][textlink to another file]]
         "};
      assert_eq!(result, expected,
                 "Multi root view limit=3 truncates by the §5.5 budget");

      Ok (( )) }}

async fn test_limit_with_multiple_sibling_groups (
  config : &SkgConfig,
  driver : &Arc<TypeDBDriver>,
) -> Result<(), Box<dyn Error>> {
  {
      // Test that truncation correctly stops at sibling group boundaries
      // Tree structure:
      //   1 (gen 1)
      //   ├─ 11 (gen 2)
      //   │  ├─ 111 (gen 3)
      //   │  └─ 112 (gen 3)
      //   └─ 12 (gen 2)
      //      └─ 121 (gen 3)
      //
      // §5.5 node budget, limit=4 (cost 1 per vognode EXPANSION): expansions are
      // 1, 11, 12, 111. Each parent draws its WHOLE child group (never a partial
      // sibling set), and once the budget is spent the remaining children stay
      // indefinitive: 11's group 111,112 is whole (111 expanded, 112 indef), and
      // 12's group 121 is whole (indef). graphStats(contents N) flags the
      // collapsed nodes, so nothing is silently missing.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 4;

      let (result, _pids, _) : (String, Vec<ID>, _)
      = single_root_view ( driver, &test_config, None,
                           &ID ( "1" . to_string () ),
                           false
                         ) . await ?;

      println!("Result with multiple sibling groups:\n{}", result);

      let expected = indoc! {"* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 2)))) 1
                              1 body
                              ** (skg (node (id 11) (source main) (graphStats (contents 2)))) 11
                              11 body
                              *** (skg (node (id 111) (source main))) 111
                              111 body
                              *** (skg (node (id 112) (source main) indef)) 112
                              ** (skg (node (id 12) (source main) (graphStats (contents 1)))) 12
                              12 body
                              *** (skg (node (id 121) (source main) indef)) 121
                              "};
      assert_eq!(result, expected,
                 "limit=4: whole groups drawn (111,112 and 121); expansion stops at the budget");

      Ok (( )) }}
