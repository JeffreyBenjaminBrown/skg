// cargo test --test initial_view_bfs -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::render::content_view::multi_root_view;
use skg::test_utils::run_with_test_db;
use skg::types::misc::ID;


#[test]
fn test_bfs_limit_across_multiple_trees
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-multi-tree",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-multi-tree",
    |config, driver, _tantivy| Box::pin ( async move {
      // Tree structure across 3 roots:
      // 1
      //   11
      //   12
      //     121
      //     122
      //     123
      //   13
      // 2
      //   21 <- limit hit around here
      //   22
      //     221
      //     222
      //     223
      //   23
      // 3
      //   31
      //   32
      //     321
      //     322
      //     323
      //   33
      //
      // §5.5 node budget (phase 8): the driver creates content children in
      // level order, decrementing the per-buffer budget per NEW node, and stops
      // creating once it hits 0 (no indef placeholders -- the old generation
      // truncation's indef nodes are gone). limit=7 => the 7 content children
      // 11,12,13,21,22,23,31 are created (definitive); 32,33 and all of gen 3
      // are simply not drawn. graphStats(contents N) still flags truncated nodes.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 7;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () ),
        ID ( "3" . to_string () )
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, &test_config, None, & focii, false
                        ) . await ?;

      println!("BFS multi-tree limit result:\n{}", result);

      let expected = indoc! {"* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
                              ** (skg (node (id 11) (source main))) 11
                              ** (skg (node (id 12) (source main) (graphStats (contents 3)))) 12
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) (graphStats (contents 3)))) 22
                              ** (skg (node (id 23) (source main))) 23
                              * (skg (node (id 3) (source main) (parentIs absent) (graphStats (contents 3)))) 3
                              ** (skg (node (id 31) (source main))) 31
                              "};
      assert_eq!(result, expected,
                 "BFS should truncate by the §5.5 node budget");

      Ok (( )) } )) }

#[test]
fn test_bfs_limit_9_three_branches
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-9-three-branches",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-9-three-branches",
    |config, driver, _tantivy| Box::pin ( async move {
      // §5.5 node budget, limit=9: the 9 content children 11,12,13,21,22,23,
      // 31,32,33 (all of generation 2) are created definitively; generation 3 is
      // not drawn. (Contrast limit=7, which stops after 31.)

      let mut test_config = config . clone();
      test_config . initial_node_limit = 9;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () ),
        ID ( "3" . to_string () )
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, &test_config, None, & focii, false
                        ) . await ?;

      println!("BFS limit=9 three branches result:\n{}", result);

      let expected = indoc! {"* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
                              ** (skg (node (id 11) (source main))) 11
                              ** (skg (node (id 12) (source main) (graphStats (contents 3)))) 12
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) (graphStats (contents 3)))) 22
                              ** (skg (node (id 23) (source main))) 23
                              * (skg (node (id 3) (source main) (parentIs absent) (graphStats (contents 3)))) 3
                              ** (skg (node (id 31) (source main))) 31
                              ** (skg (node (id 32) (source main) (graphStats (contents 3)))) 32
                              ** (skg (node (id 33) (source main))) 33
                              "};
      assert_eq!(result, expected,
                 "BFS limit=9 draws all of generation 2 by the §5.5 budget");

      Ok (( )) } )) }

#[test]
fn test_bfs_limit_8_two_branches
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-8-two-branches",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-8-two-branches",
    |config, driver, _tantivy| Box::pin ( async move {
      // §5.5 node budget, limit=8, roots [1,2]: level-order creation spends the
      // budget as 11,12,13,21,22,23 (gen 2 = 6) then 121,122 (first two gen-3
      // children of node 12) = 8. 123 and the rest of gen 3 are not drawn. So
      // unlike the old generation truncation, §5.5 descends partway into gen 3.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 8;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () )
        // Note: node 3 and its descendants not included
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, &test_config, None, & focii, false
                        ) . await ?;

      println!("BFS limit=8 two branches result:\n{}", result);

      let expected = indoc! {"* (skg (node (id 1) (source main) (parentIs absent) (graphStats (contents 3)))) 1
                              ** (skg (node (id 11) (source main))) 11
                              ** (skg (node (id 12) (source main) (graphStats (contents 3)))) 12
                              *** (skg (node (id 121) (source main))) 121
                              *** (skg (node (id 122) (source main))) 122
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) (graphStats (contents 3)))) 22
                              ** (skg (node (id 23) (source main))) 23
                              "};
      assert_eq!(result, expected,
                 "BFS limit=8 spends the §5.5 budget into gen 3 (121,122)");

      Ok (( )) } )) }
