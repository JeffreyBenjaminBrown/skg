// cargo test --test initial_view_bfs -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::content_view::multi_root_view;
use skg::test_utils::run_with_test_db;
use skg::types::ID;

#[test]
fn test_bfs_limit_across_multiple_trees
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-multi-tree",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-multi-tree",
    |config, driver| Box::pin ( async move {
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
      // With limit=7: can render [1,2,3,11,12,13,21] = 7 nodes
      // Generation 1: [1, 2, 3] = 3 nodes
      // Generation 2: [11,12,13,21,22,23,31,32,33] = 9 nodes
      // Total would be 12 > 7, so truncation triggers
      //
      // Truncation algorithm:
      // - overflow_index = 7 - 3 = 4
      // - Limit node is at index 4 in gen 2 (node 22)
      // - Limit node parent is node 2
      // - Render gen 2 as truncated, completing sibling group of limit node:
      //   - 11, 12, 13 (parent=1): all truncated
      //   - 21, 22, 23 (parent=2): all truncated
      //   - Stop before 31 (parent=3 ≠ 2)
      // - Then truncate everything after parent (2) in gen 1:
      //   - Truncate node 3

      let mut test_config = config.clone();
      test_config.initial_node_limit = 7;

      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () ),
        ID ( "3".to_string () )
      ];

      let result : String = multi_root_view (
        driver,
        &test_config,
        & focii
      ) . await ?;

      println!("BFS multi-tree limit result:\n{}", result);

      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 3)))) 1
                              ** (skg (id 11) (source main) (code indefinitive)) 11
                              ** (skg (id 12) (source main) (view (rels (contents 3))) (code indefinitive)) 12
                              ** (skg (id 13) (source main) (code indefinitive)) 13
                              * (skg (id 2) (source main) (view (rels (containers 0) (contents 3)))) 2
                              ** (skg (id 21) (source main) (code indefinitive)) 21
                              ** (skg (id 22) (source main) (view (rels (contents 3))) (code indefinitive)) 22
                              ** (skg (id 23) (source main) (code indefinitive)) 23
                              * (skg (id 3) (source main) (view (rels (containers 0) (contents 3))) (code indefinitive)) 3
                              "};
      assert_eq!(result, expected,
                 "BFS should truncate across multiple trees correctly");

      Ok (( )) } )) }

#[test]
fn test_bfs_limit_9_three_branches
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-9-three-branches",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-9-three-branches",
    |config, driver| Box::pin ( async move {
      // With limit=9: [1,2,3,11,12,13,21,22,23] = exactly 9 nodes
      // Generation 1: [1, 2, 3] = 3 nodes
      // Generation 2: [11,12,13,21,22,23,31,32,33] = 9 nodes
      // Total would be 12 > 9, so truncation triggers
      //
      // Truncation algorithm:
      // - overflow_index = 9 - 3 = 6
      // - Limit node is at index 6 in gen 2 (node 31, parent=3)
      // - But we haven't started parent 3's children yet (indices 0-5 are parents 1 and 2)
      // - Don't complete parent 3's sibling group, stop at index 5
      // - Render gen 2 as truncated:
      //   - 11, 12, 13 (parent=1): all truncated
      //   - 21, 22, 23 (parent=2): all truncated
      //   - Stop before 31 (parent=3 ≠ 2)
      // - Then truncate everything after parent (2) in gen 1:
      //   - Truncate node 3

      let mut test_config = config.clone();
      test_config.initial_node_limit = 9;

      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () ),
        ID ( "3".to_string () )
      ];

      let result : String = multi_root_view (
        driver,
        &test_config,
        & focii
      ) . await ?;

      println!("BFS limit=9 three branches result:\n{}", result);

      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 3)))) 1
                              ** (skg (id 11) (source main) (code indefinitive)) 11
                              ** (skg (id 12) (source main) (view (rels (contents 3))) (code indefinitive)) 12
                              ** (skg (id 13) (source main) (code indefinitive)) 13
                              * (skg (id 2) (source main) (view (rels (containers 0) (contents 3)))) 2
                              ** (skg (id 21) (source main) (code indefinitive)) 21
                              ** (skg (id 22) (source main) (view (rels (contents 3))) (code indefinitive)) 22
                              ** (skg (id 23) (source main) (code indefinitive)) 23
                              * (skg (id 3) (source main) (view (rels (containers 0) (contents 3))) (code indefinitive)) 3
                              "};
      assert_eq!(result, expected,
                 "BFS with limit=9 should produce same result as limit=7");

      Ok (( )) } )) }

#[test]
fn test_bfs_limit_8_two_branches
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-bfs-limit-8-two-branches",
    "tests/initial_view_bfs/fixtures",
    "/tmp/tantivy-test-bfs-limit-8-two-branches",
    |config, driver| Box::pin ( async move {
      // With limit=8 and only branches 1 and 2 (no branch 3):
      // [1,2,11,12,13,21,22,23] = exactly 8 nodes
      // Generation 1: [1, 2] = 2 nodes
      // Generation 2: [11,12,13,21,22,23] = 6 nodes
      // Total would be 8 = 8, but overflow_index = 6 is out of bounds
      // So we hit the limit exactly at node 23
      //
      // All of gen 2 should be indefinitive
      // Gen 1 nodes (1, 2) should be definitive (they have all their children)
      // Gen 3 should NOT be rendered (verify we don't go beyond the limit)

      let mut test_config = config.clone();
      test_config.initial_node_limit = 8;

      let focii = vec![
        ID ( "1".to_string () ),
        ID ( "2".to_string () )
        // Note: node 3 and its descendants not included
      ];

      let result : String = multi_root_view (
        driver,
        &test_config,
        & focii
      ) . await ?;

      println!("BFS limit=8 two branches result:\n{}", result);

      let expected = indoc! {"* (skg (id 1) (source main) (view (rels (containers 0) (contents 3)))) 1
                              ** (skg (id 11) (source main) (code indefinitive)) 11
                              ** (skg (id 12) (source main) (view (rels (contents 3))) (code indefinitive)) 12
                              ** (skg (id 13) (source main) (code indefinitive)) 13
                              * (skg (id 2) (source main) (view (rels (containers 0) (contents 3)))) 2
                              ** (skg (id 21) (source main) (code indefinitive)) 21
                              ** (skg (id 22) (source main) (view (rels (contents 3))) (code indefinitive)) 22
                              ** (skg (id 23) (source main) (code indefinitive)) 23
                              "};
      assert_eq!(result, expected,
                 "BFS with limit=8 and 2 branches should stop at gen 2, not render gen 3");

      Ok (( )) } )) }
