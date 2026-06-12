// cargo test --test initial_view_bfs -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::render::content_view::multi_root_view;
use skg::test_utils::run_with_shared_test_db;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};

use std::sync::Arc;
use typedb_driver::TypeDBDriver;


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/initial_view_bfs/fixtures";
  run_with_shared_test_db (
    "skg-test-initial-view-bfs",
    |s| Box::pin ( async move {
      s . reset ("test_bfs_limit_across_multiple_trees", fixtures) . await ?;
      test_bfs_limit_across_multiple_trees (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_bfs_limit_9_three_branches", fixtures) . await ?;
      test_bfs_limit_9_three_branches (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_bfs_limit_8_two_branches", fixtures) . await ?;
      test_bfs_limit_8_two_branches (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      s . reset ("test_budget_content_beats_subscribers",
                 "tests/initial_view_bfs/fixtures-content-vs-subscribers") . await ?;
      test_budget_content_beats_subscribers (
        &s . config, &s . driver, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_bfs_limit_across_multiple_trees (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
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
      // §5.5 node budget: the budget counts vognode EXPANSIONS (cost 1 each), in
      // level order; a whole child group is always drawn (never truncated
      // mid-group), and once the budget hits 0 every later vognode is left
      // indefinitive (visible, collapsed -- graphStats(contents N) still shows it
      // has hidden content). View roots are never truncated. limit=7 expansions:
      // 1,2,3,11,12,13,21. So 12 drew its whole gen-3 group 121..123 (all then
      // indefinitive); 22,23,31,32,33 are reached after the budget is spent and
      // stay indefinitive.

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
                              *** (skg (node (id 121) (source main) indef)) 121
                              *** (skg (node (id 122) (source main) indef)) 122
                              *** (skg (node (id 123) (source main) indef)) 123
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) indef (graphStats (contents 3)))) 22
                              ** (skg (node (id 23) (source main) indef)) 23
                              * (skg (node (id 3) (source main) (parentIs absent) (graphStats (contents 3)))) 3
                              ** (skg (node (id 31) (source main) indef)) 31
                              ** (skg (node (id 32) (source main) indef (graphStats (contents 3)))) 32
                              ** (skg (node (id 33) (source main) indef)) 33
                              "};
      assert_eq!(result, expected,
                 "BFS truncates by the §5.5 budget, leaving whole groups indefinitive");

      Ok (( )) }

async fn test_bfs_limit_9_three_branches (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // §5.5 node budget, limit=9 (cost 1 per expansion): expansions are
      // 1,2,3,11,12,13,21,22,23. So roots 1 and 2 fully expand (12 and 22 each
      // draw their whole gen-3 group, indefinitive); root 3 is reached after the
      // budget is spent, so 31,32,33 stay indefinitive. (Contrast limit=7, which
      // runs out one root sooner.)

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
                              *** (skg (node (id 121) (source main) indef)) 121
                              *** (skg (node (id 122) (source main) indef)) 122
                              *** (skg (node (id 123) (source main) indef)) 123
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) (graphStats (contents 3)))) 22
                              *** (skg (node (id 221) (source main) indef)) 221
                              *** (skg (node (id 222) (source main) indef)) 222
                              *** (skg (node (id 223) (source main) indef)) 223
                              ** (skg (node (id 23) (source main))) 23
                              * (skg (node (id 3) (source main) (parentIs absent) (graphStats (contents 3)))) 3
                              ** (skg (node (id 31) (source main) indef)) 31
                              ** (skg (node (id 32) (source main) indef (graphStats (contents 3)))) 32
                              ** (skg (node (id 33) (source main) indef)) 33
                              "};
      assert_eq!(result, expected,
                 "BFS limit=9 fully expands the first two roots; the third stays indefinitive");

      Ok (( )) }

async fn test_bfs_limit_8_two_branches (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // §5.5 node budget, limit=8, roots [1,2] (cost 1 per expansion): expansions
      // are 1,2,11,12,13,21,22,23 -- both roots fully expand their gen-2. 12 and
      // 22 each draw their whole gen-3 group (121..123, 221..223), all left
      // indefinitive (budget spent by then). Whole groups, never a partial set.

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
                              *** (skg (node (id 121) (source main) indef)) 121
                              *** (skg (node (id 122) (source main) indef)) 122
                              *** (skg (node (id 123) (source main) indef)) 123
                              ** (skg (node (id 13) (source main))) 13
                              * (skg (node (id 2) (source main) (parentIs absent) (graphStats (contents 3)))) 2
                              ** (skg (node (id 21) (source main))) 21
                              ** (skg (node (id 22) (source main) (graphStats (contents 3)))) 22
                              *** (skg (node (id 221) (source main) indef)) 221
                              *** (skg (node (id 222) (source main) indef)) 222
                              *** (skg (node (id 223) (source main) indef)) 223
                              ** (skg (node (id 23) (source main))) 23
                              "};
      assert_eq!(result, expected,
                 "BFS limit=8 fully expands both roots' gen-2; gen-3 groups whole + indefinitive");

      Ok (( )) }

// §5.5: content wins the budget race; a col (here a SubscribeeCol) fills WHOLE
// and is budget-NEUTRAL. Fixture: root r -> content chain c1 -> c2, and r also
// subscribes to s1, s2. With budget = 3 (exactly the content chain r, c1, c2):
// the chain fully expands (c2 is definitive, body and all), AND both subscribers
// are shown (the col is whole), indefinitive. If the col had spent the budget,
// s1 and s2 would have eaten two of the three units and c2 would be left
// indefinitive -- it is not, which is the guarantee this test pins. Subscribers
// also sit one level deeper than content (r -> SubscribeeCol -> subscriber vs
// r -> c1), so BFS-by-depth reaches content first regardless.
async fn test_budget_content_beats_subscribers (
  config   : &SkgConfig,
  driver   : &Arc<TypeDBDriver>,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut test_config = config . clone();
      test_config . initial_node_limit = 3;

      let focii = vec![ ID ( "r" . to_string () ) ];
      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( driver, &test_config, None, & focii, false
                        ) . await ?;

      println!("content-vs-subscribers (budget 3):\n{}", result);

      let expected = indoc! {"* (skg (node (id r) (source main) (parentIs absent) (graphStats (contents 1) subscribing))) r
                              ** (skg subscribeeCol)
                              *** (skg (node (id s1) (source main) indef (graphStats (containers 0) subscribing))) s1
                              *** (skg (node (id s2) (source main) indef (graphStats (containers 0) subscribing))) s2
                              ** (skg (node (id c1) (source main) (graphStats (contents 1)))) c1
                              *** (skg (node (id c2) (source main))) c2
                              "};
      assert_eq!(result, expected,
                 "budget 3 expands the whole content chain; the SubscribeeCol is whole + budget-neutral");

      Ok (( )) }
