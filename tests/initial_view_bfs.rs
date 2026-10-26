// cargo nextest run --test grouped_views -E 'test(initial_view_bfs::)'

use indoc::indoc;
use std::error::Error;

use skg::to_org::render::content_view::multi_root_view;
use skg::assert_metadata_eq;
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig, TantivyIndex};



#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/initial_view_bfs/fixtures";
  run_with_shared_test_stores (
    "skg-test-initial-view-bfs",
    |s| Box::pin ( async move {
      s . reset ("test_bfs_limit_across_multiple_trees", fixtures) ?;
      test_bfs_limit_across_multiple_trees (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_bfs_limit_9_three_branches", fixtures) ?;
      test_bfs_limit_9_three_branches (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_bfs_limit_8_two_branches", fixtures) ?;
      test_bfs_limit_8_two_branches (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_budget_content_beats_subscribers",
                 "tests/initial_view_bfs/fixtures-content-vs-subscribers") ?;
      test_budget_content_beats_subscribers (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_bfs_limit_across_multiple_trees (
  config : &SkgConfig,
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
      // §5.5 node budget: the budget counts vognode extensions (cost 1 each), in
      // level order; a whole child group is always shown (never truncated
      // mid-group), and once the budget hits 0 every later vognode is left
      // write-protected (visible, collapsed -- graphStats(contents N) still shows it
      // has hidden content). View roots are never truncated. limit=7 extensions:
      // 1,2,3,11,12,13,21. So 12 showed its whole gen-3 group 121..123 (all then
      // write-protected); 22,23,31,32,33 are reached after the budget is spent and
      // stay write-protected.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 7;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () ),
        ID ( "3" . to_string () )
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( &test_config, None, & focii
                        ) ?;

      println!("BFS multi-tree limit result:\n{}", result);

      let expected = indoc! {"* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 1
                              ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 11
                              ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 12
                              *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 121
                              *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 122
                              *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 123
                              ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 13
                              * (skg node (gnid 2) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 2
                              ** (skg node (gnid 21) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 21
                              ** (skg node (gnid 22) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 22
                              ** (skg node (gnid 23) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 23
                              * (skg node (gnid 3) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 3
                              ** (skg node (gnid 31) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 31
                              ** (skg node (gnid 32) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 32
                              ** (skg node (gnid 33) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 33
                              "};
      assert_metadata_eq!(result, expected,
                 "BFS truncates by the §5.5 budget, leaving whole groups writeProtected");

      Ok (( )) }

async fn test_bfs_limit_9_three_branches (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // §5.5 node budget, limit=9 (cost 1 per extension): extensions are
      // 1,2,3,11,12,13,21,22,23. So roots 1 and 2 fully extend (12 and 22 each
      // show their whole gen-3 group, write-protected); root 3 is reached after the
      // budget is spent, so 31,32,33 stay write-protected. (Contrast limit=7, which
      // runs out one root sooner.)

      let mut test_config = config . clone();
      test_config . initial_node_limit = 9;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () ),
        ID ( "3" . to_string () )
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( &test_config, None, & focii
                        ) ?;

      println!("BFS limit=9 three branches result:\n{}", result);

      let expected = indoc! {"* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 1
                              ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 11
                              ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 12
                              *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 121
                              *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 122
                              *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 123
                              ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 13
                              * (skg node (gnid 2) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 2
                              ** (skg node (gnid 21) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 21
                              ** (skg node (gnid 22) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 22
                              *** (skg node (gnid 221) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 221
                              *** (skg node (gnid 222) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 222
                              *** (skg node (gnid 223) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 223
                              ** (skg node (gnid 23) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 23
                              * (skg node (gnid 3) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 3
                              ** (skg node (gnid 31) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 31
                              ** (skg node (gnid 32) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 32
                              ** (skg node (gnid 33) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 33
                              "};
      assert_metadata_eq!(result, expected,
                 "BFS limit=9 fully extends the first two roots; the third stays write-protected");

      Ok (( )) }

async fn test_bfs_limit_8_two_branches (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      // §5.5 node budget, limit=8, roots [1,2] (cost 1 per extension): extensions
      // are 1,2,11,12,13,21,22,23 -- both roots fully extend their gen-2. 12 and
      // 22 each show their whole gen-3 group (121..123, 221..223), all left
      // write-protected (budget spent by then). Whole groups, never a partial set.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 8;

      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () )
        // Note: node 3 and its descendants not included
      ];

      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( &test_config, None, & focii
                        ) ?;

      println!("BFS limit=8 two branches result:\n{}", result);

      let expected = indoc! {"* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 1
                              ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 11
                              ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 12
                              *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 121
                              *** (skg node (gnid 122) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 122
                              *** (skg node (gnid 123) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 123
                              ** (skg node (gnid 13) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 13
                              * (skg node (gnid 2) (skgRepos (nodeRepo main)) (view (rels (contains (out 3))))) 2
                              ** (skg node (gnid 21) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 21
                              ** (skg node (gnid 22) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 3))) (birth (contains in 1)))) 22
                              *** (skg node (gnid 221) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 221
                              *** (skg node (gnid 222) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 222
                              *** (skg node (gnid 223) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 223
                              ** (skg node (gnid 23) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 23
                              "};
      assert_metadata_eq!(result, expected,
                 "BFS limit=8 fully extends both roots' gen-2; gen-3 groups whole + writeProtected");

      Ok (( )) }

// §5.5: content wins the budget race; a folder (here a SubscribeeFolder) fills whole
// and is budget-neutral. Fixture: root r -> content chain c1 -> c2, and r also
// subscribes to s1, s2. With budget = 3 (exactly the content chain r, c1, c2):
// the chain fully extends (c2 is editable, body and all), and both subscribers
// are shown (the folder is whole), write-protected. If the folder had spent the budget,
// s1 and s2 would have eaten two of the three units and c2 would be left
// write-protected -- it is not, which is the guarantee this test pins. Subscribers
// also sit one level deeper than content (r -> SubscribeeFolder -> subscriber vs
// r -> c1), so BFS-by-depth reaches content first regardless.
async fn test_budget_content_beats_subscribers (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      let mut test_config = config . clone();
      test_config . initial_node_limit = 3;

      let focii = vec![ ID ( "r" . to_string () ) ];
      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( &test_config, None, & focii
                        ) ?;

      println!("content-vs-subscribers (budget 3):\n{}", result);

      let expected = indoc! {"* (skg node (gnid r) (skgRepos (nodeRepo main)) (view (rels (contains (out 1)) (subscribesTo (out 2))))) r
                              ** (skg subscribeeFolder)
                              *** (skg node (gnid s1) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)))) s1
                              *** (skg node (gnid s2) (skgRepos (nodeRepo main)) writeProtected (view (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 1 (ancestors 2)))) (birth (subscribesTo in 2)))) s2
                              ** (skg node (gnid c1) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (birth (contains in 1)))) c1
                              *** (skg node (gnid c2) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) c2
                              "};
      assert_metadata_eq!(result, expected,
                 "budget 3 extends the whole content chain; the SubscribeeFolder is whole + budget-neutral");

      Ok (( )) }
