// cargo nextest run --test grouped_views -E 'test(content_view::)'

use indoc::indoc;
use std::error::Error;

use skg::test_utils::{multi_root_view, single_root_view};
use skg::assert_metadata_eq;
use skg::test_utils::run_with_shared_test_stores;
use skg::types::misc::{ID, SkgConfig};


#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  run_with_shared_test_stores (
    "skg-test-content-view",
    |s| Box::pin ( async move {
      s . reset ("test_multi_root_view_logic",
                 "tests/content_view/fixtures-2") ?;
      test_multi_root_view_logic (
        &s . config ) . await ?;
      s . reset ("test_single_root_view_with_cycle",
                 "tests/content_view/fixtures-shared-cycle") ?;
      test_single_root_view_with_cycle (
        &s . config ) . await ?;
      s . reset ("test_multi_root_view_with_shared_nodes",
                 "tests/content_view/fixtures-shared-cycle") ?;
      test_multi_root_view_with_shared_nodes (
        &s . config ) . await ?;
      s . reset ("test_multi_root_view_with_node_limit",
                 "tests/content_view/fixtures-shared-cycle") ?;
      test_multi_root_view_with_node_limit (
        &s . config ) . await ?;
      s . reset ("test_limit_with_multiple_sibling_groups",
                 "tests/content_view/fixtures-3") ?;
      test_limit_with_multiple_sibling_groups (
        &s . config ) . await ?;
      Ok (( )) } )) }

async fn test_multi_root_view_logic (
  config : &SkgConfig,

) -> Result<(), Box<dyn std::error::Error>> {

  let focii : Vec<ID> = vec![
    ID("1" . to_string()),
    ID("2" . to_string()),
    ID("1" . to_string())
  ];
  let (result, _pids, _)
    : (String, Vec<ID>, _)
    = multi_root_view ( & config, None, & focii
                      ) ?;

  println!("Multi-root view result:\n{}", result);

  let expected = indoc! {"* (skg node (gnid 1) (skgRepos (nodeRepo main))) 1
                          1 has a body
                          * (skg node (gnid 2) (skgRepos (nodeRepo main))) 2
                          "};
                          // The two roots standing for 1 are siblings,
                          // which the refresh merges into the first.
  assert_metadata_eq!(result, expected,
             "Multi-root view should produce exact expected output");

  Ok (( )) }

async fn test_single_root_view_with_cycle (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  {
      // Test with node "a" which has a cycle (a -> b -> c -> b)
      let (result, _pids, _)
        : (String, Vec<ID>, _)
        = single_root_view ( config, None,
                             &ID ( "a" . to_string () )
                           ) ?;

      println!("Single root view with cycle result:\n{}", result);

      let expected = indoc! {"* (skg node (gnid a) (skgRepos (nodeRepo main)) (view (rels (contains (out 1))))) a
                              ** (skg node (gnid b) (skgRepos (nodeRepo main)) (view (rels (contains (in 2 (ancestors 1)) (out 1))) (birth (contains in 1)))) b
                              b has a body
                              *** (skg node (gnid c) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 1 (ancestors 1)))) (birth (contains in 1)))) c
                              **** (skg node (gnid b) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (in 2 (ancestors 1)) (out 1 (ancestors 1)))) (birth (contains in 1)) cycle)) b
                              "};
      assert_metadata_eq!(result, expected,
                 "Single root view should detect cycle and mark repeated node");

      Ok (( )) }}

async fn test_multi_root_view_with_shared_nodes (
  config : &SkgConfig,
) -> Result<(), Box<dyn Error>> {
  {
      // Test with multiple roots that share a node
      let focii = vec![
        ID ( "1" . to_string () ),
        ID ( "2" . to_string () )
      ];
      let (result, _pids, _) : (String, Vec<ID>, _) =
        multi_root_view ( config, None, & focii
                        ) ?;

      println!("Multi root view with shared nodes result:\n{}", result);

      // BFS processes all roots (generation 1) before children (generation 2),
      // so node 2 appears first as a root, then as a child (marked write-protected).
      // Editable nodes with subscriptions get SubscribeeFolder children,
      // and each SubscribeeFolder has Subscribee children with the subscribed IDs.
      // Because node 2 has node 1 as a container, the root 2 gets a
      // containers folder, whose one write-protected member is node 1;
      // node 1 has no container, so no chain hangs under it.
      let expected = indoc! {
        "* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 2)) (hidesFromSubs (out 2))))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg node (gnid 2) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (in 1 (ancestors 1))) (linksTo (in 1 (substantive 1))) (subscribesTo (out 2))) (props (extraIds 1)) (birth (contains in 1)))) title 2
         ** (skg node (gnid 5) (skgRepos (nodeRepo main)) (overridesHere 3) (view (rels (linksTo (in 1 (substantive 1)) (out 3)) (subscribesTo (in 2)) (overrides (out 2)) (hidesFromSubs (in 1 (ancestors 1)))) (props (extraIds 1)) (birth (overrides out)))) this title includes a [[id:22][textlink to another file]]
         this body includes more textlinks:  [[id:33][to the third]] and [[id:55][even to itself]]
         * (skg node (gnid 2) (skgRepos (nodeRepo main)) (view (rels (contains (in 1)) (linksTo (in 1 (substantive 1))) (subscribesTo (out 2))) (props (extraIds 1)))) title 2
         this one string could span pages
         ** (skg containerFolder)
         *** (skg node (gnid 1) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 2 (ancestors 2))) (hidesFromSubs (out 2))) (birth (contains out 2)))) title 1
         ** (skg subscribeeFolder)
         *** (skg node (gnid 4) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 2 (ancestors 2))) (overrides (in 1)) (hidesFromSubs (in 1))) (props (extraIds 1)) (birth (subscribesTo in 2)))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg node (gnid 5) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 0 (unintegrated 0))) (linksTo (in 1 (substantive 1)) (out 3 (ancestors 2))) (subscribesTo (in 2 (ancestors 2))) (overrides (out 2)) (hidesFromSubs (in 1))) (props (extraIds 1)) (birth (subscribesTo in 2)))) this title includes a [[id:22][textlink to another file]]
         "};
      assert_metadata_eq!(result, expected,
                 "Multi root view should detect cross-tree duplicates");

      Ok (( )) }}

async fn test_multi_root_view_with_node_limit (
  config : &SkgConfig,
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
        multi_root_view ( &test_config, None, & focii
                        ) ?;

      println!("Multi root view with limit=3 result:\n{}", result);

      // §5.5 node budget, limit=3: level-order creation spends the budget on the
      // content children (2, and node 5 shown in place of 3, under root 1;
      // root 2's containers folder costs nothing). Those nodes are created
      // definitively (no write-protected placeholders), so the shown 5 and root 2 extend
      // their bodies + default folders (folders are not budget-bound);
      // subscribee members are write-protected.
      let expected = indoc! {
        "* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 2)) (hidesFromSubs (out 2))))) title 1
         This one string could span pages,
         and it can include newlines, no problem.
         ** (skg node (gnid 2) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (in 1 (ancestors 1))) (linksTo (in 1 (substantive 1))) (subscribesTo (out 2))) (props (extraIds 1)) (birth (contains in 1)))) title 2
         ** (skg node (gnid 5) (skgRepos (nodeRepo main)) (overridesHere 3) (view (rels (linksTo (in 1 (substantive 1)) (out 3)) (subscribesTo (in 2)) (overrides (out 2)) (hidesFromSubs (in 1 (ancestors 1)))) (props (extraIds 1)) (birth (overrides out)))) this title includes a [[id:22][textlink to another file]]
         this body includes more textlinks:  [[id:33][to the third]] and [[id:55][even to itself]]
         * (skg node (gnid 2) (skgRepos (nodeRepo main)) (view (rels (contains (in 1)) (linksTo (in 1 (substantive 1))) (subscribesTo (out 2))) (props (extraIds 1)))) title 2
         this one string could span pages
         ** (skg containerFolder)
         *** (skg node (gnid 1) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 2 (ancestors 2))) (hidesFromSubs (out 2))) (birth (contains out 2)))) title 1
         ** (skg subscribeeFolder)
         *** (skg node (gnid 4) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 0 (unintegrated 0))) (subscribesTo (in 2 (ancestors 2))) (overrides (in 1)) (hidesFromSubs (in 1))) (props (extraIds 1)) (birth (subscribesTo in 2)))) This is a [[id:shgulasdghu][test]] of a second kind.
         *** (skg node (gnid 5) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (out 0 (unintegrated 0))) (linksTo (in 1 (substantive 1)) (out 3 (ancestors 2))) (subscribesTo (in 2 (ancestors 2))) (overrides (out 2)) (hidesFromSubs (in 1))) (props (extraIds 1)) (birth (subscribesTo in 2)))) this title includes a [[id:22][textlink to another file]]
         "};
      assert_metadata_eq!(result, expected,
                 "Multi root view limit=3 truncates by the §5.5 budget");

      Ok (( )) }}

async fn test_limit_with_multiple_sibling_groups (
  config : &SkgConfig,
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
      // §5.5 node budget, limit=4 (cost 1 per vognode extension): extensions are
      // 1, 11, 12, 111. Each parent shows its whole child group (never a partial
      // sibling set), and once the budget is spent the remaining children stay
      // write-protected: 11's group 111,112 is whole (111 extended, 112 write-protected), and
      // 12's group 121 is whole (write-protected). graphStats(contents N) flags the
      // collapsed nodes, so nothing is silently missing.

      let mut test_config = config . clone();
      test_config . initial_node_limit = 4;

      let (result, _pids, _) : (String, Vec<ID>, _)
      = single_root_view ( &test_config, None,
                           &ID ( "1" . to_string () )
                         ) ?;

      println!("Result with multiple sibling groups:\n{}", result);

      let expected = indoc! {"* (skg node (gnid 1) (skgRepos (nodeRepo main)) (view (rels (contains (out 2))))) 1
                              1 body
                              ** (skg node (gnid 11) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 2))) (birth (contains in 1)))) 11
                              11 body
                              *** (skg node (gnid 111) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 111
                              111 body
                              *** (skg node (gnid 112) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 112
                              ** (skg node (gnid 12) (skgRepos (nodeRepo main)) (view (rels (contains (in 1 (ancestors 1)) (out 1))) (birth (contains in 1)))) 12
                              12 body
                              *** (skg node (gnid 121) (skgRepos (nodeRepo main)) writeProtected (view omittedBody (rels (contains (in 1 (ancestors 1)))) (birth (contains in 1)))) 121
                              "};
      assert_metadata_eq!(result, expected,
                 "limit=4: whole groups shown (111,112 and 121); extension stops at the budget");

      Ok (( )) }}
