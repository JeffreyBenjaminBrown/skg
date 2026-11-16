// cargo nextest run --test grouped_forks -E 'test(children_of_a_raw_root_are_raw::)'
//
// Substitution does not apply to the immediate children of a
// forked node shown raw ('docs/mirrors.org', "Mirror
// substitution").
//
// Fixture (the docs/forks.org shape): clone R forks original X, and
// R's child R1 forks X's child X1. Opening X as a view root shows
// X raw (roots are shown raw); its immediate child X1 must therefore be shown
// raw too -- not substituted by R1 -- because the user is looking at
// the original, not at its mirror R.

use std::error::Error;

use skg::test_utils::run_with_test_stores;
use skg::test_utils::multi_root_view;
use skg::types::misc::ID;
use ego_tree::Tree;
use skg::types::viewnode::Viewnode;
use skg::metadata_canon::without_viewnode_ids;

#[test]
fn children_of_a_raw_root_are_raw
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-children-of-a-raw-root-are-raw",
    "tests/extend_partner_folder_member/fixtures",
    "/tmp/tantivy-test-children-of-a-raw-root-are-raw",
    |config, tantivy| Box::pin ( async move {
      let (x_view, _pids, _tree)
        : (String, Vec<ID>, Tree<Viewnode>) =
        multi_root_view (
          config, Some (tantivy), &[ID::from ("X")] )
 ?;
      // X is shown raw (a view root).
      assert! ( x_view . lines () . any (
          |l| without_viewnode_ids (l) . starts_with ("* (skg node (gnid X)") ),
        "X should be the raw root:\n{}", x_view );
      // X's immediate content child X1 is shown raw, as itself, under X.
      assert! ( x_view . lines () . any (
          |l| without_viewnode_ids (l) . starts_with ("** (skg node (gnid X1)") ),
        "X1 should appear raw as X's immediate child:\n{}", x_view );
      // Nothing is substituted in this view: no mirror stands in for
      // X1 (the bug this change removes).
      assert! ( ! x_view . contains ("inPlaceOf"),
        "no immediate child of a raw-shown forked node may be \
         substituted:\n{}", x_view );
      assert! ( ! x_view . lines () . any (
          |l| without_viewnode_ids (l) . starts_with ("** (skg node (gnid R1)") ),
        "the mirror R1 must not stand in for X1:\n{}", x_view );
      Ok (( )) } )) }
