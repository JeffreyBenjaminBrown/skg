// cargo nextest run --test grouped_overrides -E 'test(extend_raw_overridden_children::)'
//
// Substitution does not apply to the immediate children of an
// overridden node shown raw ('docs/override.org', "Automatic override
// substitution").
//
// Fixture (the docs/forks.org shape): clone R overrides original X, and
// R's child R1 overrides X's child X1. Opening X as a view root shows
// X raw (roots are shown raw); its immediate child X1 must therefore be shown
// raw too -- not substituted by R1 -- because the user is looking at
// the original. (Before this change, a raw title could sit over the
// overrider's substituted children.)

use std::error::Error;

use skg::test_utils::run_with_test_stores;
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::ID;
use ego_tree::Tree;
use skg::types::viewnode::Viewnode;
use skg::metadata_canon::without_viewnode_ids;

#[test]
fn raw_shown_overridden_root_children_are_raw
  () -> Result<(), Box<dyn Error>> {
  run_with_test_stores (
    "skg-test-extend-raw-overridden-children",
    "tests/extend_partner_folder_member/fixtures",
    "/tmp/tantivy-test-extend-raw-overridden-children",
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
      // Nothing is substituted in this view: no overrider stands in for
      // X1 (the bug this change removes).
      assert! ( ! x_view . contains ("overridesHere"),
        "no immediate child of a raw-shown overridden node may be \
         substituted:\n{}", x_view );
      assert! ( ! x_view . lines () . any (
          |l| without_viewnode_ids (l) . starts_with ("** (skg node (gnid R1)") ),
        "the overrider R1 must not stand in for X1:\n{}", x_view );
      Ok (( )) } )) }
