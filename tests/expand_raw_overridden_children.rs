// cargo nextest run --test expand_raw_overridden_children
//
// Production change (2), the second half of the expansion test
// (TODO/full-schema/13_test-rel-matrix.org): substitution does not
// apply to the immediate children of an overridden node drawn RAW.
//
// Fixture (the docs/forks.md shape): clone R overrides original X, and
// R's child R1 overrides X's child X1. Opening X as a view root draws
// X raw (roots draw raw); its immediate child X1 must therefore draw
// RAW too -- NOT substituted by R1 -- because the user is looking at
// the original. (Before this change, a raw title could sit over the
// overrider's substituted children.)
//
// Its own target -- it installs the process-global graph handle
// (substitution reads snapshot_global).

use std::error::Error;

use skg::dbs::in_rust_graph::{
  InRustGraphHandle, init_global_handle_for_first_time_or_panic};
use skg::test_utils::{run_with_test_db, graph_handle_from_config};
use skg::to_org::render::content_view::multi_root_view;
use skg::types::misc::ID;
use ego_tree::Tree;
use skg::types::viewnode::ViewNode;

#[test]
fn raw_drawn_overridden_root_children_are_raw
  () -> Result<(), Box<dyn Error>> {
  run_with_test_db (
    "skg-test-expand-raw-overridden-children",
    "tests/expand_partner_col_member/fixtures",
    "/tmp/tantivy-test-expand-raw-overridden-children",
    |config, driver, tantivy| Box::pin ( async move {
      let graph : InRustGraphHandle =
        graph_handle_from_config (config) ?;
      init_global_handle_for_first_time_or_panic ( graph . clone () );
      let (x_view, _pids, _tree)
        : (String, Vec<ID>, Tree<ViewNode>) =
        multi_root_view (
          driver, config, Some (tantivy), &[ID::from ("X")], false )
        . await ?;
      // X is drawn raw (a view root).
      assert! ( x_view . lines () . any (
          |l| l . starts_with ("* (skg (node (id X)") ),
        "X should be the raw root:\n{}", x_view );
      // X's immediate content child X1 is drawn RAW, as itself, under X.
      assert! ( x_view . lines () . any (
          |l| l . starts_with ("** (skg (node (id X1)") ),
        "X1 should appear raw as X's immediate child:\n{}", x_view );
      // Nothing is substituted in this view: no overrider stands in for
      // X1 (the bug this change removes).
      assert! ( ! x_view . contains ("overridesHere"),
        "no immediate child of a raw-drawn overridden node may be \
         substituted:\n{}", x_view );
      assert! ( ! x_view . lines () . any (
          |l| l . starts_with ("** (skg (node (id R1)") ),
        "the overrider R1 must not stand in for X1:\n{}", x_view );
      Ok (( )) } )) }
