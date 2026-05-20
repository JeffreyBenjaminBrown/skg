// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::collections::HashMap;
use std::error::Error;

use skg::update_buffer::complete_postorder::aliascol::reconcile_alias_col_children;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::test_utils::run_with_test_db;
use skg::types::viewnode::ViewNode;
use skg::types::misc::SkgConfig;
use skg::types::misc::SourceName;
use skg::types::git::SourceDiff;

use ego_tree::{Tree, NodeId};

#[test]
fn test_reconcile_alias_col_children
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-aliascol",
    "tests/rebuild/complete_aliascol/fixtures",
    "/tmp/tantivy-test-complete-aliascol",
    |config, driver, _tantivy| Box::pin ( async move {
      test_reconcile_alias_col_children_logic ( config, driver ) . await
    } )) }

async fn test_reconcile_alias_col_children_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  let source_diffs : Option<HashMap<SourceName, SourceDiff>> = None;

  // Create org text with three AliasCol scenarios
  let org_text : &str =
    indoc! { "
      * (skg (node (id a) (source main))) a
      ** (skg aliasCol) aliases 1
      *** (skg alias) c
      *** (skg alias) d
      *** (skg alias) c
      *** (skg alias) d
      ** (skg aliasCol) aliases 2
      *** (skg alias) b
      *** (skg focused alias) d
      * (skg aliasCol) aliases 3
      ** (skg alias) the above should break
    " };

  let unchecked_viewforest =
    org_to_uninterpreted_nodes (org_text) ?. 0;
  let mut viewforest : Tree < ViewNode > =
    maybePlaced_to_placed_tree (unchecked_viewforest) ?;

  // Get the first "tree root" (node "a" and its children)
  let tree_a_id : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for the AliasCol nodes
  let aliascol_1_id : NodeId = {
    viewforest . get (tree_a_id) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let aliascol_2_id : NodeId = {
    viewforest . get (tree_a_id) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test 1: First AliasCol should have b and c (deduped, valid only, disk order)
  reconcile_alias_col_children ( &mut viewforest, aliascol_1_id, &source_diffs, config )?;

  {
    let aliascol_1_ref =
      viewforest . get (aliascol_1_id) . unwrap ();
    let children : Vec < String > =
      aliascol_1_ref . children () . map (
        |n| n . value() . title() . to_string() )
      . collect();

    assert_eq! (
      children . len (),
      2,
      "AliasCol 1 should have exactly 2 children"
    );
    assert_eq! (
      children [ 0 ],
      "b",
      "First child should be 'b'"
    );
    assert_eq! (
      children [ 1 ],
      "c",
      "Second child should be 'c'"
    );
  }

  // Test 2: Second AliasCol should have b and c, and gain focus
  reconcile_alias_col_children ( &mut viewforest, aliascol_2_id, &source_diffs, config )?;

  {
    let aliascol_2_ref =
      viewforest . get (aliascol_2_id) . unwrap ();
    let aliascol_2_new : &ViewNode = aliascol_2_ref . value ();
    let children : Vec < String > =
      aliascol_2_ref . children () . map (
        |n| n . value() . title() . to_string() )
      . collect();

    assert_eq! (
      children . len (),
      2,
      "AliasCol 2 should have exactly 2 children"
    );
    assert_eq! (
      children [ 0 ],
      "b",
      "First child should be 'b'"
    );
    assert_eq! (
      children [ 1 ],
      "c",
      "Second child should be 'c'"
    );
    assert! (
      aliascol_2_new . focused,
      "AliasCol 2 should have gained focus"
    );
  }

  // Test 3: Third AliasCol should error (no parent or parent has no ID)
  // Get the second "tree root" (AliasCol 3)
  let aliascol_3_id : NodeId =
    viewforest . root () . first_child () . unwrap ()
    . next_sibling () . unwrap ()
    . id ();

  let result : Result < (), Box<dyn Error> > =
    reconcile_alias_col_children (
      &mut viewforest,
      aliascol_3_id,
      &source_diffs,
      config
    );

  assert! (
    result . is_err (),
    "AliasCol 3 should error (no parent)"
  );

  Ok (( )) }

#[test]
fn test_reconcile_alias_col_children_duplicate_aliases_different_orders
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-aliascol-duplicates",
    "tests/rebuild/complete_aliascol/fixtures",
    "/tmp/tantivy-test-complete-aliascol-duplicates",
    |config, driver, _tantivy| Box::pin ( async move {
      test_reconcile_alias_col_children_duplicate_aliases_different_orders_logic (
        config, driver ) . await } )) }

async fn test_reconcile_alias_col_children_duplicate_aliases_different_orders_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  let source_diffs : Option<HashMap<SourceName, SourceDiff>> = None;

  let org_text : &str =
    indoc! { "
      * (skg (node (id a) (source main))) a
      ** (skg aliasCol) aliases
      *** (skg alias) b
      *** (skg focused alias) b
      ** (skg aliasCol) aliases
      *** (skg focused alias) b
      *** (skg alias) b
    " };

  let unchecked_viewforest =
    org_to_uninterpreted_nodes (org_text) ?. 0;
  let mut viewforest : Tree < ViewNode > =
    maybePlaced_to_placed_tree (unchecked_viewforest) ?;

  let tree_root_id : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for both AliasCol nodes
  let first_aliascol_id : NodeId = {
    viewforest . get (tree_root_id) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let second_aliascol_id : NodeId = {
    viewforest . get (tree_root_id) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test first AliasCol
  reconcile_alias_col_children (
    &mut viewforest,
    first_aliascol_id,
    &source_diffs,
    config
  )?;

  {
    let aliascol_ref =
      viewforest . get (first_aliascol_id) . unwrap ();
    let aliascol_vn : &ViewNode = aliascol_ref . value ();
    let children_new : Vec < &ViewNode > =
      aliascol_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children_new . len (),
      2,
      "First AliasCol should have exactly 2 children (b, c)"
    );
    assert_eq! (
      children_new [ 0 ] . title (),
      "b",
      "First child should be 'b'"
    );
    assert! (
      ! children_new [ 0 ] . focused,
      "First child should not be focused (focus transferred to AliasCol)"
    );
    assert! (
      aliascol_vn . focused,
      "AliasCol itself should have gained focus"
    );
    assert_eq! (
      children_new [ 1 ] . title (),
      "c",
      "Second child should be 'c'"
    );
    assert! (
      ! children_new [ 1 ] . focused,
      "Second child should not be focused"
    );
  }

  // Test second AliasCol
  reconcile_alias_col_children (
    &mut viewforest,
    second_aliascol_id,
    &source_diffs,
    config
  )?;

  {
    let aliascol_ref =
      viewforest . get (second_aliascol_id) . unwrap ();
    let children : Vec < &ViewNode > =
      aliascol_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children . len (),
      2,
      "Second AliasCol should have exactly 2 children (b focused, c)"
    );
    assert_eq! (
      children [ 0 ] . title (),
      "b",
      "First child should be 'b'"
    );
    assert! (
      children [ 0 ] . focused,
      "First child should be focused"
    );
    assert_eq! (
      children [ 1 ] . title (),
      "c",
      "Second child should be 'c'"
    );
    assert! (
      ! children [ 1 ] . focused,
      "Second child should not be focused"
    );
  }

  Ok (( )) }
