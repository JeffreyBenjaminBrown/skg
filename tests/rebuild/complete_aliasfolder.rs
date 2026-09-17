// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::collections::HashMap;
use std::error::Error;

use skg::update_buffer::reconcile::aliasfolder::reconcile_aliasFolder_children;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::types::maybe_placed_viewnode::maybePlaced_to_placed_tree;
use skg::test_utils::{run_with_shared_test_stores, graph_handle_from_config};
use skg::types::viewnode::ViewNode;
use skg::types::misc::SkgConfig;
use skg::types::misc::SourceName;
use skg::types::misc::TantivyIndex;
use skg::types::git::SourceDiff;

use ego_tree::{Tree, NodeId};

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str = "tests/rebuild/complete_aliasfolder/fixtures";
  run_with_shared_test_stores (
    "skg-test-rebuild-complete-aliasFolder",
    |s| Box::pin ( async move {
      s . reset ("test_reconcile_alias_folder_children", fixtures) ?;
      test_reconcile_alias_folder_children (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_reconcile_alias_folder_children_duplicate_aliases_different_orders",
                 fixtures) ?;
      test_reconcile_alias_folder_children_duplicate_aliases_different_orders (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_reconcile_alias_folder_children (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_reconcile_alias_folder_children_logic ( config ) . await }

async fn test_reconcile_alias_folder_children_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  let source_diffs : Option<HashMap<SourceName, SourceDiff>> = None;
  let graph = graph_handle_from_config (config)? . load_full ();

  // Create org text with three AliasFolder scenarios
  let org_text : &str =
    indoc! { "
      * (skg (node (id a) (source main))) a
      ** (skg aliasFolder) aliases 1
      *** (skg alias) c
      *** (skg alias) d
      *** (skg alias) c
      *** (skg alias) d
      ** (skg aliasFolder) aliases 2
      *** (skg alias) b
      *** (skg focused alias) d
      * (skg aliasFolder) aliases 3
      ** (skg alias) the above should break
    " };

  let unchecked_viewforest =
    org_to_uninterpreted_nodes (org_text) ?. 0;
  let mut viewforest : Tree < ViewNode > =
    maybePlaced_to_placed_tree (unchecked_viewforest) ?;

  // Get the first "tree root" (node "a" and its children)
  let tree_a_id : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for the AliasFolder nodes
  let aliasfolder_1_id : NodeId = {
    viewforest . get (tree_a_id) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let aliasfolder_2_id : NodeId = {
    viewforest . get (tree_a_id) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test 1: First AliasFolder should have b and c (deduped, valid only, disk order)
  reconcile_aliasFolder_children (
    &mut viewforest, aliasfolder_1_id, &graph, &source_diffs, config )?;

  {
    let aliasfolder_1_ref =
      viewforest . get (aliasfolder_1_id) . unwrap ();
    let children : Vec < String > =
      aliasfolder_1_ref . children () . map (
        |n| n . value() . title() . to_string() )
      . collect();

    assert_eq! (
      children . len (),
      2,
      "AliasFolder 1 should have exactly 2 children"
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

  // Test 2: Second AliasFolder should have b and c, and gain focus
  reconcile_aliasFolder_children (
    &mut viewforest, aliasfolder_2_id, &graph, &source_diffs, config )?;

  {
    let aliasfolder_2_ref =
      viewforest . get (aliasfolder_2_id) . unwrap ();
    let aliasfolder_2_new : &ViewNode = aliasfolder_2_ref . value ();
    let children : Vec < String > =
      aliasfolder_2_ref . children () . map (
        |n| n . value() . title() . to_string() )
      . collect();

    assert_eq! (
      children . len (),
      2,
      "AliasFolder 2 should have exactly 2 children"
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
      aliasfolder_2_new . focused,
      "AliasFolder 2 should have gained focus"
    );
  }

  // Test 3: Third AliasFolder should error (no parent or parent has no ID)
  // Get the second "tree root" (AliasFolder 3)
  let aliasfolder_3_id : NodeId =
    viewforest . root () . first_child () . unwrap ()
    . next_sibling () . unwrap ()
    . id ();

  let result : Result < (), Box<dyn Error> > =
    reconcile_aliasFolder_children (
      &mut viewforest,
      aliasfolder_3_id,
      &graph,
      &source_diffs,
      config
    );

  assert! (
    result . is_err (),
    "AliasFolder 3 should error (no parent)"
  );

  Ok (( )) }

async fn test_reconcile_alias_folder_children_duplicate_aliases_different_orders (
  config : &SkgConfig,
  _tantivy : &mut TantivyIndex,
) -> Result < (), Box<dyn Error> > {
      test_reconcile_alias_folder_children_duplicate_aliases_different_orders_logic (
        config ) . await }

async fn test_reconcile_alias_folder_children_duplicate_aliases_different_orders_logic (
  config : &SkgConfig,

) -> Result < (), Box<dyn Error> > {

  let source_diffs : Option<HashMap<SourceName, SourceDiff>> = None;
  let graph = graph_handle_from_config (config)? . load_full ();

  let org_text : &str =
    indoc! { "
      * (skg (node (id a) (source main))) a
      ** (skg aliasFolder) aliases
      *** (skg alias) b
      *** (skg focused alias) b
      ** (skg aliasFolder) aliases
      *** (skg focused alias) b
      *** (skg alias) b
    " };

  let unchecked_viewforest =
    org_to_uninterpreted_nodes (org_text) ?. 0;
  let mut viewforest : Tree < ViewNode > =
    maybePlaced_to_placed_tree (unchecked_viewforest) ?;

  let tree_root_id : NodeId =
    viewforest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for both AliasFolder nodes
  let first_aliasfolder_id : NodeId = {
    viewforest . get (tree_root_id) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let second_aliasfolder_id : NodeId = {
    viewforest . get (tree_root_id) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test first AliasFolder
  reconcile_aliasFolder_children (
    &mut viewforest,
    first_aliasfolder_id,
    &graph,
    &source_diffs,
    config
  )?;

  {
    let aliasfolder_ref =
      viewforest . get (first_aliasfolder_id) . unwrap ();
    let aliasfolder_vn : &ViewNode = aliasfolder_ref . value ();
    let children_new : Vec < &ViewNode > =
      aliasfolder_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children_new . len (),
      2,
      "First AliasFolder should have exactly 2 children (b, c)"
    );
    assert_eq! (
      children_new [ 0 ] . title (),
      "b",
      "First child should be 'b'"
    );
    assert! (
      ! children_new [ 0 ] . focused,
      "First child should not be focused (focus transferred to AliasFolder)"
    );
    assert! (
      aliasfolder_vn . focused,
      "AliasFolder itself should have gained focus"
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

  // Test second AliasFolder
  reconcile_aliasFolder_children (
    &mut viewforest,
    second_aliasfolder_id,
    &graph,
    &source_diffs,
    config
  )?;

  {
    let aliasfolder_ref =
      viewforest . get (second_aliasfolder_id) . unwrap ();
    let children : Vec < &ViewNode > =
      aliasfolder_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children . len (),
      2,
      "Second AliasFolder should have exactly 2 children (b focused, c)"
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
