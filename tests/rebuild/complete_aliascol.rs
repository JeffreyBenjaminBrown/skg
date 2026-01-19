// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::to_org::complete::aliascol::completeAliasCol;
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::orgnode::OrgNode;
use skg::types::misc::SkgConfig;
use skg::types::skgnode::{SkgNodeMap, skgnode_map_from_forest};

use ego_tree::{Tree, NodeId};

#[test]
fn test_completeAliasCol
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-aliascol",
    "tests/rebuild/complete_aliascol/fixtures",
    "/tmp/tantivy-test-complete-aliascol",
    |config, driver, _tantivy| Box::pin ( async move {
      test_completeAliasCol_logic ( config, driver ) . await
    } )) }

async fn test_completeAliasCol_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  // Create org text with three AliasCol scenarios
  let org_text : &str =
    indoc! { "
      * (skg (node (id a))) a
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

  let mut forest : Tree < OrgNode > =
    org_to_uninterpreted_nodes ( org_text ) ?.0;
  let mut map : SkgNodeMap =
    skgnode_map_from_forest ( & forest, config, driver ) . await ?;

  // Get the first "tree root" (node "a" and its children)
  let tree_a_id : NodeId =
    forest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for the AliasCol nodes
  let aliascol_1_id : NodeId = {
    forest . get ( tree_a_id ) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let aliascol_2_id : NodeId = {
    forest . get ( tree_a_id ) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test 1: First AliasCol should have c and b (deduped, valid only)
  completeAliasCol (
    &mut forest,
    &mut map,
    aliascol_1_id,
    & config,
    driver
  ) .await?;

  {
    let aliascol_1_ref =
      forest . get ( aliascol_1_id ) . unwrap ();
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
      "c",
      "First child should be 'c'"
    );
    assert_eq! (
      children [ 1 ],
      "b",
      "Second child should be 'b'"
    );
  }

  // Test 2: Second AliasCol should have b and c, and gain focus
  completeAliasCol (
    &mut forest,
    &mut map,
    aliascol_2_id,
    & config,
    driver
  ) .await?;

  {
    let aliascol_2_ref =
      forest . get ( aliascol_2_id ) . unwrap ();
    let aliascol_2_new : &OrgNode = aliascol_2_ref . value ();
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
    forest . root () . first_child () . unwrap ()
    . next_sibling () . unwrap ()
    . id ();

  let result : Result < (), Box<dyn Error> > =
    completeAliasCol (
      &mut forest,
      &mut map,
      aliascol_3_id,
      & config,
      driver
    ).await;

  assert! (
    result . is_err (),
    "AliasCol 3 should error (no parent)"
  );

  Ok (( )) }

#[test]
fn test_completeAliasCol_duplicate_aliases_different_orders
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-aliascol-duplicates",
    "tests/rebuild/complete_aliascol/fixtures",
    "/tmp/tantivy-test-complete-aliascol-duplicates",
    |config, driver, _tantivy| Box::pin ( async move {
      test_completeAliasCol_duplicate_aliases_different_orders_logic (
        config, driver ). await } )) }

async fn test_completeAliasCol_duplicate_aliases_different_orders_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  let org_text : &str =
    indoc! { "
      * (skg (node (id a))) a
      ** (skg aliasCol) aliases
      *** (skg alias) b
      *** (skg focused alias) b
      ** (skg aliasCol) aliases
      *** (skg focused alias) b
      *** (skg alias) b
    " };

  let mut forest : Tree < OrgNode > =
    org_to_uninterpreted_nodes ( org_text ) ?.0;
  let mut map : SkgNodeMap =
    skgnode_map_from_forest ( & forest, config, driver ) . await ?;

  let tree_root_id : NodeId =
    forest . root () . first_child () . unwrap () . id ();

  // Find the NodeIds for both AliasCol nodes
  let first_aliascol_id : NodeId = {
    forest . get ( tree_root_id ) . unwrap ()
      . first_child () . unwrap ()
      . id ()
  };
  let second_aliascol_id : NodeId = {
    forest . get ( tree_root_id ) . unwrap ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test first AliasCol
  completeAliasCol (
    &mut forest,
    &mut map,
    first_aliascol_id,
    & config,
    driver
  ) .await?;

  {
    let aliascol_ref =
      forest . get ( first_aliascol_id ) . unwrap ();
    let children_new : Vec < &OrgNode > =
      aliascol_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children_new . len (),
      2,
      "First AliasCol should have exactly 2 children (b focused, c)"
    );
    assert_eq! (
      children_new [ 0 ] . title (),
      "b",
      "First child should be 'b'"
    );
    assert! (
      children_new [ 0 ] . focused,
      "First child should be focused"
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
  completeAliasCol (
    &mut forest,
    &mut map,
    second_aliascol_id,
    & config,
    driver
  ) .await?;

  {
    let aliascol_ref =
      forest . get ( second_aliascol_id ) . unwrap ();
    let children : Vec < &OrgNode > =
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
