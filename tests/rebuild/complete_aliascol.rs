// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::error::Error;

use skg::rebuild::completeAliasCol;
use skg::save::buffer_to_orgnodes::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::OrgNode;

#[test]
fn test_completeAliasCol
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-aliascol",
    "tests/rebuild/complete_aliascol/fixtures",
    "/tmp/tantivy-test-complete-aliascol",
    |config, _driver| Box::pin ( async move {
      test_completeAliasCol_logic ( config ) . await
    } )) }

async fn test_completeAliasCol_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {

  // Create org text with three AliasCol scenarios
  let org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (treatment aliasCol)) aliases 1
      *** (skg (treatment alias)) c
      *** (skg (treatment alias)) d
      *** (skg (treatment alias)) c
      *** (skg (treatment alias)) d
      ** (skg (treatment aliasCol)) aliases 2
      *** (skg (treatment alias)) b
      *** (skg (treatment alias) focused) d
      * (skg (treatment aliasCol)) aliases 3
      ** (skg (treatment alias)) the above should break
    " };

  let mut forest : Vec < ego_tree::Tree < OrgNode > > =
    org_to_uninterpreted_nodes ( org_text ) ?;

  // Get the first tree (node "a" and its children)
  let tree_a : &mut ego_tree::Tree < OrgNode > =
    & mut forest [ 0 ];

  // Find the NodeIds for the AliasCol nodes
  let aliascol_1_id : ego_tree::NodeId = {
    tree_a . root ()
      . first_child () . unwrap ()
      . id ()
  };
  let aliascol_2_id : ego_tree::NodeId = {
    tree_a . root ()
      . first_child () . unwrap ()
      . next_sibling () . unwrap ()
      . id ()
  };

  // Test 1: First AliasCol should have c and b (deduped, valid only)
  completeAliasCol (
    tree_a,
    aliascol_1_id,
    & config
  ) ?;

  {
    let aliascol_1_ref : ego_tree::NodeRef < OrgNode > =
      tree_a . get ( aliascol_1_id ) . unwrap ();
    let children : Vec < & OrgNode > =
      aliascol_1_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children . len (),
      2,
      "AliasCol 1 should have exactly 2 children"
    );
    assert_eq! (
      children [ 0 ] . title,
      "c",
      "First child should be 'c'"
    );
    assert_eq! (
      children [ 1 ] . title,
      "b",
      "Second child should be 'b'"
    );
  }

  // Test 2: Second AliasCol should have b and c, and gain focus
  completeAliasCol (
    tree_a,
    aliascol_2_id,
    & config
  ) ?;

  {
    let aliascol_2_ref : ego_tree::NodeRef < OrgNode > =
      tree_a . get ( aliascol_2_id ) . unwrap ();
    let aliascol_2_node : & OrgNode =
      aliascol_2_ref . value ();
    let children : Vec < & OrgNode > =
      aliascol_2_ref . children ()
      . map ( |n| n . value () )
      . collect ();

    assert_eq! (
      children . len (),
      2,
      "AliasCol 2 should have exactly 2 children"
    );
    assert_eq! (
      children [ 0 ] . title,
      "b",
      "First child should be 'b'"
    );
    assert_eq! (
      children [ 1 ] . title,
      "c",
      "Second child should be 'c'"
    );
    assert! (
      aliascol_2_node . metadata . focused,
      "AliasCol 2 should have gained focus"
    );
  }

  // Test 3: Third AliasCol should error (no parent or parent has no ID)
  let tree_aliascol_3 : &mut ego_tree::Tree < OrgNode > =
    & mut forest [ 1 ];
  let aliascol_3_id : ego_tree::NodeId =
    tree_aliascol_3 . root () . id ();

  let result : Result < (), Box<dyn Error> > =
    completeAliasCol (
      tree_aliascol_3,
      aliascol_3_id,
      & config
    );

  assert! (
    result . is_err (),
    "AliasCol 3 should error (no parent)"
  );

  Ok (( )) }
