use indoc::indoc;
use std::error::Error;

use ego_tree::{Tree, NodeId};
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use crate::org_to_text::viewnode_forest_to_string;
use crate::test_utils::{ run_with_test_db, viewnode_forest_to_paired};
use crate::to_org::util::{DefinitiveMap, make_indef_if_repeat_then_extend_defmap, truenode_in_tree_is_indefinitive};
use super::content_children::completeAndReorder_childrenOf_definitiveViewnode;
use crate::to_org::complete::contents::{clobberIndefinitiveViewnode, ensure_skgnode};
use crate::types::tree::PairTree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{ViewNode, ViewNodeKind};

/// Get the NodeId of the first "tree root" (child of BufferRoot)
fn first_tree_root_id ( forest : &PairTree ) -> NodeId {
  forest . root () . first_child () . unwrap () . id () }

/// Helper to call ensure_skgnode, make_indef_if_repeat_then_extend_defmap_or_cycle,
/// and then clobberIndefinitiveViewnode or completeAndReorder_childrenOf_definitiveViewnode.
/// (matches the pattern used in complete_or_restore_each_node_in_branch).
/// TODO: Unify with complete_or_restore_each_node_in_branch.
/// Does nothing for Scaffolds.
async fn check_and_complete_if_truenode (
  tree     : &mut PairTree,
  node_id  : NodeId,
  config   : &SkgConfig,
  driver   : &typedb_driver::TypeDBDriver,
  visited  : &mut DefinitiveMap,
) -> Result < (), Box<dyn Error> > {
  { let node_ref = tree . get ( node_id )
      . ok_or ( "Node not found" ) ?;
    let ViewNodeKind::True ( _ ) = & node_ref . value () . viewnode . kind
      else { return Ok (( )); }; } // Skip Scaffolds.
  ensure_skgnode (
    tree, node_id, config, driver ) . await ?;
  make_indef_if_repeat_then_extend_defmap (
    tree, node_id, visited ) ?;
  // 'make_indef_if_repeat_then_extend_defmap_or_cycle' may have changed 'indefinitive'.
  if truenode_in_tree_is_indefinitive ( tree, node_id ) ? {
    clobberIndefinitiveViewnode (
      tree, node_id ) ?;
  } else {
    completeAndReorder_childrenOf_definitiveViewnode (
      tree, node_id, config, driver ) . await ?; }
  Ok (( )) }

#[test]
fn test_indefinitive_identity_at_multiple_levels
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-indefinitive",
    "server/abandoned/to-org_complete_content-children/fixtures",
    "/tmp/tantivy-test-complete-contents-indefinitive",
    |config, driver, _tantivy| Box::pin ( async move {
      test_indefinitive_identity_at_multiple_levels_logic (
        config, driver ) . await
    } )) }

async fn test_indefinitive_identity_at_multiple_levels_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (node (id a) indefinitive)) a
      ** (skg (node (id c) indefinitive)) c
      *** (skg (node (id d) parentIgnores)) d
    " };

  { // Test running on root with empty visited
    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();
    check_and_complete_if_truenode ( // processes root but *not* its descendents
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a) (source main) indefinitive)) a
        ** (skg (node (id c) indefinitive)) c
        *** (skg (node (id d) parentIgnores)) d
      " };
    let output_org_text : String =
      viewnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Running on root with empty visited should fetch canonical info for indefinitive node" );
    assert_eq! (
      visited . len (), 0,
      "Visited should be empty, because we only ran it on the indefinitive root" ); }

  { // Test running on root with 'a' in visited
    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();
    // Pre-populate visited with 'a' at a dummy location
    visited . insert ( ID::new ( "a" ), root_id );
    check_and_complete_if_truenode (
      &mut forest, root_id, config, driver, &mut visited ). await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a) (source main) indefinitive)) a
        ** (skg (node (id c) indefinitive)) c
        *** (skg (node (id d) parentIgnores)) d
      " };
    let output_org_text : String =
      viewnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Since ID 'a' was in 'visited', viewnode for 'a' should be marked 'indefinitive' with source, but children preserved." );
    assert_eq! (
      visited . len (), 1,
      "Visited should still contain only 'a'" );
    assert! (
      visited . contains_key ( & ID::new ( "a" )),
      "Visited should contain 'a'" );
  }

  { // Test running on second node (c)
    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let second_node_id : ego_tree::NodeId =
      forest . root ()
      . first_child () . unwrap () // first "tree root"
      . first_child () . unwrap () // first child of that
      . id ();
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();

    check_and_complete_if_truenode (
      &mut forest, second_node_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a) indefinitive)) a
        ** (skg (node (id c) (source main) indefinitive)) c
        *** (skg (node (id d) parentIgnores)) d
      " };
    assert_eq! (
      viewnode_forest_to_string ( & forest ) ?,
      expected_output,
      "Running on second indefinitive node should fetch canonical info from disk" );
  }

  Ok (( )) }

#[test]
fn test_visited_and_indefinitive
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-indefinitive",
    "server/abandoned/to-org_complete_content-children/fixtures",
    "/tmp/tantivy-test-complete-contents-visited-indefinitive",
    |config, driver, _tantivy| Box::pin ( async move {
      test_visited_and_indefinitive_logic (
        config, driver ) . await
    } )) }

async fn test_visited_and_indefinitive_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  // Test 1: indefinitive with non-indefinitive child
  {
    let input_org_text : &str =
      indoc! { "
        * (skg (node (id a) indefinitive)) a
        ** (skg (node (id c))) c
        *** (skg (node (id d) parentIgnores)) d
      " };

    // Test with empty visited
    {
      let viewnode_forest : Tree < ViewNode > =
        org_to_uninterpreted_nodes ( input_org_text ) ?.0;
      let mut forest : PairTree =
        viewnode_forest_to_paired ( viewnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : DefinitiveMap =
        DefinitiveMap::new ();

      check_and_complete_if_truenode (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (node (id a) (source main) indefinitive)) a
          ** (skg (node (id c))) c
          *** (skg (node (id d) parentIgnores)) d
        " };
      let output_org_text : String =
        viewnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with empty visited should fetch canonical info from disk" );
    }

    // Test with 'a' in visited - should mark as repeated
    {
      let viewnode_forest : Tree < ViewNode > =
        org_to_uninterpreted_nodes ( input_org_text ) ?.0;
      let mut forest : PairTree =
        viewnode_forest_to_paired ( viewnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : DefinitiveMap =
        DefinitiveMap::new ();
      visited . insert ( ID::new ( "a" ), root_id );

      check_and_complete_if_truenode (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (node (id a) (source main) indefinitive)) a
          ** (skg (node (id c))) c
          *** (skg (node (id d) parentIgnores)) d
        " };
      let output_org_text : String =
        viewnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with ID in visited should get canonical info from disk" );
    }
  }

  // Test 2: self-referential tree
  {
    let input_org_text_self_ref : &str =
      indoc! { "
        * (skg (node (id d) indefinitive)) d
        ** (skg (node (id d))) d
        *** (skg (node (id d))) d
        **** (skg (node (id d))) d
      " };

    // Running from root
    {
      let viewnode_forest : Tree < ViewNode > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?.0;
      let mut forest : PairTree =
        viewnode_forest_to_paired ( viewnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : DefinitiveMap =
        DefinitiveMap::new ();

      check_and_complete_if_truenode (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (node (id d) (source main) indefinitive)) d
          ** (skg (node (id d))) d
          *** (skg (node (id d))) d
          **** (skg (node (id d))) d
        " };
      let output_org_text : String =
        viewnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "Self-referential indefinitive root should fetch canonical info from disk" );
    }

    // Running from second node
    {
      let viewnode_forest : Tree < ViewNode > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?.0;
      let mut forest : PairTree =
        viewnode_forest_to_paired ( viewnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let second_node_id : ego_tree::NodeId =
        forest . root ()
        . first_child () . unwrap () // first "tree root"
        . first_child () . unwrap () // first child
        . id ();
      let mut visited : DefinitiveMap =
        DefinitiveMap::new ();
      visited . insert ( ID::new ( "d" ), root_id );

      check_and_complete_if_truenode (
        &mut forest, second_node_id, config, driver, &mut visited ) . await ?;

      let expected_output_from_second : &str =
        indoc! { "
          * (skg (node (id d) indefinitive)) d
          ** (skg (node (id d) (source main) indefinitive cycle)) d
          *** (skg (node (id d))) d
          **** (skg (node (id d))) d
        " };
      let output_org_text : String =
        viewnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output_from_second,
        "Self-referential second node should become indefinitive with source, but children preserved" );
    }
  }

  Ok (( )) }

#[test]
fn test_visited_and_not_indefinitive
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-not-indefinitive",
    "server/abandoned/to-org_complete_content-children/fixtures",
    "/tmp/tantivy-test-complete-contents-visited-not-indefinitive",
    |config, driver, _tantivy| Box::pin ( async move {
      test_visited_and_not_indefinitive_logic (
        config, driver ) . await
    } )) }

async fn test_visited_and_not_indefinitive_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (node (id a))) a
      ** (skg (node (id c))) c
      ** (skg (node (id d) parentIgnores)) d
    " };

  // Test with 'a' in visited
  {
    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();
    visited . insert ( ID::new ( "a" ), root_id );
    let visited_keys_before : Vec < ID > =
      visited . keys () . cloned () . collect ();

    check_and_complete_if_truenode (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a) (source main) indefinitive)) a
        ** (skg (node (id c))) c
        ** (skg (node (id d) parentIgnores)) d
      " };
    let output_org_text : String =
      viewnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Node 'a' in visited should become indefinitive with source, but children preserved" );
    let visited_keys_after : Vec < ID > =
      visited . keys () . cloned () . collect ();
    assert_eq! (
      visited_keys_after, visited_keys_before,
      "Visited keys should be unchanged" );
  }

  // Test with empty visited
  {
    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();

    check_and_complete_if_truenode (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a))) a
        ** (skg (node (id d) parentIgnores)) d
        ** (skg (node (id b) (source main))) b
        ** (skg (node (id c))) c
      " };
    let output_org_text : String =
      viewnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Node 'a' not in visited should be completed from disk" );
    assert! (
      visited . contains_key ( & ID::new ( "a" )),
      "Visited should now contain 'a'" );
  }

  // Test with existing content that needs reordering
  {
    let input_with_existing_content : &str =
      indoc! { "
        * (skg (node (id a))) a
        ** (skg (node (id b))) b
        ** (skg (node (id d) parentIgnores)) d
      " };

    let viewnode_forest : Tree < ViewNode > =
      org_to_uninterpreted_nodes ( input_with_existing_content ) ?.0;
    let mut forest : PairTree =
      viewnode_forest_to_paired ( viewnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : DefinitiveMap =
      DefinitiveMap::new ();

    check_and_complete_if_truenode (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (node (id a))) a
        ** (skg (node (id d) parentIgnores)) d
        ** (skg (node (id b))) b
        ** (skg (node (id c) (source main))) c
      " };
    let output_org_text : String =
      viewnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Content should be reordered and completed from disk" );
  }

  Ok (( )) }

#[test]
fn test_false_content
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-false-content",
    "server/abandoned/to-org_complete_content-children/fixtures",
    "/tmp/tantivy-test-complete-contents-false-content",
    |config, driver, _tantivy| Box::pin ( async move {
      test_false_content_logic (
        config, driver ) . await
    } )) }

async fn test_false_content_logic (
  config : &SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (node (id a))) a
      ** (skg (node (id c))) c
      ** (skg (node (id d))) d
    " };

  let viewnode_forest : Tree < ViewNode > =
    org_to_uninterpreted_nodes ( input_org_text ) ?.0;
  let mut forest : PairTree =
    viewnode_forest_to_paired ( viewnode_forest );
  let root_id : ego_tree::NodeId =
    first_tree_root_id ( &forest );
  let mut visited : DefinitiveMap =
    DefinitiveMap::new ();

  check_and_complete_if_truenode (
    &mut forest, root_id, config, driver, &mut visited ) . await ?;

  let expected_output : &str =
    indoc! { "
      * (skg (node (id a))) a
      ** (skg (node (id d) parentIgnores)) d
      ** (skg (node (id b) (source main))) b
      ** (skg (node (id c))) c
    " };
  let output_org_text : String =
    viewnode_forest_to_string ( & forest ) ?;
  assert_eq! (
    output_org_text, expected_output,
    "False content 'd' should be marked parentIgnores and moved first" );

  Ok (( )) }
