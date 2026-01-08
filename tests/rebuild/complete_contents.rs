// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::error::Error;

use ego_tree::{Tree, NodeId};
use skg::from_text::buffer_to_orgnodes::uninterpreted::org_to_uninterpreted_nodes;
use skg::org_to_text::orgnode_forest_to_string;
use skg::test_utils::{ run_with_test_db, orgnode_forest_to_paired};
use skg::to_org::util::{VisitedMap, mark_if_visited_or_repeat_or_cycle};
use skg::to_org::complete::contents::{completeDefinitiveOrgnode, clobberIndefinitiveOrgnode, ensure_skgnode};
use skg::types::tree::PairTree;
use skg::types::misc::{ID, SkgConfig};
use skg::types::orgnode_new::OrgNode;

/// Get the NodeId of the first "tree root" (child of ForestRoot)
fn first_tree_root_id ( forest : &PairTree ) -> NodeId {
  forest . root () . first_child () . unwrap () . id () }

/// Helper to call ensure_skgnode, mark_if_visited_or_repeat_or_cycle,
/// and then clobberIndefinitiveOrgnode or completeDefinitiveOrgnode.
/// (matches the pattern used in completeAndRestoreNode_collectingViewRequests).
/// TODO: Unify with completeAndRestoreNode_collectingViewRequests.
async fn check_and_complete (
  tree     : &mut PairTree,
  node_id  : NodeId,
  config   : &SkgConfig,
  driver   : &typedb_driver::TypeDBDriver,
  visited  : &mut VisitedMap,
) -> Result < (), Box<dyn Error> > {
  ensure_skgnode (
    tree, node_id, config, driver ) . await ?;
  mark_if_visited_or_repeat_or_cycle (
    tree, node_id, visited ) ?;
  let is_indefinitive : bool = {
    // 'mark_if_visited_or_repeat_or_cycle' may have changed this value.
    let node_ref = tree . get ( node_id )
      . ok_or ( "Node not found" ) ?;
    node_ref . value () . orgnode () . is_indefinitive () };
  if is_indefinitive {
    clobberIndefinitiveOrgnode (
      tree, node_id ) ?;
  } else {
    completeDefinitiveOrgnode (
      tree, node_id, config, driver ) . await ?; }
  Ok (( )) }

#[test]
fn test_indefinitive_identity_at_multiple_levels
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-indefinitive",
    "tests/rebuild/complete_contents/fixtures",
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
      * (skg (id a) (code indefinitive)) a
      ** (skg (id c) (code indefinitive)) c
      *** (skg (id d) (code (interp parentIgnores))) d
    " };

  { // Test running on root with empty visited
    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : VisitedMap =
      VisitedMap::new ();
    check_and_complete ( // processes root but *not* its descendents
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c) (code indefinitive)) c
        *** (skg (id d) (code (interp parentIgnores))) d
      " };
    let output_org_text : String =
      orgnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Running on root with empty visited should fetch canonical info for indefinitive node" );
    assert_eq! (
      visited . len (), 0,
      "Visited should be empty, because we only ran it on the indefinitive root" ); }

  { // Test running on root with 'a' in visited
    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : VisitedMap =
      VisitedMap::new ();
    // Pre-populate visited with 'a' at a dummy location
    visited . insert ( ID::new ( "a" ), root_id );
    check_and_complete (
      &mut forest, root_id, config, driver, &mut visited ). await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c) (code indefinitive)) c
        *** (skg (id d) (code (interp parentIgnores))) d
      " };
    let output_org_text : String =
      orgnode_forest_to_string ( & forest ) ?;
    assert_eq! (
      output_org_text, expected_output,
      "Since ID 'a' was in 'visited', orgnode for 'a' should be marked 'indefinitive' with source, but children preserved." );
    assert_eq! (
      visited . len (), 1,
      "Visited should still contain only 'a'" );
    assert! (
      visited . contains_key ( & ID::new ( "a" )),
      "Visited should contain 'a'" );
  }

  { // Test running on second node (c)
    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let second_node_id : ego_tree::NodeId =
      forest . root ()
      . first_child () . unwrap () // first "tree root"
      . first_child () . unwrap () // first child of that
      . id ();
    let mut visited : VisitedMap =
      VisitedMap::new ();

    check_and_complete (
      &mut forest, second_node_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (code indefinitive)) a
        ** (skg (id c) (source main) (code indefinitive)) c
        *** (skg (id d) (code (interp parentIgnores))) d
      " };
    assert_eq! (
      orgnode_forest_to_string ( & forest ) ?,
      expected_output,
      "Running on second indefinitive node should fetch canonical info from disk" );
  }

  Ok (( )) }

#[test]
fn test_visited_and_indefinitive
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-indefinitive",
    "tests/rebuild/complete_contents/fixtures",
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
        * (skg (id a) (code indefinitive)) a
        ** (skg (id c)) c
        *** (skg (id d) (code (interp parentIgnores))) d
      " };

    // Test with empty visited
    {
      let orgnode_forest : Tree < OrgNode > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let mut forest : PairTree =
        orgnode_forest_to_paired ( orgnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : VisitedMap =
        VisitedMap::new ();

      check_and_complete (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id a) (source main) (code indefinitive)) a
          ** (skg (id c)) c
          *** (skg (id d) (code (interp parentIgnores))) d
        " };
      let output_org_text : String =
        orgnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with empty visited should fetch canonical info from disk" );
    }

    // Test with 'a' in visited - should mark as repeated
    {
      let orgnode_forest : Tree < OrgNode > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let mut forest : PairTree =
        orgnode_forest_to_paired ( orgnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : VisitedMap =
        VisitedMap::new ();
      visited . insert ( ID::new ( "a" ), root_id );

      check_and_complete (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id a) (source main) (code indefinitive)) a
          ** (skg (id c)) c
          *** (skg (id d) (code (interp parentIgnores))) d
        " };
      let output_org_text : String =
        orgnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with ID in visited should get canonical info from disk" );
    }
  }

  // Test 2: self-referential tree
  {
    let input_org_text_self_ref : &str =
      indoc! { "
        * (skg (id d) (code indefinitive)) d
        ** (skg (id d)) d
        *** (skg (id d)) d
        **** (skg (id d)) d
      " };

    // Running from root
    {
      let orgnode_forest : Tree < OrgNode > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let mut forest : PairTree =
        orgnode_forest_to_paired ( orgnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let mut visited : VisitedMap =
        VisitedMap::new ();

      check_and_complete (
        &mut forest, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id d) (source main) (code indefinitive)) d
          ** (skg (id d)) d
          *** (skg (id d)) d
          **** (skg (id d)) d
        " };
      let output_org_text : String =
        orgnode_forest_to_string ( & forest ) ?;
      assert_eq! (
        output_org_text, expected_output,
        "Self-referential indefinitive root should fetch canonical info from disk" );
    }

    // Running from second node
    {
      let orgnode_forest : Tree < OrgNode > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let mut forest : PairTree =
        orgnode_forest_to_paired ( orgnode_forest );
      let root_id : ego_tree::NodeId =
        first_tree_root_id ( &forest );
      let second_node_id : ego_tree::NodeId =
        forest . root ()
        . first_child () . unwrap () // first "tree root"
        . first_child () . unwrap () // first child
        . id ();
      let mut visited : VisitedMap =
        VisitedMap::new ();
      visited . insert ( ID::new ( "d" ), root_id );

      check_and_complete (
        &mut forest, second_node_id, config, driver, &mut visited ) . await ?;

      let expected_output_from_second : &str =
        indoc! { "
          * (skg (id d) (code indefinitive)) d
          ** (skg (id d) (source main) (view cycle) (code indefinitive)) d
          *** (skg (id d)) d
          **** (skg (id d)) d
        " };
      let output_org_text : String =
        orgnode_forest_to_string ( & forest ) ?;
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
    "tests/rebuild/complete_contents/fixtures",
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
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d) (code (interp parentIgnores))) d
    " };

  // Test with 'a' in visited
  {
    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : VisitedMap =
      VisitedMap::new ();
    visited . insert ( ID::new ( "a" ), root_id );
    let visited_keys_before : Vec < ID > =
      visited . keys () . cloned () . collect ();

    check_and_complete (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c)) c
        ** (skg (id d) (code (interp parentIgnores))) d
      " };
    let output_org_text : String =
      orgnode_forest_to_string ( & forest ) ?;
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
    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : VisitedMap =
      VisitedMap::new ();

    check_and_complete (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (interp parentIgnores))) d
        ** (skg (id b) (source main)) b
        ** (skg (id c)) c
      " };
    let output_org_text : String =
      orgnode_forest_to_string ( & forest ) ?;
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
        * (skg (id a)) a
        ** (skg (id b)) b
        ** (skg (id d) (code (interp parentIgnores))) d
      " };

    let orgnode_forest : Tree < OrgNode > =
      org_to_uninterpreted_nodes ( input_with_existing_content ) ?;
    let mut forest : PairTree =
      orgnode_forest_to_paired ( orgnode_forest );
    let root_id : ego_tree::NodeId =
      first_tree_root_id ( &forest );
    let mut visited : VisitedMap =
      VisitedMap::new ();

    check_and_complete (
      &mut forest, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (interp parentIgnores))) d
        ** (skg (id b)) b
        ** (skg (id c) (source main)) c
      " };
    let output_org_text : String =
      orgnode_forest_to_string ( & forest ) ?;
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
    "tests/rebuild/complete_contents/fixtures",
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
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d)) d
    " };

  let orgnode_forest : Tree < OrgNode > =
    org_to_uninterpreted_nodes ( input_org_text ) ?;
  let mut forest : PairTree =
    orgnode_forest_to_paired ( orgnode_forest );
  let root_id : ego_tree::NodeId =
    first_tree_root_id ( &forest );
  let mut visited : VisitedMap =
    VisitedMap::new ();

  check_and_complete (
    &mut forest, root_id, config, driver, &mut visited ) . await ?;

  let expected_output : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id d) (code (interp parentIgnores))) d
      ** (skg (id b) (source main)) b
      ** (skg (id c)) c
    " };
  let output_org_text : String =
    orgnode_forest_to_string ( & forest ) ?;
  assert_eq! (
    output_org_text, expected_output,
    "False content 'd' should be marked parentIgnores and moved first" );

  Ok (( )) }
