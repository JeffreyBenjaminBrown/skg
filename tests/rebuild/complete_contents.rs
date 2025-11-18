// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::collections::HashSet;
use std::error::Error;

use skg::to_org::{completeDefinitiveOrgnode, clobberIndefinitiveOrgnode, make_indefinitive_if_repeated};
use skg::read_buffer::buffer_to_orgnodes::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::{ID, OrgNode, SkgConfig};
use skg::to_org::content_view::render_forest_to_org;
use ego_tree::{Tree, NodeId};

/// Helper to call make_indefinitive_if_repeated followed by completeContents
/// (matches the pattern used in complete_node_preorder)
async fn check_and_complete (
  tree    : &mut Tree < OrgNode >,
  node_id : NodeId,
  config  : &SkgConfig,
  driver  : &typedb_driver::TypeDBDriver,
  visited : &mut HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  make_indefinitive_if_repeated (
    tree, node_id, visited ) . await ?;
  let is_indefinitive : bool = {
    // 'make_indefinitive_if_repeated' may have changed this value.
    let node_ref = tree . get ( node_id )
      . ok_or ( "Node not found" ) ?;
    node_ref . value () . metadata . code . indefinitive };
  if is_indefinitive {
    clobberIndefinitiveOrgnode (
      tree, node_id, config, driver ) . await ?;
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
    |config, driver| Box::pin ( async move {
      test_indefinitive_identity_at_multiple_levels_logic (
        config, driver ) . await
    } )) }

async fn test_indefinitive_identity_at_multiple_levels_logic (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a) (code indefinitive)) a
      ** (skg (id c) (code indefinitive)) c
      *** (skg (id d) (code (relToParent parentIgnores))) d
    " };

  { // Test running on root with empty visited
    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();
    check_and_complete ( // processes root but *not* its descendents
      tree, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c) (code indefinitive)) c
        *** (skg (id d) (code (relToParent parentIgnores))) d
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Running on root with empty visited should fetch canonical info for indefinitive node" );
    assert_eq! (
      visited . len (), 0,
      "Visited should be empty, because we only ran it on the indefinitive root" ); }

  { // Test running on root with 'a' in visited
    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();
    visited . insert ( ID::new ( "a" ));
    check_and_complete (
      tree, root_id, config, driver, &mut visited ). await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c) (code indefinitive)) c
        *** (skg (id d) (code (relToParent parentIgnores))) d
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Since ID 'a' was in 'visited', orgnode for 'a' should be marked 'indefinitive' with source, but children preserved." );
    assert_eq! (
      visited . len (), 1,
      "Visited should still contain only 'a'" );
    assert! (
      visited . contains ( & ID::new ( "a" )),
      "Visited should contain 'a'" );
  }

  { // Test running on second node (c)
    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let second_node_id : ego_tree::NodeId =
      tree . root ()
      . first_child () . unwrap ()
      . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, second_node_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (code indefinitive)) a
        ** (skg (id c) (source main) (code indefinitive)) c
        *** (skg (id d) (code (relToParent parentIgnores))) d
      " };
    assert_eq! (
      render_forest_to_org ( & forest ),
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
    |config, driver| Box::pin ( async move {
      test_visited_and_indefinitive_logic (
        config, driver ) . await
    } )) }

async fn test_visited_and_indefinitive_logic (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  // Test 1: indefinitive with non-indefinitive child
  {
    let input_org_text : &str =
      indoc! { "
        * (skg (id a) (code indefinitive)) a
        ** (skg (id c)) c
        *** (skg (id d) (code (relToParent parentIgnores))) d
      " };

    // Test with empty visited
    {
      let mut forest : Vec < Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let tree : &mut Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();

      check_and_complete (
        tree, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id a) (source main) (code indefinitive)) a
          ** (skg (id c)) c
          *** (skg (id d) (code (relToParent parentIgnores))) d
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with empty visited should fetch canonical info from disk" );
    }

    // Test with 'a' in visited - should mark as repeated
    {
      let mut forest : Vec < Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let tree : &mut Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();
      visited . insert ( ID::new ( "a" ));

      check_and_complete (
        tree, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id a) (source main) (code indefinitive)) a
          ** (skg (id c)) c
          *** (skg (id d) (code (relToParent parentIgnores))) d
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
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
      let mut forest : Vec < Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let tree : &mut Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();

      check_and_complete (
        tree, root_id, config, driver, &mut visited ) . await ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id d) (source main) (code indefinitive)) d
          ** (skg (id d)) d
          *** (skg (id d)) d
          **** (skg (id d)) d
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, expected_output,
        "Self-referential indefinitive root should fetch canonical info from disk" );
    }

    // Running from second node
    {
      let mut forest : Vec < Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let tree : &mut Tree < OrgNode > =
        & mut forest [ 0 ];
      let second_node_id : ego_tree::NodeId =
        tree . root ()
        . first_child () . unwrap ()
        . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();
      visited . insert ( ID::new ( "d" ));

      check_and_complete (
        tree, second_node_id, config, driver, &mut visited ) . await ?;

      let expected_output_from_second : &str =
        indoc! { "
          * (skg (id d) (code indefinitive)) d
          ** (skg (id d) (source main) (code indefinitive)) d
          *** (skg (id d)) d
          **** (skg (id d)) d
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
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
    |config, driver| Box::pin ( async move {
      test_visited_and_not_indefinitive_logic (
        config, driver ) . await
    } )) }

async fn test_visited_and_not_indefinitive_logic (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d) (code (relToParent parentIgnores))) d
    " };

  // Test with 'a' in visited
  {
    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();
    visited . insert ( ID::new ( "a" ));
    let visited_before : HashSet < ID > =
      visited . clone ();

    check_and_complete (
      tree, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (source main) (code indefinitive)) a
        ** (skg (id c)) c
        ** (skg (id d) (code (relToParent parentIgnores))) d
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Node 'a' in visited should become indefinitive with source, but children preserved" );
    assert_eq! (
      visited, visited_before,
      "Visited should be unchanged" );
  }

  // Test with empty visited
  {
    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (relToParent parentIgnores))) d
        ** (skg (id b) (source main)) b
        ** (skg (id c)) c
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Node 'a' not in visited should be completed from disk" );
    assert! (
      visited . contains ( & ID::new ( "a" )),
      "Visited should now contain 'a'" );
  }

  // Test with existing content that needs reordering
  {
    let input_with_existing_content : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id b)) b
        ** (skg (id d) (code (relToParent parentIgnores))) d
      " };

    let mut forest : Vec < Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_with_existing_content ) ?;
    let tree : &mut Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, root_id, config, driver, &mut visited ) . await ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (relToParent parentIgnores))) d
        ** (skg (id b)) b
        ** (skg (id c) (source main)) c
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
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
    |config, driver| Box::pin ( async move {
      test_false_content_logic (
        config, driver ) . await
    } )) }

async fn test_false_content_logic (
  config : &skg::types::SkgConfig,
  driver : &typedb_driver::TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d)) d
    " };

  let mut forest : Vec < Tree < OrgNode > > =
    org_to_uninterpreted_nodes ( input_org_text ) ?;
  let tree : &mut Tree < OrgNode > =
    & mut forest [ 0 ];
  let root_id : ego_tree::NodeId =
    tree . root () . id ();
  let mut visited : HashSet < ID > =
    HashSet::new ();

  check_and_complete (
    tree, root_id, config, driver, &mut visited ) . await ?;

  let expected_output : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id d) (code (relToParent parentIgnores))) d
      ** (skg (id b) (source main)) b
      ** (skg (id c)) c
    " };
  let output_org_text : String =
    render_forest_to_org ( & forest );
  assert_eq! (
    output_org_text, expected_output,
    "False content 'd' should be marked parentIgnores and moved first" );

  Ok (( )) }
