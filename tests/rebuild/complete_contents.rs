// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::collections::HashSet;
use std::error::Error;

use skg::rebuild::{completeContents, check_and_mark_repetition};
use skg::save::buffer_to_orgnodes::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::{ID, OrgNode, SkgConfig};
use skg::mk_org_text::content_view::render_forest_to_org;
use ego_tree::{Tree, NodeId};

/// Helper to call check_and_mark_repetition followed by completeContents
/// (matches the pattern used in complete_node_preorder)
fn check_and_complete (
  tree    : &mut Tree < OrgNode >,
  node_id : NodeId,
  config  : &SkgConfig,
  visited : &mut HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let is_repeat : bool =
    check_and_mark_repetition (
      tree, node_id, config, visited ) ?;
  if ! is_repeat {
    completeContents (
      tree, node_id, config ) ?; }
  Ok (( )) }

#[test]
fn test_indefinitive_identity_at_multiple_levels
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-indefinitive",
    "tests/rebuild/complete_contents/fixtures",
    "/tmp/tantivy-test-complete-contents-indefinitive",
    |config, _driver| Box::pin ( async move {
      test_indefinitive_identity_at_multiple_levels_logic (
        config ) . await
    } )) }

async fn test_indefinitive_identity_at_multiple_levels_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a) (code indefinitive)) a
      ** (skg (id c) (code indefinitive)) c
      *** (skg (id d) (code (relToParent parentIgnores))) d
    " };

  { // Test running on root with empty visited
    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, root_id, config, &mut visited ) ?;

    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, input_org_text,
      "Running on root with empty visited should preserve tree" );
    assert_eq! (
      visited . len (), 1,
      "Visited should contain 'a'" );
  }

  { // Test running on root with 'a' in visited
    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();
    visited . insert ( ID::new ( "a" ));

    check_and_complete (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (view repeated) (code indefinitive)) a
        Repeated, probably above. Edit there, not here.
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Running on root with 'a' in visited should mark as repeated" );
    assert_eq! (
      visited . len (), 1,
      "Visited should still contain only 'a'" );
    assert! (
      visited . contains ( & ID::new ( "a" )),
      "Visited should contain 'a'" );
  }

  { // Test running on second node (c)
    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let second_node_id : ego_tree::NodeId =
      tree . root ()
      . first_child () . unwrap ()
      . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, second_node_id, config, &mut visited ) ?;

    assert_eq! (
      render_forest_to_org ( & forest ),
      input_org_text,
      "Running on second node should preserve tree" );
  }

  Ok (( )) }

#[test]
fn test_visited_and_indefinitive
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-indefinitive",
    "tests/rebuild/complete_contents/fixtures",
    "/tmp/tantivy-test-complete-contents-visited-indefinitive",
    |config, _driver| Box::pin ( async move {
      test_visited_and_indefinitive_logic (
        config ) . await
    } )) }

async fn test_visited_and_indefinitive_logic (
  config : &skg::types::SkgConfig,
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
      let mut forest : Vec < ego_tree::Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let tree : &mut ego_tree::Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();

      check_and_complete (
        tree, root_id, config, &mut visited ) ?;

      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, input_org_text,
        "indefinitive root with empty visited should preserve tree" );
    }

    // Test with 'a' in visited - should mark as repeated
    {
      let mut forest : Vec < ego_tree::Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text ) ?;
      let tree : &mut ego_tree::Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();
      visited . insert ( ID::new ( "a" ));

      check_and_complete (
        tree, root_id, config, &mut visited ) ?;

      let expected_output : &str =
        indoc! { "
          * (skg (id a) (view repeated) (code indefinitive)) a
          Repeated, probably above. Edit there, not here.
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, expected_output,
        "indefinitive root with 'a' in visited should mark as repeated" );
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
      let mut forest : Vec < ego_tree::Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let tree : &mut ego_tree::Tree < OrgNode > =
        & mut forest [ 0 ];
      let root_id : ego_tree::NodeId =
        tree . root () . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();

      check_and_complete (
        tree, root_id, config, &mut visited ) ?;

      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, input_org_text_self_ref,
        "Self-referential tree from root should be unchanged" );
    }

    // Running from second node
    {
      let mut forest : Vec < ego_tree::Tree < OrgNode > > =
        org_to_uninterpreted_nodes ( input_org_text_self_ref ) ?;
      let tree : &mut ego_tree::Tree < OrgNode > =
        & mut forest [ 0 ];
      let second_node_id : ego_tree::NodeId =
        tree . root ()
        . first_child () . unwrap ()
        . id ();
      let mut visited : HashSet < ID > =
        HashSet::new ();
      visited . insert ( ID::new ( "d" ));

      check_and_complete (
        tree, second_node_id, config, &mut visited ) ?;

      let expected_output_from_second : &str =
        indoc! { "
          * (skg (id d) (code indefinitive)) d
          ** (skg (id d) (view repeated) (code indefinitive)) d
          Repeated, probably above. Edit there, not here.
        " };
      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, expected_output_from_second,
        "Self-referential second node should become repeated" );
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
    |config, _driver| Box::pin ( async move {
      test_visited_and_not_indefinitive_logic (
        config ) . await
    } )) }

async fn test_visited_and_not_indefinitive_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d) (code (relToParent parentIgnores))) d
    " };

  // Test with 'a' in visited
  {
    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();
    visited . insert ( ID::new ( "a" ));
    let visited_before : HashSet < ID > =
      visited . clone ();

    check_and_complete (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) (view repeated) (code indefinitive)) a
        Repeated, probably above. Edit there, not here.
      " };
    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, expected_output,
      "Node 'a' in visited should become repeated" );
    assert_eq! (
      visited, visited_before,
      "Visited should be unchanged" );
  }

  // Test with empty visited
  {
    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_org_text ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (relToParent parentIgnores))) d
        ** (skg (id b)) b
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

    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_with_existing_content ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    check_and_complete (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (code (relToParent parentIgnores))) d
        ** (skg (id b)) b
        ** (skg (id c)) c
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
    |config, _driver| Box::pin ( async move {
      test_false_content_logic (
        config ) . await
    } )) }

async fn test_false_content_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d)) d
    " };

  let mut forest : Vec < ego_tree::Tree < OrgNode > > =
    org_to_uninterpreted_nodes ( input_org_text ) ?;
  let tree : &mut ego_tree::Tree < OrgNode > =
    & mut forest [ 0 ];
  let root_id : ego_tree::NodeId =
    tree . root () . id ();
  let mut visited : HashSet < ID > =
    HashSet::new ();

  check_and_complete (
    tree, root_id, config, &mut visited ) ?;

  let expected_output : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id d) (code (relToParent parentIgnores))) d
      ** (skg (id b)) b
      ** (skg (id c)) c
    " };
  let output_org_text : String =
    render_forest_to_org ( & forest );
  assert_eq! (
    output_org_text, expected_output,
    "False content 'd' should be marked parentIgnores and moved first" );

  Ok (( )) }
