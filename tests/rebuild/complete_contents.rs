// cargo test --test rebuild -- --nocapture

use indoc::indoc;
use std::collections::HashSet;
use std::error::Error;

use skg::rebuild::completeContents;
use skg::save::buffer_to_orgnodes::org_to_uninterpreted_nodes;
use skg::test_utils::run_with_test_db;
use skg::types::{ID, OrgNode};
use skg::mk_org_text::content_view::render_forest_to_org;

#[test]
fn test_mightContainMore_identity_at_multiple_levels
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-mightcontainmore",
    "tests/rebuild/complete_contents/fixtures",
    "/tmp/tantivy-test-complete-contents-mightcontainmore",
    |config, _driver| Box::pin ( async move {
      test_mightContainMore_identity_at_multiple_levels_logic (
        config ) . await
    } )) }

async fn test_mightContainMore_identity_at_multiple_levels_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a) mightContainMore) a
      ** (skg (id c) mightContainMore) c
      *** (skg (id d) (treatment parentIgnores)) d
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

    completeContents (
      tree, root_id, config, &mut visited ) ?;

    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, input_org_text,
      "Running on root with empty visited should preserve tree" );
    assert! (
      visited . is_empty (),
      "Visited should remain empty" );
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

    completeContents (
      tree, root_id, config, &mut visited ) ?;

    let output_org_text : String =
      render_forest_to_org ( & forest );
    assert_eq! (
      output_org_text, input_org_text,
      "Running on root with 'a' in visited should preserve tree" );
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

    completeContents (
      tree, second_node_id, config, &mut visited ) ?;

    assert_eq! (
      render_forest_to_org ( & forest ),
      input_org_text,
      "Running on second node should preserve tree" );
  }

  Ok (( )) }

#[test]
fn test_visited_and_mightContainMore
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-mightcontainmore",
    "tests/rebuild/complete_contents/fixtures",
    "/tmp/tantivy-test-complete-contents-visited-mightcontainmore",
    |config, _driver| Box::pin ( async move {
      test_visited_and_mightContainMore_logic (
        config ) . await
    } )) }

async fn test_visited_and_mightContainMore_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  // Test 1: mightContainMore with non-mightContainMore child
  {
    let input_org_text : &str =
      indoc! { "
        * (skg (id a) mightContainMore) a
        ** (skg (id c)) c
        *** (skg (id d) (treatment parentIgnores)) d
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

      completeContents (
        tree, root_id, config, &mut visited ) ?;

      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, input_org_text,
        "mightContainMore root with empty visited should preserve tree" );
    }

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

      completeContents (
        tree, root_id, config, &mut visited ) ?;

      let output_org_text : String =
        render_forest_to_org ( & forest );
      assert_eq! (
        output_org_text, input_org_text,
        "mightContainMore root with 'a' in visited should preserve tree" );
    }
  }

  // Test 2: self-referential tree
  {
    let input_org_text_self_ref : &str =
      indoc! { "
        * (skg (id d) mightContainMore) d
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

      completeContents (
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

      completeContents (
        tree, second_node_id, config, &mut visited ) ?;

      let expected_output_from_second : &str =
        indoc! { "
          * (skg (id d) mightContainMore) d
          ** (skg (id d) repeated) d
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
fn test_visited_and_not_mightContainMore
  () -> Result < (), Box<dyn Error> > {
  run_with_test_db (
    "skg-test-complete-contents-visited-not-mightcontainmore",
    "tests/rebuild/complete_contents/fixtures",
    "/tmp/tantivy-test-complete-contents-visited-not-mightcontainmore",
    |config, _driver| Box::pin ( async move {
      test_visited_and_not_mightContainMore_logic (
        config ) . await
    } )) }

async fn test_visited_and_not_mightContainMore_logic (
  config : &skg::types::SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let input_org_text : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id c)) c
      ** (skg (id d) (treatment parentIgnores)) d
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

    completeContents (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a) repeated) a
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

    completeContents (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (treatment parentIgnores)) d
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
        ** (skg (id d) (treatment parentIgnores)) d
      " };

    let mut forest : Vec < ego_tree::Tree < OrgNode > > =
      org_to_uninterpreted_nodes ( input_with_existing_content ) ?;
    let tree : &mut ego_tree::Tree < OrgNode > =
      & mut forest [ 0 ];
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    let mut visited : HashSet < ID > =
      HashSet::new ();

    completeContents (
      tree, root_id, config, &mut visited ) ?;

    let expected_output : &str =
      indoc! { "
        * (skg (id a)) a
        ** (skg (id d) (treatment parentIgnores)) d
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

  completeContents (
    tree, root_id, config, &mut visited ) ?;

  let expected_output : &str =
    indoc! { "
      * (skg (id a)) a
      ** (skg (id d) (treatment parentIgnores)) d
      ** (skg (id b)) b
      ** (skg (id c)) c
    " };
  let output_org_text : String =
    render_forest_to_org ( & forest );
  assert_eq! (
    output_org_text, expected_output,
    "False content 'd' should be marked parentIgnores and moved first" );

  Ok (( )) }
