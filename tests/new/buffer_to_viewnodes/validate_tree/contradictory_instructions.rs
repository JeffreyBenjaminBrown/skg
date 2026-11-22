// cargo test contradictory_instructions

use indoc::indoc;
use skg::types::tree::forest::ViewForest;
use skg::types::misc::ID;
use skg::types::errors::BufferValidationError;
use skg::from_text::buffer_to_viewnodes::add_missing_info::IdResolution;
use skg::from_text::buffer_to_viewnodes::uninterpreted::org_to_viewforest;
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use skg::from_text::buffer_to_viewnodes::validate_tree::contradictory_instructions::{
  find_contradictory_processed_requests, find_inconsistent_instructions};
use skg::from_text::buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving_in_graph;
use skg::dbs::in_rust_graph::InRustGraph;
use skg::types::views_state::Perspective;
use skg::types::errors::HeadlineName;
use skg::types::config::{SkgConfig, Skgrepo};
use skg::types::misc::SkgrepoName;
use skg::types::viewnode::ViewnodeId;
use skg::diff_view::named_leaf_set::NamedLeafSet;
use std::collections::HashMap;
use std::path::PathBuf;
use skg::test_utils::run_with_test_stores;
use std::error::Error;

#[test]
fn test_find_inconsistent_toDelete_instructions() {
  // Test case with consistent toDelete instructions (no conflicts)
  let input_consistent: &str =
    indoc! {"
            * (skg node (gnid node1) (editRequest delete)) first node
            * (skg node (gnid node2)) second node
            * (skg node (gnid node1) (editRequest delete)) duplicate of first node (same delete instruction)
        "};

  let viewforest_consistent: ViewForest =
    org_to_viewforest (input_consistent, IdResolution::AsWritten) . unwrap() . 0;
  let (inconsistent_ids_consistent, _) =
    find_inconsistent_instructions (&viewforest_consistent);
  assert_eq!(inconsistent_ids_consistent . len(), 0, "Should have no inconsistent IDs when all nodes with same ID have same toDelete value");

  // Test case with inconsistent toDelete instructions (conflicts)
  let input_inconsistent: &str =
    indoc! {"
            * (skg node (gnid conflict1) (editRequest delete)) first conflicting node (toDelete=true)
            * (skg node (gnid normal)) normal node
            * (skg node (gnid conflict1)) duplicate node but toDelete=false
            * (skg node (gnid conflict2) (editRequest delete)) another conflict start
            * (skg node (gnid conflict2)) another conflict end
        "};

  let viewforest_inconsistent: ViewForest =
    org_to_viewforest (input_inconsistent, IdResolution::AsWritten) . unwrap() . 0;
  let (inconsistent_gnids, _) = find_inconsistent_instructions (&viewforest_inconsistent);

  assert_eq!(inconsistent_gnids . len(), 2, "Should find exactly 2 conflicting IDs");
  assert!(inconsistent_gnids . iter () . any ( |(gnid, _)| gnid == &ID::from ("conflict1")), "Should include conflict1 ID");
  assert!(inconsistent_gnids . iter () . any ( |(gnid, _)| gnid == &ID::from ("conflict2")), "Should include conflict2 ID");

  // Test case with nodes that have no IDs (should be skipped)
  let input_no_gnids: &str =
    indoc! {"
            * (skg node (editRequest delete)) node without id (should be skipped)
            * regular node without any metadata
            * (skg node (gnid valid_node)) only node with id
        "};

  let viewforest_no_gnids: ViewForest =
    org_to_viewforest (input_no_gnids, IdResolution::AsWritten) . unwrap() . 0;
  let (inconsistent_no_gnids, _) = find_inconsistent_instructions (&viewforest_no_gnids);
  assert_eq!(inconsistent_no_gnids . len(), 0, "Should have no conflicts when only one node has each ID");

  // Test empty viewforest (just BufferRoot, no tree roots)
  let empty_viewforest: ViewForest =
    ViewForest::new();
  let (inconsistent_empty, _) =
    find_inconsistent_instructions (&empty_viewforest);
  assert_eq!(inconsistent_empty . len(), 0,
             "Should have no conflicts in empty viewforest");
}

#[test]
fn test_multiple_defining_containers() -> Result<(), Box<dyn Error>> {
  run_with_test_stores(
    "skg-test-validate-multiple-def",
    "tests/merge/merge_nodes/fixtures",
    "/tmp/tantivy-test-validate-multiple-def",
    |config, _tantivy| Box::pin(async move {
      // Test input with multiple nodes having the same ID and write-protected=false
      let input_with_multiple_defining_containers: &str =
        indoc! {"
                * (skg node (gnid duplicate)) First defining container
                Regular node with shared ID
                * (skg node (gnid duplicate)) Second defining container
                Another regular node with the same ID
                * (skg node (gnid duplicate) writeProtected) Repeated node (not defining)
                This one is ok because writeProtected=true
                * (skg node (gnid duplicate) writeProtected) Might contain more (not defining)
                This one is also ok because writeProtected=true
                * (skg node (gnid unique)) Unique node
                This one is fine
            "};

      let viewforest: ViewForest =
        org_to_viewforest (input_with_multiple_defining_containers, IdResolution::AsWritten) . unwrap() . 0;
      let errors: Vec<BufferValidationError> =
        find_buffer_errors_for_saving(&viewforest, config)?;

      let multiple_defining_errors: Vec<&BufferValidationError> = errors . iter()
        . filter(|e| matches!(e, BufferValidationError::Multiple_Defining_Viewnodes (_, _)))
        . collect();

      assert_eq!(multiple_defining_errors . len(), 1,
                 "Should find exactly 1 Multiple_Defining_Viewnodes error for the problematic ID");

      // Check that the error points to the correct ID
      if let BufferValidationError::Multiple_Defining_Viewnodes (gnid, _) = multiple_defining_errors[0] {
        assert_eq!(gnid . 0, "duplicate",
                   "Multiple_Defining_Viewnodes error should come from the duplicate ID"); }
      Ok(( )) } )) }

/// The contradictions 'find_contradictory_processed_requests' finds
/// in 'buffer' over an empty graph (each ID resolves to itself): each
/// leaf described, with the vnids of the headlines naming it.
fn processed_contradictions_in (
  buffer : &str,
) -> Vec<(String, Vec<String>)> {
  let viewforest : ViewForest =
    org_to_viewforest (buffer, IdResolution::AsWritten) . unwrap () . 0;
  find_contradictory_processed_requests (
    &viewforest, &InRustGraph::from_graphnodes (&[]) )
    . into_iter ()
    . map ( |(leaf, headlines)| (
      leaf . describe (),
      headlines . iter ()
        . map ( |headline : &HeadlineName|
                headline . viewnode_id . as_ref () . unwrap () . 0 . clone () )
        . collect () ) )
    . collect () }

#[test]
fn two_headlines_naming_one_leaf_processed_and_unprocessed_contradict () {
  assert_eq! (
    processed_contradictions_in ( indoc! {"
      * (skg (vnid p1) node (gnid p)) p
      ** (skg (vnid x1) node (gnid x) (editRequest (processed node))) x
      * (skg (vnid x2) node (gnid x) writeProtected (editRequest (unprocessed node))) x
      "} ),
    vec! [ ( "every change node x records but the relationships shown on its headline or under it" . to_string (),
             vec! [ "x1" . to_string (), "x2" . to_string () ] ) ] ); }

/// 'node' names the relationships its node records that no headline
/// under it shows: x1 shows no member, so its 'node' names x's
/// containment of c, which c1, under x2, marks the other way.
#[test]
fn a_node_request_contradicts_an_opposite_request_on_a_relationship_not_shown_under_it () {
  assert_eq! (
    processed_contradictions_in ( indoc! {"
      * (skg (vnid x1) node (gnid x) (editRequest (unprocessed node))) x
      * (skg (vnid x2) node (gnid x) writeProtected) x
      ** (skg (vnid c1) node (gnid c) writeProtected (editRequest (processed rel))) c
      "} ),
    vec! [ ( "the existence of the relationship x contains c" . to_string (),
             vec! [ "x1" . to_string (), "c1" . to_string () ] ) ] ); }

/// Where the member is shown under it, 'node' leaves its relationship
/// to the member's 'rel', and the two requests name different leaves.
#[test]
fn a_node_request_leaves_the_relationship_of_a_member_under_it_to_that_member () {
  assert! ( processed_contradictions_in ( indoc! {"
      * (skg (vnid x1) node (gnid x) (editRequest (unprocessed node))) x
      ** (skg (vnid c1) node (gnid c) (editRequest (processed rel))) c
      "} ) . is_empty () ); }

/// 'node' on a changeset marker names own leaves of the content
/// members drawn beside it: it contradicts an opposite 'node' on c,
/// one of them, and not one on y, which is no member of p.
#[test]
fn a_markers_node_request_contradicts_only_requests_on_its_members () {
  assert_eq! (
    processed_contradictions_in ( indoc! {"
      * (skg (vnid p1) node (gnid p)) p
      ** (skg (vnid m1) (changesetMarker content) (editRequest (processed node)))
      ** (skg (vnid c1) node (gnid c) (editRequest (unprocessed node))) c
      * (skg (vnid y1) node (gnid y) (editRequest (unprocessed node))) y
      "} ),
    vec! [ ( "every change node c records but the relationships shown on its headline or under it" . to_string (),
             vec! [ "m1" . to_string (), "c1" . to_string () ] ) ] ); }

#[test]
fn processed_requests_that_agree_or_name_different_leaves_pass () {
  assert! ( processed_contradictions_in ( indoc! {"
      * (skg (vnid p1) node (gnid p)) p
      ** (skg (vnid x1) node (gnid x) (editRequest (processed node))) x
      * (skg (vnid x2) node (gnid x) writeProtected (editRequest (processed))) x
      "} ) . is_empty (),
    "two requests that agree" );
  assert! ( processed_contradictions_in ( indoc! {"
      * (skg (vnid p1) node (gnid p)) p
      ** (skg (vnid x1) node (gnid x) (editRequest (processed node))) x
      * (skg (vnid x2) node (gnid x) writeProtected (editRequest (unprocessed rel))) x
      "} ) . is_empty (),
    "a node's existence and its relationship to its viewparent are \
     different leaves" ); }

#[test]
fn validation_refuses_contradictory_processed_requests () -> Result<(), Box<dyn Error>> {
  let viewforest : ViewForest = org_to_viewforest ( indoc! {"
      * (skg (vnid x1) node (gnid x) (skgRepos (nodeRepo main)) (editRequest (processed node))) x
      * (skg (vnid x2) node (gnid x) (skgRepos (nodeRepo main)) writeProtected (editRequest (unprocessed node))) x
      "}, IdResolution::AsWritten ) . unwrap () . 0;
  let config : SkgConfig = SkgConfig::dummyFromSkgrepos ( HashMap::from ([
    ( SkgrepoName::from ("main"),
      Skgrepo { name : SkgrepoName::from ("main"), abbreviation : None,
                path : PathBuf::from ("/tmp/main"), owned : true } ) ]));
  let errors : Vec<BufferValidationError> = find_buffer_errors_for_saving_in_graph (
    &viewforest, &InRustGraph::from_graphnodes (&[]), &config,
    Perspective::RootHomes ) ?;
  let refusals : Vec<&BufferValidationError> = errors . iter ()
    . filter ( |e| matches! ( e, BufferValidationError::ContradictoryProcessedRequests (..) ))
    . collect ();
  assert_eq! ( refusals . len (), 1, "errors: {:?}", errors );
  assert! ( matches! (
              refusals [0],
              BufferValidationError::ContradictoryProcessedRequests (
                NamedLeafSet::OwnLeavesOf ( pid, _ ), _ )
              if *pid == ID::from ("x") ),
            "the refusal keeps the set it found contradictory: {:?}", refusals [0] );
  assert_eq! ( refusals [0] . named_headlines () . iter ()
               . map ( |headline| headline . viewnode_id . clone () )
               . collect::<Vec<Option<ViewnodeId>>> (),
               vec! [ Some ( ViewnodeId ("x1" . into ()) ),
                      Some ( ViewnodeId ("x2" . into ()) ) ] );
  Ok (( )) }
