// cargo test merge::merge_nodes

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_viewforest;
use skg::nodeMerge::merge_nodes;
use skg::test_utils::{run_with_shared_test_stores, tantivy_contains_id, graph_handle_from_config, audit_inrustgraph_or_panic};
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex, SourceName};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{NodeEditRequest, ViewNode, ViewNodeKind, Vognode, ActiveNode, IndefOrDef, viewforest_root_viewnode, default_activeNode};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::NodeMerge;
use skg::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use skg::util::path_from_pid_and_source;
use skg::dbs::in_rust_graph::query::find_related_nodes;

use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use std::path::Path;

fn mk_test_viewnode (
  title        : &str,
  id           : &str,
  edit_request : Option<NodeEditRequest>,
) -> ViewNode {
  let t : ActiveNode = ActiveNode {
    indef_or_def : IndefOrDef::Definitive {
      body         : None,
      edit_request },
    .. default_activeNode ( ID::from (id),
                          SourceName::from ("main"),
                          title . to_string() ) };
  ViewNode { focused     : false,
            folded      : false,
            body_folded : false,
            kind        : ViewNodeKind::Vognode (Vognode::Active (t)) }}

#[test]
fn all_tests
  () -> Result<(), Box<dyn Error>> {
  let fixtures : &str =
    "tests/merge/merge_nodes/fixtures";
  run_with_shared_test_stores (
    "skg-test-merge-nodes",
    |s| Box::pin ( async move {
      s . reset ("test_merge_2_into_1", fixtures) ?;
      test_merge_2_into_1 (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_merge_1_into_2", fixtures) ?;
      test_merge_1_into_2 (
        &s . config, &mut s . tantivy ) . await ?;
      s . reset ("test_inrustgraph_queries_resolve_aliases_after_merge", fixtures) ?;
      test_inrustgraph_queries_resolve_aliases_after_merge (
        &s . config, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_merge_2_into_1 (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_merge_2_into_1_impl(config, tantivy) . await?;
      Ok(( ))
    }

async fn test_merge_2_into_1_impl(
  config : &SkgConfig,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create viewnode viewforest with node 1 requesting to merge node 2
  let view_node_1 = mk_test_viewnode("1", "1", Some(NodeEditRequest::NodeMerge(ID::from ("2"))));
  let mut viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  viewforest . root_mut() . append (view_node_1);

  // Generate NodeMerge from merge request
  let nodeMerge_instructions: Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest(
      &ViewForest::from_internal_tree (viewforest),
      &graph_handle_from_config (config)? . load_full (),
      config,
  ) ?;

  // Expect 1 NodeMerge (containing 3 DefineNodes)
  assert_eq!(nodeMerge_instructions . len(),
             1,
             "Should have 1 NodeMerge");

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  merge_nodes(
    &nodeMerge_instructions,
    config . clone(),
    tantivy,
    &graph,
    &skg::types::env::new_mutation_gate (),
  ) . await ?;

  // Verify results
  verify_filesystem_after_merge_2_into_1(
    config, &nodeMerge_instructions)?;
  verify_tantivy_after_merge_2_into_1(
    tantivy, &nodeMerge_instructions )?;
  audit_inrustgraph_or_panic (&graph)?;
  Ok(( )) }


fn verify_filesystem_after_merge_2_into_1(
  config: &SkgConfig,
  nodeMerge_instructions: &[NodeMerge],
) -> Result<(), Box<dyn Error>> {
  let node_2_path: String =
    path_from_pid_and_source ( config,
                               &SourceName::from ("main"),
                               ID::from ("2")) ?;
  assert!( !Path::new (&node_2_path) . exists(),
            "2.skg should be deleted" );

  // Node 1's file should be updated
  let node_1: NodeComplete = nodecomplete_from_pid_and_source(
    config, ID::from ("1"), &SourceName::from ("main") )?;
  assert_eq!(&node_1 . pid, &ID::from ("1"));
  assert_eq!(node_1 . extra_ids . len(), 2, "Node 1 should have 2 extra_ids");
  assert_eq!(&node_1 . extra_ids[0], &ID::from ("2"));
  assert_eq!(&node_1 . extra_ids[1], &ID::from ("2-extra-id"));

  // Should have [acquiree_text_preserver_id, 11, 12, overlap, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // Note: "overlap" should appear only once (deduplicated) even though it was in both nodes
  assert_eq!( node_1 . contains . len(), 7,
              "Node 1 should contain 7 items (with overlap deduplicated)");

  let acquiree_text_preserver_id: &ID =
    &nodeMerge_instructions[0] . targets_from_nodeMerge() . 0 . pid;
  assert_eq!(&node_1 . contains[0] . member, acquiree_text_preserver_id,
             "First content should be acquiree_text_preserver");
  assert_eq!(&node_1 . contains[1] . member, &ID::from ("11"));
  assert_eq!(&node_1 . contains[2] . member, &ID::from ("12"));
  assert_eq!(&node_1 . contains[3] . member, &ID::from ("overlap"),
             "overlap should appear in position 3 (from acquirer's original contents)");
  assert_eq!(&node_1 . contains[4] . member, &ID::from ("21"));
  assert_eq!(&node_1 . contains[5] . member, &ID::from ("22"));
  assert_eq!(
    &node_1 . contains[6] . member,
    &ID::from(
      "hidden-from-subscriptions-of-1-but-in-content-of-2"));
  // Note that the second 'overlap' was stripped.

  // Verify overlap appears only once (not duplicated)
  let overlap_count: usize = node_1 . contains
    . iter()
    . filter(|m| m . member == ID::from ("overlap"))
    . count();
  assert_eq!(overlap_count, 1,
             "overlap should appear exactly once (deduplicated), not twice");

  // Verify that node 1 keeps its original relationships
  // (node 2 had no relationships to transfer)
  assert_eq!(node_1 . subscribes_to . or_default() . len(), 1,
             "Node 1 should keep its subscribes_to");
  assert_eq!(&node_1 . subscribes_to . or_default()[0] . member,
             &ID::from ("1-subscribes-to"));

  // Note: node 1 originally hid both "hidden-from-1s-subscriptions"
  // and "hidden-from-subscriptions-of-1-but-in-content-of-2".
  // The latter is now in node 1's contents, so it should be removed from hides.
  assert_eq!(node_1 . hides_from_its_subscriptions . or_default() . len(),
             1,
             "Node 1 should have 1 hides relationship");
  assert_eq!(&node_1 . hides_from_its_subscriptions . or_default()[0] . member,
             &ID::from ("hidden-from-1s-subscriptions"));

  assert_eq!(node_1 . overrides_view_of . or_default() . len(), 1,
             "Node 1 should keep its overrides_view_of");
  assert_eq!(&node_1 . overrides_view_of . or_default()[0] . member,
             &ID::from ("overridden-by-1"));

  let acquiree_text_preserver_path: String =
    path_from_pid_and_source ( config,
                               &SourceName::from ("main"),
                               acquiree_text_preserver_id . clone() ) ?;
  assert!( Path::new (&acquiree_text_preserver_path) . exists(),
           "acquiree_text_preserver file should exist" );

  let acquiree_text_preserver: NodeComplete =
    nodecomplete_from_pid_and_source( config,
                                 acquiree_text_preserver_id . clone(),
                                 &SourceName::from ("main") )?;
  assert!(acquiree_text_preserver . title . starts_with ("MERGED: "));
  assert_eq!(acquiree_text_preserver . title, "MERGED: 2");
  assert_eq!(acquiree_text_preserver . body, Some("2 body" . to_string()));
  assert!(acquiree_text_preserver . contains . is_empty(),
             "acquiree_text_preserver should have no contents (empty when empty on disk)");

  // acquiree_text_preserver should have Unspecified for relationship fields
  // (when read from disk, missing fields are Unspecified)
  assert_eq!(acquiree_text_preserver . subscribes_to, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for subscribes_to");
  assert_eq!(acquiree_text_preserver . hides_from_its_subscriptions, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for hides_from_its_subscriptions");
  assert_eq!(acquiree_text_preserver . overrides_view_of, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_2_into_1(
  tantivy_index: &TantivyIndex,
  nodeMerge_instructions: &[NodeMerge],
) -> Result<(), Box<dyn Error>> {

  // Search for node 2 - should NOT find it (it was merged and deleted)
  let found_node_2: bool =
    tantivy_contains_id(tantivy_index, "2", "2")?;
  assert!(!found_node_2,
          "Node 2 should NOT be in Tantivy index after being merged");

  // Search for node 1 - SHOULD find it (it's the acquirer)
  let found_node_1: bool =
    tantivy_contains_id(tantivy_index, "1", "1")?;
  assert!(found_node_1,
          "Node 1 SHOULD be in Tantivy index after merge");

  // Search for acquiree_text_preserver - SHOULD find it
  let acquiree_text_preserver_id: &ID =
    &nodeMerge_instructions[0] . targets_from_nodeMerge() . 0 . pid;
  let found_acquiree_text_preserver: bool =
    tantivy_contains_id(tantivy_index, "MERGED: 2", &acquiree_text_preserver_id . 0)?;
  assert!(found_acquiree_text_preserver, "acquiree_text_preserver SHOULD be in Tantivy index");
  Ok (( )) }


// ============================================================
// Test: Merging 1 into 2
// ============================================================

async fn test_merge_1_into_2 (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_merge_1_into_2_impl(config, tantivy) . await?;
      Ok(( ))
    }

async fn test_merge_1_into_2_impl(
  config : &SkgConfig,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create viewnode viewforest with node 2 requesting to merge node 1
  let view_node_2 = mk_test_viewnode("2", "2", Some(NodeEditRequest::NodeMerge(ID::from ("1"))));
  let mut viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  viewforest . root_mut() . append (view_node_2);

  // Generate NodeMerge from merge request
  let nodeMerge_instructions: Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest(
      &ViewForest::from_internal_tree (viewforest),
      &graph_handle_from_config (config)? . load_full (),
      config,
  ) ?;

  // Expect 1 NodeMerge (containing 3 DefineNodes)
  assert_eq!(nodeMerge_instructions . len(),
             1,
             "Should have 1 NodeMerge");

  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  merge_nodes(
    &nodeMerge_instructions,
    config . clone(),
    tantivy,
    &graph,
    &skg::types::env::new_mutation_gate (),
  ) . await?;

  // Verify results
  verify_filesystem_after_merge_1_into_2(
    config, &nodeMerge_instructions)?;
  verify_tantivy_after_merge_1_into_2(
    tantivy, &nodeMerge_instructions)?;
  audit_inrustgraph_or_panic (&graph)?;
  Ok(( )) }


fn verify_filesystem_after_merge_1_into_2(
  config: &SkgConfig,
  nodeMerge_instructions: &[NodeMerge],
) -> Result<(), Box<dyn Error>> {
  let node_1_path: String =
    path_from_pid_and_source ( config,
                               &SourceName::from ("main"),
                               ID::from ("1")) ?;
  assert!( !Path::new (&node_1_path) . exists(),
            "1.skg should be deleted" );

  // Node 2's file should be updated
  let node_2: NodeComplete = nodecomplete_from_pid_and_source(
    config, ID::from ("2"), &SourceName::from ("main") )?;

  // Should have pid=2, extra_ids=[2-extra-id, 1]
  assert_eq!(&node_2 . pid, &ID::from ("2"));
  assert_eq!(node_2 . extra_ids . len(), 2, "Node 2 should have 2 extra_ids");
  assert_eq!(&node_2 . extra_ids[0], &ID::from ("2-extra-id"));
  assert_eq!(&node_2 . extra_ids[1], &ID::from ("1"));

  // Should have [acquiree_text_preserver_id, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2, overlap, 11, 12]
  // Note: "overlap" should appear only once (deduplicated) even though it was in both nodes
  assert_eq!( node_2 . contains . len(), 7,
              "Node 2 should contain 7 items (with overlap deduplicated)");
  let acquiree_text_preserver_id: &ID =
    &nodeMerge_instructions[0] . targets_from_nodeMerge() . 0 . pid;
  assert_eq!(&node_2 . contains[0] . member, acquiree_text_preserver_id,
             "First content should be acquiree_text_preserver");
  assert_eq!(&node_2 . contains[1] . member, &ID::from ("21"));
  assert_eq!(&node_2 . contains[2] . member, &ID::from ("22"));
  assert_eq!(&node_2 . contains[3] . member, &ID::from(
    "hidden-from-subscriptions-of-1-but-in-content-of-2"));
  assert_eq!(&node_2 . contains[4] . member, &ID::from ("overlap"),
             "overlap should appear in position 4 (from acquirer's original contents)");
  assert_eq!(&node_2 . contains[5] . member, &ID::from ("11"));
  assert_eq!(&node_2 . contains[6] . member, &ID::from ("12"));

  // Verify overlap appears only once (not duplicated)
  let overlap_count: usize = node_2 . contains
    . iter()
    . filter(|m| m . member == ID::from ("overlap"))
    . count();
  assert_eq!(overlap_count, 1,
             "overlap should appear exactly once (deduplicated), not twice");

  // Verify subscribes_to: should have node 1's subscribes_to transferred
  assert_eq!(node_2 . subscribes_to . or_default() . len(), 1,
             "Node 2 should have 1 subscribes_to relationship");
  assert_eq!(&node_2 . subscribes_to . or_default()[0] . member,
             &ID::from ("1-subscribes-to"),
             "Node 2 should subscribe to 1-subscribes-to");

  // Verify hides_from_its_subscriptions: should have node 1's hides,
  // but NOT "hidden-from-subscriptions-of-1-but-in-content-of-2"
  // (because it's in node 2's contents)
  assert_eq!(node_2 . hides_from_its_subscriptions . or_default() . len(),
             1,
             "Node 2 should have 1 hides_from_its_subscriptions relationship");
  assert_eq!(&node_2 . hides_from_its_subscriptions . or_default()[0] . member,
             &ID::from ("hidden-from-1s-subscriptions"),
             "Node 2 should hide hidden-from-1s-subscriptions");

  // Verify overrides_view_of: should have node 1's overrides_view_of transferred
  assert_eq!(node_2 . overrides_view_of . or_default() . len(), 1,
             "Node 2 should have 1 overrides_view_of relationship");
  assert_eq!(&node_2 . overrides_view_of . or_default()[0] . member,
             &ID::from ("overridden-by-1"),
             "Node 2 should override view of overridden-by-1");

  let acquiree_text_preserver_path: String =
    path_from_pid_and_source( config,
                              &SourceName::from ("main"),
                              acquiree_text_preserver_id . clone() ) ?;
  assert!( Path::new (&acquiree_text_preserver_path) . exists(),
           "acquiree_text_preserver file should exist" );

  let acquiree_text_preserver: NodeComplete =
    nodecomplete_from_pid_and_source( config,
                                 acquiree_text_preserver_id . clone(),
                                 &SourceName::from ("main") )?;
  assert!(acquiree_text_preserver . title . starts_with ("MERGED: "));
  assert_eq!(acquiree_text_preserver . title, "MERGED: 1");
  assert_eq!(acquiree_text_preserver . body,
             Some ( "[[id:1-links-to][a link to 1-links-to]]"
                       . to_string() ));
  assert!(acquiree_text_preserver . contains . is_empty(),
             "acquiree_text_preserver should have no contents (empty when empty on disk)");

  // acquiree_text_preserver should have Unspecified for relationship fields
  // (when read from disk, missing fields are Unspecified)
  // (these relationships stay with the acquirer, not the acquiree_text_preserver)
  assert_eq!(acquiree_text_preserver . subscribes_to, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for subscribes_to");
  assert_eq!(acquiree_text_preserver . hides_from_its_subscriptions, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for hides_from_its_subscriptions");
  assert_eq!(acquiree_text_preserver . overrides_view_of, MSV::Unspecified,
             "acquiree_text_preserver should have Unspecified for overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_1_into_2(
  tantivy_index: &TantivyIndex,
  nodeMerge_instructions: &[NodeMerge],
) -> Result<(), Box<dyn Error>> {

  // Search for node 1 - should NOT find it (it was merged and deleted)
  let found_node_1: bool = tantivy_contains_id(
    tantivy_index, "1", "1" )?;
  assert!(!found_node_1,
          "Node 1 should NOT be in Tantivy index after being merged");

  // Search for node 2 - SHOULD find it (it's the acquirer)
  let found_node_2: bool = tantivy_contains_id(
    tantivy_index, "2", "2")?;
  assert!(found_node_2,
          "Node 2 SHOULD be in Tantivy index after merge");

  // Search for acquiree_text_preserver - SHOULD find it
  let acquiree_text_preserver_id: &ID =
    &nodeMerge_instructions[0] . targets_from_nodeMerge() . 0 . pid;
  let found_acquiree_text_preserver: bool = tantivy_contains_id(
    tantivy_index, "MERGED: 1", &acquiree_text_preserver_id . 0 )?;
  assert!(found_acquiree_text_preserver, "acquiree_text_preserver SHOULD be in Tantivy index");

  Ok (( )) }

// ============================================================
// Test: in-Rust graph queries resolve extra_ids after a merge
// ============================================================
//
// After merging 1 into 2, neighbors' raw references to "1" should
// surface as canonical "2" when queried via the in-Rust graph path.
// Before the canonical-keyed-inverse + forward-resolve-on-read
// changes, inverse queries under-reported (raw-keyed) and forward
// queries returned raw IDs; the same public function's old backend
// fallback has always returned canonical pids. This test locks in
// the in-Rust graph path's canonicalized behavior.
async fn test_inrustgraph_queries_resolve_aliases_after_merge (
  config : &SkgConfig,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
        test_inrustgraph_queries_resolve_aliases_after_merge_impl (
          config, tantivy ) . await }

async fn test_inrustgraph_queries_resolve_aliases_after_merge_impl (
  config : &SkgConfig,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // NodeMerge 1 into 2. Acquirer=2, acquiree=1.
  let view_node_2 =
    mk_test_viewnode ("2", "2",
                      Some (NodeEditRequest::NodeMerge (ID::from ("1"))));
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  viewforest . root_mut () . append (view_node_2);
  let nodeMerge_instructions : Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest (
      &ViewForest::from_internal_tree (viewforest),
      &graph_handle_from_config (config)? . load_full (),
      config )?;
  let graph : InRustGraphHandle =
    graph_handle_from_config (config) ?;
  merge_nodes (
    &nodeMerge_instructions, config . clone (),
    tantivy, &graph,
    &skg::types::env::new_mutation_gate () ) . await?;

  let snap = graph . load_full ();
  let input_acquirer : Vec<ID> = vec![ID::from ("2")];

  // === Inverse queries: "who points at pid 2?" ===

  let subscribers : HashSet<ID> =
    find_related_nodes (
      &snap, &input_acquirer,
      "subscribes", "subscribee", "subscriber" );
  assert!( subscribers . contains (&ID::from ("subscribes-to-1")),
           "inverse subscribes under pid 2 should include \
            subscribes-to-1 (its subscribes_to = [1], which aliases 2)" );

  let hiders : HashSet<ID> =
    find_related_nodes (
      &snap, &input_acquirer,
      "hides_from_its_subscriptions", "hidden", "hider" );
  assert!( hiders . contains (&ID::from ("hides-1-from-subscriptions")),
           "inverse hides under pid 2 should include \
            hides-1-from-subscriptions" );

  let overriders : HashSet<ID> =
    find_related_nodes (
      &snap, &input_acquirer,
      "overrides_view_of", "overridden", "overrider" );
  assert!( overriders . contains (&ID::from ("overrider-of-1")),
           "inverse overrides_view_of under pid 2 should include \
            overrider-of-1" );

  let textlink_sources : HashSet<ID> =
    find_related_nodes (
      &snap, &input_acquirer,
      "textlinks_to", "dest", "source" );
  assert!( textlink_sources . contains (&ID::from ("links-to-1")),
           "inverse textlinks_to under pid 2 should include \
            links-to-1 (its body has a link to id 1, which aliases 2)" );

  // === Forward queries: neighbors' outbound should resolve 1 → 2 ===

  let subscribee_of_s2_1 : HashSet<ID> =
    find_related_nodes (
      &snap, &vec![ID::from ("subscribes-to-1")],
      "subscribes", "subscriber", "subscribee" );
  assert!( subscribee_of_s2_1 . contains (&ID::from ("2")),
           "subscribes-to-1's forward subscribes should resolve to \
            canonical pid 2 (was raw 1 on disk)" );
  assert!( ! subscribee_of_s2_1 . contains (&ID::from ("1")),
           "forward query should NOT return raw acquiree pid 1" );

  let hidden_by_h1 : HashSet<ID> =
    find_related_nodes (
      &snap, &vec![ID::from ("hides-1-from-subscriptions")],
      "hides_from_its_subscriptions", "hider", "hidden" );
  assert!( hidden_by_h1 . contains (&ID::from ("2")),
           "hides-1-from-subscriptions's forward hides should include \
            canonical pid 2" );
  assert!( hidden_by_h1 . contains (&ID::from ("11")),
           "hides-1-from-subscriptions also hides 11 (unchanged)" );

  let overridden_by_ov1 : HashSet<ID> =
    find_related_nodes (
      &snap, &vec![ID::from ("overrider-of-1")],
      "overrides_view_of", "overrider", "overridden" );
  assert!( overridden_by_ov1 . contains (&ID::from ("2")),
           "overrider-of-1's forward overrides should resolve \
            to canonical pid 2" );

  let destinations_of_l1 : HashSet<ID> =
    find_related_nodes (
      &snap, &vec![ID::from ("links-to-1")],
      "textlinks_to", "source", "dest" );
  assert!( destinations_of_l1 . contains (&ID::from ("2")),
           "links-to-1's forward textlinks should resolve to \
            canonical pid 2" );

  Ok (( )) }
