// cargo test merge::merge_nodes

use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_viewforest;
use skg::nodeMerge::merge_nodes;
use skg::test_utils::{
  run_with_shared_test_graph, all_pids_from_graph, tantivy_contains_id,
  extra_ids_from_pid};
use skg::types::misc::{ID, MSV, SkgConfig, TantivyIndex, SourceName};
use skg::types::tree::forest::ViewForest;
use skg::types::viewnode::{EditRequest, ViewNode, ViewNodeKind, Vognode, ActiveNode, IndefOrDef, viewforest_root_viewnode, default_activeNode};
use skg::types::nodes::complete::NodeComplete;
use skg::types::save::NodeMerge;
use skg::dbs::filesystem::one_node::nodecomplete_from_pid_and_source;
use skg::util::path_from_pid_and_source;
use skg::dbs::graph_queries::relations::{contains_from_pids, find_related_nodes};
use skg::dbs::in_rust_graph::relation_accessors::RelationRole;

use ego_tree::Tree;
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::path::Path;

fn mk_test_viewnode (
  title        : &str,
  id           : &str,
  edit_request : Option<EditRequest>,
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
  run_with_shared_test_graph (
    "skg-test-merge-nodes",
    |s| Box::pin ( async move {
      s . reset ("test_merge_2_into_1", fixtures) . await ?;
      test_merge_2_into_1 (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_merge_1_into_2", fixtures) . await ?;
      test_merge_1_into_2 (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      s . reset ("test_inrustgraph_queries_resolve_aliases_after_merge", fixtures) . await ?;
      test_inrustgraph_queries_resolve_aliases_after_merge (
        &s . config, &s . graph, &mut s . tantivy ) . await ?;
      Ok (( )) } )) }

async fn test_merge_2_into_1 (
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_merge_2_into_1_impl(config, fixture_graph, tantivy) . await?;
      Ok(( ))
    }

async fn test_merge_2_into_1_impl(
  config: &SkgConfig,
  fixture_graph: &InRustGraphHandle,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create viewnode viewforest with node 1 requesting to merge node 2
  let view_node_1 = mk_test_viewnode("1", "1", Some(EditRequest::NodeMerge(ID::from ("2"))));
  let mut viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  viewforest . root_mut() . append (view_node_1);
  // Generate NodeMerge from merge request
  let nodeMerge_instructions: Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest(
      &fixture_graph . load_full () . graph,
      &ViewForest::from_internal_tree (viewforest),
      config,
  ) . await?;

  // Expect 1 NodeMerge (containing 3 DefineNodes)
  assert_eq!(nodeMerge_instructions . len(),
             1,
             "Should have 1 NodeMerge");

  merge_nodes(
    &nodeMerge_instructions,
    config . clone(),
    tantivy,
    fixture_graph,
  ) . await?;

  // Verify results
  verify_graph_after_merge_2_into_1(
    config, fixture_graph) . await?;
  verify_filesystem_after_merge_2_into_1(
    config, &nodeMerge_instructions)?;
  verify_tantivy_after_merge_2_into_1(
    tantivy, &nodeMerge_instructions )?;
  Ok(( )) }

async fn verify_graph_after_merge_2_into_1 (
  _config: &SkgConfig,
  graph_handle: &InRustGraphHandle,
) -> Result<(), Box<dyn Error>> {
  let snapshot = graph_handle . load_full ();
  let graph = &snapshot . graph;

  // Node 2 should be gone as a primary node.
  let all_primary_node_ids: HashSet<ID> =
    all_pids_from_graph (graph);
  assert!(!all_primary_node_ids . contains(&ID::from ("2")),
          "PID 2 should not exist. It was merged and deleted.");

  // Node 1 should have extra_ids: 2 and 2-extra-id
  let node_1_extra_ids: Vec<ID> =
    extra_ids_from_pid (graph, &ID::from ("1"));
  assert!(node_1_extra_ids . contains(&ID::from ("2")),
          "Node 1 should have extra_id '2'");
  assert!(node_1_extra_ids . contains(&ID::from ("2-extra-id")),
          "Node 1 should have extra_id '2-extra-id'");

  // Node 1 should contain 7 things: [acquiree_text_preserver_id, 11, 12, overlap, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // IMPORTANT: contains_from_pids only returns relationships where BOTH nodes are in the input list, so we must include all the child nodes we want to check for. We query with all nodes in the DB.
  let all_node_ids: HashSet<ID> = all_pids_from_graph (graph);
  let input_pids: Vec<ID> = all_node_ids . into_iter() . collect();
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids (graph, &input_pids, None);

  let node_1_contents: &HashSet<ID> =
    container_to_contents . get(
      &ID::from ("1"))
    . ok_or ("Node 1 should have contains relationships")?;

  assert_eq!(node_1_contents . len(), 7,
             "Node 1 should contain 7 items after merge (with overlap deduplicated)");
  // acquiree_text_preserver_id is a UUID we don't know, but it should be in the set.
  // We can, however, test for the other six:
  assert!(node_1_contents . contains(&ID::from ("11")));
  assert!(node_1_contents . contains(&ID::from ("12")));
  assert!(node_1_contents . contains(&ID::from ("overlap")),
          "overlap should be present (deduplicated)");
  assert!(node_1_contents . contains(&ID::from ("21")));
  assert!(node_1_contents . contains(&ID::from ("22")));
  assert!(node_1_contents . contains(
    &ID::from ("hidden-from-subscriptions-of-1-but-in-content-of-2")));
  // Note that the second 'overlap' was stripped.

  // In this test,
  // relationships beyond contains and extra_id are ignored,
  // because nothing happens to them.
  // See the other test, of merging 1 into 2, for that action.
  Ok (( )) }

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
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
      test_merge_1_into_2_impl(config, fixture_graph, tantivy) . await?;
      Ok(( ))
    }

async fn test_merge_1_into_2_impl(
  config: &SkgConfig,
  fixture_graph: &InRustGraphHandle,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create viewnode viewforest with node 2 requesting to merge node 1
  let view_node_2 = mk_test_viewnode("2", "2", Some(EditRequest::NodeMerge(ID::from ("1"))));
  let mut viewforest: Tree<ViewNode> = Tree::new(viewforest_root_viewnode());
  viewforest . root_mut() . append (view_node_2);
  // Generate NodeMerge from merge request
  let nodeMerge_instructions: Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest(
      &fixture_graph . load_full () . graph,
      &ViewForest::from_internal_tree (viewforest),
      config,
  ) . await?;

  // Expect 1 NodeMerge (containing 3 DefineNodes)
  assert_eq!(nodeMerge_instructions . len(),
             1,
             "Should have 1 NodeMerge");

  merge_nodes(
    &nodeMerge_instructions,
    config . clone(),
    tantivy,
    fixture_graph,
  ) . await?;

  // Verify results
  verify_graph_after_merge_1_into_2(
    config, fixture_graph, &nodeMerge_instructions) . await?;
  verify_filesystem_after_merge_1_into_2(
    config, &nodeMerge_instructions)?;
  verify_tantivy_after_merge_1_into_2(
    tantivy, &nodeMerge_instructions)?;
  Ok(( )) }

async fn verify_graph_after_merge_1_into_2 (
  _config: &SkgConfig,
  graph_handle: &InRustGraphHandle,
  nodeMerge_instructions: &[NodeMerge],
) -> Result<(), Box<dyn Error>> {
  let snapshot = graph_handle . load_full ();
  let graph = &snapshot . graph;

  // Node 1 should be gone as a primary node.
  let all_primary_node_ids: HashSet<ID> =
    all_pids_from_graph (graph);
  assert!(!all_primary_node_ids . contains(&ID::from ("1")),
          "PID 1 should NOT exist (it was merged and deleted)");

  // Node 2 should have new extra_id '1',
  // in addition to its preexisting extra id '2-extra-id'.
  let node_2_extra_ids: Vec<ID> =
    extra_ids_from_pid (graph, &ID::from ("2"));
  assert!(node_2_extra_ids . contains(&ID::from ("1")),
          "Node 2 should have extra_id '1'");

  // IMPORTANT: contains_from_pids only returns
  // relationships where BOTH nodes are in the input list,
  // so we query with all nodes in the DB.
  let all_node_ids: HashSet<ID> =
    all_pids_from_graph (graph);
  let input_pids: Vec<ID> = all_node_ids . into_iter() . collect();
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids (graph, &input_pids, None);
  let node_2_contents: &HashSet<ID> =
    container_to_contents . get(&ID::from ("2"))
    . ok_or ("Node 2 should have contains relationships")?;

  assert_eq!(node_2_contents . len(), 7,
             "Node 2 should contain 7 items after merge (4 + 4 - the duplicated node called 'overlap')");
  // acquiree_text_preserver_id is a UUID we don't know, but it should be in the set.
  // We can test for the other six:
  assert!(node_2_contents . contains(&ID::from ("11")));
  assert!(node_2_contents . contains(&ID::from ("12")));
  assert!(node_2_contents . contains(&ID::from ("overlap")),
          "overlap should be present (deduplicated)");
  assert!(node_2_contents . contains(&ID::from ("21")));
  assert!(node_2_contents . contains(&ID::from ("22")));
  assert!(node_2_contents . contains(
    &ID::from ("hidden-from-subscriptions-of-1-but-in-content-of-2")));
  // Note that the would-be second 'overlap' node was removed.

  // TextLinks should be rerouted
  // The old link from 1 to 1-links-to should now be from acquiree_text_preserver,
  // because acquiree_text_preserver has what was node 1's body text.
  let acquiree_text_preserver_id: &ID =
    &nodeMerge_instructions[0] . targets_from_nodeMerge() . 0 . pid;
  let acquiree_text_preserver_textlink_dests: HashSet<ID> =
    find_related_nodes(
      graph, & [ acquiree_text_preserver_id . clone () ],
      RelationRole::LINK_SOURCE, None );
  assert!(
    acquiree_text_preserver_textlink_dests . contains(&ID::from ("1-links-to")),
    "acquiree_text_preserver should textlink to 1-links-to");

  // - Node 2 should NOT have the outbound textlink from node 1
  //   (the textlink is in the text, which went to acquiree_text_preserver)
  let node_2_textlink_dests: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("2") ], RelationRole::LINK_SOURCE, None );
  assert!(
    !node_2_textlink_dests . contains(&ID::from ("1-links-to")),
    "Node 2 should NOT textlink to 1-links-to");

  // - The textlink from links-to-1 to 1 should now be from links-to-1 to 2
  //   (inbound textlinks target the acquirer because acquiree's ID becomes an extra_id)
  let links_to_1_dests: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("links-to-1") ],
    RelationRole::LINK_SOURCE, None );
  assert!(links_to_1_dests . contains(&ID::from ("2")),
          "links-to-1 should textlink to 2 (rerouted from 1)");
  assert!(!links_to_1_dests . contains(&ID::from ("1")),
          "links-to-1 should NOT textlink to 1 (1 was merged)");

  // Subscribes relationships should be rerouted
  // - Node 1's subscribes_to [1-subscribes-to] should transfer to node 2
  let node_2_subscribes_to: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("2") ], RelationRole::SUBSCRIBER, None );
  assert!(node_2_subscribes_to . contains(&ID::from ("1-subscribes-to")),
          "Node 2 should subscribe to 1-subscribes-to");

  // - subscribes-to-1, which subscribed to [1],
  // should now subscribe to [2]
  let subscribes_to_1_targets: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("subscribes-to-1") ],
    RelationRole::SUBSCRIBER, None );
  assert!(subscribes_to_1_targets . contains(&ID::from ("2")),
          "subscribes-to-1 should subscribe to 2 (rerouted from 1)");
  assert!(!subscribes_to_1_targets . contains(&ID::from ("1")),
          "subscribes-to-1 should NOT subscribe to 1 (1 was merged)");

  // Hides relationships should be processed correctly
  // - Node 1 hides [hidden-from-1s-subscriptions, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // - After merge: Node 2 should hide [hidden-from-1s-subscriptions] but NOT [hidden-from-subscriptions-of-1-but-in-content-of-2]
  //   because hidden-from-subscriptions-of-1-but-in-content-of-2 IS in node 2's contents.
  let node_2_hides: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("2") ], RelationRole::HIDER, None );
  assert!(node_2_hides . contains(&ID::from ("hidden-from-1s-subscriptions")),
          "Node 2 should hide hidden-from-1s-subscriptions (transferred from node 1)");
  assert!(!node_2_hides . contains(&ID::from ("hidden-from-subscriptions-of-1-but-in-content-of-2")),
          "Node 2 should NOT hide hidden-from-subscriptions-of-1-but-in-content-of-2");

  // - hides-1-from-subscriptions hid [1, 11] on disk.
  //   Post-merge, the graph resolves the "1" reference through extra IDs
  //   to node 2 (the acquirer). So hides-1-from-subscriptions should
  //   hide [2, 11]. Node 1 itself no longer exists as a node entity.
  let hides_1_targets: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("hides-1-from-subscriptions") ],
    RelationRole::HIDER, None );
  assert!(hides_1_targets . contains(&ID::from ("11")),
          "hides-1-from-subscriptions should still hide 11");
  assert!(hides_1_targets . contains(&ID::from ("2")),
          "hides-1-from-subscriptions should now hide 2 (extra_id resolution of 1 → 2)");
  assert!(!hides_1_targets . contains(&ID::from ("1")),
          "Node 1 no longer exists as a node entity");

  // Overrides relationships should be processed correctly
  // - Node 1's overrides [overridden-by-1] should transfer to node 2
  let node_2_overrides: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("2") ], RelationRole::OVERRIDER, None );
  assert!(node_2_overrides . contains(&ID::from ("overridden-by-1")),
          "Node 2 should override view of overridden-by-1 (transferred from node 1)");

  // - overrider-of-1 overrode [1] on disk. Post-merge, extra_id
  //   resolution redirects "1" to node 2, so it now overrides [2].
  let overrider_of_1_targets: HashSet<ID> = find_related_nodes(
    graph, & [ ID::from ("overrider-of-1") ],
    RelationRole::OVERRIDER, None );
  assert!(overrider_of_1_targets . contains(&ID::from ("2")),
          "overrider-of-1 should now override 2 (extra_id resolution of 1 → 2)");
  assert!(!overrider_of_1_targets . contains(&ID::from ("1")),
          "Node 1 no longer exists as a node entity");
  Ok (( )) }

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
// queries returned raw IDs. This test locks in canonicalized behavior.
async fn test_inrustgraph_queries_resolve_aliases_after_merge (
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &mut TantivyIndex,
) -> Result<(), Box<dyn Error>> {
        test_inrustgraph_queries_resolve_aliases_after_merge_impl (
          config, fixture_graph, tantivy ) . await }

async fn test_inrustgraph_queries_resolve_aliases_after_merge_impl (
  config  : &SkgConfig,
  fixture_graph  : &InRustGraphHandle,
  tantivy : &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // NodeMerge 1 into 2. Acquirer=2, acquiree=1.
  let view_node_2 =
    mk_test_viewnode ("2", "2",
                      Some (EditRequest::NodeMerge (ID::from ("1"))));
  let mut viewforest : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  viewforest . root_mut () . append (view_node_2);
  let nodeMerge_instructions : Vec<NodeMerge> =
    nodeMerge_instructions_from_viewforest (
      &fixture_graph . load_full () . graph,
      &ViewForest::from_internal_tree (viewforest),
      config ) . await?;
  merge_nodes (
    &nodeMerge_instructions, config . clone (),
    tantivy, fixture_graph ) . await?;

  let snap = fixture_graph . load_full ();
  let input_acquirer : Vec<ID> = vec![ID::from ("2")];

  // === Inverse queries: "who points at pid 2?" ===

  let subscribers : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &input_acquirer, RelationRole::SUBSCRIBEE, None );
  assert!( subscribers . contains (&ID::from ("subscribes-to-1")),
           "inverse subscribes under pid 2 should include \
            subscribes-to-1 (its subscribes_to = [1], which aliases 2)" );

  let hiders : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &input_acquirer, RelationRole::HIDDEN, None );
  assert!( hiders . contains (&ID::from ("hides-1-from-subscriptions")),
           "inverse hides under pid 2 should include \
            hides-1-from-subscriptions" );

  let overriders : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &input_acquirer, RelationRole::OVERRIDDEN, None );
  assert!( overriders . contains (&ID::from ("overrider-of-1")),
           "inverse overrides_view_of under pid 2 should include \
            overrider-of-1" );

  let textlink_sources : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &input_acquirer, RelationRole::LINK_DEST, None );
  assert!( textlink_sources . contains (&ID::from ("links-to-1")),
           "inverse textlinks_to under pid 2 should include \
            links-to-1 (its body has a link to id 1, which aliases 2)" );

  // === Forward queries: neighbors' outbound should resolve 1 → 2 ===

  let subscribee_of_s2_1 : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &vec![ID::from ("subscribes-to-1")],
      RelationRole::SUBSCRIBER, None );
  assert!( subscribee_of_s2_1 . contains (&ID::from ("2")),
           "subscribes-to-1's forward subscribes should resolve to \
            canonical pid 2 (was raw 1 on disk)" );
  assert!( ! subscribee_of_s2_1 . contains (&ID::from ("1")),
           "forward query should NOT return raw acquiree pid 1" );

  let hidden_by_h1 : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &vec![ID::from ("hides-1-from-subscriptions")],
      RelationRole::HIDER, None );
  assert!( hidden_by_h1 . contains (&ID::from ("2")),
           "hides-1-from-subscriptions's forward hides should include \
            canonical pid 2" );
  assert!( hidden_by_h1 . contains (&ID::from ("11")),
           "hides-1-from-subscriptions also hides 11 (unchanged)" );

  let overridden_by_ov1 : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &vec![ID::from ("overrider-of-1")],
      RelationRole::OVERRIDER, None );
  assert!( overridden_by_ov1 . contains (&ID::from ("2")),
           "overrider-of-1's forward overrides should resolve \
            to canonical pid 2" );

  let destinations_of_l1 : HashSet<ID> =
    find_related_nodes (
      &snap . graph, &vec![ID::from ("links-to-1")],
      RelationRole::LINK_SOURCE, None );
  assert!( destinations_of_l1 . contains (&ID::from ("2")),
           "links-to-1's forward textlinks should resolve to \
            canonical pid 2" );

  Ok (( )) }
