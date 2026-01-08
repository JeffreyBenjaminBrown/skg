// cargo test merge::merge_nodes

use skg::merge::mergeInstructionTriple::instructiontriples_from_the_merges_in_an_orgnode_forest;
use skg::merge::merge_nodes;
use skg::test_utils::{run_with_test_db, all_pids_from_typedb, tantivy_contains_id, extra_ids_from_pid};
use skg::types::misc::{ID, SkgConfig, TantivyIndex};
use skg::types::orgnode::{OrgNode, OrgnodeMetadata, EditRequest, OrgnodeCode, Interp};
use skg::types::orgnode_new::{NewOrgNode, forest_root_new_orgnode, from_old_orgnode};
use skg::types::skgnode::SkgNode;
use skg::types::save::MergeInstructionTriple;
use skg::dbs::filesystem::one_node::skgnode_from_pid_and_source;
use skg::util::path_from_pid_and_source;
use skg::dbs::typedb::search::contains_from_pids::contains_from_pids;
use skg::dbs::typedb::search::find_related_nodes;

use ego_tree::Tree;
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_2_into_1() -> Result<(), Box<dyn Error>> {
  let fixtures_path = PathBuf::from("tests/merge/merge_nodes/fixtures");
  let temp_fixtures_path = PathBuf::from("/tmp/merge-test-2-into-1-fixtures");
  let tantivy_path = PathBuf::from("/tmp/tantivy-test-merge-2-into-1");

  // Clean up temp directories if they exist from a previous run
  if temp_fixtures_path.exists() {
    fs::remove_dir_all(&temp_fixtures_path)?;
  }
  if tantivy_path.exists() {
    fs::remove_dir_all(&tantivy_path)?;
  }

  // Copy fixtures to temp directory
  copy_dir_all(&fixtures_path, &temp_fixtures_path)?;

  let result : Result<(), Box<dyn Error>> =
    run_with_test_db(
    "skg-test-merge-2-into-1",
    "/tmp/merge-test-2-into-1-fixtures",
    "/tmp/tantivy-test-merge-2-into-1",
    |config, driver, tantivy| Box::pin(async move {
      test_merge_2_into_1_impl(config, driver, tantivy).await?;
      Ok(( ))
    } ));

  // Clean up temp directory
  if temp_fixtures_path.exists() {
    fs::remove_dir_all(&temp_fixtures_path)?;
  }

  result
}

async fn test_merge_2_into_1_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create orgnode forest with node 1 requesting to merge node 2
  let org_node_1: OrgNode = OrgNode {
    metadata: OrgnodeMetadata {
      id: Some(ID::from("1")),
      source: None,
      viewData: Default::default(),
      code: OrgnodeCode {
        interp: Interp::Content,
        indefinitive: false,
        editRequest: Some(EditRequest::Merge(ID::from("2"))),
        viewRequests: HashSet::new(), }, },
    title: "1".to_string(),
    body: None, };
  let mut forest: Tree<NewOrgNode> = Tree::new(forest_root_new_orgnode());
  forest.root_mut().append(from_old_orgnode(&org_node_1));

  // Generate MergeInstructionTriple from merge request
  let merge_instructions: Vec<MergeInstructionTriple> =
    instructiontriples_from_the_merges_in_an_orgnode_forest(
      &forest,
      config,
      driver,
  ).await?;

  // Expect 1 MergeInstructionTriple (containing 3 SaveInstructions)
  assert_eq!(merge_instructions.len(),
             1,
             "Should have 1 MergeInstructionTriple");

  merge_nodes(
    merge_instructions.clone(),
    config.clone(),
    tantivy,
    driver,
  ).await?;

  // Verify results
  verify_typedb_after_merge_2_into_1(
    config, driver).await?;
  verify_filesystem_after_merge_2_into_1(
    config, &merge_instructions)?;
  verify_tantivy_after_merge_2_into_1(
    tantivy, &merge_instructions )?;
  Ok(( )) }

async fn verify_typedb_after_merge_2_into_1 (
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let db_name: &String = &config.db_name;

  // Node 2 should be gone from TypeDB as primary node
  let all_primary_node_ids: HashSet<ID> =
    all_pids_from_typedb(
      db_name, driver ). await ?;
  assert!(!all_primary_node_ids.contains(&ID::from("2")),
          "PID 2 should not exist. It was merged and deleted.");

  // Node 1 should have extra_ids: 2 and 2-extra-id
  let node_1_extra_ids: Vec<ID> =
    extra_ids_from_pid(
      db_name, driver, &ID::from("1")).await?;
  assert!(node_1_extra_ids.contains(&ID::from("2")),
          "Node 1 should have extra_id '2'");
  assert!(node_1_extra_ids.contains(&ID::from("2-extra-id")),
          "Node 1 should have extra_id '2-extra-id'");

  // Node 1 should contain 7 things: [acquiree_text_preserver_id, 11, 12, overlap, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // IMPORTANT: contains_from_pids only returns relationships where BOTH nodes are in the input list, so we must include all the child nodes we want to check for. We query with all nodes in the DB.
  let all_node_ids: HashSet<ID> = all_pids_from_typedb(db_name, driver).await?;
  let input_pids: Vec<ID> = all_node_ids.into_iter().collect();
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids(
      db_name, driver, &input_pids ). await ?;

  let node_1_contents: &HashSet<ID> =
    container_to_contents.get(
      &ID::from("1"))
    . ok_or("Node 1 should have contains relationships")?;

  assert_eq!(node_1_contents.len(), 7,
             "Node 1 should contain 7 items after merge (with overlap deduplicated)");
  // acquiree_text_preserver_id is a UUID we don't know, but it should be in the set.
  // We can, however, test for the other six:
  assert!(node_1_contents.contains(&ID::from("11")));
  assert!(node_1_contents.contains(&ID::from("12")));
  assert!(node_1_contents.contains(&ID::from("overlap")),
          "overlap should be present (deduplicated)");
  assert!(node_1_contents.contains(&ID::from("21")));
  assert!(node_1_contents.contains(&ID::from("22")));
  assert!(node_1_contents.contains(
    &ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")));
  // Note that the second 'overlap' was stripped.

  // In this test,
  // relationships beyond contains and extra_id are ignored,
  // because nothing happens to them.
  // See the other test, of merging 1 into 2, for that action.
  Ok (( )) }

fn verify_filesystem_after_merge_2_into_1(
  config: &SkgConfig,
  merge_instructions: &[MergeInstructionTriple],
) -> Result<(), Box<dyn Error>> {
  let node_2_path: String = path_from_pid_and_source ( config, "main",
                                            ID::from("2"));
  assert!( !Path::new(&node_2_path).exists(),
            "2.skg should be deleted" );

  // Node 1's file should be updated
  let node_1: SkgNode = skgnode_from_pid_and_source(
    config, ID::from("1"), "main" )?;
  assert_eq!(node_1.ids.len(), 3, "Node 1 should have 3 ids");
  assert_eq!(&node_1.ids[0], &ID::from("1"));
  assert_eq!(&node_1.ids[1], &ID::from("2"));
  assert_eq!(&node_1.ids[2], &ID::from("2-extra-id"));

  // Should have [acquiree_text_preserver_id, 11, 12, overlap, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // Note: "overlap" should appear only once (deduplicated) even though it was in both nodes
  assert_eq!( node_1.contains.as_ref().unwrap().len(), 7,
              "Node 1 should contain 7 items (with overlap deduplicated)");

  let acquiree_text_preserver_id: &ID = &merge_instructions[0].acquiree_text_preserver.0.ids[0];
  assert_eq!(&node_1.contains.as_ref().unwrap()[0], acquiree_text_preserver_id,
             "First content should be acquiree_text_preserver");
  assert_eq!(&node_1.contains.as_ref().unwrap()[1], &ID::from("11"));
  assert_eq!(&node_1.contains.as_ref().unwrap()[2], &ID::from("12"));
  assert_eq!(&node_1.contains.as_ref().unwrap()[3], &ID::from("overlap"),
             "overlap should appear in position 3 (from acquirer's original contents)");
  assert_eq!(&node_1.contains.as_ref().unwrap()[4], &ID::from("21"));
  assert_eq!(&node_1.contains.as_ref().unwrap()[5], &ID::from("22"));
  assert_eq!(
    &node_1.contains.as_ref().unwrap()[6],
    &ID::from(
      "hidden-from-subscriptions-of-1-but-in-content-of-2"));
  // Note that the second 'overlap' was stripped.

  // Verify overlap appears only once (not duplicated)
  let overlap_count: usize = node_1.contains.as_ref().unwrap()
    .iter()
    .filter(|id| *id == &ID::from("overlap"))
    .count();
  assert_eq!(overlap_count, 1,
             "overlap should appear exactly once (deduplicated), not twice");

  // Verify that node 1 keeps its original relationships
  // (node 2 had no relationships to transfer)
  assert_eq!(node_1.subscribes_to.as_ref().map(|v| v.len()), Some(1),
             "Node 1 should keep its subscribes_to");
  assert_eq!(&node_1.subscribes_to.as_ref().unwrap()[0],
             &ID::from("1-subscribes-to"));

  // Note: node 1 originally hid both "hidden-from-1s-subscriptions"
  // and "hidden-from-subscriptions-of-1-but-in-content-of-2".
  // The latter is now in node 1's contents, so it should be removed from hides.
  assert_eq!(node_1.hides_from_its_subscriptions.as_ref().map(|v| v.len()),
             Some(1),
             "Node 1 should have 1 hides relationship");
  assert_eq!(&node_1.hides_from_its_subscriptions.as_ref().unwrap()[0],
             &ID::from("hidden-from-1s-subscriptions"));

  assert_eq!(node_1.overrides_view_of.as_ref().map(|v| v.len()), Some(1),
             "Node 1 should keep its overrides_view_of");
  assert_eq!(&node_1.overrides_view_of.as_ref().unwrap()[0],
             &ID::from("1-overrides-view-of"));

  let acquiree_text_preserver_path: String =
    path_from_pid_and_source ( config, "main",
                    acquiree_text_preserver_id . clone() );
  assert!( Path::new(&acquiree_text_preserver_path).exists(),
           "acquiree_text_preserver file should exist" );

  let acquiree_text_preserver: SkgNode = skgnode_from_pid_and_source(
    config, acquiree_text_preserver_id.clone(), "main" )?;
  assert!(acquiree_text_preserver.title.starts_with("MERGED: "));
  assert_eq!(acquiree_text_preserver.title, "MERGED: 2");
  assert_eq!(acquiree_text_preserver.body, Some("2 body".to_string()));
  assert_eq!(acquiree_text_preserver.contains, None,
             "acquiree_text_preserver should have no contents (None when empty on disk)");

  // acquiree_text_preserver should have None for relationship fields
  // (when read from disk, missing fields are None)
  assert_eq!(acquiree_text_preserver.subscribes_to, None,
             "acquiree_text_preserver should have None for subscribes_to");
  assert_eq!(acquiree_text_preserver.hides_from_its_subscriptions, None,
             "acquiree_text_preserver should have None for hides_from_its_subscriptions");
  assert_eq!(acquiree_text_preserver.overrides_view_of, None,
             "acquiree_text_preserver should have None for overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_2_into_1(
  tantivy_index: &TantivyIndex,
  merge_instructions: &[MergeInstructionTriple],
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
  let acquiree_text_preserver_id: &ID = &merge_instructions[0].acquiree_text_preserver.0.ids[0];
  let found_acquiree_text_preserver: bool =
    tantivy_contains_id(tantivy_index, "\"MERGED: 2\"", &acquiree_text_preserver_id.0)?;
  assert!(found_acquiree_text_preserver, "acquiree_text_preserver SHOULD be in Tantivy index");
  Ok (( )) }


// ============================================================
// Test: Merging 1 into 2
// ============================================================

#[test]
fn test_merge_1_into_2() -> Result<(), Box<dyn Error>> {
  let fixtures_path = PathBuf::from("tests/merge/merge_nodes/fixtures");
  let temp_fixtures_path = PathBuf::from("/tmp/merge-test-1-into-2-fixtures");
  let tantivy_path = PathBuf::from("/tmp/tantivy-test-merge-1-into-2");

  // Clean up temp directories if they exist from a previous run
  if temp_fixtures_path.exists() {
    fs::remove_dir_all(&temp_fixtures_path)?;
  }
  if tantivy_path.exists() {
    fs::remove_dir_all(&tantivy_path)?;
  }

  // Copy fixtures to temp directory
  copy_dir_all(&fixtures_path, &temp_fixtures_path)?;

  let result : Result<(), Box<dyn Error>> =
    run_with_test_db(
    "skg-test-merge-1-into-2",
    "/tmp/merge-test-1-into-2-fixtures",
    "/tmp/tantivy-test-merge-1-into-2",
    |config, driver, tantivy| Box::pin(async move {
      test_merge_1_into_2_impl(config, driver, tantivy).await?;
      Ok(( ))
    } ));

  // Clean up temp directory
  if temp_fixtures_path.exists() {
    fs::remove_dir_all(&temp_fixtures_path)?;
  }

  result
}

async fn test_merge_1_into_2_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
  tantivy: &TantivyIndex,
) -> Result<(), Box<dyn Error>> {
  // Create orgnode forest with node 2 requesting to merge node 1
  let org_node_2: OrgNode = OrgNode {
    metadata: OrgnodeMetadata {
      id: Some(ID::from("2")),
      source: None,
      viewData: Default::default(),
      code: OrgnodeCode {
        interp: Interp::Content,
        indefinitive: false,
        editRequest: Some(EditRequest::Merge(ID::from("1"))),
        viewRequests: HashSet::new(), }, },
    title: "2".to_string(),
    body: None, };
  let mut forest: Tree<NewOrgNode> = Tree::new(forest_root_new_orgnode());
  forest.root_mut().append(from_old_orgnode(&org_node_2));

  // Generate MergeInstructionTriple from merge request
  let merge_instructions: Vec<MergeInstructionTriple> =
    instructiontriples_from_the_merges_in_an_orgnode_forest(
      &forest,
      config,
      driver,
  ).await?;

  // Expect 1 MergeInstructionTriple (containing 3 SaveInstructions)
  assert_eq!(merge_instructions.len(),
             1,
             "Should have 1 MergeInstructionTriple");

  merge_nodes(
    merge_instructions.clone(),
    config.clone(),
    tantivy,
    driver,
  ).await?;

  // Verify results
  verify_typedb_after_merge_1_into_2(
    config, driver, &merge_instructions).await?;
  verify_filesystem_after_merge_1_into_2(
    config, &merge_instructions)?;
  verify_tantivy_after_merge_1_into_2(
    tantivy, &merge_instructions)?;
  Ok(( )) }

async fn verify_typedb_after_merge_1_into_2 (
  config: &SkgConfig,
  driver: &TypeDBDriver,
  merge_instructions: &[MergeInstructionTriple],
) -> Result<(), Box<dyn Error>> {
  let db_name: &String = &config.db_name;

  // Node 1 should be gone from TypeDB as primary node
  let all_primary_node_ids: HashSet<ID> =
    all_pids_from_typedb(db_name, driver).await?;
  assert!(!all_primary_node_ids.contains(&ID::from("1")),
          "PID 1 should NOT exist (it was merged and deleted)");

  // Node 2 should have new extra_id '1',
  // in addition to its preexisting extra id '2-extra-id'.
  let node_2_extra_ids: Vec<ID> =
    extra_ids_from_pid(db_name, driver, &ID::from("2")).await?;
  assert!(node_2_extra_ids.contains(&ID::from("1")),
          "Node 2 should have extra_id '1'");

  // IMPORTANT: contains_from_pids only returns
  // relationships where BOTH nodes are in the input list,
  // so we query with all nodes in the DB.
  let all_node_ids: HashSet<ID> =
    all_pids_from_typedb(db_name, driver).await?;
  let input_pids: Vec<ID> = all_node_ids.into_iter().collect();
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids(
      db_name, driver, &input_pids ). await ?;
  let node_2_contents: &HashSet<ID> =
    container_to_contents.get(&ID::from("2"))
    .ok_or("Node 2 should have contains relationships")?;

  assert_eq!(node_2_contents.len(), 7,
             "Node 2 should contain 7 items after merge (4 + 4 - the duplicated node called 'overlap')");
  // acquiree_text_preserver_id is a UUID we don't know, but it should be in the set.
  // We can test for the other six:
  assert!(node_2_contents.contains(&ID::from("11")));
  assert!(node_2_contents.contains(&ID::from("12")));
  assert!(node_2_contents.contains(&ID::from("overlap")),
          "overlap should be present (deduplicated)");
  assert!(node_2_contents.contains(&ID::from("21")));
  assert!(node_2_contents.contains(&ID::from("22")));
  assert!(node_2_contents.contains(
    &ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")));
  // Note that the would-be second 'overlap' node was removed.

  // TextLinks should be rerouted
  // The old link from 1 to 1-links-to should now be from acquiree_text_preserver,
  // because acquiree_text_preserver has what was node 1's body text.
  let acquiree_text_preserver_id: &ID = &merge_instructions[0].acquiree_text_preserver.0.ids[0];
  let acquiree_text_preserver_textlink_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ acquiree_text_preserver_id . clone () ],
    "textlinks_to", "source", "dest" ). await ?;
  assert!(
    acquiree_text_preserver_textlink_dests.contains(&ID::from("1-links-to")),
    "acquiree_text_preserver should textlink to 1-links-to");

  // - Node 2 should NOT have the outbound textlink from node 1
  //   (the textlink is in the text, which went to acquiree_text_preserver)
  let node_2_textlink_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("2") ],
    "textlinks_to", "source", "dest" ). await ?;
  assert!(
    !node_2_textlink_dests.contains(&ID::from("1-links-to")),
    "Node 2 should NOT textlink to 1-links-to");

  // - The textlink from links-to-1 to 1 should now be from links-to-1 to 2
  //   (inbound textlinks target the acquirer because acquiree's ID becomes an extra_id)
  let links_to_1_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("links-to-1") ],
    "textlinks_to", "source", "dest" ). await ?;
  assert!(links_to_1_dests.contains(&ID::from("2")),
          "links-to-1 should textlink to 2 (rerouted from 1)");
  assert!(!links_to_1_dests.contains(&ID::from("1")),
          "links-to-1 should NOT textlink to 1 (1 was merged)");

  // Subscribes relationships should be rerouted
  // - Node 1's subscribes_to [1-subscribes-to] should transfer to node 2
  let node_2_subscribes_to: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("2") ],
    "subscribes", "subscriber", "subscribee" ). await ?;
  assert!(node_2_subscribes_to.contains(&ID::from("1-subscribes-to")),
          "Node 2 should subscribe to 1-subscribes-to");

  // - subscribes-to-1, which subscribed to [1],
  // should now subscribe to [2]
  let subscribes_to_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("subscribes-to-1") ],
    "subscribes", "subscriber", "subscribee" ). await ?;
  assert!(subscribes_to_1_targets.contains(&ID::from("2")),
          "subscribes-to-1 should subscribe to 2 (rerouted from 1)");
  assert!(!subscribes_to_1_targets.contains(&ID::from("1")),
          "subscribes-to-1 should NOT subscribe to 1 (1 was merged)");

  // Hides relationships should be processed correctly
  // - Node 1 hides [hidden-from-1s-subscriptions, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // - After merge: Node 2 should hide [hidden-from-1s-subscriptions] but NOT [hidden-from-subscriptions-of-1-but-in-content-of-2]
  //   because hidden-from-subscriptions-of-1-but-in-content-of-2 IS in node 2's contents.
  let node_2_hides: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("2") ], "hides_from_its_subscriptions", "hider", "hidden"
  ).await?;
  assert!(node_2_hides.contains(&ID::from("hidden-from-1s-subscriptions")),
          "Node 2 should hide hidden-from-1s-subscriptions (transferred from node 1)");
  assert!(!node_2_hides.contains(&ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")),
          "Node 2 should NOT hide hidden-from-subscriptions-of-1-but-in-content-of-2");

  // - hides-1-from-subscriptions hid [1, 11] → should now hide [11] only (relationship hiding 1 is DROPPED)
  let hides_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("hides-1-from-subscriptions") ], "hides_from_its_subscriptions", "hider", "hidden"
  ).await?;
  assert!(hides_1_targets.contains(&ID::from("11")),
          "hides-1-from-subscriptions should still hide 11");
  assert!(!hides_1_targets.contains(&ID::from("1")) && !hides_1_targets.contains(&ID::from("2")),
          "hides-1-from-subscriptions should NOT hide 1 or 2 (relationship to merged node is DROPPED)");

  // Overrides relationships should be processed correctly
  // - Node 1's overrides [1-overrides-view-of] should transfer to node 2
  let node_2_overrides: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("2") ], "overrides_view_of", "replacement", "replaced"
  ).await?;
  assert!(node_2_overrides.contains(&ID::from("1-overrides-view-of")),
          "Node 2 should override view of 1-overrides-view-of (transferred from node 1)");

  // - overrides-view-of-1 that overrode [1] should have that relationship DROPPED (1 no longer exists to be replaced)
  let overrides_view_of_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, & [ ID::from("overrides-view-of-1") ],
    "overrides_view_of", "replacement", "replaced" ). await ?;
  assert!(!overrides_view_of_1_targets.contains(&ID::from("1")) &&
          !overrides_view_of_1_targets.contains(&ID::from("2")),
          "overrides-view-of-1 should NOT override 1 or 2 (relationship to merged node is DROPPED)");
  Ok (( )) }

fn verify_filesystem_after_merge_1_into_2(
  config: &SkgConfig,
  merge_instructions: &[MergeInstructionTriple],
) -> Result<(), Box<dyn Error>> {
  let node_1_path: String =
    path_from_pid_and_source ( config, "main", ID::from("1") );
  assert!( !Path::new(&node_1_path).exists(),
            "1.skg should be deleted" );

  // Node 2's file should be updated
  let node_2: SkgNode = skgnode_from_pid_and_source(
    config, ID::from("2"), "main" )?;

  // Should have 3 ids: [2, 2-extra-id, 1]
  assert_eq!(node_2.ids.len(), 3, "Node 2 should have 3 ids");
  assert_eq!(&node_2.ids[0], &ID::from("2"));
  assert_eq!(&node_2.ids[1], &ID::from("2-extra-id"));
  assert_eq!(&node_2.ids[2], &ID::from("1"));

  // Should have [acquiree_text_preserver_id, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2, overlap, 11, 12]
  // Note: "overlap" should appear only once (deduplicated) even though it was in both nodes
  assert_eq!( node_2.contains.as_ref().unwrap().len(), 7,
              "Node 2 should contain 7 items (with overlap deduplicated)");
  let acquiree_text_preserver_id: &ID = &merge_instructions[0].acquiree_text_preserver.0.ids[0];
  assert_eq!(&node_2.contains.as_ref().unwrap()[0], acquiree_text_preserver_id,
             "First content should be acquiree_text_preserver");
  assert_eq!(&node_2.contains.as_ref().unwrap()[1], &ID::from("21"));
  assert_eq!(&node_2.contains.as_ref().unwrap()[2], &ID::from("22"));
  assert_eq!(&node_2.contains.as_ref().unwrap()[3], &ID::from(
    "hidden-from-subscriptions-of-1-but-in-content-of-2"));
  assert_eq!(&node_2.contains.as_ref().unwrap()[4], &ID::from("overlap"),
             "overlap should appear in position 4 (from acquirer's original contents)");
  assert_eq!(&node_2.contains.as_ref().unwrap()[5], &ID::from("11"));
  assert_eq!(&node_2.contains.as_ref().unwrap()[6], &ID::from("12"));

  // Verify overlap appears only once (not duplicated)
  let overlap_count: usize = node_2.contains.as_ref().unwrap()
    .iter()
    .filter(|id| *id == &ID::from("overlap"))
    .count();
  assert_eq!(overlap_count, 1,
             "overlap should appear exactly once (deduplicated), not twice");

  // Verify subscribes_to: should have node 1's subscribes_to transferred
  assert_eq!(node_2.subscribes_to.as_ref().map(|v| v.len()), Some(1),
             "Node 2 should have 1 subscribes_to relationship");
  assert_eq!(&node_2.subscribes_to.as_ref().unwrap()[0],
             &ID::from("1-subscribes-to"),
             "Node 2 should subscribe to 1-subscribes-to");

  // Verify hides_from_its_subscriptions: should have node 1's hides,
  // but NOT "hidden-from-subscriptions-of-1-but-in-content-of-2"
  // (because it's in node 2's contents)
  assert_eq!(node_2.hides_from_its_subscriptions.as_ref().map(|v| v.len()),
             Some(1),
             "Node 2 should have 1 hides_from_its_subscriptions relationship");
  assert_eq!(&node_2.hides_from_its_subscriptions.as_ref().unwrap()[0],
             &ID::from("hidden-from-1s-subscriptions"),
             "Node 2 should hide hidden-from-1s-subscriptions");

  // Verify overrides_view_of: should have node 1's overrides_view_of transferred
  assert_eq!(node_2.overrides_view_of.as_ref().map(|v| v.len()), Some(1),
             "Node 2 should have 1 overrides_view_of relationship");
  assert_eq!(&node_2.overrides_view_of.as_ref().unwrap()[0],
             &ID::from("1-overrides-view-of"),
             "Node 2 should override view of 1-overrides-view-of");

  let acquiree_text_preserver_path: String = path_from_pid_and_source(
    config, "main", acquiree_text_preserver_id.clone() );
  assert!( Path::new(&acquiree_text_preserver_path).exists(),
           "acquiree_text_preserver file should exist" );

  let acquiree_text_preserver: SkgNode = skgnode_from_pid_and_source(
    config, acquiree_text_preserver_id.clone(), "main" )?;
  assert!(acquiree_text_preserver.title.starts_with("MERGED: "));
  assert_eq!(acquiree_text_preserver.title, "MERGED: 1");
  assert_eq!(acquiree_text_preserver.body,
             Some ( "[[id:1-links-to][a link to 1-links-to]]"
                       .to_string() ));
  assert_eq!(acquiree_text_preserver.contains, None,
             "acquiree_text_preserver should have no contents (None when empty on disk)");

  // acquiree_text_preserver should have None for relationship fields
  // (when read from disk, missing fields are None)
  // (these relationships stay with the acquirer, not the acquiree_text_preserver)
  assert_eq!(acquiree_text_preserver.subscribes_to, None,
             "acquiree_text_preserver should have None for subscribes_to");
  assert_eq!(acquiree_text_preserver.hides_from_its_subscriptions, None,
             "acquiree_text_preserver should have None for hides_from_its_subscriptions");
  assert_eq!(acquiree_text_preserver.overrides_view_of, None,
             "acquiree_text_preserver should have None for overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_1_into_2(
  tantivy_index: &TantivyIndex,
  merge_instructions: &[MergeInstructionTriple],
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
  let acquiree_text_preserver_id: &ID = &merge_instructions[0].acquiree_text_preserver.0.ids[0];
  let found_acquiree_text_preserver: bool = tantivy_contains_id(
    tantivy_index, "\"MERGED: 1\"", &acquiree_text_preserver_id.0 )?;
  assert!(found_acquiree_text_preserver, "acquiree_text_preserver SHOULD be in Tantivy index");

  Ok (( )) }

/// Recursively copy a directory
fn copy_dir_all(
  src: &PathBuf,
  dst: &PathBuf,
) -> std::io::Result<()> {
  fs::create_dir_all(dst)?;
  for entry in fs::read_dir(src)? {
    let entry : fs::DirEntry = entry?;
    let ty : fs::FileType = entry.file_type()?;
    if ty.is_dir() {
      copy_dir_all(
        &entry.path(),
        &dst.join(entry.file_name()),
      )?;
    } else {
      fs::copy( entry.path(),
                dst.join(entry.file_name()))?; }}
  Ok (( ))
}
