// cargo test merge::merge_nodes_in_graph

use skg::merge::{
  saveinstructions_from_the_merges_in_an_orgnode_forest,
  merge_nodes_in_graph};
use skg::test_utils::{run_with_test_db, all_pids_from_typedb, tantivy_contains_id};
use skg::types::{ID, OrgNode, OrgnodeMetadata, NodeRequest, SkgConfig, SkgNode, NodeSaveAction};
use skg::file_io::read_node;
use skg::util::path_from_pid;
use skg::typedb::search::{
  contains_from_pids,
  extra_ids_from_pid,
  find_related_nodes};
use skg::types::TantivyIndex;

use ego_tree::Tree;
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::path::Path;
use typedb_driver::TypeDBDriver;

#[test]
fn test_merge_2_into_1() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-merge-2-into-1",
    "tests/merge/merge_nodes_in_graph/fixtures",
    "/tmp/tantivy-test-merge-2-into-1",
    |config, driver| Box::pin(async move {
      test_merge_2_into_1_impl(config, driver).await?;
      Ok(( ))
    } )) }

async fn test_merge_2_into_1_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Create orgnode forest with node 1 requesting to merge node 2
  let mut node_requests: HashSet<NodeRequest> = HashSet::new();
  node_requests.insert(NodeRequest::Merge(ID::from("2")));

  let org_node_1: OrgNode = OrgNode {
    metadata: OrgnodeMetadata {
      id: Some(ID::from("1")),
      viewData: Default::default(),
      code: skg::types::OrgnodeCode {
        relToParent: skg::types::RelToParent::Content,
        indefinitive: false,
        repeat: false,
        toDelete: false,
        nodeRequests: node_requests, }, },
    title: "1".to_string(),
    body: None, };
  let forest: Vec<Tree<OrgNode>> =
    vec![Tree::new(org_node_1)];

  // Generate SaveInstructions from merge request
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    saveinstructions_from_the_merges_in_an_orgnode_forest(
      &forest,
      config,
      driver,
  ).await?;

  // Expect MERGED node, updated acquirer, deleted acquiree
  assert_eq!(instructions.len(),
             3,
             "Should have 3 SaveInstructions");

  // Create a temporary Tantivy index for testing,
  // in the same directory that 'run_with_test_db' uses.
  let mut schema_builder: tantivy::schema::SchemaBuilder =
    tantivy::schema::Schema::builder();
  let id_field: tantivy::schema::Field =
    schema_builder.add_text_field(
      "id",
      tantivy::schema::STRING | tantivy::schema::STORED);
  let title_or_alias_field: tantivy::schema::Field =
    schema_builder.add_text_field(
      "title_or_alias",
      tantivy::schema::TEXT | tantivy::schema::STORED);
  let tantivy_schema: tantivy::schema::Schema =
    schema_builder.build();
  let tantivy_index: tantivy::Index =
    tantivy::Index::create_in_dir(
      &config.tantivy_folder, tantivy_schema)?;
  let tantivy_index_wrapper: TantivyIndex = TantivyIndex {
    index: std::sync::Arc::new(tantivy_index.clone()),
    id_field,
    title_or_alias_field, };

  merge_nodes_in_graph(
    instructions.clone(),
    config.clone(),
    &tantivy_index,
    driver,
  ).await?;

  // Verify results
  verify_typedb_after_merge_2_into_1(
    config, driver).await?;
  verify_filesystem_after_merge_2_into_1(
    config, &instructions)?;
  verify_tantivy_after_merge_2_into_1(
    &tantivy_index_wrapper, &instructions )?;
  Ok(( )) }

async fn verify_typedb_after_merge_2_into_1 (
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let db_name: &String = &config.db_name;

  // 1. Node 2 should be gone from TypeDB as primary node
  let all_primary_node_ids: HashSet<ID> =
    all_pids_from_typedb(
      db_name, driver ). await ?;
  assert!(!all_primary_node_ids.contains(&ID::from("2")),
          "PID 2 should not exist. It was merged and deleted.");

  // 2. Node 1 should have extra_ids: 2 and 2-extra-id
  let node_1_extra_ids: Vec<ID> =
    extra_ids_from_pid(
      db_name, driver, &ID::from("1")).await?;
  assert!(node_1_extra_ids.contains(&ID::from("2")),
          "Node 1 should have extra_id '2'");
  assert!(node_1_extra_ids.contains(&ID::from("2-extra-id")),
          "Node 1 should have extra_id '2-extra-id'");

  // 3. Node 1 should contain 6 things: [MERGED_ID, 11, 12, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  let input_pids: Vec<ID> = vec![ID::from("1")];
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids(
      db_name, driver, &input_pids ). await ?;

  let node_1_contents: &HashSet<ID> =
    container_to_contents.get(
      &ID::from("1"))
    . ok_or("Node 1 should have contains relationships")?;

  assert_eq!(node_1_contents.len(), 6,
             "Node 1 should contain 6 items after merge");
  // MERGED_ID is a UUID we don't know, but it should be in the set.
  // We can, however, test for the other five:
  assert!(node_1_contents.contains(&ID::from("11")));
  assert!(node_1_contents.contains(&ID::from("12")));
  assert!(node_1_contents.contains(&ID::from("21")));
  assert!(node_1_contents.contains(&ID::from("22")));
  assert!(node_1_contents.contains(
    &ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")));

  // In this test,
  // relationships beyond contains and extra_id are ignored,
  // because nothing happens to them.
  // See the other test, of merging 1 into 2, for that action.
  Ok (( )) }

fn verify_filesystem_after_merge_2_into_1(
  config: &SkgConfig,
  instructions: &[(SkgNode, NodeSaveAction)],
) -> Result<(), Box<dyn Error>> {
  let node_2_path: String = path_from_pid ( config,
                                            ID::from("2"));
  assert!( !Path::new(&node_2_path).exists(),
            "2.skg should be deleted" );

  // 1.skg should be updated
  let node_1: SkgNode = read_node(
    &Path::new(&path_from_pid(config, ID::from("1")) )) ?;
  assert_eq!(node_1.ids.len(), 3, "Node 1 should have 3 ids");
  assert_eq!(&node_1.ids[0], &ID::from("1"));
  assert_eq!(&node_1.ids[1], &ID::from("2"));
  assert_eq!(&node_1.ids[2], &ID::from("2-extra-id"));

  // Should have [MERGED_ID, 11, 12, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2]
  assert_eq!( node_1.contains.len(), 6,
              "Node 1 should contain 6 items");

  let merged_id: &ID = &instructions[0].0.ids[0];
  assert_eq!(&node_1.contains[0], merged_id,
             "First content should be MERGED node");
  assert_eq!(&node_1.contains[1], &ID::from("11"));
  assert_eq!(&node_1.contains[2], &ID::from("12"));
  assert_eq!(&node_1.contains[3], &ID::from("21"));
  assert_eq!(&node_1.contains[4], &ID::from("22"));
  assert_eq!(
    &node_1.contains[5],
    &ID::from(
      "hidden-from-subscriptions-of-1-but-in-content-of-2"));

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

  let merged_node_path: String =
    path_from_pid ( config, merged_id.clone() );
  assert!( Path::new(&merged_node_path).exists(),
           "MERGED node file should exist" );

  let merged_node: SkgNode = read_node(
    &Path::new(&merged_node_path )) ?;
  assert!(merged_node.title.starts_with("MERGED: "));
  assert_eq!(merged_node.title, "MERGED: 2");
  assert_eq!(merged_node.body, Some("2 body".to_string()));
  assert_eq!(merged_node.contains.len(), 0,
             "MERGED node should have no contents");

  // MERGED node should have Some([]) for relationship fields
  // (when read from disk, missing fields become Some([]), not None)
  assert_eq!(merged_node.subscribes_to, Some(vec![]),
             "MERGED node should have empty subscribes_to");
  assert_eq!(merged_node.hides_from_its_subscriptions, Some(vec![]),
             "MERGED node should have empty hides_from_its_subscriptions");
  assert_eq!(merged_node.overrides_view_of, Some(vec![]),
             "MERGED node should have empty overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_2_into_1(
  tantivy_index: &TantivyIndex,
  instructions: &[(SkgNode, NodeSaveAction)],
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

  // Search for MERGED node - SHOULD find it
  let merged_id: &ID = &instructions[0].0.ids[0];
  let found_merged: bool =
    tantivy_contains_id(tantivy_index, "MERGED: 2", &merged_id.0)?;
  assert!(found_merged, "MERGED node SHOULD be in Tantivy index");
  Ok (( )) }


// ============================================================
// Test: Merging 1 into 2
// ============================================================

#[test]
fn test_merge_1_into_2() -> Result<(), Box<dyn Error>> {
  run_with_test_db(
    "skg-test-merge-1-into-2",
    "tests/merge/merge_nodes_in_graph/fixtures",
    "/tmp/tantivy-test-merge-1-into-2",
    |config, driver| Box::pin(async move {
      test_merge_1_into_2_impl(config, driver).await?;
      Ok(( ))
    } )) }

async fn test_merge_1_into_2_impl(
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // Create orgnode forest with node 2 requesting to merge node 1
  let mut node_requests: HashSet<NodeRequest> = HashSet::new();
  node_requests.insert(NodeRequest::Merge(ID::from("1")));

  let org_node_2: OrgNode = OrgNode {
    metadata: OrgnodeMetadata {
      id: Some(ID::from("2")),
      viewData: Default::default(),
      code: skg::types::OrgnodeCode {
        relToParent: skg::types::RelToParent::Content,
        indefinitive: false,
        repeat: false,
        toDelete: false,
        nodeRequests: node_requests, }, },
    title: "2".to_string(),
    body: None, };
  let forest: Vec<Tree<OrgNode>> =
    vec![Tree::new(org_node_2)];

  // Generate SaveInstructions from merge request
  let instructions: Vec<(SkgNode, NodeSaveAction)> =
    saveinstructions_from_the_merges_in_an_orgnode_forest(
      &forest,
      config,
      driver,
  ).await?;

  // Expect MERGED node, updated acquirer, deleted acquiree
  assert_eq!(instructions.len(),
             3,
             "Should have 3 SaveInstructions");

  // Create a temporary Tantivy index for testing
  let mut schema_builder: tantivy::schema::SchemaBuilder =
    tantivy::schema::Schema::builder();
  let id_field: tantivy::schema::Field =
    schema_builder.add_text_field(
      "id",
      tantivy::schema::STRING | tantivy::schema::STORED);
  let title_or_alias_field: tantivy::schema::Field =
    schema_builder.add_text_field(
      "title_or_alias",
      tantivy::schema::TEXT | tantivy::schema::STORED);
  let tantivy_schema: tantivy::schema::Schema =
    schema_builder.build();
  let tantivy_index: tantivy::Index =
    tantivy::Index::create_in_dir(
      &config.tantivy_folder, tantivy_schema)?;
  let tantivy_index_wrapper: TantivyIndex = TantivyIndex {
    index: std::sync::Arc::new(tantivy_index.clone()),
    id_field,
    title_or_alias_field, };

  merge_nodes_in_graph(
    instructions.clone(),
    config.clone(),
    &tantivy_index,
    driver,
  ).await?;

  // Verify results
  verify_typedb_after_merge_1_into_2(
    config, driver, &instructions).await?;
  verify_filesystem_after_merge_1_into_2(
    config, &instructions)?;
  verify_tantivy_after_merge_1_into_2(
    &tantivy_index_wrapper, &instructions)?;
  Ok(( )) }

async fn verify_typedb_after_merge_1_into_2 (
  config: &SkgConfig,
  driver: &TypeDBDriver,
  instructions: &[(SkgNode, NodeSaveAction)],
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

  let input_pids: Vec<ID> = vec![ID::from("2")];
  let (container_to_contents, _content_to_containers)
    : (HashMap<ID, HashSet<ID>>, HashMap<ID, HashSet<ID>>)
    = contains_from_pids(
      db_name, driver, &input_pids ). await ?;
  let node_2_contents: &HashSet<ID> =
    container_to_contents.get(&ID::from("2"))
    .ok_or("Node 2 should have contains relationships")?;

  assert_eq!(node_2_contents.len(), 6,
             "Node 2 should contain 6 items after merge");
  // MERGED_ID is a UUID we don't know, but it should be in the set.
  // We can test for the other five:
  assert!(node_2_contents.contains(&ID::from("11")));
  assert!(node_2_contents.contains(&ID::from("12")));
  assert!(node_2_contents.contains(&ID::from("21")));
  assert!(node_2_contents.contains(&ID::from("22")));
  assert!(node_2_contents.contains(
    &ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")));

  // 4. Hyperlinks should be rerouted
  // The old link from 1 to 1-links-to should now be from MERGED,
  // because MERGED has what was node 1's body text.
  let merged_id: &ID = &instructions[0].0.ids[0];
  let merged_hyperlink_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, merged_id,
    "hyperlinks_to", "source", "dest" ). await ?;
  assert!(
    merged_hyperlink_dests.contains(&ID::from("1-links-to")),
    "MERGED node should hyperlink to 1-links-to");

  // - Node 2 should NOT have the outbound hyperlink from node 1
  //   (the hyperlink is in the text, which went to MERGED)
  let node_2_hyperlink_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("2"),
    "hyperlinks_to", "source", "dest" ). await ?;
  assert!(
    !node_2_hyperlink_dests.contains(&ID::from("1-links-to")),
    "Node 2 should NOT hyperlink to 1-links-to");

  // - The hyperlink from links-to-1 to 1 should now be from links-to-1 to 2
  //   (inbound hyperlinks target the acquirer because acquiree's ID becomes an extra_id)
  let links_to_1_dests: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("links-to-1"),
    "hyperlinks_to", "source", "dest" ). await ?;
  assert!(links_to_1_dests.contains(&ID::from("2")),
          "links-to-1 should hyperlink to 2 (rerouted from 1)");
  assert!(!links_to_1_dests.contains(&ID::from("1")),
          "links-to-1 should NOT hyperlink to 1 (1 was merged)");

  // 5. Subscribes relationships should be rerouted
  // - Node 1's subscribes_to [1-subscribes-to] should transfer to node 2
  let node_2_subscribes_to: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("2"),
    "subscribes", "subscriber", "subscribed_to" ). await ?;
  assert!(node_2_subscribes_to.contains(&ID::from("1-subscribes-to")),
          "Node 2 should subscribe to 1-subscribes-to");

  // - subscribes-to-1, which subscribed to [1],
  // should now subscribe to [2]
  let subscribes_to_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("subscribes-to-1"),
    "subscribes", "subscriber", "subscribed_to" ). await ?;
  assert!(subscribes_to_1_targets.contains(&ID::from("2")),
          "subscribes-to-1 should subscribe to 2 (rerouted from 1)");
  assert!(!subscribes_to_1_targets.contains(&ID::from("1")),
          "subscribes-to-1 should NOT subscribe to 1 (1 was merged)");

  // 6. Hides relationships should be processed correctly
  // - Node 1 hides [hidden-from-1s-subscriptions, hidden-from-subscriptions-of-1-but-in-content-of-2]
  // - After merge: Node 2 should hide [hidden-from-1s-subscriptions] but NOT [hidden-from-subscriptions-of-1-but-in-content-of-2]
  //   because hidden-from-subscriptions-of-1-but-in-content-of-2 IS in node 2's contents.
  let node_2_hides: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("2"), "hides_from_its_subscriptions", "hider", "hidden"
  ).await?;
  assert!(node_2_hides.contains(&ID::from("hidden-from-1s-subscriptions")),
          "Node 2 should hide hidden-from-1s-subscriptions (transferred from node 1)");
  assert!(!node_2_hides.contains(&ID::from("hidden-from-subscriptions-of-1-but-in-content-of-2")),
          "Node 2 should NOT hide hidden-from-subscriptions-of-1-but-in-content-of-2");

  // - hides-1-from-subscriptions hid [1, 11] → should now hide [11] only (relationship hiding 1 is DROPPED)
  let hides_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("hides-1-from-subscriptions"), "hides_from_its_subscriptions", "hider", "hidden"
  ).await?;
  assert!(hides_1_targets.contains(&ID::from("11")),
          "hides-1-from-subscriptions should still hide 11");
  assert!(!hides_1_targets.contains(&ID::from("1")) && !hides_1_targets.contains(&ID::from("2")),
          "hides-1-from-subscriptions should NOT hide 1 or 2 (relationship to merged node is DROPPED)");

  // 7. Overrides relationships should be processed correctly
  // - Node 1's overrides [1-overrides-view-of] should transfer to node 2
  let node_2_overrides: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("2"), "overrides_view_of", "overrider", "overridden"
  ).await?;
  assert!(node_2_overrides.contains(&ID::from("1-overrides-view-of")),
          "Node 2 should override view of 1-overrides-view-of (transferred from node 1)");

  // - overrides-view-of-1 that overrode [1] should have that relationship DROPPED (1 no longer exists to be replaced)
  let overrides_view_of_1_targets: HashSet<ID> = find_related_nodes(
    db_name, driver, &ID::from("overrides-view-of-1"),
    "overrides_view_of", "overrider", "overridden" ). await ?;
  assert!(!overrides_view_of_1_targets.contains(&ID::from("1")) &&
          !overrides_view_of_1_targets.contains(&ID::from("2")),
          "overrides-view-of-1 should NOT override 1 or 2 (relationship to merged node is DROPPED)");
  Ok (( )) }

fn verify_filesystem_after_merge_1_into_2(
  config: &SkgConfig,
  instructions: &[(SkgNode, NodeSaveAction)],
) -> Result<(), Box<dyn Error>> {
  let node_1_path: String = path_from_pid(config, ID::from("1"));
  assert!( !Path::new(&node_1_path).exists(),
            "1.skg should be deleted" );

  // 2.skg should be updated
  let node_2: SkgNode = read_node(
    &Path::new(&path_from_pid(config, ID::from("2")) )) ?;

  // Should have 3 ids: [2, 2-extra-id, 1]
  assert_eq!(node_2.ids.len(), 3, "Node 2 should have 3 ids");
  assert_eq!(&node_2.ids[0], &ID::from("2"));
  assert_eq!(&node_2.ids[1], &ID::from("2-extra-id"));
  assert_eq!(&node_2.ids[2], &ID::from("1"));

  // Should have [MERGED_ID, 21, 22, hidden-from-subscriptions-of-1-but-in-content-of-2, 11, 12]
  assert_eq!( node_2.contains.len(), 6,
              "Node 2 should contain 6 items");
  let merged_id: &ID = &instructions[0].0.ids[0];
  assert_eq!(&node_2.contains[0], merged_id,
             "First content should be MERGED node");
  assert_eq!(&node_2.contains[1], &ID::from("21"));
  assert_eq!(&node_2.contains[2], &ID::from("22"));
  assert_eq!(&node_2.contains[3], &ID::from(
    "hidden-from-subscriptions-of-1-but-in-content-of-2"));
  assert_eq!(&node_2.contains[4], &ID::from("11"));
  assert_eq!(&node_2.contains[5], &ID::from("12"));

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

  let merged_node_path: String = path_from_pid(
    config, merged_id.clone() );
  assert!( Path::new(&merged_node_path).exists(),
           "MERGED node file should exist" );

  let merged_node: SkgNode = read_node(
    &Path::new(&merged_node_path))?;
  assert!(merged_node.title.starts_with("MERGED: "));
  assert_eq!(merged_node.title, "MERGED: 1");
  assert_eq!(merged_node.body,
             Some ( "[[id:1-links-to][a link to 1-links-to]]"
                       .to_string() ));
  assert_eq!(merged_node.contains.len(), 0,
             "MERGED node should have no contents");

  // MERGED node should have Some([]) for relationship fields
  // (when read from disk, missing fields become Some([]), not None)
  // (these relationships stay with the acquirer, not the MERGED node)
  assert_eq!(merged_node.subscribes_to, Some(vec![]),
             "MERGED node should have empty subscribes_to");
  assert_eq!(merged_node.hides_from_its_subscriptions, Some(vec![]),
             "MERGED node should have empty hides_from_its_subscriptions");
  assert_eq!(merged_node.overrides_view_of, Some(vec![]),
             "MERGED node should have empty overrides_view_of");

  Ok(( )) }

fn verify_tantivy_after_merge_1_into_2(
  tantivy_index: &TantivyIndex,
  instructions: &[(SkgNode, NodeSaveAction)],
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

  // Search for MERGED node - SHOULD find it
  let merged_id: &ID = &instructions[0] . 0 . ids[0];
  let found_merged: bool = tantivy_contains_id(
    tantivy_index, "MERGED: 1", &merged_id.0 )?;
  assert!(found_merged, "MERGED node SHOULD be in Tantivy index");

  Ok (( )) }
