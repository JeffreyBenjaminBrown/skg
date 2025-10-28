// cargo test merge::merge_nodes_in_graph

use skg::merge::{
  saveinstructions_from_the_merges_in_an_orgnode_forest,
  merge_nodes_in_graph};
use skg::test_utils::run_with_test_db;
use skg::types::{ID, OrgNode, OrgnodeMetadata, NodeRequest, SkgConfig};
use skg::file_io::read_node;
use skg::util::path_from_pid;

use ego_tree::Tree;
use std::collections::HashSet;
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
  let mut node_requests = HashSet::new();
  node_requests.insert(NodeRequest::Merge(ID::from("2")));

  let org_node_1 = OrgNode {
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
  let instructions =
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
  let tantivy_schema = tantivy::schema::Schema::builder().build();
  let tantivy_index =
    tantivy::Index::create_in_dir(
      &config.tantivy_folder, tantivy_schema)?;

  merge_nodes_in_graph(
    instructions.clone(),
    config.clone(),
    &tantivy_index,
    driver,
  ).await?;

  // Verify results - these won't run until merge_nodes_in_graph is implemented
  verify_typedb_after_merge_2_into_1(config, driver).await?;
  verify_filesystem_after_merge_2_into_1(config, &instructions)?;
  verify_tantivy_after_merge_2_into_1(&tantivy_index)?;
  Ok(( )) }

async fn verify_typedb_after_merge_2_into_1 (
  _config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  // TODO: Implement TypeDB verification once merge_nodes_in_typedb is implemented
  // 1. Node 2 should be gone (or resolve to node 1 via extra_id)
  // 2. Node 1 should have extra_ids: 2 and 2-extra-id
  // 3. Node 1 should contain 5 things: [MERGED_ID, 11, 12, 21, 22]
  // 4. Hyperlinks should be rerouted
  // 5. Subscribes relationships should be rerouted
  // 6. Hides relationships should be processed correctly
  // 7. Overrides relationships should be processed correctly
  Ok(( )) }

fn verify_filesystem_after_merge_2_into_1(
  config: &SkgConfig,
  instructions: &[(skg::types::SkgNode, skg::types::NodeSaveAction)],
) -> Result<(), Box<dyn Error>> {
  let node_2_path = path_from_pid(config, ID::from("2"));
  assert!(
    !Path::new(&node_2_path).exists(),
    "2.skg should be deleted" );

  // 1.skg should be updated
  let node_1 = read_node(
    &Path::new(&path_from_pid(config, ID::from("1")) )) ?;
  assert_eq!(node_1.ids.len(), 3, "Node 1 should have 3 ids");
  assert!(node_1.ids.contains(&ID::from("1")));
  assert!(node_1.ids.contains(&ID::from("2")));
  assert!(node_1.ids.contains(&ID::from("2-extra-id")));
  assert_eq!( node_1.contains.len(), 5,
              // Should have [MERGED_ID, 11, 12, 21, 22]
              "Node 1 should contain 5 items");

  let merged_id = &instructions[0].0.ids[0];
  assert_eq!(&node_1.contains[0], merged_id,
             "First content should be MERGED node");
  assert_eq!(&node_1.contains[1], &ID::from("11"));
  assert_eq!(&node_1.contains[2], &ID::from("12"));
  assert_eq!(&node_1.contains[3], &ID::from("21"));
  assert_eq!(&node_1.contains[4], &ID::from("22"));

  let merged_node_path = path_from_pid(config, merged_id.clone());
  assert!( Path::new(&merged_node_path).exists(),
           "MERGED node file should exist" );

  let merged_node = read_node(&Path::new(&merged_node_path))?;
  assert!(merged_node.title.starts_with("MERGED: "));
  assert_eq!(merged_node.title, "MERGED: 2");
  assert_eq!(merged_node.body, Some("2 body".to_string()));
  assert_eq!(merged_node.contains.len(), 0,
             "MERGED node should have no contents");
  Ok(( )) }

fn verify_tantivy_after_merge_2_into_1(
  _index: &tantivy::Index,
) -> Result<(), Box<dyn Error>> {
  // 1. "2" should NOT turn up as a standalone node in search results
  // TODO: Search for "2" and verify node 2 is not in results

  // 2. "1" should turn up in search results
  // TODO: Search for "1" and verify node 1 is in results

  // 3. "MERGED: 2" should turn up in search results
  // TODO: Search for "MERGED: 2" and verify it's in results

  Ok(())
}
