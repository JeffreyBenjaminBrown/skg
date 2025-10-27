use crate::file_io::read_node;
use crate::types::{SaveInstruction, SkgConfig, OrgNode, SkgNode, NodeSaveAction, ID, NodeRequest};
use crate::util::path_from_pid;
use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Creates SaveInstructions for merge operations from an orgnode forest.
///
/// For each node with a merge instruction, this creates three SaveInstructions:
/// 1. A new MERGED node containing the acquiree's title and body
/// 2. An updated acquirer node with modified contents and extra IDs
/// 3. A deletion instruction for the acquiree node
///
/// TODO: This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let mut instructions = Vec::new();

  // Walk the forest to find nodes with merge requests
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node = node_ref.value();

        // Check if this node has merge requests
        for request in &node.metadata.code.nodeRequests {
          if let NodeRequest::Merge(acquiree_id) = request {
            let acquirer_id = node.metadata.id.as_ref()
              .ok_or("Node with merge request must have an ID")?;

            // Fetch acquirer and acquiree from disk
            let acquirer_from_disk =
              read_node(&path_from_pid(config, acquirer_id.clone()))?;
            let acquiree_from_disk =
              read_node(&path_from_pid(config, acquiree_id.clone()))?;

            // Create MERGED node
            let merged_node = create_merged_node(&acquiree_from_disk);
            let merged_id = &merged_node.ids[0];

            // Create updated acquirer with:
            // - ids: [acquirer_id, acquiree_id] (plus any extra_ids from both)
            // - contains: [MERGED_ID] + acquirer's old contains + acquiree's contains
            let mut updated_acquirer = acquirer_from_disk.clone();

            // Update IDs: start with acquirer's current ids, add acquiree_id
            updated_acquirer.ids = acquirer_from_disk.ids.clone();
            if !updated_acquirer.ids.contains(&acquiree_id) {
              updated_acquirer.ids.push(acquiree_id.clone());
            }

            // Update contains: [MERGED] + acquirer's old + acquiree's old
            let mut new_contains = vec![merged_id.clone()];
            new_contains.extend(acquirer_from_disk.contains.clone());
            new_contains.extend(acquiree_from_disk.contains.clone());
            updated_acquirer.contains = new_contains;

            // Add the three SaveInstructions
            instructions.push((
              merged_node,
              NodeSaveAction { indefinitive: false, toDelete: false }
            ));
            instructions.push((
              updated_acquirer,
              NodeSaveAction { indefinitive: false, toDelete: false }
            ));
            instructions.push((
              acquiree_from_disk,
              NodeSaveAction { indefinitive: false, toDelete: true }
            )); }} }} }
  Ok(instructions) }

/// Create a MERGED node from the acquiree's data
fn create_merged_node(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains: vec![],
    subscribes_to: vec![],
    hides_from_its_subscriptions: vec![],
    overrides_view_of: vec![],
  }}
