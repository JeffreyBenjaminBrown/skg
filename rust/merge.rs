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
  let mut instructions: Vec<SaveInstruction> = Vec::new();

  // Walk the forest to find nodes with merge requests
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node: &OrgNode = node_ref.value();

        // Check if this node has merge requests
        for request in &node.metadata.code.nodeRequests {
          if let NodeRequest::Merge(acquiree_id) = request {
            let acquirer_id = node.metadata.id.as_ref()
              .ok_or("Node with merge request must have an ID")?;

            // Fetch acquirer and acquiree from disk
            let acquirer_from_disk: SkgNode =
              read_node(&path_from_pid(config, acquirer_id.clone()))?;
            let acquiree_from_disk: SkgNode =
              read_node(&path_from_pid(config, acquiree_id.clone()))?;

            // Create MERGED node
            let merged_node: SkgNode = create_merged_node(
              &acquiree_from_disk);
            let merged_id: &ID = &merged_node.ids[0];

            // Append acquiree's IDs to acquirer's
            let mut updated_acquirer: SkgNode =
              acquirer_from_disk.clone();
            updated_acquirer.ids = acquirer_from_disk.ids.clone();
            for id in &acquiree_from_disk.ids {
              if !updated_acquirer.ids.contains(id) {
                updated_acquirer.ids.push(id.clone( )); }}

            // Update contains: [MERGED] + acquirer's old + acquiree's old
            let mut new_contains: Vec<ID> = vec![merged_id.clone()];
            new_contains.extend(acquirer_from_disk.contains.clone());
            new_contains.extend(acquiree_from_disk.contains.clone());
            updated_acquirer.contains = new_contains;

            // Add the three SaveInstructions
            instructions.push((
              merged_node,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ));
            instructions.push((
              updated_acquirer,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ));
            instructions.push((
              acquiree_from_disk,
              NodeSaveAction { indefinitive: false,
                               toDelete: true } )); }} }} }
  Ok(instructions) }

/// Create a MERGED node from the acquiree's data
fn create_merged_node(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains: vec![],
    subscribes_to: None,
    hides_from_its_subscriptions: None,
    overrides_view_of: None,
  }}

/// Merges nodes in the graph by applying merge SaveInstructions.
/// Updates three systems in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes_in_graph (
  instructions  : Vec<SaveInstruction>,
  config        : SkgConfig,
  tantivy_index : &tantivy::Index,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!(
    "Merging nodes in TypeDB, FS, and Tantivy, in that order ..." );
  let db_name : &str = &config.db_name;
  { println!( "1) Merging in TypeDB database '{}' ...", db_name );
    merge_nodes_in_typedb (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB merge complete." ); }
  { println!( "2) Merging in filesystem ..." );
    merge_nodes_in_fs (
      instructions.clone (), config.clone () ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    merge_nodes_in_tantivy (
      &instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }

/// Merges nodes in TypeDB by applying merge SaveInstructions.
async fn merge_nodes_in_typedb (
  _db_name      : &str,
  _driver       : &TypeDBDriver,
  _instructions : &[SaveInstruction],
) -> Result < (), Box<dyn Error> > {
  // TODO: Implement TypeDB merge operations:
  // - Add extra_ids from acquiree to acquirer
  // - Reroute relationships according to merge rules
  // - Delete acquiree node
  todo!("merge_nodes_in_typedb not yet implemented") }

/// Merges nodes in filesystem by applying merge SaveInstructions.
fn merge_nodes_in_fs (
  _instructions : Vec<SaveInstruction>,
  _config       : SkgConfig,
) -> Result < (), Box<dyn Error> > {
  // TODO: Implement filesystem merge operations:
  // - Create MERGED node .skg file
  // - Update acquirer .skg file with new IDs and contents
  // - Delete acquiree .skg file
  todo!("merge_nodes_in_fs not yet implemented") }

/// Merges nodes in Tantivy by applying merge SaveInstructions.
fn merge_nodes_in_tantivy (
  _instructions : &[SaveInstruction],
  _index        : &tantivy::Index,
) -> Result < (), Box<dyn Error> > {
  // TODO: Implement Tantivy merge operations:
  // - Remove acquiree from search index
  // - Update acquirer in search index
  // - Add MERGED node to search index
  todo!("merge_nodes_in_tantivy not yet implemented") }
