mod typedb;
mod fs;
mod tantivy;

use crate::file_io::read_node;
use crate::types::{SaveInstruction, SkgConfig, OrgNode, SkgNode, NodeSaveAction, ID, NodeRequest};
use crate::util::path_from_pid;
use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Creates SaveInstructions for merge operations from an orgnode forest.
///
/// For each node with a merge instruction, this creates three SaveInstructions:
/// - A new acquiree_text_preserver containing the acquiree's title and body
/// - An updated acquirer node with modified contents and extra IDs
/// - A deletion instruction for the acquiree node
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
        let node_instructions : Vec<SaveInstruction> =
          saveinstructions_from_the_merge_in_a_node(
            node, config)?;
        instructions.extend(node_instructions); }} }
  Ok(instructions) }

/// Processes merge requests in a single OrgNode and returns SaveInstructions.
fn saveinstructions_from_the_merge_in_a_node(
  node: &OrgNode,
  config: &SkgConfig,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
  let mut instructions: Vec<SaveInstruction> = Vec::new();

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

      // Create acquiree_text_preserver node
      let acquiree_text_preserver: SkgNode =
        create_acquiree_text_preserver(
          &acquiree_from_disk);
      let acquiree_text_preserver_id: &ID =
        &acquiree_text_preserver.ids[0];

      // Append acquiree's IDs to acquirer's
      let mut updated_acquirer: SkgNode =
        acquirer_from_disk.clone();
      updated_acquirer.ids = acquirer_from_disk.ids.clone();
      for id in &acquiree_from_disk.ids {
        if !updated_acquirer.ids.contains(id) {
          updated_acquirer.ids.push(id.clone( )); }}

      // Update contains: [acquiree_text_preserver] + acquirer's old + acquiree's old
      let mut new_contains: Vec<ID> = vec![acquiree_text_preserver_id.clone()];
      new_contains.extend(acquirer_from_disk.contains.clone());
      new_contains.extend(acquiree_from_disk.contains.clone());
      updated_acquirer.contains = new_contains;

      { // Add the three SaveInstructions
        instructions.push((
          acquiree_text_preserver,
          NodeSaveAction { indefinitive: false,
                           toDelete: false } ));
        instructions.push((
          updated_acquirer,
          NodeSaveAction { indefinitive: false,
                           toDelete: false } ));
        instructions.push((
          acquiree_from_disk,
          NodeSaveAction { indefinitive: false,
                           toDelete: true } )); }} }
  Ok(instructions) }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains: vec![],
    subscribes_to: Some(vec![]),
    hides_from_its_subscriptions: Some(vec![]),
    overrides_view_of: Some(vec![]),
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
  tantivy_index : &::tantivy::Index,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!(
    "Merging nodes in TypeDB, FS, and Tantivy, in that order ..." );
  let db_name : &str = &config.db_name;

  // Categorize merge instructions once for all three systems
  let (acquiree_text_preservers, updated_acquirers, deleted_acquirees) =
    categorize_merge_instructions(&instructions);

  { println!( "1) Merging in TypeDB database '{}' ...", db_name );
    typedb::merge_nodes_in_typedb (
      db_name,
      driver,
      &instructions,
      acquiree_text_preservers.clone(),
      updated_acquirers.clone(),
      deleted_acquirees.clone()
    ). await ?;
    println!( "   TypeDB merge complete." ); }
  { println!( "2) Merging in filesystem ..." );
    fs::merge_nodes_in_fs (
      instructions.clone (),
      config.clone (),
      acquiree_text_preservers.clone(),
      updated_acquirers.clone(),
      deleted_acquirees.clone()
    ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    tantivy::merge_nodes_in_tantivy (
      &instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }

/// Categorizes merge SaveInstructions into their three components.
/// Returns (acquiree_text_preservers, updated_acquirers, deleted_acquirees).
fn categorize_merge_instructions(
  instructions: &[SaveInstruction]
) -> (Vec<&SaveInstruction>, Vec<&SaveInstruction>, Vec<&SaveInstruction>) {
  let mut acquiree_text_preservers: Vec<&SaveInstruction> = Vec::new();
  let mut updated_acquirers: Vec<&SaveInstruction> = Vec::new();
  let mut deleted_acquirees: Vec<&SaveInstruction> = Vec::new();

  let mut i: usize = 0;
  while i < instructions.len() {
    let instr: &SaveInstruction = &instructions[i];

    if instr.0.title.starts_with("MERGED: ") {
      // This is an acquiree_text_preserver
      acquiree_text_preservers.push(instr);

      // Next should be updated acquirer
      if i + 1 < instructions.len() {
        updated_acquirers.push(&instructions[i + 1]);
      }

      // Next should be deleted acquiree
      if i + 2 < instructions.len() {
        deleted_acquirees.push(&instructions[i + 2]);
      }

      i += 3;
    } else {
      i += 1;
    }
  }

  (acquiree_text_preservers, updated_acquirers, deleted_acquirees)
}
