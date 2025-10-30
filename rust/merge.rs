mod typedb;
mod fs;
mod tantivy;

use crate::file_io::read_node;
use crate::types::{Merge3SaveInstructions, SkgConfig, OrgNode, SkgNode, NodeSaveAction, ID, NodeRequest};
use crate::util::path_from_pid;
use ego_tree::Tree;
use std::error::Error;
use ::tantivy::Index;
use typedb_driver::TypeDBDriver;

/// Creates Merge3SaveInstructions for merge operations from an orgnode forest.
///
/// For each node with a merge instruction, this creates a Merge3SaveInstructions:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - deleted_acquiree: acquiree marked for deletion
///
/// TODO: This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<Merge3SaveInstructions>, Box<dyn Error>> {
  let mut merge_instructions: Vec<Merge3SaveInstructions> = Vec::new();

  // Walk the forest to find nodes with merge requests
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node: &OrgNode = node_ref.value();
        let node_merge_instructions : Vec<Merge3SaveInstructions> =
          saveinstructions_from_the_merge_in_a_node(
            node, config)?;
        merge_instructions.extend(node_merge_instructions); }} }
  Ok(merge_instructions) }

/// Processes merge requests in a single OrgNode and returns Merge3SaveInstructions.
fn saveinstructions_from_the_merge_in_a_node(
  node: &OrgNode,
  config: &SkgConfig,
) -> Result<Vec<Merge3SaveInstructions>, Box<dyn Error>> {
  let mut merge_instructions: Vec<Merge3SaveInstructions> = Vec::new();

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

      { // Create Merge3SaveInstructions struct
        merge_instructions.push(
          Merge3SaveInstructions {
            acquiree_text_preserver : (
              acquiree_text_preserver,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ),
            updated_acquirer : (
              updated_acquirer,
              NodeSaveAction { indefinitive: false,
                               toDelete: false } ),
            deleted_acquiree : (
              acquiree_from_disk,
              NodeSaveAction { indefinitive: false,
                               toDelete: true } ),
          } ); }} }
  Ok(merge_instructions) }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED_{}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains: vec![],
    subscribes_to: Some(vec![]),
    hides_from_its_subscriptions: Some(vec![]),
    overrides_view_of: Some(vec![]),
  }}

/// Merges nodes in the graph by applying Merge3SaveInstructions.
/// Updates three systems in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes_in_graph (
  merge_instructions : Vec<Merge3SaveInstructions>,
  config             : SkgConfig,
  tantivy_index      : &Index,
  driver             : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!(
    "Merging nodes in TypeDB, FS, and Tantivy, in that order ..." );
  let db_name : &str = &config.db_name;

  { println!( "1) Merging in TypeDB database '{}' ...", db_name );
    typedb::merge_nodes_in_typedb (
      db_name,
      driver,
      &merge_instructions
    ). await ?;
    println!( "   TypeDB merge complete." ); }
  { println!( "2) Merging in filesystem ..." );
    fs::merge_nodes_in_fs (
      config.clone (),
      &merge_instructions
    ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    tantivy::merge_nodes_in_tantivy (
      &merge_instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }
