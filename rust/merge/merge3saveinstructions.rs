use crate::file_io::read_node;
use crate::types::{MergeInstructionTriple, SkgConfig, OrgNode, SkgNode, NodeSaveAction, ID, NodeRequest};
use crate::util::path_from_pid;
use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Creates MergeInstructionTriples from an orgnode forest.
///
/// For each node with a merge instruction, this creates a MergeInstructionTriple:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - deleted_acquiree: acquiree marked for deletion
///
/// TODO: This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn instructiontriples_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<MergeInstructionTriple>, Box<dyn Error>> {
  let mut merge_instructions: Vec<MergeInstructionTriple> = Vec::new();

  // Walk the forest to find nodes with merge requests
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node: &OrgNode = node_ref.value();
        let node_merge_instructions : Vec<MergeInstructionTriple> =
          saveinstructions_from_the_merge_in_a_node(
            node, config)?;
        merge_instructions.extend(node_merge_instructions); }} }
  Ok(merge_instructions) }

/// Processes merge requests in a single OrgNode and returns MergeInstructionTriple.
fn saveinstructions_from_the_merge_in_a_node(
  node: &OrgNode,
  config: &SkgConfig,
) -> Result<Vec<MergeInstructionTriple>, Box<dyn Error>> {
  let mut merge_instructions: Vec<MergeInstructionTriple> = Vec::new();

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

      // Compute updated acquirer with all fields properly merged
      let updated_acquirer: SkgNode =
        compute_updated_acquirer(
          &acquirer_from_disk,
          &acquiree_from_disk,
          &acquiree_text_preserver);

      { // Create MergeInstructionTriple struct
        merge_instructions.push(
          MergeInstructionTriple {
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

/// Computes the updated acquirer node with all fields properly merged.
/// Returns a new SkgNode with:
/// - Combined IDs from both nodes
/// - contains: [acquiree_text_preserver] + acquirer's + acquiree's novel contents
///   - 'Novel' = not among the acquirer's contents
/// - Combined relationship fields (subscribes_to, overrides_view_of)
/// - Filtered hides_from_its_subscriptions (can't hide your own content)
fn compute_updated_acquirer(
  acquirer_from_disk: &SkgNode,
  acquiree_from_disk: &SkgNode,
  acquiree_text_preserver: &SkgNode,
) -> SkgNode {
  let mut updated_acquirer: SkgNode =
    acquirer_from_disk.clone();
  let acquiree_text_preserver_id: &ID =
    &acquiree_text_preserver.ids[0];

  // Append acquiree's IDs to acquirer's
  updated_acquirer.ids = acquirer_from_disk.ids.clone();
  for id in &acquiree_from_disk.ids {
    if !updated_acquirer.ids.contains(id) {
      updated_acquirer.ids.push(id.clone( )); }}

  // Update contains: [acquiree_text_preserver] + acquirer's old + (filtered) acquiree's old
  // Filtered: remove from acquiree's contents anything already in acquirer's contents
  let mut new_contains: Vec<ID> = vec![acquiree_text_preserver_id.clone()];
  if let Some(acquirer_contains) = &acquirer_from_disk.contains {
    new_contains.extend(acquirer_contains.clone()); }

  { // Filter out acquiree contents already in acquirer's.
    let acquirer_contains_set: HashSet<ID> = (
      acquirer_from_disk . contains . as_ref()
        . map( |v| v.iter().cloned().collect() )
        . unwrap_or_default () );
    if let Some(acquiree_contains) = &acquiree_from_disk.contains {
      let filtered_acquiree_contents: Vec<ID> = acquiree_contains
        .iter()
        .filter(|id| !acquirer_contains_set.contains(id))
        .cloned()
        .collect();
      new_contains.extend(filtered_acquiree_contents); }
    updated_acquirer.contains = Some(new_contains.clone() ); }

  // Compute acquirer_final_contains for filtering hides_from_its_subscriptions
  let acquirer_final_contains: HashSet<ID> =
    new_contains.iter().cloned().collect();

  // Combine subscribes_to
  updated_acquirer.subscribes_to = Some(
    acquirer_from_disk.subscribes_to.clone().unwrap_or_default()
      .into_iter()
      .chain ( acquiree_from_disk.subscribes_to
               .clone().unwrap_or_default())
      .collect() );

  // Combine hides_from_its_subscriptions (with filtering)
  let mut combined_hides: Vec<ID> = Vec::new();
  for list in [&acquirer_from_disk.hides_from_its_subscriptions,
               &acquiree_from_disk.hides_from_its_subscriptions] {
    if let Some(hides_list) = list {
      for hidden_id in hides_list {
        if !acquirer_final_contains.contains(hidden_id)
           && !combined_hides.contains(hidden_id) {
             combined_hides.push(hidden_id.clone()); }} }}
  updated_acquirer.hides_from_its_subscriptions = Some(combined_hides);

  // Combine overrides_view_of
  updated_acquirer.overrides_view_of = Some(
    acquirer_from_disk.overrides_view_of.clone().unwrap_or_default()
      .into_iter()
      .chain( acquiree_from_disk.overrides_view_of
              . clone().unwrap_or_default())
      .collect() );
  updated_acquirer }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED_{}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains                     : Some(vec![]),
    subscribes_to                : Some(vec![]),
    hides_from_its_subscriptions : Some(vec![]),
    overrides_view_of            : Some(vec![]),
  }}
