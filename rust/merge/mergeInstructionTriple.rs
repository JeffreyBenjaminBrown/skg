use crate::media::file_io::read_node;
use crate::types::{MergeInstructionTriple, SkgConfig, OrgNode, SkgNode, NonMerge_NodeAction, ID, EditRequest};
use crate::util::{path_from_pid, dedup_vector, setlike_vector_subtraction};
use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// PURPOSE: For each OrgNode with a merge instruction,
/// this creates a MergeInstructionTriple:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - acquiree_to_delete: acquiree marked for deletion
///
/// TODO ? This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn instructiontriples_from_the_merges_in_an_orgnode_forest(
  forest: &[Tree<OrgNode>],
  config: &SkgConfig,
  _driver: &TypeDBDriver,
) -> Result<Vec<MergeInstructionTriple>,
            Box<dyn Error>> {
  let mut triples: Vec<MergeInstructionTriple> =
    Vec::new();
  for tree in forest {
    for edge in tree.root().traverse() {
      if let ego_tree::iter::Edge::Open(node_ref) = edge {
        let node: &OrgNode = node_ref.value();
        let node_triples : Vec<MergeInstructionTriple> =
          saveinstructions_from_the_merge_in_an_orgnode(
            node, config)?;
        triples.extend(node_triples); }} }
  Ok(triples) }

/// PURPOSE: The name and type signature say it all.
/// .
/// TODO ? It's confusing that this returns a vector,
/// because merges have a monogamy constraint:
/// nothing can merge with more than one other node per save.
/// Given that the metadata permits multiple '(merge _)' instructions,
/// though, this is a natural way to write the function.
fn saveinstructions_from_the_merge_in_an_orgnode(
  node: &OrgNode,
  config: &SkgConfig,
) -> Result<Vec<MergeInstructionTriple>,
            Box<dyn Error>> {
  let mut merge_instructions: Vec<MergeInstructionTriple> =
    Vec::new();
  if let Some(EditRequest::Merge(acquiree_id))
    = &node.metadata.code.editRequest {
      let acquirer_id : &ID =
        node.metadata.id.as_ref()
        .ok_or("Node with merge request must have an ID")?;
      let acquirer_from_disk: SkgNode =
        // TODO: If this fails, it should report how.
        read_node ( &path_from_pid ( config, acquirer_id.clone() ))?;
      let acquiree_from_disk: SkgNode =
        // TODO: If this fails, it should report how.
        read_node ( &path_from_pid ( config, acquiree_id.clone() ))?;
      let acquiree_text_preserver: SkgNode =
        create_acquiree_text_preserver ( &acquiree_from_disk );
      let updated_acquirer: SkgNode =
        three_merged_skgnodes( &acquirer_from_disk,
                               &acquiree_from_disk,
                               &acquiree_text_preserver);
      { merge_instructions.push(
          MergeInstructionTriple {
            acquiree_text_preserver : (
              acquiree_text_preserver,
              NonMerge_NodeAction::SaveDefinitive ),
            updated_acquirer : (
              updated_acquirer,
              NonMerge_NodeAction::SaveDefinitive ),
            acquiree_to_delete : (
              acquiree_from_disk,
              NonMerge_NodeAction::Delete ),
          } ); }}
  Ok(merge_instructions) }

/// Computes the updated acquirer node with all fields properly merged.
/// Returns a new SkgNode with:
/// - Combined IDs from both nodes
/// - contains: [acquiree_text_preserver] + acquirer's + acquiree's novel contents
///   - 'Novel' = not among the acquirer's contents
/// - Combined relationship fields (subscribes_to, overrides_view_of)
/// - Filtered hides_from_its_subscriptions (can't hide your own content)
fn three_merged_skgnodes(
  acquirer_from_disk: &SkgNode,
  acquiree_from_disk: &SkgNode,
  acquiree_text_preserver: &SkgNode,
) -> SkgNode {
  let mut updated_acquirer: SkgNode =
    acquirer_from_disk.clone();
  let acquiree_text_preserver_id: &ID =
    &acquiree_text_preserver.ids[0];

  { // Append acquiree's IDs to acquirer's.
    let mut combined_ids : Vec<ID> =
      acquirer_from_disk.ids.clone();
    combined_ids.extend(
      acquiree_from_disk.ids.clone() );
    updated_acquirer.ids = (
      // this dedup is kind of absurdly defensive, but cheap
      dedup_vector(combined_ids) ); }

  let new_contains : Vec<ID> = {
    // [preserver] + acquirer's old content + acquiree's old content
    let mut combined : Vec<ID> =
      vec![acquiree_text_preserver_id.clone()];
    combined.extend(
      acquirer_from_disk.contains.clone().unwrap_or_default() );
    combined.extend(
      acquiree_from_disk.contains.clone().unwrap_or_default() );
    dedup_vector(combined) };
  updated_acquirer.contains = // update 'contains'
    Some(new_contains.clone());

  { // Combine subscribes_to
    let mut combined : Vec<ID> =
      acquirer_from_disk.subscribes_to.clone().unwrap_or_default();
    combined.extend(
      acquiree_from_disk.subscribes_to.clone().unwrap_or_default() );
    updated_acquirer.subscribes_to = Some(dedup_vector(combined)); }

  { // Combine hides_from_its_subscriptions,
    // filtering to hide nothing that the acquirer contains.
    let mut combined : Vec<ID> =
      acquirer_from_disk . hides_from_its_subscriptions
      . clone() . unwrap_or_default();
    combined.extend(
      acquiree_from_disk . hides_from_its_subscriptions
        . clone() . unwrap_or_default() );
    let deduped_and_filtered : Vec<ID> =
      // if it's in 'new_contains', then it's not here
      setlike_vector_subtraction(
        dedup_vector(combined),
        &new_contains);
    updated_acquirer . hides_from_its_subscriptions =
      Some(deduped_and_filtered); }

  { // Combine overrides_view_of
    let mut combined : Vec<ID> =
      acquirer_from_disk . overrides_view_of
      . clone() . unwrap_or_default();
    combined.extend(
      acquiree_from_disk . overrides_view_of
        . clone() . unwrap_or_default() );
    updated_acquirer . overrides_view_of =
      Some(dedup_vector(combined)); }

  updated_acquirer }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains                     : Some(vec![]),
    subscribes_to                : Some(vec![]),
    hides_from_its_subscriptions : Some(vec![]),
    overrides_view_of            : Some(vec![]),
  }}
