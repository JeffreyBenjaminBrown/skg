use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::types::save::{Merge, SaveSkgnode, DeleteSkgnode};
use crate::types::misc::{SkgConfig, ID};
use crate::types::orgnode::EditRequest;
use crate::types::orgnode::{OrgNode, OrgNodeKind};
use crate::types::skgnode::SkgNode;
use crate::util::{dedup_vector, setlike_vector_subtraction};
use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// PURPOSE: For each OrgNode with a merge instruction,
/// this creates a Merge:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - acquiree_to_delete: acquiree marked for deletion
///
/// TODO ? This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn instructiontriples_from_the_merges_in_an_orgnode_forest(
  forest: &Tree<OrgNode>,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<Merge>,
            Box<dyn Error>> {
  let mut merges: Vec<Merge> =
    Vec::new();
  for edge in forest.root().traverse() {
    if let ego_tree::iter::Edge::Open(node_ref) = edge {
      merges.extend(
        { let node_merges : Vec<Merge> =
            merges_from_the_merge_in_an_orgnode(
              { let orgnode: &OrgNode = node_ref.value();
                orgnode },
              config, driver ). await?;
          node_merges } ); } }
  Ok(merges) }

/// PURPOSE: The name and type signature say it all.
/// .
/// TODO ? It's confusing that this returns a vector,
/// because merges have a monogamy constraint:
/// nothing can merge with more than one other node per save.
/// Given that the metadata permits multiple '(merge _)' instructions,
/// though, this is a natural way to write the function.
async fn merges_from_the_merge_in_an_orgnode(
  node: &OrgNode,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<Merge>,
            Box<dyn Error>> {
  let mut merge_instructions: Vec<Merge> =
    Vec::new();
  if let OrgNodeKind::True(t) = &node.kind {
    if let Some(EditRequest::Merge(acquiree_id)) = &t.edit_request {
      let acquirer_id : &ID = &t.id;
      let acquirer_from_disk : SkgNode =
        skgnode_from_id(
          config, driver, acquirer_id ). await?;
      let acquiree_from_disk : SkgNode =
        skgnode_from_id(
          config, driver, acquiree_id ) . await?;
      let acquiree_text_preserver: SkgNode =
        create_acquiree_text_preserver ( &acquiree_from_disk );
      let updated_acquirer: SkgNode =
        three_merged_skgnodes( &acquirer_from_disk,
                               &acquiree_from_disk,
                               &acquiree_text_preserver)?;
      { merge_instructions.push(
          Merge {
            acquiree_text_preserver :
              SaveSkgnode(acquiree_text_preserver),
            updated_acquirer :
              SaveSkgnode(updated_acquirer),
            acquiree_to_delete :
              DeleteSkgnode {
                id: acquiree_id.clone(),
                source: acquiree_from_disk.source.clone() },
          } ); }} }
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
) -> Result<SkgNode, String> {
  let mut updated_acquirer: SkgNode =
    acquirer_from_disk.clone();
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
      vec![ acquiree_text_preserver.primary_id()? . clone() ];
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
    updated_acquirer . hides_from_its_subscriptions =
      Some( { let deduped_and_filtered : Vec<ID> =
                // if it's in 'new_contains', then it's not here
                setlike_vector_subtraction(
                    dedup_vector(combined),
                    &new_contains);
              deduped_and_filtered } ); }
  { // Combine overrides_view_of
    let mut combined : Vec<ID> =
      acquirer_from_disk . overrides_view_of
      . clone() . unwrap_or_default();
    combined.extend(
      acquiree_from_disk . overrides_view_of
        . clone() . unwrap_or_default() );
    updated_acquirer . overrides_view_of =
      Some(dedup_vector(combined)); }
  Ok(updated_acquirer) }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree.title),
    aliases: None,
    source: acquiree . source . clone(),
    ids: vec![ID(uuid::Uuid::new_v4().to_string())],
    body: acquiree.body.clone(),
    contains                     : Some(vec![]),
    subscribes_to                : Some(vec![]),
    hides_from_its_subscriptions : Some(vec![]),
    overrides_view_of            : Some(vec![]),
  }}
