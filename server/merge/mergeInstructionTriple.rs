use crate::dbs::filesystem::one_node::skgnode_from_id;
use crate::types::save::{Merge, SaveNode, DeleteNode};
use crate::types::misc::{MSV, SkgConfig, ID};
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, TrueNode};
use crate::types::skgnode::SkgNode;
use crate::types::list::dedup_vector;
use crate::util::setlike_vector_subtraction;

use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// PURPOSE: For each ViewNode with a merge instruction, creates a Merge:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - acquiree_to_delete: acquiree marked for deletion
///
/// TODO ? This is slightly inefficient, walking the forest yet again. It would be faster, but more complex, to collect a list of viewnodes with merge instructions during one of the other walks of the forest.
pub async fn instructiontriples_from_the_merges_in_an_viewnode_forest(
  forest: &Tree<ViewNode>,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<Merge>,
            Box<dyn Error>> {
  let mut merges: Vec<Merge> =
    Vec::new();
  for edge in forest . root() . traverse() {
    if let ego_tree::iter::Edge::Open (node_ref) = edge {
      if let Some (merge) =
        optmerge_from_viewnode(
          { let viewnode : &ViewNode = node_ref . value();
            viewnode },
          config, driver ) . await?
        { merges . push (merge); }} }
  Ok (merges) }

/// Returns Some(Merge) if the viewnode has a merge instruction,
/// None otherwise.
async fn optmerge_from_viewnode (
  node   : &ViewNode,
  config : &SkgConfig,
  driver : &TypeDBDriver
) -> Result<Option<Merge>,
            Box<dyn Error>> {
  let t : &TrueNode = match &node . kind {
    ViewNodeKind::True (t) => t,
    _ => return Ok (None) };
  let acquiree_id = match &t . edit_request {
    Some(EditRequest::Merge (id)) => id,
    _ => return Ok (None) };
  let acquirer_id : &ID = &t . id;
  let acquirer_from_disk : SkgNode =
    skgnode_from_id(
      config, driver, acquirer_id ) . await?;
  let acquiree_from_disk : SkgNode =
    skgnode_from_id(
      config, driver, &acquiree_id ) . await?;
  let acquiree_text_preserver : SkgNode =
    create_acquiree_text_preserver (&acquiree_from_disk);
  let updated_acquirer : SkgNode =
    three_merged_skgnodes( &acquirer_from_disk,
                           &acquiree_from_disk,
                           &acquiree_text_preserver)?;
  Ok(Some(Merge {
    acquiree_text_preserver :
      SaveNode (acquiree_text_preserver),
    updated_acquirer :
      SaveNode (updated_acquirer),
    acquiree_to_delete :
      DeleteNode {
        id     : acquiree_id . clone(),
        source : acquiree_from_disk . source . clone() }} )) }

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
    acquirer_from_disk . clone();
  { // Append acquiree's IDs (esp. its PID) to acquirer's extra_ids.
    let mut combined_extra_ids : Vec<ID> =
      acquirer_from_disk . extra_ids . clone();
    combined_extra_ids . push(
      acquiree_from_disk . pid . clone() );
    combined_extra_ids . extend(
      acquiree_from_disk . extra_ids . clone() );
    updated_acquirer . extra_ids =
      dedup_vector (combined_extra_ids); }
  let new_contains : Vec<ID> = {
    // [preserver] + acquirer's old content + acquiree's old content
    let mut combined : Vec<ID> =
      vec![ acquiree_text_preserver . pid . clone() ];
    combined . extend_from_slice(
      &acquirer_from_disk . contains );
    combined . extend_from_slice(
      &acquiree_from_disk . contains );
    dedup_vector (combined) };
  updated_acquirer . contains = // update 'contains'
    new_contains . clone();
  { // Combine subscribes_to
    let mut combined : Vec<ID> =
      acquirer_from_disk . subscribes_to . or_default() . to_vec();
    combined . extend_from_slice(
      acquiree_from_disk . subscribes_to . or_default() );
    updated_acquirer . subscribes_to =
      MSV::Specified(dedup_vector (combined)); }
  { // Combine hides_from_its_subscriptions,
    // filtering to hide nothing that the acquirer contains.
    let mut combined : Vec<ID> =
      acquirer_from_disk . hides_from_its_subscriptions
      . or_default() . to_vec();
    combined . extend_from_slice(
      acquiree_from_disk . hides_from_its_subscriptions
        . or_default() );
    updated_acquirer . hides_from_its_subscriptions =
      MSV::Specified(
        { let deduped_and_filtered : Vec<ID> =
            // if it's in 'new_contains', then it's not here
            setlike_vector_subtraction(
                dedup_vector (combined),
                &new_contains);
          deduped_and_filtered } ); }
  { // Combine overrides_view_of
    let mut combined : Vec<ID> =
      acquirer_from_disk . overrides_view_of
      . or_default() . to_vec();
    combined . extend_from_slice(
      acquiree_from_disk . overrides_view_of
        . or_default() );
    updated_acquirer . overrides_view_of =
      MSV::Specified(dedup_vector (combined)); }
  Ok (updated_acquirer) }

/// Create an acquiree_text_preserver from the acquiree's data
fn create_acquiree_text_preserver(acquiree: &SkgNode) -> SkgNode {
  SkgNode {
    title: format!("MERGED: {}", acquiree . title),
    aliases: MSV::Unspecified,
    source: acquiree . source . clone(),
    pid: ID(uuid::Uuid::new_v4() . to_string()),
    extra_ids: vec![],
    body: acquiree . body . clone(),
    contains                     : vec![],
    subscribes_to                : MSV::Specified(vec![]),
    hides_from_its_subscriptions : MSV::Specified(vec![]),
    overrides_view_of            : MSV::Specified(vec![]),
    misc                         : Vec::new (),
  }}
