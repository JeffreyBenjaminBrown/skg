use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::dbs::typedb::search::find_related_nodes;
use crate::types::memory::skgnode_from_memory_or_disk_async;
use crate::types::save::{Merge, SaveNode, DeleteNode};
use crate::types::misc::{MSV, SkgConfig, ID};
use crate::types::viewnode::EditRequest;
use crate::types::viewnode::{ViewNode, ViewNodeKind, TrueNode};
use crate::types::nodes::complete::NodeComplete;
use crate::types::list::dedup_vector;
use crate::util::setlike_vector_subtraction;

use ego_tree::Tree;
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Find every node that has an outbound relation pointing at the
/// acquiree via one of the five outbound-relation shapes. The
/// acquiree plays the object (second relationship member — role_b in
/// 'OUTBOUND_RELATIONSHIP_TYPES'); the neighbor plays the subject
/// (first relationship member — role_a). These neighbors'
/// outbound edges to acquiree would be destroyed by cascade when the
/// acquiree is deleted; they must be re-saved so the save pipeline
/// re-creates the edges (extra_id resolution then redirects them to
/// the acquirer).
pub async fn affected_neighbors_of_merge (
  db_name     : &str,
  driver      : &TypeDBDriver,
  acquiree_id : &ID,
) -> Result < HashSet<ID>, Box<dyn Error> > {
  let inputs : [ID; 1] = [ acquiree_id . clone () ];
  let mut all : HashSet<ID> = HashSet::new ();
  for (relation, neighbor_role, acquiree_role) in OUTBOUND_RELATIONSHIP_TYPES {
    let neighbors : HashSet<ID> = find_related_nodes (
      db_name, driver, &inputs,
      relation, acquiree_role, neighbor_role ) . await ?;
    all . extend (neighbors); }
  Ok (all) }

/// For a batch of merges, discover every affected neighbor across all
/// acquirees, filter out any that are themselves acquirers or acquirees
/// in the batch (those are already handled by the primary 3×N
/// DefineNodes), load each neighbor's NodeComplete from disk, and wrap
/// in SaveNode. The resulting SaveNodes carry /unchanged/ NodeCompletes
/// — the acquiree_id stays in whatever vectors it's in, and extra_id
/// resolution handles the redirection to acquirer at TypeDB
/// relationship-creation time.
pub async fn neighbor_savenodes_for_merges (
  merges : &[Merge],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < Vec<SaveNode>, Box<dyn Error> > {
  if merges . is_empty () {
    return Ok (Vec::new ()); }
  let primary_pids : HashSet<ID> = merges . iter ()
    . flat_map ( |m| [
        m . acquirer_id () . clone (),
        m . acquiree_id () . clone () ] )
    . collect ();
  let mut neighbors : HashSet<ID> = HashSet::new ();
  for merge in merges {
    let for_this : HashSet<ID> = affected_neighbors_of_merge (
      &config . db_name, driver, merge . acquiree_id ()
    ) . await ?;
    neighbors . extend (for_this); }
  let to_load : Vec<ID> =
    neighbors . difference (&primary_pids) . cloned () . collect ();
  let mut save_nodes : Vec<SaveNode> =
    Vec::with_capacity (to_load . len ());
  for pid in &to_load {
    let node : NodeComplete =
      skgnode_from_memory_or_disk_async (config, driver, pid) . await ?;
    save_nodes . push ( SaveNode (node) ); }
  Ok (save_nodes) }

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
  let acquiree_id = match t . edit_request () {
    Some(EditRequest::Merge (id)) => id,
    _ => return Ok (None) };
  let acquirer_id : &ID = &t . id;
  let acquirer_from_disk : NodeComplete =
    skgnode_from_memory_or_disk_async (
      config, driver, acquirer_id ) . await?;
  let acquiree_from_disk : NodeComplete =
    skgnode_from_memory_or_disk_async (
      config, driver, &acquiree_id ) . await?;
  let acquiree_text_preserver : NodeComplete =
    create_acquiree_text_preserver (&acquiree_from_disk);
  let updated_acquirer : NodeComplete =
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
/// Returns a new NodeComplete with:
/// - Combined IDs from both nodes
/// - contains: [acquiree_text_preserver] + acquirer's + acquiree's novel contents
///   - 'Novel' = not among the acquirer's contents
/// - Combined relationship fields (subscribes_to, overrides_view_of)
/// - Filtered hides_from_its_subscriptions (can't hide your own content)
fn three_merged_skgnodes(
  acquirer_from_disk: &NodeComplete,
  acquiree_from_disk: &NodeComplete,
  acquiree_text_preserver: &NodeComplete,
) -> Result<NodeComplete, String> {
  let mut updated_acquirer: NodeComplete =
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
  updated_acquirer . contains =
    setlike_vector_subtraction ( // prevent acquirer from containing itself
      new_contains . clone(),
      &updated_acquirer . all_ids() . cloned() . collect::<Vec<_>>() );
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
fn create_acquiree_text_preserver(acquiree: &NodeComplete) -> NodeComplete {
  NodeComplete {
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
