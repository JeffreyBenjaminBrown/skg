use crate::dbs::typedb::relationships::OUTBOUND_RELATIONSHIP_TYPES;
use crate::dbs::typedb::search::find_related_nodes;
use crate::dbs::node_lookup::{nodeComplete_rustFIrst_by_id, optNodeComplete_rustFIrst_by_id};
use crate::from_text::local_instruction_collection::lower::nodeMerge_pairs;
use crate::from_text::local_instruction_collection::traverse::collect_instructions_locally;
use crate::from_text::local_instruction_collection::types::CollectedIntents;
use crate::types::save::{NodeMerge, SaveNode, DeleteNode};
use crate::types::misc::{MSV, SkgConfig, ID};
use crate::types::nodes::complete::NodeComplete;
use crate::types::list::dedup_vector;
use crate::types::tree::forest::ViewForest;
use crate::util::setlike_vector_subtraction;

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
pub async fn affected_neighbors_of_nodeMerge (
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
pub async fn neighbor_savenodes_for_nodeMerges (
  nodeMerges : &[NodeMerge],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < Vec<SaveNode>, Box<dyn Error> > {
  if nodeMerges . is_empty () {
    return Ok (Vec::new ()); }
  let primary_pids : HashSet<ID> = nodeMerges . iter ()
    . flat_map ( |m| [
        m . acquirer_id () . clone (),
        m . acquiree_id () . clone () ] )
    . collect ();
  let mut neighbors : HashSet<ID> = HashSet::new ();
  for nodeMerge in nodeMerges {
    let for_this : HashSet<ID> = affected_neighbors_of_nodeMerge (
      &config . db_name, driver, nodeMerge . acquiree_id ()
    ) . await ?;
    neighbors . extend (for_this); }
  let to_load : Vec<ID> =
    neighbors . difference (&primary_pids) . cloned () . collect ();
  let mut save_nodes : Vec<SaveNode> =
    Vec::with_capacity (to_load . len ());
  for pid in &to_load {
    let node : NodeComplete =
      nodeComplete_rustFIrst_by_id (config, driver, pid) . await ?;
    save_nodes . push ( SaveNode (node) ); }
  Ok (save_nodes) }

/// PURPOSE: For each nodeMerge request in the viewforest, this
/// creates a NodeMerge:
/// - acquiree_text_preserver: new node containing the acquiree's title and body
/// - updated_acquirer: acquirer node with modified contents and extra IDs
/// - acquiree_to_delete: acquiree marked for deletion
/// It is a convenience wrapper over local instruction collection
/// plus 'nodeMerge_instructions_from_pairs'; the production save
/// pipeline collects once and calls the pair form directly.
#[allow(non_snake_case)]
pub async fn nodeMerge_instructions_from_viewforest (
  viewforest : &ViewForest,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result<Vec<NodeMerge>, Box<dyn Error>> {
  let collected : CollectedIntents =
    collect_instructions_locally (viewforest)
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  nodeMerge_instructions_from_pairs (
    &nodeMerge_pairs (&collected), config, driver ) . await }

/// This builds the NodeMerge triples for a batch of (acquirer,
/// acquiree) pairs, as collected from the buffer by local
/// instruction collection ('nodeMerge_pairs').
#[allow(non_snake_case)]
pub async fn nodeMerge_instructions_from_pairs (
  pairs  : &[(ID, ID)],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<Vec<NodeMerge>, Box<dyn Error>> {
  let mut merges : Vec<NodeMerge> =
    Vec::with_capacity (pairs . len());
  for (acquirer_id, acquiree_id) in pairs {
    merges . push (
      nodeMerge_from_acquirer_and_acquiree (
        acquirer_id, acquiree_id, config, driver ) . await ? ); }
  Ok (merges) }

async fn nodeMerge_from_acquirer_and_acquiree (
  acquirer_id : &ID,
  acquiree_id : &ID,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<NodeMerge, Box<dyn Error>> {
  let acquirer_from_disk : NodeComplete =
    nodeComplete_rustFIrst_by_id (
      config, driver, acquirer_id ) . await?;
  let acquiree_from_disk : NodeComplete =
    nodeComplete_rustFIrst_by_id (
      config, driver, &acquiree_id ) . await?;
  let acquiree_text_preserver : NodeComplete =
    create_acquiree_text_preserver (&acquiree_from_disk);
  let shown_pre_merge : HashSet<ID> = {
    // Whatever EITHER member showed as unintegrated subscribed
    // content before the merge, the merged node must keep showing
    // (TODO/more.org, "Take something like the intersection of hides
    // when merging"), so these ids are dropped from the combined
    // hides below.
    let mut shown : HashSet<ID> =
      ids_shown_through_subscriptions (
        &acquirer_from_disk, config, driver ) . await ?;
    shown . extend (
      ids_shown_through_subscriptions (
        &acquiree_from_disk, config, driver ) . await ? );
    shown };
  let updated_acquirer : NodeComplete =
    three_nodeMerged_nodecompletes( &acquirer_from_disk,
                           &acquiree_from_disk,
                           &acquiree_text_preserver,
                           &shown_pre_merge)?;
  Ok(NodeMerge {
    acquiree_text_preserver :
      SaveNode (acquiree_text_preserver),
    updated_acquirer :
      SaveNode (updated_acquirer),
    acquiree_to_delete :
      DeleteNode {
        id     : acquiree_id . clone(),
        source : acquiree_from_disk . source . clone() }} ) }

/// Computes the updated acquirer node with all fields properly merged.
/// Returns a new NodeComplete with:
/// - Combined IDs from both nodes
/// - contains: [acquiree_text_preserver] + acquirer's + acquiree's novel contents
///   - 'Novel' = not among the acquirer's contents
/// - Combined relationship fields (subscribes_to, overrides_view_of)
/// - Filtered hides_from_its_subscriptions: can't hide your own
///   content, and can't hide what either member SHOWED pre-merge
///   ('shown_pre_merge')
fn three_nodeMerged_nodecompletes(
  acquirer_from_disk: &NodeComplete,
  acquiree_from_disk: &NodeComplete,
  acquiree_text_preserver: &NodeComplete,
  shown_pre_merge: &HashSet<ID>,
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
  { // Union aliases (parallel to extra_ids): a merged node should
    // still be findable by the acquiree's old aliases.
    let mut combined : Vec<String> =
      acquirer_from_disk . aliases . or_default() . to_vec();
    combined . extend_from_slice(
      acquiree_from_disk . aliases . or_default() );
    updated_acquirer . aliases =
      MSV::Specified(dedup_vector (combined)); }
  { // Combine subscribes_to
    let mut combined : Vec<ID> =
      acquirer_from_disk . subscribes_to . or_default() . to_vec();
    combined . extend_from_slice(
      acquiree_from_disk . subscribes_to . or_default() );
    updated_acquirer . subscribes_to =
      MSV::Specified(dedup_vector (combined)); }
  { // Combine hides_from_its_subscriptions, filtering to hide
    // nothing that the acquirer contains, and nothing either member
    // SHOWED through its subscriptions pre-merge: if it was
    // contained in one member's subscribee and unhidden by (and not
    // contained in) that member, the merge keeps showing it -- one
    // member's hide never silences the other's view. ("Something
    // like the intersection": exactly the intersection when both
    // members could see the id through some subscribee.)
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
          deduped_and_filtered
            . into_iter()
            . filter ( |id| ! shown_pre_merge . contains (id) )
            . collect() } ); }
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

/// The ids 'node' shows as unintegrated subscribed content: contained
/// by some node it subscribes to, and neither hidden by it nor among
/// its own contents (the subscribee-as-such display rule,
/// docs/sharing-model.md). A subscribee with no disk entry
/// contributes nothing.
async fn ids_shown_through_subscriptions (
  node   : &NodeComplete,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let mut shown : HashSet<ID> = HashSet::new ();
  for subscribee_id in node . subscribes_to . or_default () {
    let Some (subscribee) = optNodeComplete_rustFIrst_by_id (
      config, driver, subscribee_id ) . await ?
    else { continue; };
    for id in & subscribee . contains {
      if ! node . hides_from_its_subscriptions
             . or_default () . contains (id)
        && ! node . contains . contains (id)
      { shown . insert ( id . clone () ); }} }
  Ok (shown) }

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
