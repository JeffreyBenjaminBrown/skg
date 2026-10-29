pub mod contradictory_instructions;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::{
  carrier_on_owned_chain, resolve_override};
use crate::dbs::in_rust_graph::override_invariants::existing_owned_overrider_of;
use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{RelationshipEditRequest, NodeEditRequest, Folder, PropertyMember, ViewRequest};
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::{Vognode, Occurrence};
use crate::types::tree::forest::ViewForest;
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::nodeMerge::validate_nodeMerge::validate_nodeMerge_requests;
use contradictory_instructions::{
  find_contradictory_readiness_requests, find_inconsistent_instructions};
use super::local;
use ego_tree::iter::Edge;
use ego_tree::{NodeId, NodeRef};
use std::collections::HashMap;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// Shares responsibility for error detection
/// with 'org_to_viewforest',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of Viewnodes rather than raw text.
/// (Namely, Alias and aliasFolder should not have body text.)
///
/// Assumes the viewforest is as 'org_to_viewforest' makes it:
/// every occurrence's ID is its PID when the graph knows it
/// (otherwise two org nodes might refer to the same skg node, yet
/// appear not to), and every occurrence has a skgrepo, perhaps
/// 'SkgrepoName::not_found', which this refuses.
///
/// This is the tree validation stage: metadata is complete,
/// but role classification, save-intent extraction, and disk
/// supplementation have not happened yet.
pub fn find_buffer_errors_for_saving_in_graph (
  viewforest: &ViewForest,
  graph: &InRustGraph,
  config: &SkgConfig,
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>>
{ // Two phases: instruction validation and structure validation.
  // Many of the first are global operations --
  // they need to take the entire viewforest into account.
  // By contrast the second phase (local structure validation)
  // performs only local structural verifications:
  // each ID belongs to an idFolder, etc.
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion, defining containers, readiness)
    let (ambiguous_deletion_gnids,
         problematic_defining_gnids) =
      find_inconsistent_instructions (viewforest);
    { // transfer the relevant IDs, in the appropriate constructors.
      for (gnid, headlines) in ambiguous_deletion_gnids {
        errors . push (
          BufferValidationError::AmbiguousDeletion (gnid, headlines)); }
      for (gnid, headlines) in problematic_defining_gnids {
        errors . push(
          BufferValidationError::Multiple_Defining_Viewnodes (gnid, headlines)); }}
    for (leaf, headlines) in
      find_contradictory_readiness_requests (viewforest, graph)
    { errors . push (
        BufferValidationError::ContradictoryReadinessRequests (leaf, headlines)); }}
  { // merge validation
    for error_msg in {
      let nodeMerge_errors: Vec<String> =
        validate_nodeMerge_requests(viewforest, graph)?;
      nodeMerge_errors }
    { errors . push(
        BufferValidationError::Other (error_msg)); }}
  validate_editable_view_requests(
    viewforest, &mut errors);
  validate_fork_view_requests(
    viewforest, graph, config, &mut errors);
  idFolder_membership_errors (
    viewforest, graph, config, &mut errors ) ?;
  overridesHere_part_errors (
    viewforest, graph, config, &mut errors );
  validate_view_roots (
      viewforest, &mut errors);
  writeProtected_relRepo_request_errors (
    viewforest, &mut errors );
  { // local structure validation
    let root_gnids : Vec<NodeId> =
      viewforest . root_gnids ();
    for root_gnid in root_gnids {
      let _ = do_everywhere_in_tree_dfs_readonly(
        viewforest, root_gnid, true,
        &mut |node_ref| {
          if let Err (e) = local::validate_local_structure(
                 viewforest, node_ref . id(), config) {
            errors . push(
              BufferValidationError::LocalStructureViolation(
                e . message, e . gnid, e . headline )); }
          Ok(( )) }); }}
  Ok (errors) }

/// Transitional compatibility for direct validator tests.
pub fn find_buffer_errors_for_saving (
  viewforest : &ViewForest,
  config     : &SkgConfig,
) -> Result<Vec<BufferValidationError>, Box<dyn std::error::Error>> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)?;
  let graph = InRustGraph::from_graphnodes (&nodes);
  find_buffer_errors_for_saving_in_graph (
    viewforest, &graph, config ) }

/// Removing an ID from an idFolder aborts the save (see the
/// glossary's "write-protected set"): for each present idFolder whose
/// parent is an Occurrence with an ID, every ID of the
/// recorder (its pid and extra_ids) must stand beneath it. The order
/// is view-local (the rerender keeps it). The idFolder is
/// write-protected, so no ID there claims membership: an ID beyond
/// the recorder's (one the user typed, or, in the diff view, one the
/// worktree lost) saves nothing and is not refused; the refresh shows
/// it unwarranted. The message names the escape hatch (edit the .skg
/// file directly). An absent idFolder means no opinion, as for other
/// folders. Shapes that other validations reject (an idFolder without
/// an Occurrence parent, a parent without an ID) are skipped
/// here rather than double-reported.
#[allow(non_snake_case)]
fn idFolder_membership_errors (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) -> Result<(), Box<dyn std::error::Error>> {
  for edge in viewforest . root () . traverse () {
    let node_ref = match edge {
      Edge::Open (node_ref)
        if matches! ( &node_ref . value () . kind,
                      ViewnodeKind::Folder (Folder::ID) )
        => node_ref,
      _ => continue };
    let (recorder, recorder_headline) : (ID, HeadlineName) =
      match node_ref . parent ()
        . map ( |p| (p . value (), &p . value () . kind) ) {
        Some ((parent, ViewnodeKind::Vognode (Vognode::Occurrence (t))))
          => (t . gnid . clone (), parent . headline_name ()),
        _ => continue };
    let mut buffer_gnids : Vec<ID> =
      node_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
          ViewnodeKind::PropertyMember (PropertyMember::ID { gnid, .. })
            => Some ( gnid . clone () ),
          _ => None } )
      . collect ();
    let real_gnids : Option<Vec<ID>> =
      opt_graphnode_by_gnid (graph, config, &recorder)
 ?
      . map ( |nc| nc . all_gnids () . cloned () . collect () );
    match real_gnids {
      None =>
        errors . push ( BufferValidationError::IDFolder_Edited (
          recorder, buffer_gnids, Vec::new (), recorder_headline )),
      Some (real) => {
        buffer_gnids . sort ();
        if real . iter () . any ( |id| ! buffer_gnids . contains (id) ) {
          errors . push ( BufferValidationError::IDFolder_Edited (
            recorder, buffer_gnids, real, recorder_headline )); }}, }}
  Ok (( )) }

/// Tamper validation for the '(overridesHere N)' part
/// ('docs/override.org', "The overridesHere part").
/// The part is load-bearing text: wherever it appears, save
/// extraction collects N instead of the carrier's own ID. So a
/// part the server would not have rendered must abort the save --
/// otherwise hand-edited (or yanked, or stale) metadata could
/// rewrite arbitrary contains members. The check: the carrier's ID
/// must be on N's owned override chain
/// ('carrier_on_owned_chain', visibility-ungated so ownership
/// still gates but a part honest when rendered does not start
/// failing after a skgrepo-set switch). With chains the shown node can
/// be a middle link (when a later link's mentioner is hidden), so the
/// check accepts any honest carrier and rejects only an off-chain
/// fact.
/// The explicit graph is required, so every present fact is checked against
/// the same graph snapshot used by the rest of save planning.
#[allow(non_snake_case)]
fn overridesHere_part_errors (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let (carrier, original) : (ID, ID) =
      match &node_ref . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
          match &t . overridesHere {
            Some (original) =>
              ( t . gnid . clone (), original . clone () ),
            None => continue },
        _ => continue };
    let chain_ok : bool =
      carrier_on_owned_chain (config, graph, &original, &carrier);
    if ! chain_ok {
      let effective : Option<ID> = // the chain end, for the message
        Some (resolve_override (config, graph, None, &original) . effective);
      errors . push (
        BufferValidationError::OverridesHere_Mismatch (
          Some (carrier), original, effective,
          node_ref . value () . headline_name () )); }}}

/// For each node carrying a Fork view request (the explicit
/// 'skg-fork-node' gesture):
/// - at most one fork request per id ('ForkRequestMultiple');
/// - the id must exist in the graph -- you cannot fork an unsaved
///   headline ('ForkRequestOnUnknownNode'); a new headline got a fresh
///   pid from enrichment, which is not in the graph;
/// - if the node already has an owned overrider, fail early with
///   'ForkAlreadyExists' (the helpful message) rather than a later
///   monogamy abort at skgsave-commit.
/// These checks use the explicit save-planning graph; the skgsave-commit-time
/// invariant check remains a defense in depth.
#[allow(non_snake_case)]
fn validate_fork_view_requests (
  viewforest : &ViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  for (gnid, headlines) in
    occurrences_requesting (viewforest, &ViewRequest::Fork)
  { if headlines . len () > 1 {
      errors . push (
        BufferValidationError::ForkRequestMultiple (
          gnid . clone (), headlines . clone () )); }
    if graph . pid_of (&gnid) . is_none () {
      // Not in the graph: an unsaved headline cannot be forked.
      errors . push (
        BufferValidationError::ForkRequestOnUnknownNode (
          gnid, headlines [0] . clone () ));
      continue; }
    if let Some (existing) =
      existing_owned_overrider_of (config, graph, &gnid) {
      errors . push (
        BufferValidationError::ForkAlreadyExists (
          gnid, existing )); }} }

fn validate_view_roots (
  viewforest : &ViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for root in viewforest . roots () {
    if ! matches! (
      &root . value () . kind,
        ViewnodeKind::Vognode (Vognode::Occurrence (_))
        | ViewnodeKind::Tombstone // e.g. where a restricted root stood, kept for its children (the API doc's "Restricted-skgrepo nodes")
        | ViewnodeKind::Vognode (Vognode::GnidOnly (_))
        | ViewnodeKind::Vognode (Vognode::Ghost (_)))
    { errors . push (
        BufferValidationError::Other (
          "View roots must be Occurrences, tombstones, restricted headlines, references to unknown nodes or ghosts."
          . to_string () )); }}}

/// No two nodes with the same ID may carry editable view requests,
/// because there can only be one editable view.
/// Nor may one occurrence carry both an editable view request and a
/// write-protect request; on different occurrences of a node the two
/// move editability, and are fine.
/// A request on an editable node, or on one with content children,
/// is fine: the refresh hands it to the node's content children.
/// Children typed under a write-protected occurrence are refused
/// elsewhere, by 'errors_for_writeProtected_occurrences'.
fn validate_editable_view_requests (
  viewforest : &ViewForest,
  errors : &mut Vec<BufferValidationError>,
) {
  for (gnid, headlines) in
    occurrences_requesting (viewforest, &ViewRequest::Editable)
  { if headlines . len () > 1
    { errors . push( BufferValidationError::MultipleEditableViewRequestsForSameId(
        gnid, headlines )); }}
  for edge in viewforest . root() . traverse()
  { if let Edge::Open (node_ref) = edge
    { let viewnode : &Viewnode =
        node_ref . value();
      // Only an occurrence carries view_requests; a ghost or an unknown never can,
      // so the Editable-request validations below apply to occurrences only.
      if let ViewnodeKind::Vognode (
        Vognode::Occurrence (t))
      = &viewnode . kind
      { if t . view_requests . contains (&ViewRequest::Editable)
        { { let gnid : &ID = &t . gnid;
          if t . view_requests . contains (&ViewRequest::WriteProtected)
             || t . view_requests . contains (&ViewRequest::WriteProtectedSubtree)
          { errors . push( BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence(
            gnid . clone(), viewnode . headline_name () )); }} }}} }}

/// For each ID some occurrence of which carries
/// 'request', in buffer order of first appearance: the names of those
/// occurrences, in buffer order.
fn occurrences_requesting (
  viewforest : &ViewForest,
  request    : &ViewRequest,
) -> Vec<(ID, Vec<HeadlineName>)> {
  let mut gnids_in_order : Vec<ID> = Vec::new ();
  let mut headlines_by_gnid : HashMap<ID, Vec<HeadlineName>> =
    HashMap::new ();
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      & node_ref . value () . kind else { continue; };
    if ! t . view_requests . contains (request) { continue; }
    let gnid : &ID = & t . gnid;
    if ! headlines_by_gnid . contains_key (gnid) {
      gnids_in_order . push (gnid . clone ()); }
    headlines_by_gnid . entry (gnid . clone ()) . or_default ()
      . push (node_ref . value () . headline_name ()); }
  gnids_in_order . into_iter ()
    . map ( |gnid| {
      let headlines : Vec<HeadlineName> =
        headlines_by_gnid . remove (&gnid) . unwrap_or_default ();
      (gnid, headlines) } )
    . collect () }

/// A relRepo request describes the relationship from a node's view-parent
/// to the node, so it is an instruction of the view-parent's, not of the
/// node's. A write-protected occurrence may therefore carry one, and save
/// collects it ('content_members', 'partnerFolder_members' in
/// 'from_text/local_fieldintent_collection/traverse.rs'). But when no
/// save-eligible node writes that relationship -- most often because the
/// view-parent is itself write-protected -- the request would silently
/// vanish, so it is an error.
#[allow(non_snake_case)]
fn writeProtected_relRepo_request_errors (
  viewforest : &ViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
      &node_ref . value () . kind else { continue; };
    if t . is_writeProtected ()
       && RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . is_some ()
       && ! relRepo_request_is_collected (node_ref, t)
    { let headline : HeadlineName = node_ref . value () . headline_name ();
      errors . push ( BufferValidationError::Other ( with_org_subheadlines (
        &format! (
          "relRepo request on write-protected node {} that no editable node collects:\n- Its view-parent is write-protected (or marked for deletion, or a subscribee shown as such), or it is not a member of that view-parent.\n- Make the relRepo request from a view where the view-parent is editable.\n",
          t . gnid . 0 ),
        &[ headline ] ))); }} }

/// Whether save reads the relRepo request on this occurrence:
/// the node is a member of its view-parent, and the relationship's
/// writer is save-eligible. That writer is the view-parent, or, when the
/// view-parent is a writable partner folder, the folder's view-parent.
fn relRepo_request_is_collected (
  node_ref : NodeRef<Viewnode>,
  t        : &Occurrence,
) -> bool {
  let is_member : bool =
    t . claimsMembership;
  is_member
  && match node_ref . parent () {
    Some (parent) => match &parent . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (_)) =>
        is_saveEligible_occurrence (parent),
      ViewnodeKind::Folder (
        Folder::Subscribee | Folder::Overridden ) =>
        parent . parent ()
        . map ( is_saveEligible_occurrence )
        . unwrap_or (false),
      _ => false },
    None => false } }

/// As the save-eligibility of
/// 'from_text/local_fieldintent_collection/traverse.rs', at the
/// tree validation stage: an editable occurrence, not marked for
/// deletion, not in subscribee-as-such position.
#[allow(non_snake_case)]
fn is_saveEligible_occurrence (
  node_ref : NodeRef<Viewnode>,
) -> bool {
  let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &node_ref . value () . kind else { return false; };
  let is_subscribee_as_such : bool =
    t . claimsMembership
    && node_ref . parent ()
       . map ( |p| matches! ( &p . value () . kind,
                 ViewnodeKind::Folder (Folder::Subscribee) ))
       . unwrap_or (false);
  ! t . is_writeProtected ()
  && ! matches! ( t . edit_request (), Some (&NodeEditRequest::Delete) )
  && ! is_subscribee_as_such }
