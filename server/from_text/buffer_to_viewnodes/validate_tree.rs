pub mod contradictory_instructions;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::{
  carrier_on_owned_chain, resolve_override};
use crate::dbs::in_rust_graph::override_invariants::existing_owned_overrider_of;
use crate::dbs::node_lookup::opt_graphnode_by_skgid;
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{RelationshipEditRequest, NodeEditRequest, Folder, PropertyMember, ViewRequest};
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom, MpUnrestrictedVognode};
use crate::types::tree::forest::MpViewForest;
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::nodeMerge::validate_nodeMerge::validate_nodeMerge_requests;
use contradictory_instructions::find_inconsistent_instructions;
use super::local;
use ego_tree::iter::Edge;
use ego_tree::{NodeId, NodeRef};
use std::collections::HashMap;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// Shares responsibility for error detection
/// with 'org_to_uninterpreted_nodes',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of MpViewnodes rather than raw text.
/// (Namely, Alias and AliasFolder should not have body text.)
///
/// Assumes that in the viewforest:
/// - IDs have been replaced with PIDs, per
///   'assign_pids_throughout_viewforest'. (Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.)
/// - All nodes have skgrepos, per
///   'inherit_skgrepo_from_nearest_unrestricted_ancestor_if_possible'.
///
/// This is the maybePlaced tree validation stage: metadata is complete,
/// but role classification, save-intent extraction, and disk
/// supplementation have not happened yet.
pub fn find_buffer_errors_for_saving_in_graph (
  viewforest: &MpViewForest,
  graph: &InRustGraph,
  config: &SkgConfig,
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>>
{ // Two phases: instruction validation and structure validation.
  // Many of the first are global operations --
  // they need to take the entire viewforest into account.
  // By contrast the second phase (local structure validation)
  // performs only local structural verifications:
  // each ID belongs to an IDFolder, etc.
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion and defining containers)
    let (ambiguous_deletion_skgids,
         problematic_defining_skgids) =
      find_inconsistent_instructions (viewforest);
    { // transfer the relevant IDs, in the appropriate constructors.
      for (skgid, headlines) in ambiguous_deletion_skgids {
        errors . push (
          BufferValidationError::AmbiguousDeletion (skgid, headlines)); }
      for (skgid, headlines) in problematic_defining_skgids {
        errors . push(
          BufferValidationError::Multiple_Defining_Viewnodes (skgid, headlines)); }} }
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
  overridesHere_fact_errors (
    viewforest, graph, config, &mut errors );
  validate_view_roots (
      viewforest, &mut errors);
  writeProtected_relRepo_request_errors (
    viewforest, &mut errors );
  { // local structure validation
    let root_skgids : Vec<NodeId> =
      viewforest . root_skgids ();
    for root_skgid in root_skgids {
      let _ = do_everywhere_in_tree_dfs_readonly(
        viewforest, root_skgid, true,
        &mut |node_ref| {
          if let Err (e) = local::validate_local_structure(
                 viewforest, node_ref . id(), config) {
            errors . push(
              BufferValidationError::LocalStructureViolation(
                e . message, e . skgid, e . headline )); }
          Ok(( )) }); }}
  Ok (errors) }

/// Transitional compatibility for direct validator tests.
pub fn find_buffer_errors_for_saving (
  viewforest : &MpViewForest,
  config     : &SkgConfig,
) -> Result<Vec<BufferValidationError>, Box<dyn std::error::Error>> {
  let nodes = crate::dbs::filesystem::multiple_nodes
    ::read_all_skg_files_from_skgrepos (config)?;
  let graph = InRustGraph::from_graphnodes (&nodes);
  find_buffer_errors_for_saving_in_graph (
    viewforest, &graph, config ) }

/// Edits to an idFolder's membership abort the save (see the
/// glossary's "write-protected set"): for each present
/// idFolder whose parent is an UnrestrictedVognode with an ID, the multiset of ID
/// non-vognodes beneath it must equal the recorder's real ID list (pid
/// plus extra_ids). Reordering passes (the order is view-local:
/// the rerender keeps it); adding, deleting or text-editing an ID property fails,
/// with a message naming the escape hatch (edit the .skg file
/// directly). In the diff view, an ID entry tagged '(rel removed)'
/// is a lost ID, not a membership claim, and is excluded before
/// comparing. An absent idFolder means no opinion, as
/// for other folders. Shapes that other validations reject (an idFolder
/// without an UnrestrictedVognode parent, a parent without an ID) are skipped
/// here rather than double-reported.
#[allow(non_snake_case)]
fn idFolder_membership_errors (
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) -> Result<(), Box<dyn std::error::Error>> {
  for edge in viewforest . root () . traverse () {
    let node_ref = match edge {
      Edge::Open (node_ref)
        if matches! ( &node_ref . value () . kind,
                      MpViewnodeKind::Folder (Folder::ID) )
        => node_ref,
      _ => continue };
    let (recorder, recorder_headline) : (ID, HeadlineName) =
      match node_ref . parent ()
        . map ( |p| (p . value (), &p . value () . kind) ) {
        Some ((parent, MpViewnodeKind::Vognode (MpVognode::Unrestricted (t))))
          => match &t . skgid {
              Some (skgid) => (skgid . clone (), parent . headline_name ()),
              None      => continue },
        _ => continue };
    let mut buffer_skgids : Vec<ID> =
      node_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
          // An ID with a ghost warrant is one the worktree lost.
          MpViewnodeKind::PropertyMember (PropertyMember::ID { skgid, .. })
            if ! child . value () . is_ghostWarranted ()
            => Some ( skgid . clone () ),
          _ => None } )
      . collect ();
    let real_skgids : Option<Vec<ID>> =
      opt_graphnode_by_skgid (graph, config, &recorder)
 ?
      . map ( |nc| nc . all_skgids () . cloned () . collect () );
    match real_skgids {
      None =>
        errors . push ( BufferValidationError::IDFolder_Edited (
          recorder, buffer_skgids, Vec::new (), recorder_headline )),
      Some (real) => {
        let mut real_sorted : Vec<ID> = real . clone ();
        real_sorted . sort ();
        buffer_skgids . sort ();
        if buffer_skgids != real_sorted {
          errors . push ( BufferValidationError::IDFolder_Edited (
            recorder, buffer_skgids, real, recorder_headline )); }}, }}
  Ok (( )) }

/// Tamper validation for the '(overridesHere N)' fact
/// ('docs/override.org', "The overridesHere fact").
/// The fact is load-bearing text: wherever it appears, save
/// extraction collects N instead of the carrier's own ID. So a
/// fact the server would not have rendered must abort the save --
/// otherwise hand-edited (or yanked, or stale) metadata could
/// rewrite arbitrary contains members. The check: the carrier's ID
/// must be on N's owned override chain
/// ('carrier_on_owned_chain', visibility-ungated so ownership
/// still gates but a fact honest when rendered does not start
/// failing after a skgrepo-set switch). With chains the shown node can
/// be a middle link (when a later link's mentioner is hidden), so the
/// check accepts any honest carrier and rejects only an off-chain
/// fact.
/// The explicit graph is required, so every present fact is checked against
/// the same graph snapshot used by the rest of save planning.
#[allow(non_snake_case)]
fn overridesHere_fact_errors (
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let (carrier, original) : (Option<ID>, ID) =
      match &node_ref . value () . kind {
        MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =>
          match &t . overridesHere {
            Some (original) =>
              ( t . skgid . clone (), original . clone () ),
            None => continue },
        _ => continue };
    let chain_ok : bool =
      match &carrier {
        Some (c) =>
          carrier_on_owned_chain (config, graph, &original, c),
        // An id-less carrier is never a node the server legitimately showed as
        // a substitute, so fail closed.
        _ => false };
    if ! chain_ok {
      let effective : Option<ID> = // the chain end, for the message
        Some (resolve_override (config, graph, None, &original) . effective);
      errors . push (
        BufferValidationError::OverridesHere_Mismatch (
          carrier, original, effective,
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
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  for (skgid, headlines) in
    occurrences_requesting (viewforest, &ViewRequest::Fork)
  { if headlines . len () > 1 {
      errors . push (
        BufferValidationError::ForkRequestMultiple (
          skgid . clone (), headlines . clone () )); }
    if graph . pid_of (&skgid) . is_none () {
      // Not in the graph: an unsaved headline cannot be forked.
      errors . push (
        BufferValidationError::ForkRequestOnUnknownNode (
          skgid, headlines [0] . clone () ));
      continue; }
    if let Some (existing) =
      existing_owned_overrider_of (config, graph, &skgid) {
      errors . push (
        BufferValidationError::ForkAlreadyExists (
          skgid, existing )); }} }

fn validate_view_roots (
  viewforest : &MpViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for root in viewforest . roots () {
    if ! matches! (
      &root . value () . kind,
        MpViewnodeKind::Vognode (MpVognode::Unrestricted (_))
        | MpViewnodeKind::Tombstone // e.g. a root the skgrepo restriction hides, kept for its children (the API doc's "Restricted-skgrepo nodes")
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (_)))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_)))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Historical (_))))
    { errors . push (
        BufferValidationError::Other (
          "View roots must be UnrestrictedVognodes, tombstones, deleted nodes, Unknown phantoms or historical phantoms."
          . to_string () )); }}}

/// No two nodes with the same ID may carry editable view requests,
/// because there can only be one editable view.
/// Nor may one occurrence carry both an editable view request and a
/// write-protect request; on different occurrences of a node the two
/// move editability, and are fine.
/// A request on an editable node, or on one with content children,
/// is fine: completion hands it to the node's content children.
/// Children typed under a write-protected occurrence are refused
/// elsewhere, by 'errors_for_writeProtected_occurrences'.
fn validate_editable_view_requests (
  viewforest : &MpViewForest,
  errors : &mut Vec<BufferValidationError>,
) {
  for (skgid, headlines) in
    occurrences_requesting (viewforest, &ViewRequest::Editable)
  { if headlines . len () > 1
    { errors . push( BufferValidationError::MultipleEditableViewRequestsForSameId(
        skgid, headlines )); }}
  for edge in viewforest . root() . traverse()
  { if let Edge::Open (node_ref) = edge
    { let viewnode : &MpViewnode =
        node_ref . value();
      // Only an Unrestricted node carries view_requests; a phantom never can,
      // so the Editable-request validations below apply to Unrestricted only.
      if let MpViewnodeKind::Vognode (
        MpVognode::Unrestricted (t))
      = &viewnode . kind
      { if t . view_requests . contains (&ViewRequest::Editable)
        { if let Some (skgid) = &t . skgid {
          if t . view_requests . contains (&ViewRequest::WriteProtected)
             || t . view_requests . contains (&ViewRequest::WriteProtectedSubtree)
          { errors . push( BufferValidationError::WriteProtectAndEditableViewRequestsOnOneOccurrence(
            skgid . clone(), viewnode . headline_name () )); }} }}} }}

/// For each ID some Unrestricted occurrence of which carries
/// 'request', in buffer order of first appearance: the names of those
/// occurrences, in buffer order. An occurrence without an ID is
/// skipped (enrichment gives every node a pid).
fn occurrences_requesting (
  viewforest : &MpViewForest,
  request    : &ViewRequest,
) -> Vec<(ID, Vec<HeadlineName>)> {
  let mut skgids_in_order : Vec<ID> = Vec::new ();
  let mut headlines_by_skgid : HashMap<ID, Vec<HeadlineName>> =
    HashMap::new ();
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =
      & node_ref . value () . kind else { continue; };
    if ! t . view_requests . contains (request) { continue; }
    let Some (skgid) = & t . skgid else { continue; };
    if ! headlines_by_skgid . contains_key (skgid) {
      skgids_in_order . push (skgid . clone ()); }
    headlines_by_skgid . entry (skgid . clone ()) . or_default ()
      . push (node_ref . value () . headline_name ()); }
  skgids_in_order . into_iter ()
    . map ( |skgid| {
      let headlines : Vec<HeadlineName> =
        headlines_by_skgid . remove (&skgid) . unwrap_or_default ();
      (skgid, headlines) } )
    . collect () }

/// A relRepo request describes the relationship from a node's view-parent
/// to the node, so it is an instruction of the view-parent's, not of the
/// node's. A write-protected occurrence may therefore carry one, and save
/// collects it ('content_members', 'subscribeeFolder_members',
/// 'partnerFolder_members' in
/// 'from_text/local_fieldintent_collection/traverse.rs'). But when no
/// save-eligible node writes that relationship -- most often because the
/// view-parent is itself write-protected -- the request would silently
/// vanish, so it is an error.
#[allow(non_snake_case)]
fn writeProtected_relRepo_request_errors (
  viewforest : &MpViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =
      &node_ref . value () . kind else { continue; };
    if t . is_writeProtected ()
       && RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . is_some ()
       && ! relRepo_request_is_collected (node_ref, t)
    { let headline : HeadlineName = node_ref . value () . headline_name ();
      errors . push ( BufferValidationError::Other ( with_org_subheadlines (
        &format! (
          "relRepo request on write-protected node {} that no editable node collects:\n- Its view-parent is write-protected (or marked for deletion, or a subscribee shown as such), or it is not a member of that view-parent.\n- Make the relRepo request from a view where the view-parent is editable.\n",
          t . skgid . as_ref () . map ( |skgid| skgid . 0 . as_str () )
            . unwrap_or ("(no ID)") ),
        &[ headline ] ))); }} }

/// Whether save reads the relRepo request on this Unrestricted node:
/// the node is a member of its view-parent, and the relationship's
/// writer is save-eligible. That writer is the view-parent, or, when the
/// view-parent is a writable PartnerFolder, the folder's view-parent.
fn relRepo_request_is_collected (
  node_ref : NodeRef<MpViewnode>,
  t        : &MpUnrestrictedVognode,
) -> bool {
  let is_member : bool =
    t . claimsMembership;
  is_member
  && match node_ref . parent () {
    Some (parent) => match &parent . value () . kind {
      MpViewnodeKind::Vognode (MpVognode::Unrestricted (_)) =>
        is_saveEligible_unrestricted_vognode (parent),
      MpViewnodeKind::Folder (
        Folder::Subscribee | Folder::Overridden ) =>
        parent . parent ()
        . map ( is_saveEligible_unrestricted_vognode )
        . unwrap_or (false),
      _ => false },
    None => false } }

/// As the save-eligibility of
/// 'from_text/local_fieldintent_collection/traverse.rs', at the
/// maybe-placed stage: an editable Unrestricted node, not marked for
/// deletion, not in subscribee-as-such position.
#[allow(non_snake_case)]
fn is_saveEligible_unrestricted_vognode (
  node_ref : NodeRef<MpViewnode>,
) -> bool {
  let MpViewnodeKind::Vognode (MpVognode::Unrestricted (t)) =
    &node_ref . value () . kind else { return false; };
  let is_subscribee_as_such : bool =
    t . claimsMembership
    && node_ref . parent ()
       . map ( |p| matches! ( &p . value () . kind,
                 MpViewnodeKind::Folder (Folder::Subscribee) ))
       . unwrap_or (false);
  ! t . is_writeProtected ()
  && ! matches! ( t . edit_request (), Some (&NodeEditRequest::Delete) )
  && ! is_subscribee_as_such }
