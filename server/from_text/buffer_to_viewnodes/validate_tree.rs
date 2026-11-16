pub mod contradictory_instructions;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::mirrors::{
  is_mirror_of, mirror_shown, perspective_owner_of_tree_containing};
use crate::dbs::node_lookup::opt_graphnode_by_gnid;
use crate::from_text::fork::existing_owned_fork_of;
use crate::types::misc::{ID, SkgConfig, SkgrepoOwner};
use crate::types::viewnode::{RelationshipEditRequest, NodeEditRequest, Folder, PropertyMember, ViewRequest};
use crate::types::viewnode::{Viewnode, ViewnodeKind};
use crate::types::viewnode::{Vognode, Occurrence};
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::Perspective;
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::errors::{BufferValidationError, HeadlineName, with_org_subheadlines};
use crate::nodeMerge::validate_nodeMerge::validate_nodeMerge_requests;
use crate::skgrepo_sets::SkgrepoRestriction;
use contradictory_instructions::{
  find_contradictory_processed_requests, find_inconsistent_instructions};
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
  perspective: Perspective, // the perspective the buffer was drawn from
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>>
{ // Two kinds of check, in order: the global checks
  // (instructions, nodeMerges, view requests, idFolder membership,
  // inPlaceOf parts, view roots, write-protected relRepo requests),
  // which judge viewnodes against each other across the viewforest,
  // or one viewnode against the graph;
  // then the local-structure validation,
  // which performs only local structural verifications:
  // each ID belongs to an idFolder, etc.
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion, defining containers, processed requests)
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
      find_contradictory_processed_requests (viewforest, graph)
    { errors . push (
        BufferValidationError::ContradictoryProcessedRequests (leaf, headlines)); }}
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
  inPlaceOf_part_errors (
    viewforest, graph, config, perspective, &mut errors );
  validate_view_roots (
      viewforest, &mut errors);
  writeProtected_relRepo_request_errors (
    viewforest, &mut errors );
  { // local-structure validation
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
    viewforest, &graph, config, Perspective::RootAuthors ) }

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

/// Tamper validation for the '(inPlaceOf N)' part
/// ('docs/mirrors.org', "The inPlaceOf part").
/// The part is load-bearing text: wherever it appears, save
/// extraction collects N instead of the carrier's own ID. So a
/// part the server would not have rendered must abort the save --
/// otherwise hand-edited (or yanked, or stale) metadata could
/// rewrite arbitrary contains members. The check: the carrier must
/// be a mirror of N ('is_mirror_of'), under the unrestricted skgrepo
/// restriction, so that a part honest when rendered does not start
/// failing after a switch of the skgrepo restriction or of the
/// perspective. Any mirror is honest: which one the refresh shows
/// depends on the perspective and the skgrepo restriction. A refusal
/// names the mirror the occurrence's tree's perspective
/// ('perspective_owner_of_tree_containing') would show.
/// The explicit graph is required, so every present fact is checked against
/// the same graph snapshot used by the rest of save planning.
#[allow(non_snake_case)]
fn inPlaceOf_part_errors (
  viewforest         : &ViewForest,
  graph              : &InRustGraph,
  config             : &SkgConfig,
  perspective        : Perspective,
  errors             : &mut Vec<BufferValidationError>,
) {
  let unrestricted : SkgrepoRestriction = SkgrepoRestriction::unrestricted (config);
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let (carrier, original) : (ID, ID) =
      match &node_ref . value () . kind {
        ViewnodeKind::Vognode (Vognode::Occurrence (t)) =>
          match &t . in_place_of {
            Some (original) =>
              ( t . gnid . clone (), original . clone () ),
            None => continue },
        _ => continue };
    if ! is_mirror_of (graph, &unrestricted, &original, &carrier) {
      let owner : SkgrepoOwner = perspective_owner_of_tree_containing (
        config, graph, &perspective, node_ref);
      let effective : Option<ID> = // the mirror shown, for the message
        Some ( mirror_shown (
          config, graph, &unrestricted, &owner, &original ));
      errors . push (
        BufferValidationError::InPlaceOf_Mismatch (
          Some (carrier), original, effective,
          node_ref . value () . headline_name () )); }}}

/// For each node carrying a Fork view request (the explicit
/// 'skg-fork-node' gesture):
/// - at most one fork request per id ('ForkRequestMultiple');
/// - the id must exist in the graph -- you cannot fork an unsaved
///   headline ('ForkRequestOnUnknownNode'); a new headline got a fresh
///   pid from the parse, which is not in the graph;
/// - if the user already owns a fork of the node, 'ForkAlreadyExists',
///   naming it: the fork operation's guard ('validate_fork_specs_in_graph'
///   says why).
/// These checks use the explicit save-planning graph.
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
      existing_owned_fork_of (config, graph, &gnid) {
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
          "View roots must be Occurrences, tombstones, restricted viewnodes, unknown viewnodes or ghosts."
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
    if RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . is_some ()
       && is_forks_folder_member_claiming_nothing (node_ref, t)
    { errors . push ( BufferValidationError::Other ( with_org_subheadlines (
        &format! (
          "relRepo request on {}, a member of a forks folder that claims nothing:\n- Its view-parent's note does not fork it; it forks that note, and records so in its own file.\n- Set that relationship's skgrepo where it is recorded, or attach the member first to make the view-parent's note fork it.\n",
          t . gnid . 0 ),
        &[ node_ref . value () . headline_name () ] )));
      continue; }
    if t . is_writeProtected ()
       && RelationshipEditRequest::relRepo_of (&t . relationshipEditRequest) . is_some ()
       && ! relRepo_request_is_collected (node_ref, t)
    { let headline : HeadlineName = node_ref . value () . headline_name ();
      errors . push ( BufferValidationError::Other ( with_org_subheadlines (
        &format! (
          "relRepo request on write-protected node {} that no editable node collects:\n- Its view-parent is write-protected (or marked for deletion, or a forkee shown as such), or it is not a member of that view-parent.\n- Make the relRepo request from a view where the view-parent is editable.\n",
          t . gnid . 0 ),
        &[ headline ] ))); }} }

/// Whether the occurrence is a member of a forks folder that claims
/// nothing: one its view-parent's note does not fork (a forker-only
/// member, or a detached one), whose relRepo the save never reads.
fn is_forks_folder_member_claiming_nothing (
  node_ref : NodeRef<Viewnode>,
  t        : &Occurrence,
) -> bool {
  ! t . claimsMembership
  && node_ref . parent () . is_some_and ( |parent| matches! (
       &parent . value () . kind, ViewnodeKind::Folder (Folder::Forks) )) }

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
      ViewnodeKind::Folder (Folder::Forks) =>
        parent . parent ()
        . map ( is_saveEligible_occurrence )
        . unwrap_or (false),
      _ => false },
    None => false } }

/// As the save-eligibility of
/// 'from_text/local_fieldintent_collection/traverse.rs', at the
/// tree validation stage: an editable occurrence, not marked for
/// deletion, not in forkee-as-such position (a claiming member of a
/// forks folder).
#[allow(non_snake_case)]
fn is_saveEligible_occurrence (
  node_ref : NodeRef<Viewnode>,
) -> bool {
  let ViewnodeKind::Vognode (Vognode::Occurrence (t)) =
    &node_ref . value () . kind else { return false; };
  let is_forkee_as_such : bool =
    t . claimsMembership
    && node_ref . parent ()
       . map ( |p| matches! ( &p . value () . kind,
                 ViewnodeKind::Folder (Folder::Forks) ))
       . unwrap_or (false);
  ! t . is_writeProtected ()
  && ! matches! ( t . edit_request (), Some (&NodeEditRequest::Delete) )
  && ! is_forkee_as_such }
