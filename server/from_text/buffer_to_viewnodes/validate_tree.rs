pub mod contradictory_instructions;

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::override_resolution::{
  carrier_on_owned_chain, resolve_override};
use crate::dbs::in_rust_graph::override_invariants::existing_owned_overrider_of;
use crate::dbs::node_lookup::opt_graphnode_by_skgid;
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{AffectsParent, Property, PropertyFolder, ViewRequest};
use crate::types::maybe_placed_viewnode::{MpViewnode, MpViewnodeKind};
use crate::types::maybe_placed_viewnode::{MpVognode, MpPhantom};
use crate::types::tree::forest::MpViewForest;
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::errors::BufferValidationError;
use crate::nodeMerge::validate_nodeMerge::validate_nodeMerge_requests;
use contradictory_instructions::find_inconsistent_instructions;
use super::local;
use ego_tree::iter::Edge;
use ego_tree::NodeId;
use std::collections::HashSet;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// SHARES RESPONSIBILITY for error detection
/// with 'org_to_uninterpreted_nodes',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of MpViewnodes rather than raw text.
/// (Namely, Alias and AliasFolder should not have body text.)
///
/// ASSUMES that in the viewforest:
/// - IDs have been replaced with PIDs, per
///   'assign_pids_throughout_viewforest'. (Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.)
/// - All nodes have skgrepos, per 'inherit_parent_repo_if_possible'.
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
  { // inconsistent instructions (deletion, defining containers, and skgrepos)
    let (ambiguous_deletion_skgids,
         problematic_defining_skgids,
         inconsistent_skgrepo_skgids) =
      find_inconsistent_instructions (viewforest);
    { // transfer the relevant IDs, in the appropriate constructors.
      for skgid in ambiguous_deletion_skgids {
        errors . push (
          BufferValidationError::AmbiguousDeletion (skgid)); }
      for skgid in problematic_defining_skgids {
        errors . push(
          BufferValidationError::Multiple_Defining_Viewnodes (skgid)); }
      for (skgid, skgrepos) in inconsistent_skgrepo_skgids {
        errors . push(
          BufferValidationError::InconsistentSkgRepos(skgid, skgrepos));
      }} }
  { // merge validation
    for error_msg in {
      let nodeMerge_errors: Vec<String> =
        validate_nodeMerge_requests(viewforest, graph)?;
      nodeMerge_errors }
    { errors . push(
        BufferValidationError::Other (error_msg)); }}
  validate_definitive_view_requests(
    viewforest, &mut errors);
  validate_fork_view_requests(
    viewforest, graph, config, &mut errors);
  idFolder_membership_errors (
    viewforest, graph, config, &mut errors ) ?;
  overridesHere_marker_errors (
    viewforest, graph, config, &mut errors );
  validate_view_roots (
      viewforest, &mut errors);
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
                e . message, e . skgid )); }
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

/// Edits to an idFolder's membership abort the save (decision from
/// vision.org, via metaplan_2.org and
/// TODO/DONE/full-schema/DONE/8_readonly-set-ergonomics.org): for each present
/// idFolder whose parent is an ActiveVognode with an ID, the multiset of ID
/// non-vognodes beneath it must equal the recorder's real ID list (pid
/// plus extra_ids). Reordering passes (the rerender re-sorts
/// anyway); adding, deleting or text-editing an ID property fails,
/// with a message naming the escape hatch (edit the .skg file
/// directly). In diff mode, an ID entry whose relationship axes mark
/// it net-removed is git history, not a membership claim, and is
/// excluded before comparing. An absent idFolder means no opinion, as
/// for other folders. Shapes that other validations reject (an idFolder
/// without an ActiveVognode parent, a parent without an ID) are skipped
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
                      MpViewnodeKind::PropertyFolder (PropertyFolder::ID) )
        => node_ref,
      _ => continue };
    let recorder : ID =
      match node_ref . parent ()
        . map ( |p| &p . value () . kind ) {
        Some (MpViewnodeKind::Vognode (MpVognode::Active (t)))
          => match &t . skgid {
              Some (skgid) => skgid . clone (),
              None      => continue },
        _ => continue };
    let mut buffer_skgids : Vec<ID> =
      node_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
          MpViewnodeKind::Property (Property::ID { skgid, relationship_axes })
            if relationship_axes . net_is_present ()
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
          recorder, buffer_skgids, Vec::new () )),
      Some (real) => {
        let mut real_sorted : Vec<ID> = real . clone ();
        real_sorted . sort ();
        buffer_skgids . sort ();
        if buffer_skgids != real_sorted {
          errors . push ( BufferValidationError::IDFolder_Edited (
            recorder, buffer_skgids, real )); }}, }}
  Ok (( )) }

/// Tamper validation for the '(overridesHere N)' marker (plan 11).
/// The marker is load-bearing text: wherever it appears, save
/// extraction collects N instead of the carrier's own ID. So a
/// marker the server would not have drawn must abort the save --
/// otherwise hand-edited (or yanked, or stale) metadata could
/// rewrite arbitrary contains members. The check: the carrier's ID
/// must be ON N's owned override chain
/// ('carrier_on_owned_chain', VISIBILITY-UNGATED so ownership
/// still gates but a marker honest when rendered does not start
/// failing after a skgrepo-set switch). With chains the drawn node can
/// be a MIDDLE link (when a later link's mentioner is hidden), so the
/// check accepts any honest carrier and rejects only an off-chain
/// marker. Markers on retained InactiveVognodes are checked identically.
/// The explicit graph is required, so every present marker is checked against
/// the same graph snapshot used by the rest of save planning.
#[allow(non_snake_case)]
fn overridesHere_marker_errors (
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let (carrier, original) : (Option<ID>, ID) =
      match &node_ref . value () . kind {
        MpViewnodeKind::Vognode (MpVognode::Active (t)) =>
          match &t . viewStats . overridesHere {
            Some (original) =>
              ( t . skgid . clone (), original . clone () ),
            None => continue },
        // An inactive vognode is anonymous: it carries no override
        // marker, so it can never mismatch.
        MpViewnodeKind::Vognode (MpVognode::Inactive (_)) => continue,
        _ => continue };
    let chain_ok : bool =
      match &carrier {
        Some (c) =>
          carrier_on_owned_chain (config, graph, &original, c),
        // An id-less carrier is never a node the server legitimately drew as
        // a substitute, so fail closed.
        _ => false };
    if ! chain_ok {
      let effective : Option<ID> = // the chain end, for the message
        Some (resolve_override (config, graph, None, &original) . effective);
      errors . push (
        BufferValidationError::OverridesHere_Mismatch (
          carrier, original, effective )); }}}

/// For each node carrying a Fork view request (the explicit
/// 'skg-fork-node' gesture):
/// - at most one fork request per id ('ForkRequestMultiple');
/// - the id must exist in the graph -- you cannot fork an unsaved
///   headline ('ForkRequestOnUnknownNode'); a new headline got a fresh
///   pid from enrichment, which is not in the graph;
/// - if the node already has an owned overrider, fail early with
///   'ForkAlreadyExists' (the helpful message) rather than a later
///   monogamy abort at commit.
/// These checks use the explicit save-planning graph; the commit-time
/// invariant check remains a defense in depth.
#[allow(non_snake_case)]
fn validate_fork_view_requests (
  viewforest : &MpViewForest,
  graph      : &InRustGraph,
  config     : &SkgConfig,
  errors     : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> = HashSet::new ();
  for edge in viewforest . root () . traverse () {
    let Edge::Open (node_ref) = edge else { continue; };
    let MpViewnodeKind::Vognode (MpVognode::Active (t)) =
      & node_ref . value () . kind else { continue; };
    if ! t . view_requests . contains (& ViewRequest::Fork) { continue; }
    let Some (skgid) = & t . skgid else { continue; }; // enrichment gives every node a pid
    if ! ids_with_requests . insert (skgid . clone ()) {
      errors . push (
        BufferValidationError::ForkRequestMultiple (skgid . clone ()) );
      continue; }
    if graph . pid_of (skgid) . is_none () {
        // Not in the graph: an unsaved headline cannot be forked.
        errors . push (
          BufferValidationError::ForkRequestOnUnknownNode (skgid . clone ()) );
        continue; }
      if let Some (existing) =
        existing_owned_overrider_of (config, graph, skgid) {
        errors . push (
          BufferValidationError::ForkAlreadyExists (
            skgid . clone (), existing )); }} }

fn validate_view_roots (
  viewforest : &MpViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for root in viewforest . roots () {
    if ! matches! (
      &root . value () . kind,
        MpViewnodeKind::Vognode (MpVognode::Active (_))
        | MpViewnodeKind::Vognode (MpVognode::Inactive (_)) // a retained inactive root (TODO/DONE/full-schema/DONE/9-2_source-set-safety.org)
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Deleted (_)))
        | MpViewnodeKind::Vognode (MpVognode::Phantom (MpPhantom::Unknown (_))))
    { errors . push (
        BufferValidationError::Other (
          "View roots must be ActiveVognodes, inactive vognodes, deleted nodes or Unknown phantoms."
          . to_string () )); }}}

/// For each node in the viewforest, if it has an editable view request,
/// verify that:
/// - The node is write-protected.
/// - It has no content children (ActiveVognode children with affectsParent ==
///   Container). Non-content children — containerward role tree stubs,
///   mentioners, non-vognodes, etc. — don't block expansion:
///   they won't be clobbered by it.
/// - No other node with the same ID has an editable view request,
///   because there can only be one editable view.
fn validate_definitive_view_requests (
  viewforest : &MpViewForest,
  errors : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> =
    HashSet::new();
  for edge in viewforest . root() . traverse()
  { if let Edge::Open (node_ref) = edge
    { let viewnode : &MpViewnode =
        node_ref . value();
      // TODO/DONE/local-view-update/plan_v2.org §11: only an Active node carries view_requests; a phantom never can,
      // so the Definitive-request validations below apply to Active only.
      if let MpViewnodeKind::Vognode (
        MpVognode::Active (t))
      = &viewnode . kind
      { if t . view_requests . contains (&ViewRequest::Definitive)
        { if let Some (skgid) = &t . skgid {
          { // Must be write-protected
            if ! t . is_writeProtected ()
            { errors . push( BufferValidationError::DefinitiveRequestOnEditableNode(
              skgid . clone() )); }}
          { // Must have no content children.
            let has_content_children : bool =
              node_ref . children () . any ( |child| matches! (
                &child . value () . kind,
                MpViewnodeKind::Vognode (MpVognode::Active (ct))
                  if ct . affectsParent == AffectsParent::True ));
            if has_content_children
            { errors . push(
              BufferValidationError::DefinitiveRequestOnNodeWithContentChildren(
                skgid . clone() )); }}
          { // At most one request per ID
            if ! ids_with_requests . insert(skgid . clone())
            { errors . push( BufferValidationError::MultipleDefinitiveRequestsForSameId(
              skgid . clone() )); }} }}} }}}
