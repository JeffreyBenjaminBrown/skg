pub mod contradictory_instructions;

use crate::dbs::node_lookup::optNodeComplete_rustFIrst_by_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{ParentIs, Qual, QualCol, ViewRequest};
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
use typedb_driver::TypeDBDriver;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// SHARES RESPONSIBILITY for error detection
/// with 'org_to_uninterpreted_nodes',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of MpViewnodes rather than raw text.
/// (Namely, Alias and AliasCol should not have body text.)
///
/// ASSUMES that in the viewforest:
/// - IDs have been replaced with PIDs, per
///   'assign_pids_throughout_viewforest'. (Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.)
/// - All nodes have sources, per 'inherit_parent_source_if_possible'.
///
/// This is the maybePlaced tree validation stage: metadata is complete,
/// but role classification, save-intent extraction, and disk
/// supplementation have not happened yet.
pub async fn find_buffer_errors_for_saving (
  viewforest: &MpViewForest,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<Vec<BufferValidationError>,
            Box<dyn std::error::Error>>
{ // Two phases: instruction validation and structure validation.
  // Many of the first are global operations --
  // they need to take the entire viewforest into account.
  // By contrast the second phase (local structure validation)
  // performs only local structural verifications:
  // each ID belongs to an IDCol, etc.
  let mut errors: Vec<BufferValidationError> = Vec::new();
  { // inconsistent instructions (deletion, defining containers, and sources)
    let (ambiguous_deletion_ids,
         problematic_defining_ids,
         inconsistent_source_ids) =
      find_inconsistent_instructions (viewforest);
    { // transfer the relevant IDs, in the appropriate constructors.
      for id in ambiguous_deletion_ids {
        errors . push (
          BufferValidationError::AmbiguousDeletion (id)); }
      for id in problematic_defining_ids {
        errors . push(
          BufferValidationError::Multiple_Defining_Viewnodes (id)); }
      for (id, sources) in inconsistent_source_ids {
        errors . push(
          BufferValidationError::InconsistentSources(id, sources));
      }} }
  { // merge validation
    for error_msg in {
      let nodeMerge_errors: Vec<String> =
        validate_nodeMerge_requests(viewforest, config, driver) . await?;
      nodeMerge_errors }
    { errors . push(
        BufferValidationError::Other (error_msg)); }}
  validate_definitive_view_requests(
    viewforest, &mut errors);
  idCol_membership_errors (
    viewforest, config, driver, &mut errors ) . await ?;
  validate_view_roots (
      viewforest, &mut errors);
  { // local structure validation
    let root_ids : Vec<NodeId> =
      viewforest . root_ids ();
    for root_id in root_ids {
      let _ = do_everywhere_in_tree_dfs_readonly(
        viewforest, root_id, true,
        &mut |node_ref| {
          if let Err (e) = local::validate_local_structure(
                 viewforest, node_ref . id(), config) {
            errors . push(
              BufferValidationError::LocalStructureViolation(
                e . message, e . id )); }
          Ok(( )) }); }}
  Ok (errors) }

/// Edits to an idCol's membership abort the save (decision from
/// vision.org, via metaplan_2.org and
/// TODO/full-schema/8_readonly-set-ergonomics.org): for each present
/// idCol whose parent is a TrueNode with an ID, the multiset of ID
/// scaffolds beneath it must equal the owner's real ID list (pid
/// plus extra_ids). Reordering passes (the rerender re-sorts
/// anyway); adding, deleting or text-editing an ID scaffold fails,
/// with a message naming the escape hatch (edit the .skg file
/// directly). In diff mode, an ID entry whose membership axes mark
/// it net-removed is git history, not a membership claim, and is
/// excluded before comparing. An absent idCol means no opinion, as
/// for other cols. Shapes that other validations reject (an idCol
/// without a TrueNode parent, a parent without an ID) are skipped
/// here rather than double-reported.
#[allow(non_snake_case)]
async fn idCol_membership_errors (
  viewforest : &MpViewForest,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
  errors     : &mut Vec<BufferValidationError>,
) -> Result<(), Box<dyn std::error::Error>> {
  for edge in viewforest . root () . traverse () {
    let node_ref = match edge {
      Edge::Open (node_ref)
        if matches! ( &node_ref . value () . kind,
                      MpViewnodeKind::QualCol (QualCol::ID) )
        => node_ref,
      _ => continue };
    let owner : ID =
      match node_ref . parent ()
        . map ( |p| &p . value () . kind ) {
        Some (MpViewnodeKind::Vognode (MpVognode::Active (t)))
          => match &t . id {
              Some (id) => id . clone (),
              None      => continue },
        _ => continue };
    let mut buffer_ids : Vec<ID> =
      node_ref . children ()
      . filter_map ( |child| match &child . value () . kind {
          MpViewnodeKind::Qual (Qual::ID { id, membership })
            if membership . net_is_present ()
            => Some ( id . clone () ),
          _ => None } )
      . collect ();
    let real_ids : Option<Vec<ID>> =
      optNodeComplete_rustFIrst_by_id (config, driver, &owner)
      . await ?
      . map ( |nc| nc . all_ids () . cloned () . collect () );
    match real_ids {
      None =>
        errors . push ( BufferValidationError::IDCol_Edited (
          owner, buffer_ids, Vec::new () )),
      Some (real) => {
        let mut real_sorted : Vec<ID> = real . clone ();
        real_sorted . sort ();
        buffer_ids . sort ();
        if buffer_ids != real_sorted {
          errors . push ( BufferValidationError::IDCol_Edited (
            owner, buffer_ids, real )); }}, }}
  Ok (( )) }

fn validate_view_roots (
  viewforest : &MpViewForest,
  errors     : &mut Vec<BufferValidationError>,
) {
  for root in viewforest . roots () {
    if ! matches! (
      &root . value () . kind,
      MpViewnodeKind::Vognode (MpVognode::Active (_))
        | MpViewnodeKind::Phantom (MpPhantom::Deleted (_)))
    { errors . push (
        BufferValidationError::Other (
          "View roots must be TrueNodes or deleted nodes."
          . to_string () )); }}}

/// For each node in the viewforest, if it has a definitive view request,
/// verify that:
/// - The node is indefinitive.
/// - It has no content children (TrueNode children with parentIs ==
///   Container). Non-content children — containerward ancestry stubs,
///   link sources, scaffolds, etc. — don't block expansion:
///   they won't be clobbered by it.
/// - No other node with the same ID has a definitive view request,
///   because there can only be one definitive view.
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
      // so the Definitive-request validations below apply to Normal only.
      if let MpViewnodeKind::Vognode (
        MpVognode::Active (t))
      = &viewnode . kind
      { if t . view_requests . contains (&ViewRequest::Definitive)
        { if let Some (id) = &t . id {
          { // Must be indefinitive
            if ! t . is_indefinitive ()
            { errors . push( BufferValidationError::DefinitiveRequestOnDefinitiveNode(
              id . clone() )); }}
          { // Must have no content children.
            let has_content_children : bool =
              node_ref . children () . any ( |child| matches! (
                &child . value () . kind,
                MpViewnodeKind::Vognode (MpVognode::Active (ct))
                  if ct . parentIs == ParentIs::Affected ));
            if has_content_children
            { errors . push(
              BufferValidationError::DefinitiveRequestOnNodeWithContentChildren(
                id . clone() )); }}
          { // At most one request per ID
            if ! ids_with_requests . insert(id . clone())
            { errors . push( BufferValidationError::MultipleDefinitiveRequestsForSameId(
              id . clone() )); }} }}} }}}
