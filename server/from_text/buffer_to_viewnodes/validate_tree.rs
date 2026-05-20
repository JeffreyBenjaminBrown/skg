pub mod contradictory_instructions;

use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::{Birth, ViewRequest};
use crate::types::unchecked_viewnode::{UncheckedViewNode, UncheckedViewNodeKind};
use crate::types::tree::generic::do_everywhere_in_tree_dfs_readonly;
use crate::types::errors::BufferValidationError;
use crate::merge::validate_merge::validate_merge_requests;
use contradictory_instructions::find_inconsistent_instructions;
use super::local;
use ego_tree::Tree;
use ego_tree::iter::Edge;
use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

/// PURPOSE: Look for invalid structure in the org buffer
/// when a user asks to save it.
///
/// SHARES RESPONSIBILITY for error detection
/// with 'org_to_uninterpreted_nodes',
/// which runs earlier and detects a few errors that this one can't,
/// because this one acts on a tree of UncheckedViewNodes rather than raw text.
/// (Namely, Alias and AliasCol should not have body text.)
///
/// ASSUMES that in the "viewforest" (tree with BufferRoot):
/// - IDs have been replaced with PIDs, per
///   'assign_pids_throughout_viewforest'. (Otherwise two org nodes
///   might refer to the same skg node, yet appear not to.)
/// - All nodes have sources, per 'inherit_parent_source_if_possible'.
///
/// This is the unchecked-tree validation stage: metadata is complete,
/// but role classification, save-intent extraction, and disk
/// supplementation have not happened yet.
pub async fn find_buffer_errors_for_saving (
  viewforest: &Tree<UncheckedViewNode>,
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
      let merge_errors: Vec<String> =
        validate_merge_requests(viewforest, config, driver) . await?;
      merge_errors }
    { errors . push(
        BufferValidationError::Other (error_msg)); }}
  validate_definitive_view_requests(
    viewforest, &mut errors);
  { // local structure validation
    let _ = do_everywhere_in_tree_dfs_readonly(
      viewforest, viewforest . root() . id(),
      &mut |node_ref| {
        if let Err (e) = local::validate_local_structure(
               viewforest, node_ref . id(), config) {
          errors . push(
            BufferValidationError::LocalStructureViolation(
              e . message, e . id )); }
        Ok(( )) }); }
  Ok (errors) }

/// For each node in the viewforest, if it has a definitive view request,
/// verify that:
/// - The node is indefinitive.
/// - It has no content children (TrueNode children with birth ==
///   ContentOf). Non-content children — containerOf ancestry stubs,
///   linksTo references, scaffolds, etc. — don't block expansion:
///   they won't be clobbered by it.
/// - No other node with the same ID has a definitive view request,
///   because there can only be one definitive view.
fn validate_definitive_view_requests (
  viewforest : &Tree<UncheckedViewNode>, // "viewforest" = tree with BufferRoot
  errors : &mut Vec<BufferValidationError>,
) {
  let mut ids_with_requests : HashSet<ID> =
    HashSet::new();
  for edge in viewforest . root() . traverse()
  { if let Edge::Open (node_ref) = edge
    { let viewnode : &UncheckedViewNode =
        node_ref . value();
      if let UncheckedViewNodeKind::True (t) = &viewnode . kind
      { if t . view_requests . contains (&ViewRequest::Definitive)
        { if let Some (id) = &t . id {
          { // Error: must be indefinitive
            if ! t . is_indefinitive ()
            { errors . push( BufferValidationError::DefinitiveRequestOnDefinitiveNode( id . clone() )); }}
          { // Error: must have no content children.
            let has_content_children : bool =
              node_ref . children () . any ( |child| matches! (
                &child . value () . kind,
                UncheckedViewNodeKind::True (ct)
                  if ct . birth == Birth::ContentOf ));
            if has_content_children
            { errors . push( BufferValidationError::DefinitiveRequestOnNodeWithContentChildren( id . clone() )); }}
          { // Error: at most one request per ID
            if ! ids_with_requests . insert(id . clone())
            { errors . push( BufferValidationError::MultipleDefinitiveRequestsForSameId( id . clone() )); }} }}} }} }
