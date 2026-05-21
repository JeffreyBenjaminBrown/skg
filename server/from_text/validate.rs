use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName};
use crate::types::save::{DefineNode, SaveNode, DeleteNode, Merge, SourceMove};
use crate::types::nodes::complete::NodeComplete;

use std::collections::HashSet;
use typedb_driver::TypeDBDriver;

/// Validates that foreign (read-only) nodes are not being modified.
/// Filters out foreign nodes without modifications (no need to write).
///
/// ERRORS: if an instruction
/// - Would modify or delete a foreign node
/// - Would create a node in a foreign source
///
/// Requires disk-supplemented DefineNodes: unchanged foreign saves are
/// harmless only after unspecified fields have been filled from disk,
/// and foreign creates are recognized by checking disk for the pid.
pub async fn validate_and_filter_foreign_instructions(
  instructions       : Vec<DefineNode>,
  merge_instructions : &[Merge],
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
) -> Result<Vec<DefineNode>,
            Vec<BufferValidationError>> {
  let mut outcomes : Vec<ForeignPolicyOutcome> =
    Vec::new();
  let merge_definenodes : Vec<DefineNode> =
    merge_instructions . iter ()
    . flat_map ( |merge| merge . to_vec () )
    . collect ();
  for instruction in
    instructions . iter ()
    . chain (merge_definenodes . iter ())
  { outcomes . push (
      apply_foreign_policy(
        instruction, config, driver
      ) . await? ); }
  collect_foreign_policy_outcomes (&outcomes)?;
  Ok ( filter_unchanged_foreign_saves ( instructions,
                                        &outcomes )) }

/// PITFALL: This is applied to every node -- owned as well as foreign.
enum ForeignPolicyOutcome {
  Keep, // Safe to pass through to persistence.
  DropUnchangedForeignSave, // Safe to drop because the buffer expresses no change from disk.
  Reject(BufferValidationError), // Must reject before persistence.
}

async fn apply_foreign_policy(
  instr: &DefineNode,
  config: &SkgConfig,
  driver: &TypeDBDriver,
) -> Result<ForeignPolicyOutcome,
            Vec<BufferValidationError>> {
  match instr {
    DefineNode::Delete(DeleteNode { id, source }) => {
      if source_is_foreign (config, source) {
        // can't delete foreign nodes
        Ok (ForeignPolicyOutcome::Reject(
          BufferValidationError::ModifiedForeignNode(
            id . clone(),
            source . clone() )))
      } else { Ok (ForeignPolicyOutcome::Keep) }}
    DefineNode::Save(SaveNode (node)) => {
      if !source_is_foreign (config, &node . source) {
        // not foreign, so keep
        return Ok (ForeignPolicyOutcome::Keep); }
      match optnodecomplete_from_id(
        config, driver, &node . pid
      ) . await {
        Ok(Some (disk_node)) => {
          if buffernode_differs_from_disknode(node, &disk_node) {
            // can't edit foreign nodes
            Ok (ForeignPolicyOutcome::Reject(
              BufferValidationError::ModifiedForeignNode(
                node . pid . clone(),
                node . source . clone() )))
          } else {
            // drop a non-edit to a foreign node
            Ok (ForeignPolicyOutcome::DropUnchangedForeignSave)
          }}
        Ok (None) =>
          // Foreign source & PID not found
          // => trying to create a foreign node. Not allowed.
          Ok (ForeignPolicyOutcome::Reject(
            BufferValidationError::CreatedForeignNode(
              node . pid . clone(),
              node . source . clone() ))),
        Err (e) =>
          Err (vec![BufferValidationError::Other(
            format!("Error reading foreign node {}: {}",
                    node . pid . as_str(), e)) ] ) }}}
}

fn collect_foreign_policy_outcomes(
  outcomes: &[ForeignPolicyOutcome],
) -> Result<(), Vec<BufferValidationError>> {
  let mut errors: Vec<BufferValidationError> = Vec::new();
  for outcome in outcomes {
    if let ForeignPolicyOutcome::Reject (error) =
      outcome {
        errors . push (error . clone()); }}
  if errors . is_empty() { Ok (())
  } else { Err (errors) }}

/// Drop any DefineNode that
/// defines a foreign node to be unchanged.
fn filter_unchanged_foreign_saves(
  instructions: Vec<DefineNode>,
  outcomes: &[ForeignPolicyOutcome],
) -> Vec<DefineNode> {
  instructions . into_iter()
    . zip (outcomes)
    . filter_map (|(instruction, outcome)| {
      match outcome {
        ForeignPolicyOutcome::DropUnchangedForeignSave =>
          None,
        _ => Some (instruction) }})
    . collect()
}

fn source_is_foreign(
  config: &SkgConfig,
  source: &SourceName,
) -> bool {
  config . sources . get (source)
    . map(|s| !s . user_owns_it)
    . unwrap_or (false)}

/// Returns true if the buffer node differs from the disk node
/// in any definitive field (title, body, contains) or
/// any non-definitive field that the buffer expresses an opinion on.
///
/// For *definitive* fields (title, body, contains):
/// Some([]) and None are equivalent, so we normalize them for comparison.
///
/// For *non-definitive* fields (aliases, overrides_view_of,
/// subscribes_to, hides_from_its_subscriptions): Unspecified means "no opinion"
/// (because the user did not mention it in the buffer),
/// and therefore does not represent an edit.
pub(crate) fn buffernode_differs_from_disknode(
  buffer_node: &NodeComplete,
  disk_node: &NodeComplete,
) -> bool {
  fn fields_match<T: Clone + PartialEq>(
    buffer: &MSV<T>,
    disk: &MSV<T>,
  ) -> bool { buffer . is_unspecified()
              || flatten_ms (buffer) == flatten_ms (disk)
            }

  let title_matches: bool = buffer_node . title == disk_node . title;
  let body_matches: bool = buffer_node . body == disk_node . body;
  let source_matches: bool = buffer_node . source == disk_node . source;
  let contains_matches: bool =
    buffer_node . contains == disk_node . contains;
  !( title_matches
     && body_matches
     && source_matches
     && contains_matches
     && fields_match( &buffer_node . aliases,
                      &disk_node . aliases)
     && fields_match( &buffer_node . subscribes_to,
                      &disk_node . subscribes_to)
     && fields_match( &buffer_node . hides_from_its_subscriptions,
                      &disk_node . hides_from_its_subscriptions)
     && fields_match( &buffer_node . overrides_view_of,
                      &disk_node . overrides_view_of)) }

/// Lets us treat Specified([]) and Unspecified as equivalent.
pub(crate) fn flatten_ms<T: Clone>(
  v: &MSV<T>
) -> MSV<T> {
  match v {
    MSV::Specified (vec) if vec . is_empty() =>
      MSV::Unspecified,
    other => other . clone() }}

/// Validates that no node is both moved and merged in the same save.
///
/// Requires both completed non-merge extraction and merge extraction:
/// source moves are detected during disk supplementation of
/// DefineNodes, while merge acquiree/acquirer ids come from merge
/// requests in the "placed" (i.e. no longer "maybePlaced") viewforest.
pub(super) fn validate_no_simultaneous_move_and_merge (
  source_moves       : &[SourceMove],
  merge_instructions : &[Merge],
) -> Result<(), Vec<BufferValidationError>> {
  if source_moves . is_empty() || merge_instructions . is_empty() {
    return Ok (()); }
  let move_ids : HashSet<&ID> =
    source_moves . iter() . map(|sm| &sm . pid) . collect();
  let mut errors : Vec<BufferValidationError> = Vec::new();
  for merge in merge_instructions {
    if move_ids . contains (merge . acquirer_id()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          merge . acquirer_id() . clone() )); }
    if move_ids . contains (merge . acquiree_id()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          merge . acquiree_id() . clone() )); }}
  if errors . is_empty() { Ok (())
  } else { Err (errors) }}
