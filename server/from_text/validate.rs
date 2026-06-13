use crate::from_text::fork::fork_spec_from_buffer_node;
use crate::source_sets::ActiveSourceSet;
use crate::dbs::node_lookup::optNodeComplete_rustFIrst_by_id;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{ID, MSV, SkgConfig, SourceName};
use crate::types::save::{DefineNode, SaveNode, DeleteNode, ForkSpec, NodeMerge, SourceMove};
use crate::types::nodes::complete::NodeComplete;

use std::collections::{HashMap, HashSet};
use typedb_driver::TypeDBDriver;

/// Applies the foreign-node write policy, and -- this is where forking
/// begins -- turns an edit of a foreign node into a fork.
///
/// Returns the kept (owned) DefineNodes plus the forks the buffer
/// requested. Editing a foreign node N is read as a request to clone
/// it: N's own foreign SaveNode is DROPPED (N stays untouched on disk)
/// and a ForkSpec for the clone C is collected instead. The clone is
/// NOT folded into the returned DefineNodes here -- a save carrying
/// forks is gated on the user's confirmation, so the handler commits
/// the clones only on approval.
///
/// ERRORS: if an instruction
/// - Would DELETE a foreign node (deleting, unlike editing, is not a fork)
/// - Would create a node in a foreign source
/// - Requests a fork whose clone source cannot be resolved
///
/// Filters out foreign nodes without modifications (no need to write).
///
/// Requires disk-supplemented DefineNodes: unchanged foreign saves are
/// harmless only after unspecified fields have been filled from disk,
/// and foreign creates are recognized by checking disk for the pid.
pub async fn validate_and_filter_foreign_instructions(
  instructions       : Vec<DefineNode>,
  nodeMerge_instructions : &[NodeMerge],
  owned_ancestor_source : &HashMap<ID, SourceName>,
  user_set_fork_source : &HashMap<ID, SourceName>,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
) -> Result<(Vec<DefineNode>, Vec<ForkSpec>),
            Vec<BufferValidationError>> {
  let mut outcomes : Vec<ForeignPolicyOutcome> =
    Vec::new();
  let nodeMerge_definenodes : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |nodeMerge| nodeMerge . to_vec () )
    . collect ();
  // Only a DIRECT edit of a foreign node forks it. A foreign node
  // reached through a nodeMerge (merging into it, or deleting it as an
  // acquiree) is NOT a fork -- it stays a ModifiedForeignNode rejection.
  for instruction in instructions . iter () {
    outcomes . push (
      apply_foreign_policy(
        instruction, /* fork_eligible = */ true, config, driver
      ) . await? ); }
  for instruction in nodeMerge_definenodes . iter () {
    outcomes . push (
      apply_foreign_policy(
        instruction, /* fork_eligible = */ false, config, driver
      ) . await? ); }
  collect_foreign_policy_outcomes (&outcomes)?;
  // Build the clones from the fork candidates. A fork candidate only
  // ever arises from a regular `instructions` Save (a nodeMerge's saves
  // are owned), so zipping with `instructions` (outcomes for the
  // nodeMerge tail are beyond its length, harmlessly truncated) is
  // correct.
  let mut fork_specs : Vec<ForkSpec> = Vec::new ();
  let mut fork_errors : Vec<BufferValidationError> = Vec::new ();
  for outcome in &outcomes {
    if let ForeignPolicyOutcome::ForkCandidate (buffer_node) = outcome {
      match fork_spec_from_buffer_node (
        buffer_node, owned_ancestor_source, user_set_fork_source, config )
      { Ok (spec)  => fork_specs . push (spec),
        Err (e)    => fork_errors . push (e), }}}
  if ! fork_errors . is_empty () { return Err (fork_errors); }
  Ok (( filter_unchanged_foreign_saves ( instructions, &outcomes ),
        fork_specs )) }

/// PITFALL: This is applied to every node -- owned as well as foreign.
enum ForeignPolicyOutcome {
  Keep, // Safe to pass through to persistence.
  DropUnchangedForeignSave, // Safe to drop because the buffer expresses no change from disk.
  ForkCandidate(NodeComplete), // An edited foreign node: clone it (the buffer node N becomes the clone's template). Dropped from the DefineNodes; a ForkSpec is collected instead.
  Reject(BufferValidationError), // Must reject before persistence.
}

async fn apply_foreign_policy(
  instr: &DefineNode,
  fork_eligible: bool, // true for a direct buffer edit (which forks a changed foreign node); false for a nodeMerge-derived save (which still rejects).
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
      match optNodeComplete_rustFIrst_by_id(
        config, driver, &node . pid
      ) . await {
        Ok(Some (disk_node)) => {
          if buffernode_differs_from_disknode(node, &disk_node) {
            // A direct edit of a foreign node forks it (no longer a
            // ModifiedForeignNode error -- that rejection was the
            // absence of forking). The buffer node carries the edited
            // content the clone will copy. A nodeMerge-derived change to
            // a foreign node is NOT a fork and still rejects.
            if fork_eligible {
              Ok (ForeignPolicyOutcome::ForkCandidate( node . clone() ))
            } else {
              Ok (ForeignPolicyOutcome::Reject(
                BufferValidationError::ModifiedForeignNode(
                  node . pid . clone(),
                  node . source . clone() )))
            }
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

/// Drop any DefineNode that defines a foreign node to be unchanged, and
/// any that is a fork candidate (N's own save is never written -- N
/// stays untouched on disk; the clone C is committed separately, gated
/// on confirmation).
fn filter_unchanged_foreign_saves(
  instructions: Vec<DefineNode>,
  outcomes: &[ForeignPolicyOutcome],
) -> Vec<DefineNode> {
  instructions . into_iter()
    . zip (outcomes)
    . filter_map (|(instruction, outcome)| {
      match outcome {
        ForeignPolicyOutcome::DropUnchangedForeignSave
          | ForeignPolicyOutcome::ForkCandidate (_) =>
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
/// Requires both completed non-nodeMerge extraction and nodeMerge extraction:
/// source moves are detected during disk supplementation of
/// DefineNodes, while merge acquiree/acquirer ids come from merge
/// requests in the "placed" (i.e. no longer "maybePlaced") viewforest.
pub(super) fn validate_no_simultaneous_move_and_nodeMerge (
  source_moves       : &[SourceMove],
  nodeMerge_instructions : &[NodeMerge],
) -> Result<(), Vec<BufferValidationError>> {
  if source_moves . is_empty() || nodeMerge_instructions . is_empty() {
    return Ok (()); }
  let move_ids : HashSet<&ID> =
    source_moves . iter() . map(|sm| &sm . pid) . collect();
  let mut errors : Vec<BufferValidationError> = Vec::new();
  for nodeMerge in nodeMerge_instructions {
    if move_ids . contains (nodeMerge . acquirer_id()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquirer_id() . clone() )); }
    if move_ids . contains (nodeMerge . acquiree_id()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquiree_id() . clone() )); }}
  if errors . is_empty() { Ok (())
  } else { Err (errors) }}

/// TODO/full-schema/9-2_source-set-safety.org, inactive-node rewrite
/// suppression: under a restricted source-set, any instruction that
/// would modify an inactive node is DROPPED rather than executed or
/// fatal.  A stale buffer (rendered before a source-set switch) can
/// legitimately hold whole now-inactive subtrees; aborting would
/// force the user to delete them from view, which would itself be
/// destructive.  Runs after the noop filter, so an untouched stale
/// node (whose identical-to-disk instruction the noop filter already
/// discarded) does not count as suppressed.  Returns whether
/// anything was dropped, so the caller can attach the warning
/// "Inactive nodes present in saved buffer remain unchanged in
/// graph."
pub fn suppress_writes_to_inactive_nodes (
  define_nodes : Vec<DefineNode>,
  source_moves : Vec<SourceMove>,
  restricted_source_set : Option<&ActiveSourceSet>,
) -> (Vec<DefineNode>, Vec<SourceMove>, bool) {
  let Some (active) = restricted_source_set else {
    return (define_nodes, source_moves, false); };
  let mut suppressed : bool = false;
  let define_nodes : Vec<DefineNode> =
    define_nodes . into_iter ()
    . filter ( |instruction| {
        let source : &SourceName = match instruction {
          DefineNode::Save (SaveNode (node)) => &node . source,
          DefineNode::Delete (d)             => &d . source };
        let keep : bool = active . contains_source (source);
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  let source_moves : Vec<SourceMove> =
    source_moves . into_iter ()
    . filter ( |mv| {
        let keep : bool =
          active . contains_source (&mv . old_source)
          && active . contains_source (&mv . new_source);
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  (define_nodes, source_moves, suppressed) }
