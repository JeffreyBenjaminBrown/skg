use crate::from_text::fork::{CloneSkgRepoInputs, fork_spec_from_buffer_node};
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::dbs::node_lookup::opt_graphnode_by_skgid;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgRepoName, members_of};
use crate::types::save::{NodeInstruction, SaveNode, DeleteNode, ForkSpec, NodeMerge, SkgRepoMove};
use crate::types::nodes::complete::Graphnode;

use std::collections::{HashMap, HashSet};

/// Applies the foreign-node write policy, and -- this is where forking
/// begins -- turns an edit of a foreign node into a fork.
///
/// Returns the kept (owned) NodeInstructions plus the forks the buffer
/// requested. Editing a foreign node N is read as a request to clone
/// it: N's own foreign SaveNode is DROPPED (N stays untouched on disk)
/// and a ForkSpec for the clone C is collected instead. The clone is
/// NOT folded into the returned NodeInstructions here -- a save carrying
/// forks is gated on the user's confirmation, so the handler skgsave-commits
/// the clones only on approval.
///
/// ERRORS: if an nodeInstruction
/// - Would DELETE a foreign node (deleting, unlike editing, is not a fork)
/// - Would create a node in a foreign skgrepo
/// - Requests a fork whose clone skgrepo cannot be resolved
///
/// Filters out foreign nodes without modifications (no need to write).
///
/// Requires disk-supplemented NodeInstructions: unchanged foreign saves are
/// harmless only after unspecified fields have been filled from disk,
/// and foreign creates are recognized by checking disk for the pid.
pub fn validate_and_filter_foreign_instructions(
  instructions       : Vec<NodeInstruction>,
  nodeMerge_instructions : &[NodeMerge],
  graph              : &InRustGraph,
  clone_skgrepo_inputs : &CloneSkgRepoInputs, // everything clone-repo resolution can draw on, in priority order
  adopt_clone_skgrepo : &HashMap<ID, ID>, // new node -> forked N whose clone's repo it adopts (see 'new_foreign_nodes_adopting_clone_repos')
  config             : &SkgConfig,
) -> Result<(Vec<NodeInstruction>, Vec<ForkSpec>),
            Vec<BufferValidationError>> {
  let mut outcomes : Vec<ForeignPolicyOutcome> =
    Vec::new();
  let nodeMerge_nodeInstructions : Vec<NodeInstruction> =
    nodeMerge_instructions . iter ()
    . flat_map ( |nodeMerge| nodeMerge . to_vec () )
    . collect ();
  // Only a DIRECT edit of a foreign node forks it. A foreign node
  // reached through a nodeMerge (merging into it, or deleting it as an
  // acquiree) is NOT a fork -- it stays a ModifiedForeignNode rejection.
  for instruction in instructions . iter () {
    outcomes . push (
      apply_foreign_policy(
        instruction, /* fork_eligible = */ true,
        adopt_clone_skgrepo, graph, config
      )? ); }
  { let no_adoptions : HashMap<ID, ID> = HashMap::new ();
    for instruction in nodeMerge_nodeInstructions . iter () {
      outcomes . push (
        apply_foreign_policy(
          instruction, /* fork_eligible = */ false,
          &no_adoptions, graph, config
        )? ); }}
  collect_foreign_policy_outcomes (&outcomes)?;
  // Build the clones from the fork candidates. A fork candidate only
  // ever arises from a regular `instructions` Save (a nodeMerge's saves
  // are owned), so zipping with `instructions` (outcomes for the
  // nodeMerge tail are beyond its length, harmlessly truncated) is
  // correct.
  let mut fork_specs : Vec<ForkSpec> = Vec::new ();
  let mut fork_errors : Vec<BufferValidationError> = Vec::new ();
  for outcome in &outcomes {
    if let ForeignPolicyOutcome::ForkCandidate (buffer_node, disk_node)
      = outcome {
      match fork_spec_from_buffer_node (
        buffer_node, & disk_node . title,
        & members_of (& disk_node . contains),
        clone_skgrepo_inputs )
      { Ok (spec)  => fork_specs . push (spec),
        Err (e)    => fork_errors . push (e), }}}
  if ! fork_errors . is_empty () { return Err (fork_errors); }
  let kept : Vec<NodeInstruction> =
    finalize_foreign_policy_instructions (
      instructions, &outcomes, &fork_specs ) ?;
  Ok (( kept, fork_specs )) }

/// PITFALL: This is applied to every node -- owned as well as foreign.
enum ForeignPolicyOutcome {
  Keep, // Safe to pass through to persistence.
  DropUnchangedForeignSave, // Safe to drop because the buffer expresses no change from disk.
  ForkCandidate(Graphnode, // An edited foreign node: clone it (the buffer node N becomes the clone's template). Dropped from the NodeInstructions; a ForkSpec is collected instead.
               Graphnode), // N's DISK node -- the original, before the user's edit. Its title feeds the confirmation buffer's child line (which shows the original honestly, distinct from the clone's edited title); its contains feed the clone's creation-time hides (children the forking edit deleted).
  AdoptCloneSkgRepo(ID), // A NEW node (bare headline) whose foreign skgrepo was inherited from the named forked node N: kept, but rewritten to N's clone's skgrepo once the ForkSpecs exist ('finalize_foreign_policy_instructions').
  Reject(BufferValidationError), // Must reject before persistence.
}

fn apply_foreign_policy(
  instr: &NodeInstruction,
  fork_eligible: bool, // true for a direct buffer edit (which forks a changed foreign node); false for a nodeMerge-derived save (which still rejects).
  adopt_clone_skgrepo: &HashMap<ID, ID>, // new node -> forked N (empty for nodeMerge-derived saves)
  graph: &InRustGraph,
  config: &SkgConfig,
) -> Result<ForeignPolicyOutcome,
            Vec<BufferValidationError>> {
  match instr {
    NodeInstruction::Delete(DeleteNode { skgid, home_skgrepo: skgrepo }) => {
      if skgrepo_is_foreign (config, skgrepo) {
        // can't delete foreign nodes
        Ok (ForeignPolicyOutcome::Reject(
          BufferValidationError::ModifiedForeignNode(
            skgid . clone(),
            skgrepo . clone() )))
      } else { Ok (ForeignPolicyOutcome::Keep) }}
    NodeInstruction::Save(SaveNode (node)) => {
      if !skgrepo_is_foreign (config, &node . home_skgrepo) {
        // not foreign, so keep
        return Ok (ForeignPolicyOutcome::Keep); }
      match opt_graphnode_by_skgid(
        graph, config, &node . pid
      ) {
        Ok(Some (disk_node)) => {
          if buffernode_differs_from_disknode(node, &disk_node) {
            // A direct edit of a foreign node forks it (no longer a
            // ModifiedForeignNode error -- that rejection was the
            // absence of forking). The buffer node carries the edited
            // content the clone will copy. A nodeMerge-derived change to
            // a foreign node is NOT a fork and still rejects.
            if fork_eligible {
              Ok (ForeignPolicyOutcome::ForkCandidate(
                node . clone(), disk_node ))
            } else {
              Ok (ForeignPolicyOutcome::Reject(
                BufferValidationError::ModifiedForeignNode(
                  node . pid . clone(),
                  node . home_skgrepo . clone() )))
            }
          } else {
            // drop a non-edit to a foreign node
            Ok (ForeignPolicyOutcome::DropUnchangedForeignSave)
          }}
        Ok (None) => {
          // Foreign skgrepo & PID not found => trying to create a
          // foreign node. Not allowed -- EXCEPT for a bare new
          // headline whose foreign skgrepo was merely inherited from a
          // forked parent: that one adopts the parent's clone's skgrepo.
          if let Some (forked) = adopt_clone_skgrepo . get (&node . pid) {
            Ok (ForeignPolicyOutcome::AdoptCloneSkgRepo(
              forked . clone() ))
          } else {
            Ok (ForeignPolicyOutcome::Reject(
              BufferValidationError::CreatedForeignNode(
                node . pid . clone(),
                node . home_skgrepo . clone() ))) }},
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

/// Drop any NodeInstruction that defines a foreign node to be unchanged,
/// and any that is a fork candidate (N's own save is never written --
/// N stays untouched on disk; the clone C is skgsave-committed separately,
/// gated on confirmation). A new node adopting a clone's skgrepo is
/// KEPT, rewritten into that skgrepo -- its fork must exist among the
/// specs, else it degrades to the foreign-creation rejection it would
/// otherwise have been.
fn finalize_foreign_policy_instructions(
  instructions: Vec<NodeInstruction>,
  outcomes: &[ForeignPolicyOutcome],
  fork_specs: &[ForkSpec],
) -> Result<Vec<NodeInstruction>, Vec<BufferValidationError>> {
  let mut kept   : Vec<NodeInstruction> = Vec::new();
  let mut errors : Vec<BufferValidationError> = Vec::new();
  for (instruction, outcome) in
    instructions . into_iter() . zip (outcomes) {
    match outcome {
      ForeignPolicyOutcome::DropUnchangedForeignSave
        | ForeignPolicyOutcome::ForkCandidate (..) =>
        {},
      ForeignPolicyOutcome::AdoptCloneSkgRepo (forked) => {
        let clone_skgrepo : Option<&SkgRepoName> =
          fork_specs . iter()
          . find ( |spec| spec . original_skgid == *forked )
          . map ( |spec| & spec . clone . 0 . home_skgrepo );
        match (instruction, clone_skgrepo) {
          ( NodeInstruction::Save (SaveNode (mut node)),
            Some (clone_skgrepo) ) => {
            rehome_inherited_new_node (&mut node, clone_skgrepo);
            kept . push (NodeInstruction::Save (SaveNode (node))); }
          ( NodeInstruction::Save (SaveNode (node)), None ) =>
            errors . push (
              BufferValidationError::CreatedForeignNode(
                node . pid . clone(),
                node . home_skgrepo . clone() )),
          ( NodeInstruction::Delete (d), _ ) =>
            // Unreachable: only a Save earns AdoptCloneRepo.
            errors . push (
              BufferValidationError::ModifiedForeignNode(
                d . skgid . clone(),
                d . home_skgrepo . clone() )), }},
      _ => kept . push (instruction), }}
  if errors . is_empty() { Ok (kept) } else { Err (errors) }}

/// A bare new node beneath a foreign node initially inherits that foreign
/// skgrepo everywhere: as its home and as the default recording skgrepo of its
/// relationship members. When the node rides the parent's fork, adoption must
/// therefore rehome both. Changing only `node.repo` manufactures a mixed
/// telescope whose old-home section is foreign (or can sort before the new
/// home), and the checked writer correctly rejects it.
///
/// Preserve members explicitly recorded at any OTHER skgrepo. Only facts whose
/// relRepo equals the inherited home are part of this implicit adoption.
fn rehome_inherited_new_node (
  node        : &mut Graphnode,
  new_skgrepo : &SkgRepoName,
) {
  fn retag<T> (
    members     : &mut [RelPartner<T>],
    old_skgrepo : &SkgRepoName,
    new_skgrepo : &SkgRepoName,
  ) {
    for member in members {
      if member . relRepo == *old_skgrepo {
        member . relRepo = new_skgrepo . clone (); }} }

  fn retag_msv<T> (
    members     : &mut MSV<RelPartner<T>>,
    old_skgrepo : &SkgRepoName,
    new_skgrepo : &SkgRepoName,
  ) {
    if let MSV::Specified (members) = members {
      retag (members, old_skgrepo, new_skgrepo); }}

  let old_skgrepo : SkgRepoName = node . home_skgrepo . clone ();
  node . home_skgrepo = new_skgrepo . clone ();
  retag (&mut node . contains, &old_skgrepo, new_skgrepo);
  retag_msv (&mut node . aliases, &old_skgrepo, new_skgrepo);
  retag_msv (&mut node . subscribesTo, &old_skgrepo, new_skgrepo);
  retag_msv (
    &mut node . hidesFromSubs, &old_skgrepo, new_skgrepo);
  retag_msv (&mut node . overrides, &old_skgrepo, new_skgrepo);
}

fn skgrepo_is_foreign(
  config: &SkgConfig,
  skgrepo: &SkgRepoName,
) -> bool {
  config . skgrepos . get (skgrepo)
    . map(|s| !s . owned)
    . unwrap_or (false)}

/// Returns true if the buffer node differs from the disk node
/// in any editable field (title, body, contains), any flag, or
/// any non-editable field that the buffer expresses an opinion on.
///
/// For *editable* fields (title, body, contains):
/// Some([]) and None are equivalent, so we normalize them for comparison.
///
/// For *non-editable* fields (aliases, overrides,
/// subscribesTo, hidesFromSubs): Unspecified means "no opinion"
/// (because the user did not mention it in the buffer),
/// and therefore does not represent an edit.
pub(crate) fn buffernode_differs_from_disknode(
  buffer_node: &Graphnode,
  disk_node: &Graphnode,
) -> bool {
  fn fields_match<T: Clone + PartialEq>(
    buffer: &MSV<T>,
    disk: &MSV<T>,
  ) -> bool { buffer . is_unspecified()
              || flatten_ms (buffer) == flatten_ms (disk)
            }

  let title_matches: bool = buffer_node . title == disk_node . title;
  let body_matches: bool = buffer_node . body == disk_node . body;
  let skgrepo_matches: bool = buffer_node . home_skgrepo == disk_node . home_skgrepo;
  let contains_matches: bool =
    buffer_node . contains == disk_node . contains;
  let flags_match: bool = buffer_node . flags == disk_node . flags;
  !( title_matches
     && body_matches
     && skgrepo_matches
     && contains_matches
     && flags_match
     && fields_match( &buffer_node . aliases,
                      &disk_node . aliases)
     && fields_match( &buffer_node . subscribesTo,
                      &disk_node . subscribesTo)
     && fields_match( &buffer_node . hidesFromSubs,
                      &disk_node . hidesFromSubs)
     && fields_match( &buffer_node . overrides,
                      &disk_node . overrides)) }

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
/// Skgrepo moves are detected during disk supplementation of
/// NodeInstructions, while merge acquiree/acquirer ids come from merge
/// requests in the "placed" (i.e. no longer "maybePlaced") viewforest.
pub(super) fn validate_no_simultaneous_move_and_nodeMerge (
  skgrepo_moves          : &[SkgRepoMove],
  nodeMerge_instructions : &[NodeMerge],
) -> Result<(), Vec<BufferValidationError>> {
  if skgrepo_moves . is_empty() || nodeMerge_instructions . is_empty() {
    return Ok (()); }
  let move_skgids : HashSet<&ID> =
    skgrepo_moves . iter() . map(|sm| &sm . pid) . collect();
  let mut errors : Vec<BufferValidationError> = Vec::new();
  for nodeMerge in nodeMerge_instructions {
    if move_skgids . contains (nodeMerge . acquirer_skgid()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquirer_skgid() . clone() )); }
    if move_skgids . contains (nodeMerge . acquiree_skgid()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquiree_skgid() . clone() )); }}
  if errors . is_empty() { Ok (())
  } else { Err (errors) }}

/// TODO/DONE/full-schema/DONE/9-2_source-set-safety.org, inactive-node rewrite
/// suppression: under a restricted skgrepo-set, any nodeInstruction that
/// would modify an inactive node is DROPPED rather than executed or
/// fatal.  A stale buffer (rendered before a skgrepo-set switch) can
/// legitimately hold whole now-inactive subtrees; aborting would
/// force the user to delete them from view, which would itself be
/// destructive.  Runs after the noop filter, so an untouched stale
/// node (whose identical-to-disk nodeInstruction the noop filter already
/// discarded) does not count as suppressed.  Returns whether
/// anything was dropped, so the caller can attach the warning
/// "Inactive nodes present in saved buffer remain unchanged in
/// graph."
pub fn suppress_writes_to_inactive_nodes (
  node_instructions : Vec<NodeInstruction>,
  skgrepo_moves : Vec<SkgRepoMove>,
  restricted_skgrepo_set : Option<&ActiveSkgRepoSet>,
) -> (Vec<NodeInstruction>, Vec<SkgRepoMove>, bool) {
  let Some (active) = restricted_skgrepo_set else {
    return (node_instructions, skgrepo_moves, false); };
  let mut suppressed : bool = false;
  let node_instructions : Vec<NodeInstruction> =
    node_instructions . into_iter ()
    . filter ( |instruction| {
        let skgrepo : &SkgRepoName = match instruction {
          NodeInstruction::Save (SaveNode (node)) => &node . home_skgrepo,
          NodeInstruction::Delete (d)             => &d . home_skgrepo };
        let keep : bool = active . contains_skgrepo (skgrepo);
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  let skgrepo_moves : Vec<SkgRepoMove> =
    skgrepo_moves . into_iter ()
    . filter ( |mv| {
        let keep : bool =
          active . contains_skgrepo (&mv . old_skgrepo)
          && active . contains_skgrepo (&mv . new_skgrepo);
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  (node_instructions, skgrepo_moves, suppressed) }
