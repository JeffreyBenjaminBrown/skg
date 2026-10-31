use crate::from_text::fork::{CloneSkgrepoInputs, fork_spec_from_buffer_node};
use crate::dbs::in_rust_graph::relation_accessors::GnidPresence;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::dbs::node_lookup::{graphnode_from_graph, opt_graphnode_by_gnid};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::errors::BufferValidationError;
use crate::types::misc::{
  ID, MSV, RelPartner, RelationshipMemberKey, SkgConfig, SkgrepoName};
use crate::types::save::{NodeInstruction, SaveNode, DeleteNode, ForkSpec, NodeMerge, SkgrepoMove};
use crate::types::nodes::complete::Graphnode;

use std::collections::{HashMap, HashSet};

/// Applies the foreign-node write policy, and -- this is where forking
/// begins -- turns an edit of a foreign node into a fork.
///
/// Returns the kept (owned) NodeInstructions plus the forks the buffer
/// requested. Editing a foreign node N is read as a request to clone
/// it: N's own foreign SaveNode is dropped (N stays untouched on disk)
/// and a ForkSpec for the clone C is collected instead. The clone is
/// not folded into the returned NodeInstructions here -- a save carrying
/// forks is gated on the user's confirmation, so the handler skgsave-commits
/// the clones only on approval.
///
/// ERRORS: if an nodeInstruction
/// - Would delete a foreign node (deleting, unlike editing, is not a fork)
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
  clone_skgrepo_inputs : &CloneSkgrepoInputs, // everything clone-repo resolution can use, in priority order
  adopt_clone_skgrepo : &HashMap<ID, ID>, // new node -> forked N whose clone's repo it adopts (see 'new_foreign_nodes_adopting_clone_repos')
  config             : &SkgConfig,
  skgrepo_restriction : &SkgrepoRestriction, // the clones copy only the records it shows ('build_fork_clone')
) -> Result<(Vec<NodeInstruction>, Vec<ForkSpec>),
            Vec<BufferValidationError>> {
  let mut outcomes : Vec<ForeignPolicyOutcome> =
    Vec::new();
  let nodeMerge_nodeInstructions : Vec<NodeInstruction> =
    nodeMerge_instructions . iter ()
    . flat_map ( |nodeMerge| nodeMerge . to_vec () )
    . collect ();
  // Only a direct edit of a foreign node forks it. A foreign node
  // reached through a nodeMerge (merging into it, or deleting it as an
  // acquiree) is not a fork -- it stays a ModifiedForeignNode rejection.
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
        & disk_node . contains,
        clone_skgrepo_inputs, config, skgrepo_restriction )
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
               Graphnode), // N's disk node -- the original, before the user's edit. Its title feeds the confirmation buffer's child line (which shows the original honestly, distinct from the clone's edited title); its contains feed the clone's creation-time hides (children the forking edit deleted).
  AdoptCloneSkgrepo(ID), // A new node (bare headline) whose foreign skgrepo was inherited from the named forked node N: kept, but rewritten to N's clone's skgrepo once the ForkSpecs exist ('finalize_foreign_policy_instructions').
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
    NodeInstruction::Delete(DeleteNode { gnid, home_skgrepo: skgrepo }) => {
      if skgrepo_is_foreign (config, skgrepo) {
        // can't delete foreign nodes
        Ok (ForeignPolicyOutcome::Reject(
          BufferValidationError::ModifiedForeignNode(
            gnid . clone(),
            skgrepo . clone() )))
      } else { Ok (ForeignPolicyOutcome::Keep) }}
    NodeInstruction::Save(SaveNode (node)) => {
      if !skgrepo_is_foreign (config, &node . home_skgrepo) {
        // not foreign, so keep
        return Ok (ForeignPolicyOutcome::Keep); }
      match opt_graphnode_by_gnid(
        graph, config, &node . pid
      ) {
        Ok(Some (disk_node)) => {
          if buffernode_differs_from_disknode(node, &disk_node) {
            // A direct edit of a foreign node forks it (no longer a
            // ModifiedForeignNode error -- that rejection was the
            // absence of forking). The buffer node carries the edited
            // content the clone will copy. A nodeMerge-derived change to
            // a foreign node is not a fork and still rejects.
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
          // foreign node. Not allowed -- except for a bare new
          // headline whose foreign skgrepo was merely inherited from a
          // forked parent: that one adopts the parent's clone's skgrepo.
          if let Some (forked) = adopt_clone_skgrepo . get (&node . pid) {
            Ok (ForeignPolicyOutcome::AdoptCloneSkgrepo(
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
/// kept, rewritten into that skgrepo -- its fork must exist among the
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
      ForeignPolicyOutcome::AdoptCloneSkgrepo (forked) => {
        let clone_skgrepo : Option<&SkgrepoName> =
          fork_specs . iter()
          . find ( |spec| spec . original_gnid == *forked )
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
                d . gnid . clone(),
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
/// Preserve members explicitly recorded at any other skgrepo. Only facts whose
/// relRepo equals the inherited home are part of this implicit adoption.
fn rehome_inherited_new_node (
  node        : &mut Graphnode,
  new_skgrepo : &SkgrepoName,
) {
  fn retag<T> (
    members     : &mut [RelPartner<T>],
    old_skgrepo : &SkgrepoName,
    new_skgrepo : &SkgrepoName,
  ) {
    for member in members {
      if member . relRepo == *old_skgrepo {
        member . relRepo = new_skgrepo . clone (); }} }

  fn retag_msv<T> (
    members     : &mut MSV<RelPartner<T>>,
    old_skgrepo : &SkgrepoName,
    new_skgrepo : &SkgrepoName,
  ) {
    if let MSV::Specified (members) = members {
      retag (members, old_skgrepo, new_skgrepo); }}

  let old_skgrepo : SkgrepoName = node . home_skgrepo . clone ();
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
  skgrepo: &SkgrepoName,
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
/// requests in the parsed viewforest.
pub(super) fn validate_no_simultaneous_move_and_nodeMerge (
  skgrepo_moves          : &[SkgrepoMove],
  nodeMerge_instructions : &[NodeMerge],
) -> Result<(), Vec<BufferValidationError>> {
  if skgrepo_moves . is_empty() || nodeMerge_instructions . is_empty() {
    return Ok (()); }
  let move_gnids : HashSet<&ID> =
    skgrepo_moves . iter() . map(|sm| &sm . pid) . collect();
  let mut errors : Vec<BufferValidationError> = Vec::new();
  for nodeMerge in nodeMerge_instructions {
    if move_gnids . contains (nodeMerge . acquirer_gnid()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquirer_gnid() . clone() )); }
    if move_gnids . contains (nodeMerge . acquiree_gnid()) {
      errors . push (
        BufferValidationError::CannotMoveAndMergeSimultaneously(
          nodeMerge . acquiree_gnid() . clone() )); }}
  if errors . is_empty() { Ok (())
  } else { Err (errors) }}

/// Restricted-node rewrite suppression (the API doc's "Restricted-skgrepo nodes"): under a skgrepo restriction, any nodeInstruction that
/// would modify a restricted node is dropped rather than executed or
/// fatal.  A stale buffer (rendered before a switch of the skgrepo restriction) can
/// legitimately hold whole now-restricted subtrees; aborting would
/// force the user to delete them from view, which would itself be
/// destructive.  A node is restricted when the skgrepo restriction
/// keeps out its home as the graph has it, or as the buffer gives it:
/// a headline for a restricted graphnode, pasted with a visible
/// nodeRepo, leaves the graphnode where it is.  Runs after the noop
/// filter, so an untouched stale node (whose identical-to-disk
/// nodeInstruction the noop filter already discarded) does not count
/// as suppressed.  Returns whether anything was dropped, so the caller
/// can attach the warning "Restricted nodes present in saved buffer
/// remain unchanged in graph."
pub fn suppress_writes_to_restricted_nodes (
  node_instructions : Vec<NodeInstruction>,
  skgrepo_moves : Vec<SkgrepoMove>,
  graph : &InRustGraph,
  skgrepo_restriction : &SkgrepoRestriction,
) -> (Vec<NodeInstruction>, Vec<SkgrepoMove>, bool) {
  if skgrepo_restriction . is_unrestricted () {
    return (node_instructions, skgrepo_moves, false); }
  let restriction : &SkgrepoRestriction = skgrepo_restriction;
  let restricted = |gnid : &ID, buffer_home : &SkgrepoName| -> bool {
    SkgrepoRestriction::node_is_restricted (restriction, buffer_home)
    || graph . gnid_presence_gated (gnid, restriction) == GnidPresence::Restricted };
  let mut suppressed : bool = false;
  let node_instructions : Vec<NodeInstruction> =
    node_instructions . into_iter ()
    . filter ( |instruction| {
        let keep : bool = ! match instruction {
          NodeInstruction::Save (SaveNode (node)) =>
            restricted (&node . pid, &node . home_skgrepo),
          NodeInstruction::Delete (d) =>
            restricted (&d . gnid, &d . home_skgrepo), };
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  let skgrepo_moves : Vec<SkgrepoMove> =
    skgrepo_moves . into_iter ()
    . filter ( |mv| {
        let keep : bool =
          ! SkgrepoRestriction::node_is_restricted (restriction, &mv . skgrepo_change . old)
          && ! SkgrepoRestriction::node_is_restricted (restriction, &mv . skgrepo_change . new);
        if ! keep { suppressed = true; }
        keep } )
    . collect ();
  (node_instructions, skgrepo_moves, suppressed) }

/// Under a skgrepo restriction, refuses each record a save would write
/// into a section the skgrepo restriction keeps out, unless the graph
/// already holds it there: a new record (a member typed or pasted, an
/// inferred hide, the default floor of either landing in a restricted
/// section) or a moved one (an explicit relRepo request). It refuses
/// too each record the graph holds in such a section that the save
/// would hold at another relRepo, moved out of sight's reach. What the
/// user could not see, the user did not edit, and an invisible record
/// is never created silently. 'node_instructions' are the save's, after
/// the suppression of writes to restricted nodes, so each recorder
/// checked is visible.
pub fn refuse_records_landing_restricted (
  node_instructions   : &[NodeInstruction],
  graph               : &InRustGraph,
  skgrepo_restriction : &SkgrepoRestriction,
) -> Vec<BufferValidationError> {
  if skgrepo_restriction . is_unrestricted () { return Vec::new (); }
  let restriction : &SkgrepoRestriction = skgrepo_restriction;
  let mut errors : Vec<BufferValidationError> = Vec::new ();
  for instruction in node_instructions {
    let NodeInstruction::Save (SaveNode (node)) = instruction else { continue; };
    let disk : Option<Graphnode> = graphnode_from_graph (graph, &node . pid);
    let mut refuse_new_restricted_records =
      | relation : &str,
        records  : &[RelPartner<ID>],
        on_disk  : &[RelPartner<ID>] | {
        for record in records {
          if SkgrepoRestriction::record_is_visible (restriction, &record . relRepo)
            { continue; }
          let key : RelationshipMemberKey = graph . relationship_member_key (&record . member);
          let already_there : bool = on_disk . iter () . any ( |held|
            held . relRepo == record . relRepo
            && graph . relationship_member_key (&held . member) == key );
          if ! already_there {
            errors . push ( BufferValidationError::RecordSkgrepoRestricted (
              node . pid . clone (), relation . to_string (),
              record . member . 0 . clone (), record . relRepo . clone () )); }}
        for held in on_disk {
          // A restricted record the save holds at another relRepo: a
          // headline pasted for its member, with an explicit relRepo
          // request, would move it into view.
          if SkgrepoRestriction::record_is_visible (restriction, &held . relRepo)
            { continue; }
          let key : RelationshipMemberKey = graph . relationship_member_key (&held . member);
          let same_member = |record : &&RelPartner<ID>| -> bool {
            graph . relationship_member_key (&record . member) == key };
          let moved : bool =
            records . iter () . any ( |record| same_member (&record) )
            && ! records . iter () . filter (same_member)
                 . any ( |record| record . relRepo == held . relRepo );
          if moved {
            errors . push ( BufferValidationError::RecordMovedOutOfRestrictedSkgrepo (
              node . pid . clone (), relation . to_string (),
              held . member . 0 . clone (), held . relRepo . clone () )); }} };
    let disk_list = |list : fn (&Graphnode) -> &[RelPartner<ID>]| -> Vec<RelPartner<ID>> {
      disk . as_ref () . map ( |disk| list (disk) . to_vec () ) . unwrap_or_default () };
    refuse_new_restricted_records (
      "contains", &node . contains, &disk_list ( |n| &n . contains ));
    refuse_new_restricted_records (
      "subscribesTo", node . subscribesTo . or_default (),
      &disk_list ( |n| n . subscribesTo . or_default () ));
    refuse_new_restricted_records (
      "hidesFromSubs", node . hidesFromSubs . or_default (),
      &disk_list ( |n| n . hidesFromSubs . or_default () ));
    refuse_new_restricted_records (
      "overrides", node . overrides . or_default (),
      &disk_list ( |n| n . overrides . or_default () ));
    for alias in node . aliases . or_default () {
      if SkgrepoRestriction::record_is_visible (restriction, &alias . relRepo)
        { continue; }
      let already_there : bool = disk . as_ref () . is_some_and ( |disk|
        disk . aliases . or_default () . iter () . any ( |held|
          held . relRepo == alias . relRepo && held . member == alias . member ));
      if ! already_there {
        errors . push ( BufferValidationError::RecordSkgrepoRestricted (
          node . pid . clone (), "aliases" . to_string (),
          alias . member . clone (), alias . relRepo . clone () )); }} }
  errors }
