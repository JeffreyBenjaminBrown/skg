//! A checked graph candidate tied to the exact graph snapshot it was derived from.
//!
//! The constructor is the only way to obtain this token.  Store orchestration
//! can inspect its nodeInstructions and candidate while preflighting later work,
//! but the swap-in consumes it and never reapplies nodeInstructions to a newer
//! graph.

use crate::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle, apply_nodeInstructions_to_inRustGraph,
  inbound_recorders_at,
};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, format_complete_graph_errors,
};
use crate::dbs::in_rust_graph::internal_index_validation::{
  InternalIndexMismatch, LocalIndexValidation, format_internal_index_mismatches,
  validate_local_internal_indexes,
};
use crate::dbs::in_rust_graph::override_invariants::{
  AffectedOverrideValidation, OverrideCheckScope, OverrideInvariantViolation,
  derive_affected_override_scope,
  validate_affected_override_invariants_with_counts,
};
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{NodeInstruction, DeleteNode, SaveNode};
use crate::telescope::invariants::derive_affected_telescope_recorders;

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct CanonicalizationChange {
  pub(crate) skgid        : ID,
  pub(crate) old_carrier  : Option<ID>,
  pub(crate) new_carrier  : Option<ID>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct IdentityAcquisition {
  pub(crate) skgid        : ID,
  pub(crate) old_carrier  : Option<ID>,
  pub(crate) new_carrier  : ID,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GraphChangeSet {
  pub(crate) touched_pids             : HashSet<ID>,
  pub(crate) saved_pids               : HashSet<ID>,
  pub(crate) deleted_pids             : HashSet<ID>,
  pub(crate) affected_skgids             : HashSet<ID>,
  pub(crate) canonicalization_changes : Vec<CanonicalizationChange>,
  pub(crate) identity_acquisitions    : Vec<IdentityAcquisition>,
  pub(crate) recorders_to_reindex         : HashSet<ID>,
  pub(crate) override_skgrepos_to_check : HashSet<ID>,
  pub(crate) override_targets_to_check : HashSet<ID>,
  pub(crate) telescope_recorders_to_check : HashSet<ID>,
  #[cfg(test)]
  pub(crate) identity_base_lookup_bound : usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ExtraIdRevocation {
  pub(crate) pid            : ID,
  pub(crate) title          : String,
  pub(crate) dropped_skgids : Vec<ID>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct OverrideParticipant {
  pub(crate) skgid    : ID,
  pub(crate) title : String,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct MergeOverrideCollision {
  pub(crate) acquired_skgid       : ID,
  pub(crate) acquirer             : OverrideParticipant,
  pub(crate) acquiree             : OverrideParticipant,
  pub(crate) existing_overriders  : Vec<OverrideParticipant>,
  pub(crate) redirected_overriders : Vec<OverrideParticipant>,
}

#[derive(Debug)]
pub(crate) struct GraphUpdatePreparationError {
  pub(crate) complete_graph_errors : Vec<CompleteGraphError>,
  pub(crate) extra_id_revocations  : Vec<ExtraIdRevocation>,
  pub(crate) internal_index_errors : Vec<InternalIndexMismatch>,
  pub(crate) merge_override_collisions : Vec<MergeOverrideCollision>,
}

impl fmt::Display for GraphUpdatePreparationError {
  fn fmt (
    &self,
    f : &mut fmt::Formatter<'_>,
  ) -> fmt::Result {
    let mut sections : Vec<String> = Vec::new ();
    if ! self . complete_graph_errors . is_empty () {
      sections . push (format_complete_graph_errors (
        &self . complete_graph_errors)); }
    if ! self . internal_index_errors . is_empty () {
      sections . push (format_internal_index_mismatches (
        &self . internal_index_errors)); }
    for revocation in &self . extra_id_revocations {
      sections . push (format! (
        "Cannot save node '{}' ('{}') because it would revoke existing extra ID(s): {}. Skg keeps acquired IDs stable because another dataset may link to them. Raw-file editing followed by rebuild remains the explicit escape hatch.",
        revocation . pid,
        revocation . title,
        revocation . dropped_skgids . iter ()
          . map ( |skgid| skgid . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ") )); }
    for collision in &self . merge_override_collisions {
      sections . push (format_merge_override_collision (collision)); }
    write! (f, "{}", sections . join ("\n"))
  }
}

impl Error for GraphUpdatePreparationError {}

impl GraphUpdatePreparationError {
  pub(crate) fn is_internal (&self) -> bool {
    ! self . internal_index_errors . is_empty () }

  pub(crate) fn is_override_invariant_only (&self) -> bool {
    ! self . complete_graph_errors . is_empty ()
      && self . complete_graph_errors . iter () . all (|error|
        matches! (error, CompleteGraphError::Override (_)))
      && self . extra_id_revocations . is_empty ()
      && self . internal_index_errors . is_empty ()
      && self . merge_override_collisions . is_empty ()
  }
}

struct NormalizedNodeInstructionBatch {
  filesystem_nodeInstructions : Vec<NodeInstruction>,
  final_graph_nodeInstructions : Vec<NodeInstruction>,
}

#[derive(Default)]
struct CarriersForId {
  primary : BTreeSet<ID>,
  extra   : BTreeSet<ID>,
}

#[derive(Debug)]
pub(crate) struct PreparedGraphUpdate {
  base             : Arc<InRustGraph>,
  candidate        : Arc<InRustGraph>,
  nodeInstructions : Vec<NodeInstruction>,
  // Retained as proof metadata and consumed by the batch-aware mutator.
  #[allow(dead_code)]
  changes     : GraphChangeSet,
}

impl PreparedGraphUpdate {
  pub(crate) fn candidate (
    &self,
  ) -> &Arc<InRustGraph> {
    &self . candidate }

  pub(crate) fn nodeInstructions (
    &self,
  ) -> &[NodeInstruction] {
    &self . nodeInstructions }

  pub(crate) fn saved_pids (
    &self,
  ) -> &HashSet<ID> {
    &self . changes . saved_pids }

  pub(crate) fn affected_skgids (
    &self,
  ) -> &HashSet<ID> {
    &self . changes . affected_skgids }

  pub(crate) fn verify_base (
    &self,
    graph : &InRustGraphHandle,
  ) -> Result<(), String> {
    let current : Arc<InRustGraph> = graph . load_full ();
    if Arc::ptr_eq (&self . base, &current) {
      Ok (())
    } else {
      Err ("Refusing to apply a prepared graph update to a different base snapshot. This is an internal mutation-boundary error." . to_string ()) }
  }

  pub(crate) fn swap_in (
    self,
    graph : &InRustGraphHandle,
  ) -> Result<(Arc<InRustGraph>, Vec<NodeInstruction>), String> {
    self . verify_base (graph) ?;
    graph . store (self . candidate . clone ());
    Ok ((self . candidate, self . nodeInstructions)) }
}

pub(crate) fn prepare_graph_update (
  config           : &SkgConfig,
  base             : Arc<InRustGraph>,
  nodeInstructions : Vec<NodeInstruction>,
) -> Result<PreparedGraphUpdate, GraphUpdatePreparationError> {
  let _span : tracing::span::EnteredSpan =
    tracing::info_span! ("prepare_graph_update") . entered ();
  let batch : NormalizedNodeInstructionBatch = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("normalize_graph_nodeInstructions") . entered ();
    normalize_and_coalesce_nodeInstructions (nodeInstructions) };
  let (mut changes, incremental_errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<ExtraIdRevocation>) =
    { let _span : tracing::span::EnteredSpan =
        tracing::info_span! ("validate_identity_delta") . entered ();
      validate_identity_and_derive_changes (
        config, &base, &batch . final_graph_nodeInstructions) };
  if ! incremental_errors . is_empty () || ! revocations . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : incremental_errors,
      extra_id_revocations  : revocations,
      internal_index_errors : Vec::new (),
      merge_override_collisions : Vec::new (),
    }); }
  let mut candidate : InRustGraph = (*base) . clone ();
  { let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("apply_nodeInstructions_to_inRustGraph") . entered ();
    apply_nodeInstructions_to_inRustGraph (
      &mut candidate, &batch . final_graph_nodeInstructions); }
  let override_scope : OverrideCheckScope = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("derive_affected_override_scope") . entered ();
    derive_affected_override_scope (
      &base, &candidate, &changes . touched_pids, &changes . affected_skgids) };
  changes . override_skgrepos_to_check = override_scope . skgrepos . clone ();
  changes . override_targets_to_check = override_scope . targets . clone ();
  changes . telescope_recorders_to_check = derive_affected_telescope_recorders (
    &base, &candidate, &changes . saved_pids, &changes . affected_skgids);
  let affected_override_validation : AffectedOverrideValidation = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("validate_affected_override_invariants") . entered ();
    validate_affected_override_invariants_with_counts (
      config, &candidate, &override_scope) };
  let affected_override_errors : Vec<OverrideInvariantViolation> =
    affected_override_validation . violations;
  let merge_override_collisions : Vec<MergeOverrideCollision> =
    derive_merge_override_collisions (
      config, &base, &candidate, &changes, &affected_override_errors);
  let local_index_validation : LocalIndexValidation = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("validate_local_internal_indexes") . entered ();
    validate_local_internal_indexes (
      &base, &candidate, &batch . final_graph_nodeInstructions, &changes) };
  tracing::info! (
    "incremental graph work: graph_nodes={} normalized_nodeInstructions={} affected_ids={} recorders_reindexed={} override_repos_checked={} override_targets_checked={} override_chain_steps={} local_index_keys_checked={} local_node_checks={} local_identity_checks={} local_relationship_checks={}",
    base . nodes . len (),
    batch . final_graph_nodeInstructions . len (),
    changes . affected_skgids . len (),
    changes . recorders_to_reindex . len (),
    changes . override_skgrepos_to_check . len (),
    changes . override_targets_to_check . len (),
    affected_override_validation . chain_steps,
    local_index_validation . node_checks
      + local_index_validation . identity_checks
      + local_index_validation . relationship_membership_checks,
    local_index_validation . node_checks,
    local_index_validation . identity_checks,
    local_index_validation . relationship_membership_checks,
  );
  if ! local_index_validation . errors . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : Vec::new (),
      extra_id_revocations  : Vec::new (),
      internal_index_errors : local_index_validation . errors,
      merge_override_collisions : Vec::new (),
    }); }
  if ! merge_override_collisions . is_empty () {
    let merge_targets : HashSet<ID> = merge_override_collisions . iter ()
      . map (|collision| collision . acquirer . skgid . clone ()) . collect ();
    let remaining_errors : Vec<CompleteGraphError> = affected_override_errors
      . into_iter () . filter (|violation| ! matches! (
        violation,
        OverrideInvariantViolation::MultipleOwnedOverriders {
          overridden, .. } if merge_targets . contains (overridden)))
      . map (CompleteGraphError::Override)
      . collect ();
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : remaining_errors,
      extra_id_revocations  : Vec::new (),
      internal_index_errors : Vec::new (),
      merge_override_collisions,
    }); }
  if ! affected_override_errors . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : affected_override_errors . into_iter ()
        . map (CompleteGraphError::Override) . collect (),
      extra_id_revocations  : Vec::new (),
      internal_index_errors : Vec::new (),
      merge_override_collisions : Vec::new (),
    }); }
  Ok (PreparedGraphUpdate {
    base,
    candidate        : Arc::new (candidate),
    nodeInstructions : batch . filesystem_nodeInstructions,
    changes,
  })
}

fn derive_merge_override_collisions (
  config      : &SkgConfig,
  base        : &InRustGraph,
  candidate   : &InRustGraph,
  changes     : &GraphChangeSet,
  violations  : &[OverrideInvariantViolation],
) -> Vec<MergeOverrideCollision> {
  let collision_targets : HashSet<ID> = violations . iter ()
    . filter_map (|violation| match violation {
      OverrideInvariantViolation::MultipleOwnedOverriders {
        overridden, ..
      } => Some (overridden . clone ()),
      _ => None,
    }) . collect ();
  let mut result : Vec<MergeOverrideCollision> = Vec::new ();
  for acquisition in &changes . identity_acquisitions {
    let Some (acquiree_skgid) = &acquisition . old_carrier else { continue; };
    if acquiree_skgid == &acquisition . new_carrier
      || ! collision_targets . contains (&acquisition . new_carrier)
    { continue; }
    let existing_skgids : Vec<ID> = owned_overriders_at (
      config, base, &acquisition . new_carrier);
    let redirected_skgids : Vec<ID> = owned_overriders_at (
      config, base, acquiree_skgid);
    if existing_skgids . is_empty () || redirected_skgids . is_empty () {
      continue; }
    result . push (MergeOverrideCollision {
      acquired_skgid : acquisition . skgid . clone (),
      acquirer : participant (candidate, base, &acquisition . new_carrier),
      acquiree : participant (base, candidate, acquiree_skgid),
      existing_overriders : existing_skgids . iter ()
        . map (|skgid| participant (base, candidate, skgid)) . collect (),
      redirected_overriders : redirected_skgids . iter ()
        . map (|skgid| participant (base, candidate, skgid)) . collect (),
    }); }
  result . sort_by (|left, right|
    left . acquired_skgid . cmp (&right . acquired_skgid)
      . then_with (|| left . acquirer . skgid . cmp (&right . acquirer . skgid)));
  result
}

fn owned_overriders_at (
  config : &SkgConfig,
  graph  : &InRustGraph,
  target : &ID,
) -> Vec<ID> {
  let mut result : Vec<ID> = graph . overriders_of . get (target)
    . into_iter () . flatten ()
    . filter (|pid| graph . nodes . get (*pid) . is_some_and (|node|
      config . skgrepos . get (&node . home_skgrepo)
        . is_some_and (|skgrepo| skgrepo . owned)))
    . cloned () . collect ();
  result . sort ();
  result
}

fn participant (
  preferred : &InRustGraph,
  fallback  : &InRustGraph,
  skgid     : &ID,
) -> OverrideParticipant {
  let title : String = preferred . nodes . get (skgid)
    . or_else (|| fallback . nodes . get (skgid))
    . map (|node| node . title . clone ())
    . unwrap_or_else (|| "<missing>" . to_string ());
  OverrideParticipant { skgid : skgid . clone (), title }
}

fn format_merge_override_collision (
  collision : &MergeOverrideCollision,
) -> String {
  let render = |participants : &[OverrideParticipant]| -> String {
    participants . iter ()
      . map (|participant| format! (
        "{} ('{}')", participant . skgid, participant . title))
      . collect::<Vec<String>> () . join (", ") };
  format! (
    "Node merge cannot acquire ID '{}' because canonicalizing it from {} ('{}') to {} ('{}') would make one node have multiple owned overriders.\nParticipants:\n  existing overrider(s): {}\n  redirected overrider(s): {}\nBefore the merge:\n  R1 -> N1\n  R2 -> N2\nThe merge would produce:\n  R1 -> N1\n  R2 -> N1\nRemove either edge, or choose precedence:\n  R2 -> R1 -> N1\nor\n  R1 -> R2 -> N1",
    collision . acquired_skgid,
    collision . acquiree . skgid, collision . acquiree . title,
    collision . acquirer . skgid, collision . acquirer . title,
    render (&collision . existing_overriders),
    render (&collision . redirected_overriders))
}

fn normalize_and_coalesce_nodeInstructions (
  nodeInstructions : Vec<NodeInstruction>,
) -> NormalizedNodeInstructionBatch {
  let filesystem_nodeInstructions : Vec<NodeInstruction> = nodeInstructions . into_iter ()
    . map ( |mut nodeInstruction| {
      if let NodeInstruction::Save (SaveNode (node)) = &mut nodeInstruction {
        node . normalize_skgids (); }
      nodeInstruction } )
    . collect ();
  let mut final_by_pid : HashMap<ID, (usize, NodeInstruction)> = HashMap::new ();
  for (position, nodeInstruction) in filesystem_nodeInstructions . iter () . enumerate () {
    final_by_pid . insert (
      definition_pid (nodeInstruction) . clone (),
      (position, nodeInstruction . clone ()) ); }
  let mut positioned : Vec<(usize, NodeInstruction)> =
    final_by_pid . into_values () . collect ();
  positioned . sort_by_key ( |(position, _)| *position );
  NormalizedNodeInstructionBatch {
    filesystem_nodeInstructions,
    final_graph_nodeInstructions : positioned . into_iter ()
      . map ( |(_, nodeInstruction)| nodeInstruction )
      . collect (),
  }
}

fn validate_identity_and_derive_changes (
  config           : &SkgConfig,
  base             : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
) -> (GraphChangeSet, Vec<CompleteGraphError>, Vec<ExtraIdRevocation>) {
  let touched_pids : HashSet<ID> = nodeInstructions . iter ()
    . map ( |nodeInstruction| definition_pid (nodeInstruction) . clone () )
    . collect ();
  let saved_pids : HashSet<ID> = nodeInstructions . iter ()
    . filter_map ( |nodeInstruction| match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) => Some (node . pid . clone ()),
      NodeInstruction::Delete (_)             => None, } )
    . collect ();
  let deleted_pids : HashSet<ID> = nodeInstructions . iter ()
    . filter_map ( |nodeInstruction| match nodeInstruction {
      NodeInstruction::Save (_) => None,
      NodeInstruction::Delete (DeleteNode { skgid, .. }) => Some (skgid . clone ()), } )
    . collect ();
  let mut affected_skgids : HashSet<ID> = HashSet::new ();
  for pid in &touched_pids {
    affected_skgids . insert (pid . clone ());
    if let Some (old) = base . nodes . get (pid) {
      affected_skgids . extend (old . extra_ids . iter () . cloned ()); }}
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      affected_skgids . extend (node . all_skgids () . cloned ()); }}

  let mut claims : BTreeMap<ID, CarriersForId> = BTreeMap::new ();
  for skgid in &affected_skgids {
    if base . nodes . contains_key (skgid) && ! touched_pids . contains (skgid) {
      claims . entry (skgid . clone ()) . or_default ()
        . primary . insert (skgid . clone ()); }
    if let Some (carrier) = base . extra_id_to_pid . get (skgid) {
      if ! touched_pids . contains (carrier) {
        claims . entry (skgid . clone ()) . or_default ()
          . extra . insert (carrier . clone ()); }}}
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      claims . entry (node . pid . clone ()) . or_default ()
        . primary . insert (node . pid . clone ());
      for extra in &node . extra_ids {
        claims . entry (extra . clone ()) . or_default ()
          . extra . insert (node . pid . clone ()); }}}

  let mut errors : Vec<CompleteGraphError> = Vec::new ();
  for (skgid, carriers) in &claims {
    if carriers . extra . len () > 1 {
      errors . push (CompleteGraphError::DuplicateExtraId {
        skgid     : skgid . clone (),
        carriers  : carriers . extra . iter () . cloned () . collect (),
      }); }
    let extra_carriers : Vec<ID> = carriers . extra . iter ()
      . filter ( |carrier| ! carriers . primary . contains (*carrier) )
      . cloned ()
      . collect ();
    if ! carriers . primary . is_empty () && ! extra_carriers . is_empty () {
      errors . push (CompleteGraphError::PrimaryExtraCollision {
        skgid             : skgid . clone (),
        primary_carriers  : carriers . primary . iter () . cloned () . collect (),
        extra_carriers,
      }); }}
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      if ! config . skgrepos . contains_key (&node . home_skgrepo) {
        errors . push (CompleteGraphError::UnconfiguredNodeHome {
          pid     : node . pid . clone (),
          skgrepo : node . home_skgrepo . clone (),
        }); }} }
  errors . sort_by_key ( |error| format! ("{:?}", error) );

  let mut revocations : Vec<ExtraIdRevocation> = Vec::new ();
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      let Some (old) = base . nodes . get (&node . pid) else { continue; };
      let final_skgids : HashSet<&ID> = node . extra_ids . iter () . collect ();
      let mut dropped_skgids : Vec<ID> = old . extra_ids . iter ()
        . filter ( |skgid| ! final_skgids . contains (skgid) )
        . cloned ()
        . collect ();
      dropped_skgids . sort ();
      if ! dropped_skgids . is_empty () {
        revocations . push (ExtraIdRevocation {
          pid         : node . pid . clone (),
          title       : node . title . clone (),
          dropped_skgids,
        }); }} }
  revocations . sort_by ( |left, right| left . pid . cmp (&right . pid) );

  let mut canonicalization_changes : Vec<CanonicalizationChange> = Vec::new ();
  let mut identity_acquisitions : Vec<IdentityAcquisition> = Vec::new ();
  let mut sorted_affected_skgids : Vec<ID> = affected_skgids . iter () . cloned ()
    . collect ();
  sorted_affected_skgids . sort ();
  for skgid in sorted_affected_skgids {
    let old_carrier : Option<ID> = base . pid_of (&skgid);
    let new_carrier : Option<ID> = final_carrier_of (&claims, &skgid);
    if old_carrier != new_carrier {
      canonicalization_changes . push (CanonicalizationChange {
        skgid        : skgid . clone (),
        old_carrier  : old_carrier . clone (),
        new_carrier  : new_carrier . clone (),
      });
      if let Some (new_carrier) = new_carrier {
        identity_acquisitions . push (IdentityAcquisition {
          skgid,
          old_carrier,
          new_carrier,
        }); }} }

  let mut recorders_to_reindex : HashSet<ID> = touched_pids . clone ();
  for change in &canonicalization_changes {
    let old_key : &ID = change . old_carrier . as_ref ()
      . unwrap_or (&change . skgid);
    let new_key : &ID = change . new_carrier . as_ref ()
      . unwrap_or (&change . skgid);
    if old_key != new_key {
      recorders_to_reindex . extend (inbound_recorders_at (base, old_key)); }}

  #[cfg(test)]
  let identity_base_lookup_bound : usize =
    touched_pids . len () + affected_skgids . len () * 4 + saved_pids . len ();
  (GraphChangeSet {
    touched_pids,
    saved_pids,
    deleted_pids,
    affected_skgids,
    canonicalization_changes,
    identity_acquisitions,
    recorders_to_reindex,
    override_skgrepos_to_check   : HashSet::new (),
    override_targets_to_check : HashSet::new (),
    telescope_recorders_to_check : HashSet::new (),
    #[cfg(test)]
    identity_base_lookup_bound,
  }, errors, revocations)
}

fn final_carrier_of (
  claims    : &BTreeMap<ID, CarriersForId>,
  skgid     : &ID,
) -> Option<ID> {
  let carriers : &CarriersForId = claims . get (skgid) ?;
  carriers . primary . first () . or_else (|| carriers . extra . first ())
    . cloned ()
}

fn definition_pid (
  nodeInstruction : &NodeInstruction,
) -> &ID {
  match nodeInstruction {
    NodeInstruction::Save (SaveNode (node))       => &node . pid,
    NodeInstruction::Delete (DeleteNode { skgid, .. }) => skgid,
  }
}

#[cfg(test)]
#[path = "../../../tests/unit/prepared_graph_update.rs"]
mod tests;
