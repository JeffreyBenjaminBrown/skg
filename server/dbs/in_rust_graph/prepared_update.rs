//! A checked graph candidate tied to the exact graph snapshot it was derived from.
//!
//! The constructor is the only way to obtain this token.  Store orchestration
//! can inspect its nodeInstructions and candidate while preflighting later work,
//! but the swap-in consumes it and never reapplies nodeInstructions to a newer
//! graph.

use crate::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle, apply_nodeInstructions_to_inRustGraph,
  coalesced_final_nodeInstructions, inbound_recorders_at,
};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, format_complete_graph_errors,
};
use crate::dbs::in_rust_graph::internal_index_validation::{
  InternalIndexMismatch, LocalIndexValidation, format_internal_index_mismatches,
  validate_local_internal_indexes,
};
use crate::dbs::in_rust_graph::fork_rules::{
  ForkRuleViolation, derive_fork_rule_scope,
  validate_fork_rules_of_affected_notes,
};
use crate::types::config::SkgConfig;
use crate::types::misc::ID;
use crate::types::save::{NodeInstruction, SaveNode};
use crate::telescope::invariants::derive_affected_telescope_recorders;

use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::error::Error;
use std::fmt;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct CanonicalizationChange {
  pub(crate) gnid        : ID,
  pub(crate) old_carrier  : Option<ID>,
  pub(crate) new_carrier  : Option<ID>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct IdentityAcquisition {
  pub(crate) gnid        : ID,
  pub(crate) old_carrier  : Option<ID>,
  pub(crate) new_carrier  : ID,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GraphChangeSet {
  pub(crate) touched_pids             : HashSet<ID>,
  pub(crate) saved_pids               : HashSet<ID>,
  pub(crate) deleted_pids             : HashSet<ID>,
  pub(crate) affected_gnids             : HashSet<ID>,
  pub(crate) canonicalization_changes : Vec<CanonicalizationChange>,
  pub(crate) identity_acquisitions    : Vec<IdentityAcquisition>,
  pub(crate) recorders_to_reindex         : HashSet<ID>,
  pub(crate) fork_rule_pids_to_check : HashSet<ID>,
  pub(crate) telescope_recorders_to_check : HashSet<ID>,
  #[cfg(test)]
  pub(crate) identity_base_lookup_bound : usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ExtraIdRevocation {
  pub(crate) pid            : ID,
  pub(crate) title          : String,
  pub(crate) dropped_gnids : Vec<ID>,
}

#[derive(Debug)]
pub(crate) struct GraphUpdatePreparationError {
  pub(crate) complete_graph_errors : Vec<CompleteGraphError>,
  pub(crate) extra_id_revocations  : Vec<ExtraIdRevocation>,
  pub(crate) internal_index_errors : Vec<InternalIndexMismatch>,
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
        revocation . dropped_gnids . iter ()
          . map ( |gnid| gnid . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ") )); }
    write! (f, "{}", sections . join ("\n"))
  }
}

impl Error for GraphUpdatePreparationError {}

impl GraphUpdatePreparationError {
  pub(crate) fn is_internal (&self) -> bool {
    ! self . internal_index_errors . is_empty () }

  pub(crate) fn is_fork_rule_violation_only (&self) -> bool {
    ! self . complete_graph_errors . is_empty ()
      && self . complete_graph_errors . iter () . all (|error|
        matches! (error, CompleteGraphError::ForkRule (_)))
      && self . extra_id_revocations . is_empty ()
      && self . internal_index_errors . is_empty ()
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

  pub(crate) fn affected_gnids (
    &self,
  ) -> &HashSet<ID> {
    &self . changes . affected_gnids }

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
    }); }
  let mut candidate : InRustGraph = (*base) . clone ();
  { let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("apply_nodeInstructions_to_inRustGraph") . entered ();
    apply_nodeInstructions_to_inRustGraph (
      &mut candidate, &batch . final_graph_nodeInstructions); }
  changes . fork_rule_pids_to_check = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("derive_fork_rule_scope") . entered ();
    derive_fork_rule_scope (
      &base, &candidate, &changes . touched_pids, &changes . affected_gnids) };
  changes . telescope_recorders_to_check = derive_affected_telescope_recorders (
    &base, &candidate, &changes . saved_pids, &changes . affected_gnids);
  let fork_rule_errors : Vec<ForkRuleViolation> = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("validate_fork_rules_of_affected_notes") . entered ();
    validate_fork_rules_of_affected_notes (
      config, &candidate, &changes . fork_rule_pids_to_check) };
  let local_index_validation : LocalIndexValidation = {
    let _span : tracing::span::EnteredSpan =
      tracing::info_span! ("validate_local_internal_indexes") . entered ();
    validate_local_internal_indexes (
      &base, &candidate, &batch . final_graph_nodeInstructions, &changes) };
  tracing::info! (
    "incremental graph work: graph_nodes={} normalized_nodeInstructions={} affected_ids={} recorders_reindexed={} fork_rule_pids_checked={} local_index_keys_checked={} local_node_checks={} local_identity_checks={} local_relationship_checks={}",
    base . nodes . len (),
    batch . final_graph_nodeInstructions . len (),
    changes . affected_gnids . len (),
    changes . recorders_to_reindex . len (),
    changes . fork_rule_pids_to_check . len (),
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
    }); }
  if ! fork_rule_errors . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : fork_rule_errors . into_iter ()
        . map (CompleteGraphError::ForkRule) . collect (),
      extra_id_revocations  : Vec::new (),
      internal_index_errors : Vec::new (),
    }); }
  Ok (PreparedGraphUpdate {
    base,
    candidate        : Arc::new (candidate),
    nodeInstructions : batch . filesystem_nodeInstructions,
    changes,
  })
}

fn normalize_and_coalesce_nodeInstructions (
  nodeInstructions : Vec<NodeInstruction>,
) -> NormalizedNodeInstructionBatch {
  let filesystem_nodeInstructions : Vec<NodeInstruction> = nodeInstructions . into_iter ()
    . map ( |mut nodeInstruction| {
      if let NodeInstruction::Save (SaveNode (node)) = &mut nodeInstruction {
        node . normalize_gnids (); }
      nodeInstruction } )
    . collect ();
  let final_graph_nodeInstructions : Vec<NodeInstruction> =
    coalesced_final_nodeInstructions (&filesystem_nodeInstructions)
    . into_iter () . cloned () . collect ();
  NormalizedNodeInstructionBatch {
    filesystem_nodeInstructions,
    final_graph_nodeInstructions,
  }
}

/// The identity half of a graph update: which gnids the batch
/// affects and who carries each before and after, the identity
/// errors, the extra IDs the batch would revoke, and the recorders
/// whose relationships must be reindexed because a gnid changed
/// carrier.
fn validate_identity_and_derive_changes (
  config           : &SkgConfig,
  base             : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
) -> (GraphChangeSet, Vec<CompleteGraphError>, Vec<ExtraIdRevocation>) {
  let touched_pids : HashSet<ID> = nodeInstructions . iter ()
    . map ( |nodeInstruction| nodeInstruction . gnid () . clone () )
    . collect ();
  let saved_pids : HashSet<ID> = nodeInstructions . iter ()
    . filter_map (NodeInstruction::saved_graphnode)
    . map ( |node| node . pid . clone () )
    . collect ();
  let deleted_pids : HashSet<ID> = nodeInstructions . iter ()
    . filter_map (NodeInstruction::deleted_gnid)
    . cloned () . collect ();
  let affected_gnids : HashSet<ID> =
    gnids_the_batch_affects (base, &touched_pids, nodeInstructions);
  let claims : BTreeMap<ID, CarriersForId> =
    carriers_claiming_each_gnid (
      base, &touched_pids, &affected_gnids, nodeInstructions);
  let errors : Vec<CompleteGraphError> =
    identity_errors (config, &claims, nodeInstructions);
  let revocations : Vec<ExtraIdRevocation> =
    extra_id_revocations (base, nodeInstructions);
  let (canonicalization_changes, identity_acquisitions)
    : (Vec<CanonicalizationChange>, Vec<IdentityAcquisition>) =
    carrier_changes (base, &claims, &affected_gnids);
  let recorders_to_reindex : HashSet<ID> =
    recorders_to_reindex_after_carrier_changes (
      base, &touched_pids, &canonicalization_changes);
  #[cfg(test)]
  let identity_base_lookup_bound : usize =
    touched_pids . len () + affected_gnids . len () * 4 + saved_pids . len ();
  (GraphChangeSet {
    touched_pids,
    saved_pids,
    deleted_pids,
    affected_gnids,
    canonicalization_changes,
    identity_acquisitions,
    recorders_to_reindex,
    fork_rule_pids_to_check      : HashSet::new (),
    telescope_recorders_to_check : HashSet::new (),
    #[cfg(test)]
    identity_base_lookup_bound,
  }, errors, revocations)
}

/// Every pid the batch touches, every extra ID those nodes carried
/// before, and every gnid a saved node carries after.
fn gnids_the_batch_affects (
  base             : &InRustGraph,
  touched_pids     : &HashSet<ID>,
  nodeInstructions : &[NodeInstruction],
) -> HashSet<ID> {
  let mut affected_gnids : HashSet<ID> = HashSet::new ();
  for pid in touched_pids {
    affected_gnids . insert (pid . clone ());
    if let Some (old) = base . nodes . get (pid) {
      affected_gnids . extend (old . extra_ids . iter () . cloned ()); }}
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      affected_gnids . extend (node . all_gnids () . cloned ()); }}
  affected_gnids }

/// For each affected gnid, the nodes that will carry it after the
/// batch, as a pid or as an extra ID: the untouched nodes of 'base'
/// that carry it now, and the saved nodes that carry it.
fn carriers_claiming_each_gnid (
  base             : &InRustGraph,
  touched_pids     : &HashSet<ID>,
  affected_gnids   : &HashSet<ID>,
  nodeInstructions : &[NodeInstruction],
) -> BTreeMap<ID, CarriersForId> {
  let mut claims : BTreeMap<ID, CarriersForId> = BTreeMap::new ();
  for gnid in affected_gnids {
    if base . nodes . contains_key (gnid) && ! touched_pids . contains (gnid) {
      claims . entry (gnid . clone ()) . or_default ()
        . primary . insert (gnid . clone ()); }
    if let Some (carrier) = base . extra_id_to_pid . get (gnid) {
      if ! touched_pids . contains (carrier) {
        claims . entry (gnid . clone ()) . or_default ()
          . extra . insert (carrier . clone ()); }}}
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      claims . entry (node . pid . clone ()) . or_default ()
        . primary . insert (node . pid . clone ());
      for extra in &node . extra_ids {
        claims . entry (extra . clone ()) . or_default ()
          . extra . insert (node . pid . clone ()); }}}
  claims }

/// A gnid two nodes carry as an extra ID, a gnid one node carries as
/// its pid and another as an extra ID, and a saved node whose home
/// skgrepo is not configured; sorted.
fn identity_errors (
  config           : &SkgConfig,
  claims           : &BTreeMap<ID, CarriersForId>,
  nodeInstructions : &[NodeInstruction],
) -> Vec<CompleteGraphError> {
  let mut errors : Vec<CompleteGraphError> = Vec::new ();
  for (gnid, carriers) in claims {
    if carriers . extra . len () > 1 {
      errors . push (CompleteGraphError::DuplicateExtraId {
        gnid     : gnid . clone (),
        carriers  : carriers . extra . iter () . cloned () . collect (),
      }); }
    let extra_carriers : Vec<ID> = carriers . extra . iter ()
      . filter ( |carrier| ! carriers . primary . contains (*carrier) )
      . cloned ()
      . collect ();
    if ! carriers . primary . is_empty () && ! extra_carriers . is_empty () {
      errors . push (CompleteGraphError::PrimaryExtraCollision {
        gnid             : gnid . clone (),
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
  errors }

/// Each saved node that would drop an extra ID it carries in 'base',
/// with the IDs it drops; sorted by pid.
fn extra_id_revocations (
  base             : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
) -> Vec<ExtraIdRevocation> {
  let mut revocations : Vec<ExtraIdRevocation> = Vec::new ();
  for nodeInstruction in nodeInstructions {
    if let NodeInstruction::Save (SaveNode (node)) = nodeInstruction {
      let Some (old) = base . nodes . get (&node . pid) else { continue; };
      let final_gnids : HashSet<&ID> = node . extra_ids . iter () . collect ();
      let mut dropped_gnids : Vec<ID> = old . extra_ids . iter ()
        . filter ( |gnid| ! final_gnids . contains (gnid) )
        . cloned ()
        . collect ();
      dropped_gnids . sort ();
      if ! dropped_gnids . is_empty () {
        revocations . push (ExtraIdRevocation {
          pid         : node . pid . clone (),
          title       : node . title . clone (),
          dropped_gnids,
        }); }} }
  revocations . sort_by ( |left, right| left . pid . cmp (&right . pid) );
  revocations }

/// Each affected gnid whose carrier the batch changes, in gnid
/// order; and of those, each that ends with a carrier, as an
/// acquisition.
fn carrier_changes (
  base           : &InRustGraph,
  claims         : &BTreeMap<ID, CarriersForId>,
  affected_gnids : &HashSet<ID>,
) -> (Vec<CanonicalizationChange>, Vec<IdentityAcquisition>) {
  let mut canonicalization_changes : Vec<CanonicalizationChange> = Vec::new ();
  let mut identity_acquisitions : Vec<IdentityAcquisition> = Vec::new ();
  let mut sorted_affected_gnids : Vec<ID> = affected_gnids . iter () . cloned ()
    . collect ();
  sorted_affected_gnids . sort ();
  for gnid in sorted_affected_gnids {
    let old_carrier : Option<ID> = base . pid_of (&gnid);
    let new_carrier : Option<ID> = final_carrier_of (claims, &gnid);
    if old_carrier != new_carrier {
      canonicalization_changes . push (CanonicalizationChange {
        gnid        : gnid . clone (),
        old_carrier  : old_carrier . clone (),
        new_carrier  : new_carrier . clone (),
      });
      if let Some (new_carrier) = new_carrier {
        identity_acquisitions . push (IdentityAcquisition {
          gnid,
          old_carrier,
          new_carrier,
        }); }} }
  (canonicalization_changes, identity_acquisitions) }

/// The touched pids, and every recorder whose relationships 'base'
/// indexes under the old key of a gnid that changes carrier.
fn recorders_to_reindex_after_carrier_changes (
  base                     : &InRustGraph,
  touched_pids             : &HashSet<ID>,
  canonicalization_changes : &[CanonicalizationChange],
) -> HashSet<ID> {
  let mut recorders_to_reindex : HashSet<ID> = touched_pids . clone ();
  for change in canonicalization_changes {
    let old_key : &ID = change . old_carrier . as_ref ()
      . unwrap_or (&change . gnid);
    let new_key : &ID = change . new_carrier . as_ref ()
      . unwrap_or (&change . gnid);
    if old_key != new_key {
      recorders_to_reindex . extend (inbound_recorders_at (base, old_key)); }}
  recorders_to_reindex }

fn final_carrier_of (
  claims    : &BTreeMap<ID, CarriersForId>,
  gnid     : &ID,
) -> Option<ID> {
  let carriers : &CarriersForId = claims . get (gnid) ?;
  carriers . primary . first () . or_else (|| carriers . extra . first ())
    . cloned ()
}

#[cfg(test)]
#[path = "../../../tests/unit/prepared_graph_update.rs"]
mod tests;
