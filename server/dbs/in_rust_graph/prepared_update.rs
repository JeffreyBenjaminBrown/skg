//! A checked graph candidate tied to the exact snapshot it was derived from.
//!
//! The constructor is the only way to obtain this token.  Store orchestration
//! can inspect its definitions and candidate while preflighting later work,
//! but publication consumes it and never reapplies definitions to a newer
//! graph.

use crate::dbs::in_rust_graph::{
  InRustGraph, InRustGraphHandle, apply_definenodes_to_inRustGraph,
  inbound_owners_at,
};
use crate::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, CompleteGraphValidation, format_complete_graph_errors,
  validate_complete_graph_candidate,
};
use crate::dbs::in_rust_graph::internal_index_validation::{
  InternalIndexMismatch, LocalIndexValidation, format_internal_index_mismatches,
  validate_local_internal_indexes,
};
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::error::Error;
use std::fmt;
use std::sync::Arc;

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct CanonicalizationChange {
  pub(crate) id        : ID,
  pub(crate) old_owner : Option<ID>,
  pub(crate) new_owner : Option<ID>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct IdentityAcquisition {
  pub(crate) id        : ID,
  pub(crate) old_owner : Option<ID>,
  pub(crate) new_owner : ID,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct GraphChangeSet {
  pub(crate) touched_pids             : HashSet<ID>,
  pub(crate) saved_pids               : HashSet<ID>,
  pub(crate) deleted_pids             : HashSet<ID>,
  pub(crate) affected_ids             : HashSet<ID>,
  pub(crate) canonicalization_changes : Vec<CanonicalizationChange>,
  pub(crate) identity_acquisitions    : Vec<IdentityAcquisition>,
  pub(crate) owners_to_reindex        : HashSet<ID>,
  #[cfg(test)]
  pub(crate) identity_base_lookup_bound : usize,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub(crate) struct ExtraIdRevocation {
  pub(crate) pid         : ID,
  pub(crate) title       : String,
  pub(crate) dropped_ids : Vec<ID>,
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
        revocation . dropped_ids . iter ()
          . map ( |id| id . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ") )); }
    write! (f, "{}", sections . join ("\n"))
  }
}

impl Error for GraphUpdatePreparationError {}

struct NormalizedDefineNodeBatch {
  filesystem_definitions : Vec<DefineNode>,
  final_graph_definitions : Vec<DefineNode>,
}

#[derive(Default)]
struct OwnersForId {
  primary : BTreeSet<ID>,
  extra   : BTreeSet<ID>,
}

#[derive(Debug)]
pub(crate) struct PreparedGraphUpdate {
  base        : Arc<InRustGraph>,
  candidate   : Arc<InRustGraph>,
  definitions : Vec<DefineNode>,
  // Consumed by the batch-aware mutator in the next plan task.
  #[allow(dead_code)]
  changes     : GraphChangeSet,
}

impl PreparedGraphUpdate {
  pub(crate) fn candidate (
    &self,
  ) -> &Arc<InRustGraph> {
    &self . candidate }

  pub(crate) fn definitions (
    &self,
  ) -> &[DefineNode] {
    &self . definitions }

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

  pub(crate) fn publish (
    self,
    graph : &InRustGraphHandle,
  ) -> Result<(Arc<InRustGraph>, Vec<DefineNode>), String> {
    self . verify_base (graph) ?;
    graph . store (self . candidate . clone ());
    Ok ((self . candidate, self . definitions)) }
}

pub(crate) fn prepare_graph_update (
  config      : &SkgConfig,
  base        : Arc<InRustGraph>,
  definitions : Vec<DefineNode>,
) -> Result<PreparedGraphUpdate, GraphUpdatePreparationError> {
  let batch : NormalizedDefineNodeBatch =
    normalize_and_coalesce_definitions (definitions);
  let (changes, incremental_errors, revocations)
    : (GraphChangeSet, Vec<CompleteGraphError>, Vec<ExtraIdRevocation>) =
    validate_identity_and_derive_changes (
      config, &base, &batch . final_graph_definitions);
  if ! incremental_errors . is_empty () || ! revocations . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : incremental_errors,
      extra_id_revocations  : revocations,
      internal_index_errors : Vec::new (),
    }); }
  let mut candidate : InRustGraph = (*base) . clone ();
  apply_definenodes_to_inRustGraph (
    &mut candidate, &batch . final_graph_definitions);
  let local_index_validation : LocalIndexValidation =
    validate_local_internal_indexes (
      &base, &candidate, &batch . final_graph_definitions, &changes);
  if ! local_index_validation . errors . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : Vec::new (),
      extra_id_revocations  : Vec::new (),
      internal_index_errors : local_index_validation . errors,
    }); }
  let validation : CompleteGraphValidation = validate_complete_graph_candidate (
    config, &base, &batch . final_graph_definitions);
  if ! validation . errors . is_empty () {
    return Err (GraphUpdatePreparationError {
      complete_graph_errors : validation . errors,
      extra_id_revocations  : Vec::new (),
      internal_index_errors : Vec::new (),
    }); }
  Ok (PreparedGraphUpdate {
    base,
    candidate   : Arc::new (candidate),
    definitions : batch . filesystem_definitions,
    changes,
  })
}

fn normalize_and_coalesce_definitions (
  definitions : Vec<DefineNode>,
) -> NormalizedDefineNodeBatch {
  let filesystem_definitions : Vec<DefineNode> = definitions . into_iter ()
    . map ( |mut definition| {
      if let DefineNode::Save (SaveNode (node)) = &mut definition {
        node . normalize_ids (); }
      definition } )
    . collect ();
  let mut final_by_pid : HashMap<ID, (usize, DefineNode)> = HashMap::new ();
  for (position, definition) in filesystem_definitions . iter () . enumerate () {
    final_by_pid . insert (
      definition_pid (definition) . clone (),
      (position, definition . clone ()) ); }
  let mut positioned : Vec<(usize, DefineNode)> =
    final_by_pid . into_values () . collect ();
  positioned . sort_by_key ( |(position, _)| *position );
  NormalizedDefineNodeBatch {
    filesystem_definitions,
    final_graph_definitions : positioned . into_iter ()
      . map ( |(_, definition)| definition )
      . collect (),
  }
}

fn validate_identity_and_derive_changes (
  config      : &SkgConfig,
  base        : &InRustGraph,
  definitions : &[DefineNode],
) -> (GraphChangeSet, Vec<CompleteGraphError>, Vec<ExtraIdRevocation>) {
  let touched_pids : HashSet<ID> = definitions . iter ()
    . map ( |definition| definition_pid (definition) . clone () )
    . collect ();
  let saved_pids : HashSet<ID> = definitions . iter ()
    . filter_map ( |definition| match definition {
      DefineNode::Save (SaveNode (node)) => Some (node . pid . clone ()),
      DefineNode::Delete (_)             => None, } )
    . collect ();
  let deleted_pids : HashSet<ID> = definitions . iter ()
    . filter_map ( |definition| match definition {
      DefineNode::Save (_) => None,
      DefineNode::Delete (DeleteNode { id, .. }) => Some (id . clone ()), } )
    . collect ();
  let mut affected_ids : HashSet<ID> = HashSet::new ();
  for pid in &touched_pids {
    affected_ids . insert (pid . clone ());
    if let Some (old) = base . nodes . get (pid) {
      affected_ids . extend (old . extra_ids . iter () . cloned ()); }}
  for definition in definitions {
    if let DefineNode::Save (SaveNode (node)) = definition {
      affected_ids . extend (node . all_ids () . cloned ()); }}

  let mut claims : BTreeMap<ID, OwnersForId> = BTreeMap::new ();
  for id in &affected_ids {
    if base . nodes . contains_key (id) && ! touched_pids . contains (id) {
      claims . entry (id . clone ()) . or_default ()
        . primary . insert (id . clone ()); }
    if let Some (owner) = base . extra_id_to_pid . get (id) {
      if ! touched_pids . contains (owner) {
        claims . entry (id . clone ()) . or_default ()
          . extra . insert (owner . clone ()); }}}
  for definition in definitions {
    if let DefineNode::Save (SaveNode (node)) = definition {
      claims . entry (node . pid . clone ()) . or_default ()
        . primary . insert (node . pid . clone ());
      for extra in &node . extra_ids {
        claims . entry (extra . clone ()) . or_default ()
          . extra . insert (node . pid . clone ()); }}}

  let mut errors : Vec<CompleteGraphError> = Vec::new ();
  for (id, owners) in &claims {
    if owners . extra . len () > 1 {
      errors . push (CompleteGraphError::DuplicateExtraId {
        id     : id . clone (),
        owners : owners . extra . iter () . cloned () . collect (),
      }); }
    let extra_owners : Vec<ID> = owners . extra . iter ()
      . filter ( |owner| ! owners . primary . contains (*owner) )
      . cloned ()
      . collect ();
    if ! owners . primary . is_empty () && ! extra_owners . is_empty () {
      errors . push (CompleteGraphError::PrimaryExtraCollision {
        id             : id . clone (),
        primary_owners : owners . primary . iter () . cloned () . collect (),
        extra_owners,
      }); }}
  for definition in definitions {
    if let DefineNode::Save (SaveNode (node)) = definition {
      if ! config . sources . contains_key (&node . source) {
        errors . push (CompleteGraphError::UnconfiguredNodeHome {
          pid    : node . pid . clone (),
          source : node . source . clone (),
        }); }} }
  errors . sort_by_key ( |error| format! ("{:?}", error) );

  let mut revocations : Vec<ExtraIdRevocation> = Vec::new ();
  for definition in definitions {
    if let DefineNode::Save (SaveNode (node)) = definition {
      let Some (old) = base . nodes . get (&node . pid) else { continue; };
      let final_ids : HashSet<&ID> = node . extra_ids . iter () . collect ();
      let mut dropped_ids : Vec<ID> = old . extra_ids . iter ()
        . filter ( |id| ! final_ids . contains (id) )
        . cloned ()
        . collect ();
      dropped_ids . sort ();
      if ! dropped_ids . is_empty () {
        revocations . push (ExtraIdRevocation {
          pid         : node . pid . clone (),
          title       : node . title . clone (),
          dropped_ids,
        }); }} }
  revocations . sort_by ( |left, right| left . pid . cmp (&right . pid) );

  let mut canonicalization_changes : Vec<CanonicalizationChange> = Vec::new ();
  let mut identity_acquisitions : Vec<IdentityAcquisition> = Vec::new ();
  let mut sorted_affected_ids : Vec<ID> = affected_ids . iter () . cloned ()
    . collect ();
  sorted_affected_ids . sort ();
  for id in sorted_affected_ids {
    let old_owner : Option<ID> = base . pid_of (&id);
    let new_owner : Option<ID> = final_owner_of (&claims, &id);
    if old_owner != new_owner {
      canonicalization_changes . push (CanonicalizationChange {
        id        : id . clone (),
        old_owner : old_owner . clone (),
        new_owner : new_owner . clone (),
      });
      if let Some (new_owner) = new_owner {
        identity_acquisitions . push (IdentityAcquisition {
          id,
          old_owner,
          new_owner,
        }); }} }

  let mut owners_to_reindex : HashSet<ID> = touched_pids . clone ();
  for change in &canonicalization_changes {
    let old_key : &ID = change . old_owner . as_ref ()
      . unwrap_or (&change . id);
    let new_key : &ID = change . new_owner . as_ref ()
      . unwrap_or (&change . id);
    if old_key != new_key {
      owners_to_reindex . extend (inbound_owners_at (base, old_key)); }}

  #[cfg(test)]
  let identity_base_lookup_bound : usize =
    touched_pids . len () + affected_ids . len () * 4 + saved_pids . len ();
  (GraphChangeSet {
    touched_pids,
    saved_pids,
    deleted_pids,
    affected_ids,
    canonicalization_changes,
    identity_acquisitions,
    owners_to_reindex,
    #[cfg(test)]
    identity_base_lookup_bound,
  }, errors, revocations)
}

fn final_owner_of (
  claims : &BTreeMap<ID, OwnersForId>,
  id     : &ID,
) -> Option<ID> {
  let owners : &OwnersForId = claims . get (id) ?;
  owners . primary . first () . or_else (|| owners . extra . first ())
    . cloned ()
}

fn definition_pid (
  definition : &DefineNode,
) -> &ID {
  match definition {
    DefineNode::Save (SaveNode (node))       => &node . pid,
    DefineNode::Delete (DeleteNode { id, .. }) => id,
  }
}

#[cfg(test)]
#[path = "../../../tests/unit/prepared_graph_update.rs"]
mod tests;
