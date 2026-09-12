//! Canonical validation of a complete filesystem-derived node set.
//!
//! This is the canonical home for whole-graph constraints. It separates hard
//! errors (the candidate must
//! not be published) from telescope warnings (the data remains loadable but
//! deserves repair), and returns both in deterministic order.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::internal_index_validation::{
  InternalIndexMismatch, validate_internal_indexes,
};
use crate::dbs::in_rust_graph::override_invariants::{
  OverrideInvariantViolation, format_override_invariant_violations,
  validate_override_invariants,
};
use crate::telescope::invariants::{TelescopeViolation, validate_all_telescopes};
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompleteGraphError {
  DuplicatePrimaryId { pid : ID, homes : Vec<SourceName> },
  DuplicateExtraId { id : ID, owners : Vec<ID> },
  PrimaryExtraCollision { id : ID, primary_owners : Vec<ID>, extra_owners : Vec<ID> },
  UnconfiguredNodeHome { pid : ID, source : SourceName },
  Override (OverrideInvariantViolation),
  InternalIndex (InternalIndexMismatch),
}

#[derive(Clone, Debug)]
pub struct CompleteGraphValidation {
  pub graph : InRustGraph,
  pub errors : Vec<CompleteGraphError>,
  pub warnings : Vec<(ID, TelescopeViolation)>,
}

impl CompleteGraphValidation {
  pub fn is_valid (&self) -> bool { self . errors . is_empty () }
}

/// Validate all facts requiring the complete set: identity, configured homes,
/// override topology, internal indexes, and telescope provenance/privacy.
/// Relationship roles themselves are represented by distinct typed fields;
/// dangling member IDs are deliberately retained and are not errors.
pub fn validate_complete_graph (
  config : &SkgConfig,
  nodes : &[NodeComplete],
) -> CompleteGraphValidation {
  let mut primary_homes : BTreeMap<ID, Vec<SourceName>> = BTreeMap::new ();
  let mut extra_owners : BTreeMap<ID, BTreeSet<ID>> = BTreeMap::new ();
  for node in nodes {
    primary_homes . entry (node . pid . clone ()) . or_default ()
      . push (node . source . clone ());
    for extra in node . normalized_extra_ids () {
      extra_owners . entry (extra) . or_default ()
        . insert (node . pid . clone ()); }}

  let mut errors : Vec<CompleteGraphError> = Vec::new ();
  for (pid, homes) in &primary_homes {
    if homes . len () > 1 {
      let mut homes = homes . clone ();
      homes . sort ();
      errors . push (CompleteGraphError::DuplicatePrimaryId {
        pid : pid . clone (), homes }); }}
  for (id, owners) in &extra_owners {
    if owners . len () > 1 {
      errors . push (CompleteGraphError::DuplicateExtraId {
        id : id . clone (),
        owners : owners . iter () . cloned () . collect (), }); }}
  for id in primary_homes . keys () {
    if let Some (extras) = extra_owners . get (id) {
      let extra_owners : Vec<ID> = extras . iter ()
        . filter ( |owner| *owner != id )
        . cloned ()
        . collect ();
      if ! extra_owners . is_empty () {
        errors . push (CompleteGraphError::PrimaryExtraCollision {
          id : id . clone (),
          primary_owners : vec![id . clone ()],
          extra_owners, }); }}}
  let mut nodes_by_pid : Vec<&NodeComplete> = nodes . iter () . collect ();
  nodes_by_pid . sort_by (|a, b| a . pid . cmp (&b . pid));
  for node in nodes_by_pid {
    if ! config . sources . contains_key (&node . source) {
      errors . push (CompleteGraphError::UnconfiguredNodeHome {
        pid : node . pid . clone (), source : node . source . clone () }); }}

  let graph = InRustGraph::from_nodecompletes (nodes);
  // Topology and derived-index diagnostics are meaningful only when identity
  // is unambiguous; otherwise graph construction necessarily chose winners.
  let identity_is_unambiguous = ! errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::DuplicatePrimaryId { .. }
    | CompleteGraphError::DuplicateExtraId { .. }
    | CompleteGraphError::PrimaryExtraCollision { .. }));
  if identity_is_unambiguous {
    errors . extend (validate_override_invariants (config, &graph)
      . into_iter () . map (CompleteGraphError::Override));
    errors . extend (validate_internal_indexes (&graph)
      . into_iter () . map (CompleteGraphError::InternalIndex)); }
  errors . sort_by_key (error_sort_key);

  let mut warnings = validate_all_telescopes (config, &graph);
  warnings . sort_by (|(pid_a, a), (pid_b, b)|
    pid_a . cmp (pid_b) . then_with (|| a . to_string () . cmp (&b . to_string ())));
  CompleteGraphValidation { graph, errors, warnings }
}

fn error_sort_key (error : &CompleteGraphError) -> (u8, String, String) {
  match error {
    CompleteGraphError::DuplicatePrimaryId { pid, .. } =>
      (0, pid . to_string (), String::new ()),
    CompleteGraphError::DuplicateExtraId { id, .. } =>
      (1, id . to_string (), String::new ()),
    CompleteGraphError::PrimaryExtraCollision { id, .. } =>
      (2, id . to_string (), String::new ()),
    CompleteGraphError::UnconfiguredNodeHome { pid, source } =>
      (3, pid . to_string (), source . to_string ()),
    CompleteGraphError::Override (violation) =>
      (4, format!("{:?}", violation), String::new ()),
    CompleteGraphError::InternalIndex (mismatch) =>
      (5, mismatch . index . to_string (), mismatch . key . to_string ()),
  }
}

pub fn format_complete_graph_errors (errors : &[CompleteGraphError]) -> String {
  let mut lines = vec![format! ("{} complete-graph validation error(s):", errors . len ())];
  for error in errors {
    let detail = match error {
      CompleteGraphError::DuplicatePrimaryId { pid, homes } => format! (
        "duplicate primary id '{}' in homes {:?}", pid, homes),
      CompleteGraphError::DuplicateExtraId { id, owners } => format! (
        "duplicate extra id '{}' claimed by {:?}", id, owners),
      CompleteGraphError::PrimaryExtraCollision {
        id, primary_owners, extra_owners } => format! (
          "id '{}' is both primary {:?} and extra on {:?}",
          id, primary_owners, extra_owners),
      CompleteGraphError::UnconfiguredNodeHome { pid, source } => format! (
        "node '{}' has unconfigured home source '{}'", pid, source),
      CompleteGraphError::Override (violation) =>
        format_override_invariant_violations (&[violation . clone ()])
          . lines () . skip (1) . collect::<Vec<&str>> () . join (" "),
      CompleteGraphError::InternalIndex (mismatch) => format! (
        "internal index '{}' key '{}' expected {:?}, actual {:?}",
        mismatch . index, mismatch . key, mismatch . expected, mismatch . actual),
    };
    lines . push (format! ("* {}", detail)); }
  lines . join ("\n")
}

/// Convenience gate for startup/rebuild callers: preserve the independently
/// constructed graph and warnings only when no hard error was found.
pub fn validated_graph (
  config : &SkgConfig,
  nodes : &[NodeComplete],
) -> Result<(InRustGraph, Vec<(ID, TelescopeViolation)>), String> {
  let report = validate_complete_graph (config, nodes);
  if report . errors . is_empty () {
    Ok ((report . graph, report . warnings))
  } else {
    Err (format_complete_graph_errors (&report . errors)) }
}

/// Materialize a complete candidate node set by applying definitions to one
/// captured graph.  This is intentionally used before filesystem mutation so
/// save-time identity/source/topology checks happen before filesystem mutation.
pub fn validate_complete_graph_candidate (
  config : &SkgConfig,
  current : &InRustGraph,
  definitions : &[DefineNode],
) -> CompleteGraphValidation {
  let mut by_pid : BTreeMap<ID, NodeComplete> = current . nodes . iter ()
    . map (|(pid, node)| (pid . clone (), complete_from_rust (node)))
    . collect ();
  for definition in definitions {
    match definition {
      DefineNode::Save (SaveNode (node)) => {
        by_pid . insert (node . pid . clone (), node . clone ()); }
      DefineNode::Delete (DeleteNode { id, .. }) => {
        by_pid . remove (id); }
    }}
  let nodes : Vec<NodeComplete> = by_pid . into_values () . collect ();
  validate_complete_graph (config, &nodes)
}

fn complete_from_rust (node : &NodeRust) -> NodeComplete {
  NodeComplete {
    title : node . title . clone (),
    overPrivateText_telescope : node . overPrivateText_telescope,
    aliases : node . aliases . clone (),
    source : node . source . clone (),
    pid : node . pid . clone (),
    extra_ids : node . extra_ids . clone (),
    body : node . body . clone (),
    contains : node . contains . clone (),
    subscribes_to : node . subscribes_to . clone (),
    hides_from_its_subscriptions : node . hides_from_its_subscriptions . clone (),
    overrides_view_of : node . overrides_view_of . clone (),
    misc : node . misc . clone (),
  }
}
