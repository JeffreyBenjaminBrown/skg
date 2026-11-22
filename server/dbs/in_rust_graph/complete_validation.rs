//! Canonical validation of a complete filesystem-derived node set.
//!
//! This is the canonical home for whole-graph constraints. It separates hard
//! errors (the candidate must
//! not be swapped in) from warnings (the data remains loadable but
//! deserves repair): telescope warnings, and the fork rules' violations
//! a foreign note takes part in. It returns each in deterministic order.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::internal_index_validation::{
  InternalIndexMismatch, validate_internal_indexes,
};
use crate::dbs::in_rust_graph::fork_rules::{
  ForkRuleViolation, describe_fork_rule_violation,
  describe_violation_a_foreign_note_takes_part_in, foreign_note_taking_part,
  validate_fork_rules,
};
use crate::telescope::invariants::{TelescopeViolation, validate_all_telescopes};
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::save::{NodeInstruction, DeleteNode, SaveNode};

use std::collections::{BTreeMap, BTreeSet};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum CompleteGraphError {
  DuplicatePrimaryId { pid : ID, homes : Vec<SkgrepoName> },
  DuplicateExtraId { gnid : ID, carriers : Vec<ID> },
  PrimaryExtraCollision { gnid : ID, primary_carriers : Vec<ID>, extra_carriers : Vec<ID> },
  UnconfiguredNodeHome { pid : ID, skgrepo : SkgrepoName },
  ForkRule (ForkRuleViolation),
  InternalIndex (InternalIndexMismatch),
}

#[derive(Clone, Debug)]
pub struct CompleteGraphValidation {
  pub graph : InRustGraph,
  pub errors : Vec<CompleteGraphError>,
  pub warnings : Vec<(ID, TelescopeViolation)>,
  /// The fork rules' violations a foreign note takes part in
  /// ('foreign_note_taking_part'), described.
  pub fork_rule_warnings : Vec<String>,
}

impl CompleteGraphValidation {
  pub fn is_valid (&self) -> bool { self . errors . is_empty () }
}

/// Validate all facts requiring the complete set: identity, configured homes,
/// the rules of owned notes' forks, contents and hides
/// ('fork_rules.rs'; a violation a foreign note takes part in
/// is a warning), internal graph indexes, and telescope provenance/privacy.
/// Relationship roles themselves are represented by distinct typed fields;
/// dangling member IDs are deliberately retained and are not errors.
pub fn validate_complete_graph (
  config : &SkgConfig,
  nodes : &[Graphnode],
) -> CompleteGraphValidation {
  let mut primary_homes : BTreeMap<ID, Vec<SkgrepoName>> = BTreeMap::new ();
  let mut extra_carriers : BTreeMap<ID, BTreeSet<ID>> = BTreeMap::new ();
  for node in nodes {
    primary_homes . entry (node . pid . clone ()) . or_default ()
      . push (node . home_skgrepo . clone ());
    for extra in node . normalized_extra_ids () {
      extra_carriers . entry (extra) . or_default ()
        . insert (node . pid . clone ()); }}

  let mut errors : Vec<CompleteGraphError> = Vec::new ();
  for (pid, homes) in &primary_homes {
    if homes . len () > 1 {
      let mut homes = homes . clone ();
      homes . sort ();
      errors . push (CompleteGraphError::DuplicatePrimaryId {
        pid : pid . clone (), homes }); }}
  for (gnid, carriers) in &extra_carriers {
    if carriers . len () > 1 {
      errors . push (CompleteGraphError::DuplicateExtraId {
        gnid : gnid . clone (),
        carriers : carriers . iter () . cloned () . collect (), }); }}
  for gnid in primary_homes . keys () {
    if let Some (extras) = extra_carriers . get (gnid) {
      let extra_carriers : Vec<ID> = extras . iter ()
        . filter ( |carrier| *carrier != gnid )
        . cloned ()
        . collect ();
      if ! extra_carriers . is_empty () {
        errors . push (CompleteGraphError::PrimaryExtraCollision {
          gnid : gnid . clone (),
          primary_carriers : vec![gnid . clone ()],
          extra_carriers, }); }}}
  let mut nodes_by_pid : Vec<&Graphnode> = nodes . iter () . collect ();
  nodes_by_pid . sort_by (|a, b| a . pid . cmp (&b . pid));
  for node in nodes_by_pid {
    if ! config . skgrepos . contains_key (&node . home_skgrepo) {
      errors . push (CompleteGraphError::UnconfiguredNodeHome {
        pid : node . pid . clone (), skgrepo : node . home_skgrepo . clone () }); }}

  let graph = InRustGraph::from_graphnodes (nodes);
  // Topology and graph-index diagnostics are meaningful only when identity
  // is unambiguous; otherwise graph construction necessarily chose winners.
  let identity_is_unambiguous = ! errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::DuplicatePrimaryId { .. }
    | CompleteGraphError::DuplicateExtraId { .. }
    | CompleteGraphError::PrimaryExtraCollision { .. }));
  let mut fork_rule_warnings : Vec<String> = Vec::new ();
  if identity_is_unambiguous {
    for violation in validate_fork_rules (config, &graph) {
      match foreign_note_taking_part (config, &graph, &violation) {
        Some (foreign) => fork_rule_warnings . push (
          describe_violation_a_foreign_note_takes_part_in (
            &violation, &foreign )),
        None => errors . push (CompleteGraphError::ForkRule (violation)), }}
    errors . extend (validate_internal_indexes (&graph)
      . into_iter () . map (CompleteGraphError::InternalIndex)); }
  errors . sort_by_key (error_sort_key);

  let mut warnings = validate_all_telescopes (config, &graph);
  warnings . sort_by (|(pid_a, a), (pid_b, b)|
    pid_a . cmp (pid_b) . then_with (|| a . to_string () . cmp (&b . to_string ())));
  CompleteGraphValidation { graph, errors, warnings, fork_rule_warnings }
}

fn error_sort_key (error : &CompleteGraphError) -> (u8, String, String) {
  match error {
    CompleteGraphError::DuplicatePrimaryId { pid, .. } =>
      (0, pid . to_string (), String::new ()),
    CompleteGraphError::DuplicateExtraId { gnid, .. } =>
      (1, gnid . to_string (), String::new ()),
    CompleteGraphError::PrimaryExtraCollision { gnid, .. } =>
      (2, gnid . to_string (), String::new ()),
    CompleteGraphError::UnconfiguredNodeHome { pid, skgrepo } =>
      (3, pid . to_string (), skgrepo . to_string ()),
    CompleteGraphError::ForkRule (violation) =>
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
      CompleteGraphError::DuplicateExtraId { gnid, carriers } => format! (
        "duplicate extra id '{}' claimed by {:?}", gnid, carriers),
      CompleteGraphError::PrimaryExtraCollision {
        gnid, primary_carriers, extra_carriers } => format! (
          "id '{}' is both primary {:?} and extra on {:?}",
          gnid, primary_carriers, extra_carriers),
      CompleteGraphError::UnconfiguredNodeHome { pid, skgrepo } => format! (
        "node '{}' has unconfigured home repo '{}'", pid, skgrepo),
      CompleteGraphError::ForkRule (violation) =>
        describe_fork_rule_violation (violation),
      CompleteGraphError::InternalIndex (mismatch) => format! (
        "internal index '{}' key '{}' expected {:?}, actual {:?}",
        mismatch . index, mismatch . key, mismatch . expected, mismatch . actual),
    };
    lines . push (format! ("* {}", detail)); }
  lines . join ("\n")
}

/// Convenience gate for startup/rebuild callers: preserve the independently
/// constructed graph, its telescope warnings and its fork-rule warnings
/// only when no hard error was found.
pub fn validated_graph (
  config : &SkgConfig,
  nodes : &[Graphnode],
) -> Result<(InRustGraph, Vec<(ID, TelescopeViolation)>, Vec<String>), String> {
  let report = validate_complete_graph (config, nodes);
  if report . errors . is_empty () {
    Ok ((report . graph, report . warnings, report . fork_rule_warnings))
  } else {
    Err (format_complete_graph_errors (&report . errors)) }
}

/// Materialize and fully validate a candidate for differential tests and
/// explicit audits. Production save preparation proves the same invariants
/// incrementally and does not call this whole-graph oracle.
pub fn validate_complete_graph_candidate (
  config : &SkgConfig,
  current : &InRustGraph,
  nodeInstructions : &[NodeInstruction],
) -> CompleteGraphValidation {
  let mut by_pid : BTreeMap<ID, Graphnode> = current . nodes . iter ()
    . map (|(pid, node)| (pid . clone (), Graphnode::from (node)))
    . collect ();
  for nodeInstruction in nodeInstructions {
    match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) => {
        by_pid . insert (node . pid . clone (), node . clone ()); }
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => {
        by_pid . remove (gnid); }
    }}
  let nodes : Vec<Graphnode> = by_pid . into_values () . collect ();
  validate_complete_graph (config, &nodes)
}
