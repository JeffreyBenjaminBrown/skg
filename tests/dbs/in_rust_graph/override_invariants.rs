use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::audit::{
  Mismatch,
  RelationshipMismatch,
  SourceMismatch,
  format_mismatches,
};
use skg::dbs::in_rust_graph::override_invariants::{
  OverrideInvariantViolation,
  validate_override_invariants,
};
use skg::types::misc::{ID, MSV, SkgConfig, SkgfileSource, SourceName};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSources (HashMap::from ([
    ( SourceName::from ("owned"),
      SkgfileSource {
        name: SourceName::from ("owned"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned"),
        user_owns_it: true,
      }),
    ( SourceName::from ("foreign"),
      SkgfileSource {
        name: SourceName::from ("foreign"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/foreign"),
        user_owns_it: false,
      }),
  ])) }

fn node (
  pid       : &str,
  source    : &str,
  overrides : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from (source);
  node . overrides_view_of =
    if overrides . is_empty () {
      MSV::Unspecified
    } else {
      MSV::Specified (
        overrides . iter ()
        . map ( |id| ID::from (*id) )
        . collect () )
    };
  node }

fn violations_for (
  nodes : Vec<NodeComplete>,
) -> Vec<OverrideInvariantViolation> {
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
  validate_override_invariants (&config (), &graph) }

#[test]
fn one_user_owned_overrider_is_valid () {
  assert_eq! (
    violations_for (vec![
      node ("target", "owned", &[]),
      node ("overrider", "owned", &["target"]),
    ]),
    vec![] ); }

#[test]
fn two_user_owned_overriders_of_same_target_are_invalid () {
  let violations : Vec<OverrideInvariantViolation> =
    violations_for (vec![
      node ("target", "owned", &[]),
      node ("one", "owned", &["target"]),
      node ("two", "owned", &["target"]),
    ]);
  assert_eq! (violations . len (), 1);
  assert! (matches!(
    &violations[0],
    OverrideInvariantViolation::MultipleUserOwnedOverriders {
      overridden,
      overriders,
    } if overridden == &ID::from ("target")
         && overriders == &vec![ID::from ("one"), ID::from ("two")]
  )); }

#[test]
fn foreign_overriders_do_not_count_for_monogamy () {
  assert_eq! (
    violations_for (vec![
      node ("target", "owned", &[]),
      node ("owned-one", "owned", &["target"]),
      node ("foreign-one", "foreign", &["target"]),
      node ("foreign-two", "foreign", &["target"]),
    ]),
    vec![] ); }

#[test]
fn extra_id_targets_are_resolved_before_monogamy_check () {
  let mut target : NodeComplete =
    node ("target", "owned", &[]);
  target . extra_ids = vec![ID::from ("target-extra")];
  let violations : Vec<OverrideInvariantViolation> =
    violations_for (vec![
      target,
      node ("one", "owned", &["target"]),
      node ("two", "owned", &["target-extra"]),
    ]);
  assert! (matches!(
    &violations[0],
    OverrideInvariantViolation::MultipleUserOwnedOverriders {
      overridden,
      ..
    } if overridden == &ID::from ("target")
  )); }

#[test]
fn user_owned_override_chain_is_invalid () {
  let violations : Vec<OverrideInvariantViolation> =
    violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "owned", &["z"]),
      node ("x", "owned", &["y"]),
    ]);
  assert! (violations . iter () . any ( |v| matches!(
    v,
    OverrideInvariantViolation::UserOwnedOverrideChain {
      first_overrider,
      middle_overrider,
      ..
    } if first_overrider == &ID::from ("x")
         && middle_overrider == &ID::from ("y")
  ))); }

#[test]
fn chain_through_foreign_middle_is_valid () {
  assert_eq! (
    violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "foreign", &["z"]),
      node ("x", "owned", &["y"]),
    ]),
    vec![] ); }

#[test]
fn chain_from_foreign_first_is_valid () {
  assert_eq! (
    violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "owned", &["z"]),
      node ("x", "foreign", &["y"]),
    ]),
    vec![] ); }

#[test]
fn audit_format_relationship_mismatch_keeps_relation_and_role () {
  let mismatches : Vec<Mismatch> = vec![
    Mismatch::Relationship (RelationshipMismatch {
      pid: ID::from ("p"),
      relation: "contains",
      role: "container",
      in_rust_graph: HashSet::from ([ID::from ("a")]),
      typedb: HashSet::from ([ID::from ("b")]),
    }) ];
  let text : String =
    format_mismatches (&mismatches);
  assert! (text . contains ("relation=contains"));
  assert! (text . contains ("role=container")); }

#[test]
fn audit_format_missing_typedb_node () {
  let mismatches : Vec<Mismatch> = vec![
    Mismatch::MissingTypedbNode {
      pid: ID::from ("p"),
    } ];
  let text : String =
    format_mismatches (&mismatches);
  assert! (text . contains (
    "pid=p missing TypeDB node" ));
  assert! (! text . contains ("relation=has_source")); }

#[test]
fn audit_format_source_mismatch_different_source () {
  let mismatches : Vec<Mismatch> = vec![
    Mismatch::Source (SourceMismatch {
      pid: ID::from ("p"),
      in_rust_graph: SourceName::from ("main"),
      typedb: SourceName::from ("other"),
    }) ];
  let text : String =
    format_mismatches (&mismatches);
  assert! (text . contains (
    "pid=p source mismatch in-Rust graph=main typedb=other" )); }
