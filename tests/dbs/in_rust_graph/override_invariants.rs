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
  validate_touched_override_invariants,
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

fn touched_violations_for (
  nodes   : Vec<NodeComplete>,
  touched : &[&str],
) -> Vec<OverrideInvariantViolation> {
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
  let touched_set : HashSet<ID> =
    touched . iter () . map ( |p| ID::from (*p) ) . collect ();
  validate_touched_override_invariants (
    &config (), &graph, &touched_set ) }

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
fn user_owned_override_chain_is_valid () {
  // x overrides y overrides z, all owned -- a linear chain, now legal.
  assert_eq! (
    violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "owned", &["z"]),
      node ("x", "owned", &["y"]),
    ]),
    vec![] ); }

#[test]
fn one_overrider_two_targets_is_valid () {
  // A single overrider may override several distinct targets; monogamy
  // gates the overridden end, not the overrider's out-degree.
  assert_eq! (
    violations_for (vec![
      node ("t1", "owned", &[]),
      node ("t2", "owned", &[]),
      node ("overrider", "owned", &["t1", "t2"]),
    ]),
    vec![] ); }

/// True iff some violation is a user-owned cycle whose member set
/// equals 'members'.
fn has_cycle_over (
  violations : &[OverrideInvariantViolation],
  members    : &[&str],
) -> bool {
  let wanted : HashSet<ID> =
    members . iter () . map ( |s| ID::from (*s) ) . collect ();
  violations . iter () . any ( |v| match v {
    OverrideInvariantViolation::UserOwnedOverrideCycle { cycle } =>
      cycle . iter () . cloned () . collect::<HashSet<ID>> () == wanted,
    _ => false } ) }

#[test]
fn two_node_user_owned_cycle_is_invalid () {
  // a overrides b, b overrides a, both owned.
  let nodes = vec![
    node ("a", "owned", &["b"]),
    node ("b", "owned", &["a"]),
  ];
  assert! ( has_cycle_over (
    &violations_for (nodes . clone ()), &["a", "b"] ) );
  for touched in [&["a"][..], &["b"][..]] {
    assert! ( has_cycle_over (
      &touched_violations_for (nodes . clone (), touched), &["a", "b"] ),
      "touched {:?} should detect the 2-cycle", touched ); } }

#[test]
fn three_node_user_owned_cycle_is_invalid () {
  // a overrides b, b overrides c, c overrides a, all owned.
  let nodes = vec![
    node ("a", "owned", &["b"]),
    node ("b", "owned", &["c"]),
    node ("c", "owned", &["a"]),
  ];
  assert! ( has_cycle_over (
    &violations_for (nodes . clone ()), &["a", "b", "c"] ) );
  for touched in [&["a"][..], &["b"][..], &["c"][..]] {
    assert! ( has_cycle_over (
      &touched_violations_for (nodes . clone (), touched),
      &["a", "b", "c"] ),
      "touched {:?} should detect the 3-cycle", touched ); } }

#[test]
fn cycle_through_foreign_link_is_valid () {
  // a(owned) overrides b(foreign) overrides c(owned) overrides a:
  // the foreign link breaks the user-owned walk, so no user-owned
  // cycle exists.
  let nodes = vec![
    node ("a", "owned",   &["b"]),
    node ("b", "foreign", &["c"]),
    node ("c", "owned",   &["a"]),
  ];
  assert_eq! ( violations_for (nodes . clone ()), vec![] );
  assert_eq! (
    touched_violations_for (nodes, &["a", "c"]), vec![] ); }

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

// --- scoped (save-time) validator ---------------------------------

#[test]
fn touched_monogamy_detected_via_either_overrider () {
  // target gains a 2nd user-owned overrider; flagged whether the save
  // touched "one" or "two".
  let nodes = || vec![
    node ("target", "owned", &[]),
    node ("one", "owned", &["target"]),
    node ("two", "owned", &["target"]),
  ];
  for touched in [&["one"][..], &["two"][..]] {
    let violations : Vec<OverrideInvariantViolation> =
      touched_violations_for (nodes (), touched);
    assert! (violations . iter () . any ( |v| matches!(
      v,
      OverrideInvariantViolation::MultipleUserOwnedOverriders {
        overridden, ..
      } if overridden == &ID::from ("target") )),
      "touched {:?} should flag the monogamy violation", touched ); } }

#[test]
fn touched_user_owned_chain_validates_from_either_end () {
  // x → y → z, all owned -- a linear chain, now legal. The touched
  // validator accepts it whether the save touched the first or middle.
  let nodes = || vec![
    node ("z", "owned", &[]),
    node ("y", "owned", &["z"]),
    node ("x", "owned", &["y"]),
  ];
  for touched in [&["x"][..], &["y"][..]] {
    assert_eq! (
      touched_violations_for (nodes (), touched), vec![],
      "touched {:?} should accept the linear chain", touched ); } }

#[test]
fn touched_foreign_overriders_do_not_count () {
  assert_eq! (
    touched_violations_for (vec![
      node ("target", "owned", &[]),
      node ("owned-one", "owned", &["target"]),
      node ("foreign-one", "foreign", &["target"]),
      node ("foreign-two", "foreign", &["target"]),
    ], &["owned-one", "foreign-one", "foreign-two"]),
    vec![] ); }

#[test]
fn touched_chain_through_foreign_middle_is_valid () {
  assert_eq! (
    touched_violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "foreign", &["z"]),
      node ("x", "owned", &["y"]),
    ], &["x"]),
    vec![] ); }

#[test]
fn untouched_preexisting_violation_is_not_rescanned () {
  // A pre-existing monogamy violation among nodes the save did not
  // touch (here the save only touched the overridden "target", which
  // overrides nothing) is not reported — that is the whole point of
  // scoping; such states are rejected at init instead.
  assert_eq! (
    touched_violations_for (vec![
      node ("target", "owned", &[]),
      node ("one", "owned", &["target"]),
      node ("two", "owned", &["target"]),
    ], &["target"]),
    vec![] ); }

#[test]
fn touched_unknown_source_is_reported () {
  let violations : Vec<OverrideInvariantViolation> =
    touched_violations_for (vec![
      node ("ghost", "nonexistent-source", &[]),
    ], &["ghost"]);
  assert! (violations . iter () . any ( |v| matches!(
    v, OverrideInvariantViolation::UnknownSource { node, .. }
       if node == &ID::from ("ghost") ))); }

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
