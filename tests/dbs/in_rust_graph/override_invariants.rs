use skg::dbs::in_rust_graph::{InRustGraph, apply_definenodes_to_inRustGraph};
use skg::dbs::in_rust_graph::override_invariants::{
  OverrideInvariantViolation,
  derive_affected_override_scope,
  validate_affected_override_invariants,
  validate_override_invariants,
  validate_touched_override_invariants,
};
use skg::types::misc::{ID, MSV, SkgConfig, SkgfileSource, SourceName, members_at_source};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, DeleteNode, SaveNode};

use proptest::prelude::*;
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
        members_at_source (
          &node . source,
          overrides . iter ()
          . map ( |id| ID::from (*id) )
          . collect () ) )
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

fn affected_and_full (
  base_nodes  : Vec<NodeComplete>,
  definitions : Vec<DefineNode>,
) -> (Vec<OverrideInvariantViolation>, Vec<OverrideInvariantViolation>) {
  let base : InRustGraph = InRustGraph::from_nodecompletes (&base_nodes);
  assert_eq! (validate_override_invariants (&config (), &base), vec![]);
  let mut candidate : InRustGraph = base . clone ();
  apply_definenodes_to_inRustGraph (&mut candidate, &definitions);
  let touched : HashSet<ID> = definitions . iter () . map (|definition|
    match definition {
      DefineNode::Save (SaveNode (node)) => node . pid . clone (),
      DefineNode::Delete (DeleteNode { id, .. }) => id . clone (),
    }) . collect ();
  let mut affected_ids : HashSet<ID> = touched . clone ();
  for pid in &touched {
    if let Some (old) = base . nodes . get (pid) {
      affected_ids . extend (old . extra_ids . iter () . cloned ()); }
    if let Some (final_node) = candidate . nodes . get (pid) {
      affected_ids . extend (final_node . extra_ids . iter () . cloned ()); }}
  let scope = derive_affected_override_scope (
    &base, &candidate, &touched, &affected_ids);
  (validate_affected_override_invariants (&config (), &candidate, &scope),
   validate_override_invariants (&config (), &candidate))
}

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

#[test]
fn alias_redirection_that_closes_a_cycle_is_affected () {
  let mut acquirer : NodeComplete = node ("B", "owned", &["A"]);
  acquirer . extra_ids = vec![ID::from ("future")];
  let (affected, full) = affected_and_full (
    vec![
      node ("A", "owned", &["future"]),
      node ("B", "owned", &["A"]),
    ],
    vec![DefineNode::Save (SaveNode (acquirer))]);
  assert_eq! (affected, full);
  assert! (has_cycle_over (&affected, &["A", "B"]));
}

#[test]
fn ownership_class_change_can_create_monogamy_violation () {
  let (affected, full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      node ("changed", "foreign", &["target"]),
      node ("existing", "owned", &["target"]),
    ],
    vec![DefineNode::Save (SaveNode (
      node ("changed", "owned", &["target"]))) ]);
  assert_eq! (affected, full);
  assert! (matches! (
    affected . first (),
    Some (OverrideInvariantViolation::MultipleUserOwnedOverriders {
      overridden, ..
    }) if overridden == &ID::from ("target")));
}

#[test]
fn edge_and_target_deletions_do_not_create_override_errors () {
  let (edge_affected, edge_full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      node ("source", "owned", &["target"]),
    ],
    vec![DefineNode::Save (SaveNode (
      node ("source", "owned", &[]))) ]);
  assert_eq! (edge_affected, edge_full);
  assert! (edge_affected . is_empty ());

  let (delete_affected, delete_full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      node ("source", "owned", &["target"]),
    ],
    vec![DefineNode::Delete (DeleteNode {
      id : ID::from ("target"), source : SourceName::from ("owned"),
    })]);
  assert_eq! (delete_affected, delete_full);
  assert! (delete_affected . is_empty ());
}

#[test]
fn adding_a_linear_override_chain_remains_valid () {
  let (affected, full) = affected_and_full (
    vec![
      node ("A", "owned", &[]),
      node ("B", "owned", &["C"]),
      node ("C", "owned", &[]),
    ],
    vec![DefineNode::Save (SaveNode (
      node ("A", "owned", &["B"]))) ]);
  assert_eq! (affected, full);
  assert! (affected . is_empty ());
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (256))]

  #[test]
  fn affected_override_check_matches_full_for_one_node_edits (
    changed_index in 0usize..4,
    target_index in 0usize..5,
    owned in any::<bool> (),
  ) {
    let ids : [&str; 4] = ["A", "B", "C", "D"];
    let base : Vec<NodeComplete> = vec![
      node ("A", "owned", &["B"]),
      node ("B", "owned", &["C"]),
      node ("C", "owned", &[]),
      node ("D", "foreign", &["C"]),
    ];
    let targets : Vec<&str> =
      if target_index == 4 { Vec::new () }
      else { vec![ids [target_index]] };
    let changed : NodeComplete = node (
      ids [changed_index],
      if owned { "owned" } else { "foreign" },
      &targets);
    let (affected, full) = affected_and_full (
      base, vec![DefineNode::Save (SaveNode (changed))]);
    prop_assert_eq! (affected, full);
  }
}

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
