// cargo nextest run --test grouped_unit -E 'test(in_rust_graph_override_invariants::)'
//
// The rules an owned note's forks, contents and hides keep
// ('server/dbs/in_rust_graph/override_invariants.rs'): no self-fork,
// self-containment or self-hide; no note both forking and containing
// one note, nor containing a note that forks it. Several forks of one
// note, and cycles of forks, are legal.

use skg::dbs::in_rust_graph::{InRustGraph, apply_nodeInstructions_to_inRustGraph};
use skg::dbs::in_rust_graph::override_invariants::{
  OverrideInvariantViolation,
  derive_affected_override_scope,
  validate_affected_override_invariants,
  validate_override_invariants,
};
use skg::types::misc::{ID, MSV, RelPartner, SkgConfig, Skgrepo, SkgrepoName, rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::save::{NodeInstruction, DeleteNode, SaveNode};

use proptest::prelude::*;
use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSkgrepos (HashMap::from ([
    ( SkgrepoName::from ("owned"),
      Skgrepo {
        name: SkgrepoName::from ("owned"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned"),
        owned: true,
      }),
    ( SkgrepoName::from ("foreign"),
      Skgrepo {
        name: SkgrepoName::from ("foreign"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/foreign"),
        owned: false,
      }),
  ])) }

/// A note in 'skgrepo' that forks 'forks'.
fn node (
  pid     : &str,
  skgrepo : &str,
  forks   : &[&str],
) -> Graphnode {
  let mut node : Graphnode =
    empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_skgrepo = SkgrepoName::from (skgrepo);
  node . forks = msv_of (&node . home_skgrepo, forks);
  node }

fn msv_of (
  skgrepo : &SkgrepoName,
  gnids   : &[&str],
) -> MSV<RelPartner<ID>> {
  if gnids . is_empty () { MSV::Unspecified }
  else { MSV::Specified ( partners_of (skgrepo, gnids) ) }}

fn partners_of (
  skgrepo : &SkgrepoName,
  gnids   : &[&str],
) -> Vec<RelPartner<ID>> {
  rel_partners_at_relRepo (
    skgrepo,
    gnids . iter () . map ( |gnid| ID::from (*gnid) ) . collect () ) }

fn with_contains (
  mut node : Graphnode,
  contains : &[&str],
) -> Graphnode {
  node . contains = partners_of (&node . home_skgrepo, contains);
  node }

fn with_hides (
  mut node : Graphnode,
  hides    : &[&str],
) -> Graphnode {
  node . hides = msv_of (&node . home_skgrepo, hides);
  node }

fn violations_for (
  nodes : Vec<Graphnode>,
) -> Vec<OverrideInvariantViolation> {
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (&nodes);
  validate_override_invariants (&config (), &graph) }

fn affected_and_full (
  base_nodes       : Vec<Graphnode>,
  nodeInstructions : Vec<NodeInstruction>,
) -> (Vec<OverrideInvariantViolation>, Vec<OverrideInvariantViolation>) {
  let base : InRustGraph = InRustGraph::from_graphnodes (&base_nodes);
  assert_eq! (validate_override_invariants (&config (), &base), vec![]);
  let mut candidate : InRustGraph = base . clone ();
  apply_nodeInstructions_to_inRustGraph (&mut candidate, &nodeInstructions);
  let touched : HashSet<ID> = nodeInstructions . iter () . map (|nodeInstruction|
    match nodeInstruction {
      NodeInstruction::Save (SaveNode (node)) => node . pid . clone (),
      NodeInstruction::Delete (DeleteNode { gnid, .. }) => gnid . clone (),
    }) . collect ();
  let mut affected_gnids : HashSet<ID> = touched . clone ();
  for pid in &touched {
    if let Some (old) = base . nodes . get (pid) {
      affected_gnids . extend (old . extra_ids . iter () . cloned ()); }
    if let Some (final_node) = candidate . nodes . get (pid) {
      affected_gnids . extend (final_node . extra_ids . iter () . cloned ()); }}
  let scope : HashSet<ID> = derive_affected_override_scope (
    &base, &candidate, &touched, &affected_gnids);
  (validate_affected_override_invariants (&config (), &candidate, &scope),
   validate_override_invariants (&config (), &candidate))
}

#[test]
fn one_owned_fork_is_valid () {
  assert_eq! (
    violations_for (vec![
      node ("target", "owned", &[]),
      node ("overrider", "owned", &["target"]),
    ]),
    vec![] ); }

#[test]
fn several_owned_forks_of_one_note_are_valid () {
  let mut target : Graphnode = node ("target", "owned", &[]);
  target . extra_ids = vec![ID::from ("target-extra")];
  assert_eq! (
    violations_for (vec![
      target,
      node ("one", "owned", &["target"]),
      node ("two", "owned", &["target-extra"]),
    ]),
    vec![] ); }

#[test]
fn owned_fork_chain_is_valid () {
  assert_eq! (
    violations_for (vec![
      node ("z", "owned", &[]),
      node ("y", "owned", &["z"]),
      node ("x", "owned", &["y"]),
    ]),
    vec![] ); }

#[test]
fn owned_fork_cycles_are_valid () {
  assert_eq! (
    violations_for (vec![
      node ("a", "owned", &["b"]),
      node ("b", "owned", &["a"]),
    ]),
    vec![] );
  assert_eq! (
    violations_for (vec![
      node ("a", "owned", &["b"]),
      node ("b", "owned", &["c"]),
      node ("c", "owned", &["a"]),
    ]),
    vec![] ); }

#[test]
fn an_owned_self_fork_is_a_self_fork () {
  assert_eq! (
    violations_for (vec![ node ("a", "owned", &["a"]) ]),
    vec![ OverrideInvariantViolation::SelfFork { node : ID::from ("a") } ] ); }

#[test]
fn a_self_fork_by_an_extra_id_is_a_self_fork () {
  let mut a : Graphnode = node ("a", "owned", &["a-extra"]);
  a . extra_ids = vec![ID::from ("a-extra")];
  assert_eq! (
    violations_for (vec![a]),
    vec![ OverrideInvariantViolation::SelfFork { node : ID::from ("a") } ] ); }

#[test]
fn self_containment_and_self_hiding_are_refused () {
  assert_eq! (
    violations_for (vec![
      with_contains (node ("a", "owned", &[]), &["a"]) ]),
    vec![ OverrideInvariantViolation::SelfContains { node : ID::from ("a") } ] );
  assert_eq! (
    violations_for (vec![
      with_hides (node ("a", "owned", &[]), &["a"]) ]),
    vec![ OverrideInvariantViolation::SelfHides { node : ID::from ("a") } ] ); }

#[test]
fn a_foreign_note_breaking_the_rules_is_not_checked () {
  assert_eq! (
    violations_for (vec![
      with_contains (node ("a", "foreign", &["a", "b"]), &["a", "b"]),
      node ("b", "foreign", &[]),
    ]),
    vec![] ); }

#[test]
fn a_note_may_not_contain_a_note_it_forks () {
  let expected : Vec<OverrideInvariantViolation> = vec![
    OverrideInvariantViolation::ForkeeContained {
      forker : ID::from ("r"), forkee : ID::from ("e") } ];
  assert_eq! (
    violations_for (vec![
      node ("e", "foreign", &[]),
      with_contains (node ("r", "owned", &["e"]), &["e"]),
    ]),
    expected ); }

#[test]
fn a_note_may_not_contain_a_note_that_forks_it () {
  let expected : Vec<OverrideInvariantViolation> = vec![
    OverrideInvariantViolation::ForkerContained {
      forker : ID::from ("r"), forkee : ID::from ("e") } ];
  // An owned container of its foreign forker.
  assert_eq! (
    violations_for (vec![
      with_contains (node ("e", "owned", &[]), &["r"]),
      node ("r", "foreign", &["e"]),
    ]),
    expected );
  // An owned forker of its foreign container.
  assert_eq! (
    violations_for (vec![
      with_contains (node ("e", "foreign", &[]), &["r"]),
      node ("r", "owned", &["e"]),
    ]),
    expected );
  // Both owned: one violation, seen from either side.
  assert_eq! (
    violations_for (vec![
      with_contains (node ("e", "owned", &[]), &["r"]),
      node ("r", "owned", &["e"]),
    ]),
    expected ); }

#[test]
fn a_link_to_itself_and_containing_a_fork_sibling_are_fine () {
  // r and s both fork e; r contains s: siblings, not partners.
  let mut linker : Graphnode = node ("l", "owned", &[]);
  linker . body = Some ("see [[id:l][myself]]" . to_string ());
  assert_eq! (
    violations_for (vec![
      linker,
      node ("e", "foreign", &[]),
      with_contains (node ("r", "owned", &["e"]), &["s"]),
      node ("s", "owned", &["e"]),
    ]),
    vec![] ); }

#[test]
fn a_nodeMerge_that_makes_a_note_fork_and_contain_one_note_is_affected () {
  // R forks A and contains B, untouched; the save merges B into A.
  let mut acquirer : Graphnode = node ("A", "owned", &[]);
  acquirer . extra_ids = vec![ID::from ("B")];
  let (affected, full) = affected_and_full (
    vec![
      node ("A", "owned", &[]),
      node ("B", "owned", &[]),
      with_contains (node ("R", "owned", &["A"]), &["B"]),
    ],
    vec![
      NodeInstruction::Save (SaveNode (acquirer)),
      NodeInstruction::Delete (DeleteNode {
        gnid : ID::from ("B"), home_skgrepo : SkgrepoName::from ("owned") }),
    ]);
  assert_eq! (affected, full);
  assert_eq! (affected, vec![
    OverrideInvariantViolation::ForkeeContained {
      forker : ID::from ("R"), forkee : ID::from ("A") } ]); }

#[test]
fn a_dangling_id_forked_and_contained_breaks_no_rule_until_it_names_a_note () {
  let r : Graphnode = with_contains (node ("R", "owned", &["E"]), &["E"]);
  assert_eq! ( violations_for (vec![ r . clone () ]), vec![] );
  let (affected, full) = affected_and_full (
    vec![ r ],
    vec![NodeInstruction::Save (SaveNode (node ("E", "owned", &[]))) ]);
  assert_eq! (affected, full);
  assert_eq! (affected, vec![
    OverrideInvariantViolation::ForkeeContained {
      forker : ID::from ("R"), forkee : ID::from ("E") } ]); }

#[test]
fn an_extra_id_that_closes_a_self_fork_is_affected () {
  // A forks 'future'; A acquires 'future' as an extra ID.
  let mut acquirer : Graphnode = node ("A", "owned", &["future"]);
  acquirer . extra_ids = vec![ID::from ("future")];
  let (affected, full) = affected_and_full (
    vec![ node ("A", "owned", &["future"]) ],
    vec![NodeInstruction::Save (SaveNode (acquirer))]);
  assert_eq! (affected, full);
  assert_eq! (affected, vec![
    OverrideInvariantViolation::SelfFork { node : ID::from ("A") } ]); }

#[test]
fn ownership_class_change_can_create_a_violation () {
  let (affected, full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      with_contains (node ("changed", "foreign", &["target"]), &["target"]),
    ],
    vec![NodeInstruction::Save (SaveNode (
      with_contains (node ("changed", "owned", &["target"]), &["target"]))) ]);
  assert_eq! (affected, full);
  assert_eq! (affected, vec![
    OverrideInvariantViolation::ForkeeContained {
      forker : ID::from ("changed"), forkee : ID::from ("target") } ]); }

#[test]
fn relationship_and_target_deletions_do_not_create_violations () {
  let (relationship_affected, relationship_full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      node ("repo", "owned", &["target"]),
    ],
    vec![NodeInstruction::Save (SaveNode (
      node ("repo", "owned", &[]))) ]);
  assert_eq! (relationship_affected, relationship_full);
  assert! (relationship_affected . is_empty ());

  let (delete_affected, delete_full) = affected_and_full (
    vec![
      node ("target", "owned", &[]),
      node ("repo", "owned", &["target"]),
    ],
    vec![NodeInstruction::Delete (DeleteNode {
      gnid : ID::from ("target"), home_skgrepo : SkgrepoName::from ("owned"),
    })]);
  assert_eq! (delete_affected, delete_full);
  assert! (delete_affected . is_empty ());
}

proptest! {
  #![proptest_config (ProptestConfig::with_cases (256))]

  /// One note's forks, contents and ownership
  /// changed in a small graph: the save's check agrees with the whole
  /// graph's.
  #[test]
  fn affected_check_matches_full_for_one_node_edits (
    changed_index  in 0usize..4,
    fork_index     in 0usize..5,
    other_fork_index in 0usize..5,
    contain_index  in 0usize..5,
    owned in any::<bool> (),
  ) {
    let gnids : [&str; 4] = ["A", "B", "C", "D"];
    let base : Vec<Graphnode> = vec![
      node ("A", "owned", &["B"]),
      with_contains (node ("B", "owned", &["C"]), &["D"]),
      node ("C", "owned", &[]),
      with_contains (node ("D", "foreign", &["C"]), &["A"]),
    ];
    let pick = |index : usize| -> Vec<&str> {
      if index == 4 { Vec::new () } else { vec![gnids [index]] } };
    let forks : Vec<&str> = {
      let mut forks : Vec<&str> = pick (fork_index);
      for gnid in pick (other_fork_index) {
        if ! forks . contains (&gnid) { forks . push (gnid); }}
      forks };
    let changed : Graphnode =
      with_contains (
        node (
          gnids [changed_index],
          if owned { "owned" } else { "foreign" },
          &forks),
        &pick (contain_index));
    let base_graph : InRustGraph = InRustGraph::from_graphnodes (&base);
    let mut candidate : InRustGraph = base_graph . clone ();
    apply_nodeInstructions_to_inRustGraph (
      &mut candidate, &[NodeInstruction::Save (SaveNode (changed))]);
    let touched : HashSet<ID> =
      HashSet::from ([ ID::from (gnids [changed_index]) ]);
    let scope : HashSet<ID> = derive_affected_override_scope (
      &base_graph, &candidate, &touched, &touched);
    let full_before : Vec<OverrideInvariantViolation> =
      validate_override_invariants (&config (), &base_graph);
    prop_assume! (full_before . is_empty ());
    prop_assert_eq! (
      validate_affected_override_invariants (&config (), &candidate, &scope),
      validate_override_invariants (&config (), &candidate));
  }
}
