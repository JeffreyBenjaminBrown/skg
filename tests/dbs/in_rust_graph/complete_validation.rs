use skg::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, format_complete_graph_errors, validate_complete_graph,
  validate_complete_graph_candidate,
};
use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::override_invariants::OverrideInvariantViolation;
use skg::telescope::invariants::TelescopeViolation;
use skg::types::misc::{
  ID, MSV, RelPartner, SkgConfig, Skgrepo, SkgrepoName,
};
use skg::types::nodes::complete::{Graphnode, empty_graphnode};
use skg::types::save::{NodeInstruction, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let mut skgrepos = HashMap::new ();
  for (name, owned) in [("public", true), ("private", true), ("foreign", false)] {
    skgrepos . insert (SkgrepoName::from (name), Skgrepo {
      name : SkgrepoName::from (name),
      abbreviation : None,
      path : PathBuf::from (format! ("{}-path", name)),
      owned        : owned,
    }); }
  let mut config = SkgConfig::dummyFromSkgrepos (skgrepos);
  config . skgrepo_order = vec![
    SkgrepoName::from ("public"),
    SkgrepoName::from ("private"),
    SkgrepoName::from ("foreign"),
  ];
  config
}

fn node (pid : &str, skgrepo : &str) -> Graphnode {
  let mut node = empty_graphnode ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_skgrepo = SkgrepoName::from (skgrepo);
  node
}

#[test]
fn identity_errors_are_aggregated_and_deterministically_ordered () {
  let mut first = node ("same-pid", "public");
  first . extra_ids = vec![ID::from ("shared-extra"), ID::from ("also-primary")];
  let mut second = node ("same-pid", "private");
  second . extra_ids = vec![ID::from ("same-pid")];
  let mut other = node ("other", "private");
  other . extra_ids = vec![ID::from ("shared-extra")];
  let also_primary = node ("also-primary", "public");
  let report = validate_complete_graph (
    &config (), &[first, second, other, also_primary]);
  assert! (matches! (
    report . errors [0], CompleteGraphError::DuplicatePrimaryId { .. }));
  assert! (matches! (
    report . errors [1], CompleteGraphError::DuplicateExtraId { .. }));
  assert! (matches! (
    report . errors [2], CompleteGraphError::PrimaryExtraCollision { .. }));
  let text = format_complete_graph_errors (&report . errors);
  assert! (text . contains ("duplicate primary id 'same-pid'"));
  assert! (text . contains ("duplicate extra id 'shared-extra'"));
  assert! (text . contains ("id 'also-primary' is both primary"));
}

#[test]
fn repeated_gnids_of_one_recorder_are_normalized_not_rejected () {
  let mut carrier = node ("carrier", "public");
  carrier . extra_ids = vec![
    ID::from ("B"),
    ID::from ("carrier"),
    ID::from ("A"),
    ID::from ("B"),
    ID::from ("A"),
  ];
  let report = validate_complete_graph (&config (), &[carrier]);
  assert! (report . is_valid (), "{:?}", report . errors);
  assert_eq! (
    report . graph . get (&ID::from ("carrier")) . unwrap () . extra_ids,
    vec![ID::from ("B"), ID::from ("A")]);
  assert_eq! (
    report . graph . extra_id_to_pid . get (&ID::from ("B")),
    Some (&ID::from ("carrier")));
  assert! (! report . graph . extra_id_to_pid . contains_key (
    &ID::from ("carrier")));
}

#[test]
fn unknown_home_is_hard_but_relationship_provenance_is_a_warning () {
  let mut recorder = node ("recorder", "public");
  recorder . contains = vec![
    RelPartner::at_relRepo (
      SkgrepoName::from ("unconfigured-relRepo"), ID::from ("dangling")),
  ];
  let unknown_home = node ("unknown-home", "unconfigured-home");
  let report = validate_complete_graph (&config (), &[recorder, unknown_home]);
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::UnconfiguredNodeHome { pid, .. }
      if pid == &ID::from ("unknown-home"))));
  assert! (report . warnings . iter () . any (|(pid, warning)|
    pid == &ID::from ("recorder") && matches! (
      warning, TelescopeViolation::UnconfiguredRelRepo { member, .. }
        if member == &ID::from ("dangling"))));
  // The unresolved member itself is retained, not diagnosed as an error.
  assert! (report . graph . contained_by . contains_key (&ID::from ("dangling")));
}

#[test]
fn configured_dangling_members_are_tolerated_without_warning () {
  let mut recorder = node ("recorder", "public");
  recorder . forks = MSV::Specified (vec![RelPartner::at_relRepo (
    SkgrepoName::from ("public"), ID::from ("absent"))]);
  let report = validate_complete_graph (&config (), &[recorder]);
  assert! (report . is_valid ());
  assert! (report . warnings . is_empty ());
  assert! (report . graph . forkers_of . contains_key (&ID::from ("absent")));
}

#[test]
fn canonical_entry_includes_the_fork_rules_and_telescope_orientation () {
  // a and b both fork target (legal); a also contains it (not).
  let target = node ("target", "private");
  let mut a = node ("a", "public");
  let mut b = node ("b", "public");
  for forker in [&mut a, &mut b] {
    forker . forks = MSV::Specified (vec![
      RelPartner::at_relRepo (
        SkgrepoName::from ("public"), ID::from ("target")),
    ]); }
  a . contains = vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("private"), ID::from ("target")) ];
  let report = validate_complete_graph (&config (), &[target, a, b]);
  assert_eq! (report . errors, vec![
    CompleteGraphError::Override (
      OverrideInvariantViolation::ForkeeContained {
        forker : ID::from ("a"), forkee : ID::from ("target") }) ]);
  assert_eq! (report . warnings . iter () . filter (|(_, warning)| matches! (
    warning, TelescopeViolation::LeakShapedMembership {
      relation : "forks", .. })) . count (), 2);
}

/// Owned e contains foreign f, which forks e: a
/// foreign record takes part, so it is a warning naming both notes and
/// which is foreign, not an error.
#[test]
fn an_owned_note_containing_its_foreign_forker_is_a_warning () {
  let mut e = node ("e", "public");
  e . contains = vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("public"), ID::from ("f")) ];
  let mut f = node ("f", "foreign");
  f . forks = MSV::Specified (vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("foreign"), ID::from ("e")) ]);
  let report = validate_complete_graph (&config (), &[e, f]);
  assert! (report . is_valid (), "{:?}", report . errors);
  assert_eq! (report . fork_rule_warnings . len (), 1,
              "{:?}", report . fork_rule_warnings);
  let warning : &str = &report . fork_rule_warnings [0];
  assert! (warning . contains ("node e contains node f, which forks it")
           && warning . contains ("Node f is foreign"),
           "{}", warning);
}

/// Owned r forks foreign e, which contains r: a
/// warning naming e as foreign.
#[test]
fn an_owned_note_forking_its_foreign_container_is_a_warning () {
  let mut r = node ("r", "public");
  r . forks = MSV::Specified (vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("public"), ID::from ("e")) ]);
  let mut e = node ("e", "foreign");
  e . contains = vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("foreign"), ID::from ("r")) ];
  let report = validate_complete_graph (&config (), &[r, e]);
  assert! (report . is_valid (), "{:?}", report . errors);
  assert_eq! (report . fork_rule_warnings . len (), 1,
              "{:?}", report . fork_rule_warnings);
  assert! (report . fork_rule_warnings [0] . contains ("Node e is foreign"),
           "{}", report . fork_rule_warnings [0]);
}

/// Owned e contains owned r, which forks e: owned
/// records alone make the violation, so it is an error.
#[test]
fn an_owned_note_containing_its_owned_forker_is_an_error () {
  let mut e = node ("e", "public");
  e . contains = vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("public"), ID::from ("r")) ];
  let mut r = node ("r", "public");
  r . forks = MSV::Specified (vec![ RelPartner::at_relRepo (
    SkgrepoName::from ("public"), ID::from ("e")) ]);
  let report = validate_complete_graph (&config (), &[e, r]);
  assert_eq! (report . errors, vec![
    CompleteGraphError::Override (
      OverrideInvariantViolation::ForkerContained {
        forker : ID::from ("r"), forkee : ID::from ("e") }) ]);
  assert! (report . fork_rule_warnings . is_empty ());
}

#[test]
fn save_candidate_is_rejected_before_publication () {
  let current = InRustGraph::from_graphnodes (&[
    node ("existing", "public"),
    node ("edited", "public"),
  ]);
  let mut edited = node ("edited", "unconfigured-home");
  edited . extra_ids = vec![ID::from ("existing")];
  let report = validate_complete_graph_candidate (
    &config (), &current, &[NodeInstruction::Save (SaveNode (edited))]);
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::PrimaryExtraCollision { gnid, .. }
      if gnid == &ID::from ("existing"))));
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::UnconfiguredNodeHome { pid, .. }
      if pid == &ID::from ("edited"))));
  // Validation is pure: the caller's current graph remains untouched.
  assert_eq! (current . get (&ID::from ("edited")) . unwrap () . home_skgrepo,
              SkgrepoName::from ("public"));
}
