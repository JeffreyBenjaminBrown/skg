use skg::dbs::in_rust_graph::complete_validation::{
  CompleteGraphError, format_complete_graph_errors, validate_complete_graph,
  validate_complete_graph_candidate,
};
use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::override_invariants::OverrideInvariantViolation;
use skg::telescope::invariants::TelescopeViolation;
use skg::types::misc::{
  ID, MSV, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};
use skg::types::save::{DefineNode, SaveNode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  let mut sources = HashMap::new ();
  for (name, owned) in [("public", true), ("private", true), ("foreign", false)] {
    sources . insert (SourceName::from (name), SkgfileSource {
      name : SourceName::from (name),
      abbreviation : None,
      path : PathBuf::from (format! ("{}-path", name)),
      user_owns_it : owned,
    }); }
  let mut config = SkgConfig::dummyFromSources (sources);
  config . source_order = vec![
    SourceName::from ("public"),
    SourceName::from ("private"),
    SourceName::from ("foreign"),
  ];
  config
}

fn node (pid : &str, source : &str) -> NodeComplete {
  let mut node = empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from (source);
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
fn repeated_ids_of_one_owner_are_normalized_not_rejected () {
  let mut owner = node ("owner", "public");
  owner . extra_ids = vec![
    ID::from ("B"),
    ID::from ("owner"),
    ID::from ("A"),
    ID::from ("B"),
    ID::from ("A"),
  ];
  let report = validate_complete_graph (&config (), &[owner]);
  assert! (report . is_valid ());
  assert_eq! (
    report . graph . get (&ID::from ("owner")) . unwrap () . extra_ids,
    vec![ID::from ("B"), ID::from ("A")]);
  assert_eq! (
    report . graph . extra_id_to_pid . get (&ID::from ("B")),
    Some (&ID::from ("owner")));
  assert! (! report . graph . extra_id_to_pid . contains_key (
    &ID::from ("owner")));
}

#[test]
fn unknown_home_is_hard_but_edge_provenance_is_a_warning () {
  let mut owner = node ("owner", "public");
  owner . contains = vec![
    MemberAtSource::at_source (
      SourceName::from ("unconfigured-edge-source"), ID::from ("dangling")),
  ];
  let unknown_home = node ("unknown-home", "unconfigured-home");
  let report = validate_complete_graph (&config (), &[owner, unknown_home]);
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::UnconfiguredNodeHome { pid, .. }
      if pid == &ID::from ("unknown-home"))));
  assert! (report . warnings . iter () . any (|(pid, warning)|
    pid == &ID::from ("owner") && matches! (
      warning, TelescopeViolation::UnconfiguredSource { member, .. }
        if member == &ID::from ("dangling"))));
  // The unresolved member itself is retained, not diagnosed as an error.
  assert! (report . graph . contained_by . contains_key (&ID::from ("dangling")));
}

#[test]
fn configured_dangling_members_are_tolerated_without_warning () {
  let mut owner = node ("owner", "public");
  owner . subscribes_to = MSV::Specified (vec![MemberAtSource::at_source (
    SourceName::from ("public"), ID::from ("absent"))]);
  let report = validate_complete_graph (&config (), &[owner]);
  assert! (report . is_valid ());
  assert! (report . warnings . is_empty ());
  assert! (report . graph . subscribers_of . contains_key (&ID::from ("absent")));
}

#[test]
fn canonical_entry_includes_override_monogamy_and_telescope_orientation () {
  let target = node ("target", "private");
  let mut a = node ("a", "public");
  let mut b = node ("b", "public");
  for overrider in [&mut a, &mut b] {
    overrider . overrides_view_of = MSV::Specified (vec![
      MemberAtSource::at_source (
        SourceName::from ("public"), ID::from ("target")),
    ]); }
  let report = validate_complete_graph (&config (), &[target, a, b]);
  assert! (report . errors . iter () . any (|error| matches! (
    error,
    CompleteGraphError::Override (
      OverrideInvariantViolation::MultipleUserOwnedOverriders { overridden, .. })
      if overridden == &ID::from ("target"))));
  assert_eq! (report . warnings . iter () . filter (|(_, warning)| matches! (
    warning, TelescopeViolation::LeakShapedMember {
      relation : "overrides_view_of", .. })) . count (), 2);
}

#[test]
fn save_candidate_is_rejected_before_publication () {
  let current = InRustGraph::from_nodecompletes (&[
    node ("existing", "public"),
    node ("edited", "public"),
  ]);
  let mut edited = node ("edited", "unconfigured-home");
  edited . extra_ids = vec![ID::from ("existing")];
  let report = validate_complete_graph_candidate (
    &config (), &current, &[DefineNode::Save (SaveNode (edited))]);
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::PrimaryExtraCollision { id, .. }
      if id == &ID::from ("existing"))));
  assert! (report . errors . iter () . any (|error| matches! (
    error, CompleteGraphError::UnconfiguredNodeHome { pid, .. }
      if pid == &ID::from ("edited"))));
  // Validation is pure: the caller's current graph remains untouched.
  assert_eq! (current . get (&ID::from ("edited")) . unwrap () . source,
              SourceName::from ("public"));
}
