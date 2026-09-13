//! Unit tests for the telescope invariant validator: the leak shape
//! (an edge more public than its target's home) is caught; honest
//! shapes are not; absent targets use their extant owner's home; extra-id
//! anchors resolve.

use super::{TelescopeViolation, affected_telescope_warnings,
            derive_affected_telescope_owners, telescope_violations_of,
            validate_all_telescopes};
use crate::dbs::in_rust_graph::{InRustGraph, apply_definenodes_to_inRustGraph};
use crate::types::misc::{
  ID, MSV, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
  members_at_source};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};
use crate::types::save::{DefineNode, DeleteNode, SaveNode};

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

/// public < private, per source_order (dummy configs otherwise fall
/// back to ALPHABETICAL order, where "private" < "public" would
/// invert the ladder).
fn two_source_config () -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for name in ["public", "private"] {
    sources . insert (
      SourceName::from (name),
      SkgfileSource {
        name         : SourceName::from (name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . source_order = vec! [
    SourceName::from ("public"),
    SourceName::from ("private") ];
  config }

fn node_at (
  pid    : &str,
  source : &str,
) -> NodeComplete {
  let mut n : NodeComplete = empty_node_complete ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . source = SourceName::from (source);
  n }

#[test]
fn leak_shaped_member_is_caught_and_honest_shapes_are_not (
) {
  let config : SkgConfig = two_source_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let private_child : NodeComplete = node_at ("secret", "private");
  let public_child  : NodeComplete = node_at ("open", "public");
  container . contains = vec! [
    // honest: public member in the public source
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("open") ),
    // THE LEAK: private-homed member recorded in the public source
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("secret") ) ];
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (
      & [ container, private_child, public_child ] );
  let violations : Vec<TelescopeViolation> =
    telescope_violations_of (
      &config, &graph, &ID::new ("container") );
  assert_eq! ( violations . len (), 1, "{:?}", violations );
  assert! ( matches! (
    & violations [0],
    TelescopeViolation::LeakShapedMember { member, member_home, .. }
      if member . 0 == "secret" && member_home . 0 == "private" ));
}

#[test]
fn private_membership_of_a_public_member_is_fine (
) { // the private-reading-list shape: MORE private than the target
  let config : SkgConfig = two_source_config ();
  let mut container : NodeComplete = node_at ("container", "private");
  let public_child  : NodeComplete = node_at ("open", "public");
  container . contains = vec! [
    MemberAtSource::at_source ( SourceName::from ("private"),
                          ID::new ("open") ) ];
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (
      & [ container, public_child ] );
  assert! ( telescope_violations_of (
    &config, &graph, &ID::new ("container") ) . is_empty () );
}

#[test]
fn leak_check_resolves_extra_ids (
) { // an edge naming a merged-away extra id judges the OWNER's home
  let config : SkgConfig = two_source_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let mut private_child : NodeComplete = node_at ("secret", "private");
  private_child . extra_ids = vec! [ ID::new ("old-name") ];
  container . contains = vec! [
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("old-name") ) ];
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (
      & [ container, private_child ] );
  let violations : Vec<TelescopeViolation> =
    telescope_violations_of (
      &config, &graph, &ID::new ("container") );
  assert_eq! ( violations . len (), 1, "{:?}", violations );
}

#[test]
fn unconfigured_source_and_msv_relations_are_covered (
) {
  let config : SkgConfig = two_source_config ();
  let mut node : NodeComplete = node_at ("n", "public");
  let target : NodeComplete = node_at ("t", "private");
  node . subscribes_to = MSV::Specified ( vec! [
    // leak via a non-contains relation
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("t") ) ] );
  node . hides_from_its_subscriptions = MSV::Specified (
    members_at_source ( & SourceName::from ("nonexistent-source"),
                    vec! [ ID::new ("t") ] ));
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes ( & [ node, target ] );
  let violations : Vec<TelescopeViolation> =
    validate_all_telescopes (&config, &graph)
    . into_iter () . map ( |(_, v)| v ) . collect ();
  assert_eq! ( violations . len (), 2, "{:?}", violations );
  assert! ( violations . iter () . any ( |v| matches! (
    v, TelescopeViolation::LeakShapedMember {
      relation : "subscribes_to", .. } )));
  assert! ( violations . iter () . any ( |v| matches! (
    v, TelescopeViolation::UnconfiguredSource { .. } )));
}

#[test]
fn absent_targets_use_owner_home_for_all_four_relationships () {
  let config : SkgConfig = two_source_config ();
  let member : MemberAtSource<ID> = MemberAtSource::at_source (
    SourceName::from ("public"), ID::from ("absent"));
  let mut owner : NodeComplete = node_at ("owner", "private");
  owner . contains = vec![member . clone ()];
  owner . subscribes_to = MSV::Specified (vec![member . clone ()]);
  owner . hides_from_its_subscriptions =
    MSV::Specified (vec![member . clone ()]);
  owner . overrides_view_of = MSV::Specified (vec![member]);
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
  let violations : Vec<TelescopeViolation> = telescope_violations_of (
    &config, &graph, &ID::from ("owner"));
  assert_eq! (violations . len (), 4, "{violations:?}");
  for relation in [
    "contains", "subscribes_to", "hides_from_its_subscriptions",
    "overrides_view_of",
  ] {
    assert! (violations . iter () . any (|violation| matches! (
      violation,
      TelescopeViolation::AbsentTargetLeakShapedMember {
        relation : actual, owner_home, ..
      } if actual == &relation && owner_home == &SourceName::from ("private")))); }
  assert! (violations [0] . to_string () . contains (
    "target is absent, privacy is judged against the extant owner's home"));
}

#[test]
fn absent_target_at_or_below_owner_home_is_not_a_warning () {
  let config : SkgConfig = two_source_config ();
  for (owner_home, edge_source) in [
    ("private", "private"),
    ("public", "private"),
  ] {
    let mut owner : NodeComplete = node_at ("owner", owner_home);
    owner . contains = vec![MemberAtSource::at_source (
      SourceName::from (edge_source), ID::from ("absent"))];
    let graph : InRustGraph = InRustGraph::from_nodecompletes (&[owner]);
    assert! (telescope_violations_of (
      &config, &graph, &ID::from ("owner")) . is_empty ()); }
}

fn owner_with_relation (
  relation    : usize,
  owner_home  : &str,
  edge_source : &str,
  target      : &str,
) -> NodeComplete {
  let mut owner : NodeComplete = node_at ("owner", owner_home);
  let members : Vec<MemberAtSource<ID>> = vec![MemberAtSource::at_source (
    SourceName::from (edge_source), ID::from (target))];
  match relation {
    0 => owner . contains = members,
    1 => owner . subscribes_to = MSV::Specified (members),
    2 => owner . hides_from_its_subscriptions = MSV::Specified (members),
    3 => owner . overrides_view_of = MSV::Specified (members),
    _ => unreachable! (),
  }
  owner
}

fn warnings_by_owner (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> BTreeMap<ID, Vec<TelescopeViolation>> {
  let mut result : BTreeMap<ID, Vec<TelescopeViolation>> = BTreeMap::new ();
  for (owner, warning) in validate_all_telescopes (config, graph) {
    result . entry (owner) . or_default () . push (warning); }
  result
}

#[test]
fn affected_owner_derivation_covers_warning_changes_exhaustively () {
  let config : SkgConfig = two_source_config ();
  for relation in 0..4 {
    for action in 0..5 {
      for (owner_home, edge_source) in [
        ("private", "public"), ("private", "private"),
        ("public", "private"),
      ] {
        let raw_target : &str = match action {
          0 | 3 => "X",
          1 | 2 => "T",
          _     => "N2",
        };
        let owner : NodeComplete = owner_with_relation (
          relation, owner_home, edge_source, raw_target);
        let mut target : NodeComplete = node_at ("T", "public");
        target . extra_ids = vec![ID::from ("E")];
        let mut base_nodes : Vec<NodeComplete> = vec![owner, target . clone ()];
        let definitions : Vec<DefineNode> = match action {
          0 => vec![DefineNode::Save (SaveNode (node_at ("X", "public")))],
          1 => vec![DefineNode::Delete (DeleteNode {
            id : ID::from ("T"), source : SourceName::from ("public"),
          })],
          2 => vec![DefineNode::Save (SaveNode (node_at ("T", "private")))],
          3 => {
            target . extra_ids . push (ID::from ("X"));
            vec![DefineNode::Save (SaveNode (target))]
          },
          _ => {
            base_nodes . push (node_at ("N1", "public"));
            base_nodes . push (node_at ("N2", "private"));
            let mut acquirer : NodeComplete = node_at ("N1", "public");
            acquirer . extra_ids = vec![ID::from ("N2")];
            vec![
              DefineNode::Save (SaveNode (acquirer)),
              DefineNode::Delete (DeleteNode {
                id : ID::from ("N2"), source : SourceName::from ("private"),
              }),
            ]
          },
        };
        let base : InRustGraph = InRustGraph::from_nodecompletes (&base_nodes);
        let mut candidate : InRustGraph = base . clone ();
        apply_definenodes_to_inRustGraph (&mut candidate, &definitions);
        let saved_pids : HashSet<ID> = definitions . iter ()
          .filter_map (|definition| match definition {
            DefineNode::Save (SaveNode (node)) => Some (node . pid . clone ()),
            DefineNode::Delete (_) => None,
          }) . collect ();
        let touched_pids : HashSet<ID> = definitions . iter ()
          .map (|definition| match definition {
            DefineNode::Save (SaveNode (node)) => node . pid . clone (),
            DefineNode::Delete (DeleteNode { id, .. }) => id . clone (),
          }) . collect ();
        let mut affected_ids : HashSet<ID> = touched_pids . clone ();
        for pid in &touched_pids {
          if let Some (node) = base . nodes . get (pid) {
            affected_ids . extend (node . extra_ids . iter () . cloned ()); }
          if let Some (node) = candidate . nodes . get (pid) {
            affected_ids . extend (node . extra_ids . iter () . cloned ()); }}
        let affected_owners : HashSet<ID> = derive_affected_telescope_owners (
          &base, &candidate, &saved_pids, &affected_ids);
        let before : BTreeMap<ID, Vec<TelescopeViolation>> =
          warnings_by_owner (&config, &base);
        let after : BTreeMap<ID, Vec<TelescopeViolation>> =
          warnings_by_owner (&config, &candidate);
        let all_owners : HashSet<ID> = before . keys () . chain (after . keys ())
          .cloned () . collect ();
        for owner in all_owners {
          if before . get (&owner) != after . get (&owner) {
            assert! (affected_owners . contains (&owner),
              "missed owner {owner}; relation={relation}, action={action}"); }}
        let expected : Vec<(ID, TelescopeViolation)> =
          validate_all_telescopes (&config, &candidate) . into_iter ()
            .filter (|(owner, _)| affected_owners . contains (owner))
            .collect ();
        assert_eq! (
          affected_telescope_warnings (
            &config, &base, &candidate, &saved_pids, &affected_ids),
          expected);
      }} }
}

#[test]
fn combined_warning_scope_reports_only_the_final_merge_state () {
  let config : SkgConfig = two_source_config ();
  let owner : NodeComplete = owner_with_relation (
    0, "public", "public", "X");
  let destination : NodeComplete = node_at ("Y", "public");
  let base : InRustGraph =
    InRustGraph::from_nodecompletes (&[owner, destination]);
  let ordinary : Vec<DefineNode> = vec![DefineNode::Save (SaveNode (
    node_at ("X", "private")))];
  let mut intermediate : InRustGraph = base . clone ();
  apply_definenodes_to_inRustGraph (&mut intermediate, &ordinary);
  assert! (! telescope_violations_of (
    &config, &intermediate, &ID::from ("owner")) . is_empty ());

  let mut merged_destination : NodeComplete = node_at ("Y", "public");
  merged_destination . extra_ids = vec![ID::from ("X")];
  let merge : Vec<DefineNode> = vec![
    DefineNode::Save (SaveNode (merged_destination)),
    DefineNode::Delete (DeleteNode {
      id : ID::from ("X"), source : SourceName::from ("private"),
    }),
  ];
  let mut final_graph : InRustGraph = intermediate . clone ();
  apply_definenodes_to_inRustGraph (&mut final_graph, &merge);
  let saved_pids : HashSet<ID> = [ID::from ("X"), ID::from ("Y")]
    . into_iter () . collect ();
  let affected_ids : HashSet<ID> = [ID::from ("X"), ID::from ("Y")]
    . into_iter () . collect ();
  assert! (affected_telescope_warnings (
    &config, &base, &final_graph, &saved_pids, &affected_ids) . is_empty ());
}
