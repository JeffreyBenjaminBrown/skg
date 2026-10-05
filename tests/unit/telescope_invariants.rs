//! Unit tests for the telescope invariant validator: the leak shape
//! (a relationship more public than its target's home) is caught; honest
//! shapes are not; absent targets use their extant recorder's home; extra-id
//! anchors resolve.

use super::{TelescopeViolation, affected_telescope_warnings,
            derive_affected_telescope_recorders, telescope_violations_of,
            validate_all_telescopes};
use crate::dbs::in_rust_graph::{InRustGraph, apply_nodeInstructions_to_inRustGraph};
use crate::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgRepo, SkgRepoName,
  rel_partners_at_relRepo};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};
use crate::types::save::{NodeInstruction, DeleteNode, SaveNode};

use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::PathBuf;

/// public < private, per repo_order (dummy configs otherwise fall
/// back to ALPHABETICAL order, where "private" < "public" would
/// invert the ladder).
fn two_skgrepo_config () -> SkgConfig {
  let mut skgrepos : HashMap<SkgRepoName, SkgRepo> =
    HashMap::new ();
  for name in ["public", "private"] {
    skgrepos . insert (
      SkgRepoName::from (name),
      SkgRepo {
        name         : SkgRepoName::from (name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        owned        : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgRepos (skgrepos);
  config . skgrepo_order = vec! [
    SkgRepoName::from ("public"),
    SkgRepoName::from ("private") ];
  config }

fn node_at (
  pid     : &str,
  skgrepo : &str,
) -> Graphnode {
  let mut n : Graphnode = empty_graphnode ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . home_skgrepo = SkgRepoName::from (skgrepo);
  n }

#[test]
fn leak_shaped_member_is_caught_and_honest_shapes_are_not (
) {
  let config        : SkgConfig = two_skgrepo_config ();
  let mut container : Graphnode = node_at ("container", "public");
  let private_child : Graphnode = node_at ("secret", "private");
  let public_child  : Graphnode = node_at ("open", "public");
  container . contains = vec! [
    // honest: public member in the public skgrepo
    RelPartner::at_relRepo ( SkgRepoName::from ("public"),
                          ID::new ("open") ),
    // THE LEAK: private-homed member recorded in the public skgrepo
    RelPartner::at_relRepo ( SkgRepoName::from ("public"),
                          ID::new ("secret") ) ];
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (
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
  let config        : SkgConfig = two_skgrepo_config ();
  let mut container : Graphnode = node_at ("container", "private");
  let public_child  : Graphnode = node_at ("open", "public");
  container . contains = vec! [
    RelPartner::at_relRepo ( SkgRepoName::from ("private"),
                          ID::new ("open") ) ];
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (
      & [ container, public_child ] );
  assert! ( telescope_violations_of (
    &config, &graph, &ID::new ("container") ) . is_empty () );
}

#[test]
fn leak_check_resolves_extra_ids (
) { // a relationship naming a merged-away extra id judges the RECORDER's home
  let config : SkgConfig = two_skgrepo_config ();
  let mut container : Graphnode = node_at ("container", "public");
  let mut private_child : Graphnode = node_at ("secret", "private");
  private_child . extra_ids = vec! [ ID::new ("old-name") ];
  container . contains = vec! [
    RelPartner::at_relRepo ( SkgRepoName::from ("public"),
                          ID::new ("old-name") ) ];
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (
      & [ container, private_child ] );
  let violations : Vec<TelescopeViolation> =
    telescope_violations_of (
      &config, &graph, &ID::new ("container") );
  assert_eq! ( violations . len (), 1, "{:?}", violations );
}

#[test]
fn unconfigured_skgrepo_and_msv_relations_are_covered (
) {
  let config : SkgConfig = two_skgrepo_config ();
  let mut node : Graphnode = node_at ("n", "public");
  let target : Graphnode = node_at ("t", "private");
  node . subscribes_to = MSV::Specified ( vec! [
    // leak via a non-contains relation
    RelPartner::at_relRepo ( SkgRepoName::from ("public"),
                          ID::new ("t") ) ] );
  node . hides_from_its_subscriptions = MSV::Specified (
    rel_partners_at_relRepo ( & SkgRepoName::from ("nonexistent-repo"),
                    vec! [ ID::new ("t") ] ));
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ node, target ] );
  let violations : Vec<TelescopeViolation> =
    validate_all_telescopes (&config, &graph)
    . into_iter () . map ( |(_, v)| v ) . collect ();
  assert_eq! ( violations . len (), 2, "{:?}", violations );
  assert! ( violations . iter () . any ( |v| matches! (
    v, TelescopeViolation::LeakShapedMember {
      relation : "subscribes_to", .. } )));
  assert! ( violations . iter () . any ( |v| matches! (
    v, TelescopeViolation::UnconfiguredRelRepo { .. } )));
}

#[test]
fn absent_targets_use_recorder_home_for_all_four_relationships () {
  let config : SkgConfig = two_skgrepo_config ();
  let member : RelPartner<ID> = RelPartner::at_relRepo (
    SkgRepoName::from ("public"), ID::from ("absent"));
  let mut recorder : Graphnode = node_at ("recorder", "private");
  recorder . contains = vec![member . clone ()];
  recorder . subscribes_to = MSV::Specified (vec![member . clone ()]);
  recorder . hides_from_its_subscriptions =
    MSV::Specified (vec![member . clone ()]);
  recorder . overrides_view_of = MSV::Specified (vec![member]);
  let graph      : InRustGraph = InRustGraph::from_graphnodes (&[recorder]);
  let violations : Vec<TelescopeViolation> = telescope_violations_of (
    &config, &graph, &ID::from ("recorder"));
  assert_eq! (violations . len (), 4, "{violations:?}");
  for relation in [
    "contains", "subscribes_to", "hides_from_its_subscriptions",
    "overrides_view_of",
  ] {
    assert! (violations . iter () . any (|violation| matches! (
      violation,
      TelescopeViolation::AbsentTargetLeakShapedMember {
        relation : actual, recorder_home, ..
      } if actual == &relation && recorder_home == &SkgRepoName::from ("private")))); }
  assert! (violations [0] . to_string () . contains (
    "target is absent, privacy is judged against the extant recorder's home"));
}

#[test]
fn absent_target_at_or_below_recorder_home_is_not_a_warning () {
  let config : SkgConfig = two_skgrepo_config ();
  for (recorder_home, relRepo) in [
    ("private", "private"),
    ("public", "private"),
  ] {
    let mut recorder : Graphnode = node_at ("recorder", recorder_home);
    recorder . contains = vec![RelPartner::at_relRepo (
      SkgRepoName::from (relRepo), ID::from ("absent"))];
    let graph : InRustGraph = InRustGraph::from_graphnodes (&[recorder]);
    assert! (telescope_violations_of (
      &config, &graph, &ID::from ("recorder")) . is_empty ()); }
}

fn recorder_with_relation (
  relation    : usize,
  recorder_home : &str,
  relRepo : &str,
  target      : &str,
) -> Graphnode {
  let mut recorder : Graphnode = node_at ("recorder", recorder_home);
  let members : Vec<RelPartner<ID>> = vec![RelPartner::at_relRepo (
    SkgRepoName::from (relRepo), ID::from (target))];
  match relation {
    0 => recorder . contains = members,
    1 => recorder . subscribes_to = MSV::Specified (members),
    2 => recorder . hides_from_its_subscriptions = MSV::Specified (members),
    3 => recorder . overrides_view_of = MSV::Specified (members),
    _ => unreachable! (),
  }
  recorder
}

fn warnings_by_recorder (
  config : &SkgConfig,
  graph  : &InRustGraph,
) -> BTreeMap<ID, Vec<TelescopeViolation>> {
  let mut result : BTreeMap<ID, Vec<TelescopeViolation>> = BTreeMap::new ();
  for (recorder, warning) in validate_all_telescopes (config, graph) {
    result . entry (recorder) . or_default () . push (warning); }
  result
}

#[test]
fn affected_recorder_derivation_covers_warning_changes_exhaustively () {
  let config : SkgConfig = two_skgrepo_config ();
  for relation in 0..4 {
    for action in 0..5 {
      for (recorder_home, relRepo) in [
        ("private", "public"), ("private", "private"),
        ("public", "private"),
      ] {
        let raw_target : &str = match action {
          0 | 3 => "X",
          1 | 2 => "T",
          _     => "N2",
        };
        let recorder : Graphnode = recorder_with_relation (
          relation, recorder_home, relRepo, raw_target);
        let mut target : Graphnode = node_at ("T", "public");
        target . extra_ids = vec![ID::from ("E")];
        let mut base_nodes : Vec<Graphnode> = vec![recorder, target . clone ()];
        let nodeInstructions    : Vec<NodeInstruction> = match action {
          0 => vec![NodeInstruction::Save (SaveNode (node_at ("X", "public")))],
          1 => vec![NodeInstruction::Delete (DeleteNode {
            skgid : ID::from ("T"), home_skgrepo : SkgRepoName::from ("public"),
          })],
          2 => vec![NodeInstruction::Save (SaveNode (node_at ("T", "private")))],
          3 => {
            target . extra_ids . push (ID::from ("X"));
            vec![NodeInstruction::Save (SaveNode (target))]
          },
          _ => {
            base_nodes . push (node_at ("N1", "public"));
            base_nodes . push (node_at ("N2", "private"));
            let mut acquirer : Graphnode = node_at ("N1", "public");
            acquirer . extra_ids = vec![ID::from ("N2")];
            vec![
              NodeInstruction::Save (SaveNode (acquirer)),
              NodeInstruction::Delete (DeleteNode {
                skgid : ID::from ("N2"), home_skgrepo : SkgRepoName::from ("private"),
              }),
            ]
          },
        };
        let base : InRustGraph = InRustGraph::from_graphnodes (&base_nodes);
        let mut candidate : InRustGraph = base . clone ();
        apply_nodeInstructions_to_inRustGraph (&mut candidate, &nodeInstructions);
        let saved_pids : HashSet<ID> = nodeInstructions . iter ()
          .filter_map (|nodeInstruction| match nodeInstruction {
            NodeInstruction::Save (SaveNode (node)) => Some (node . pid . clone ()),
            NodeInstruction::Delete (_) => None,
          }) . collect ();
        let touched_pids : HashSet<ID> = nodeInstructions . iter ()
          .map (|nodeInstruction| match nodeInstruction {
            NodeInstruction::Save (SaveNode (node)) => node . pid . clone (),
            NodeInstruction::Delete (DeleteNode { skgid, .. }) => skgid . clone (),
          }) . collect ();
        let mut affected_skgids : HashSet<ID> = touched_pids . clone ();
        for pid in &touched_pids {
          if let Some (node) = base . nodes . get (pid) {
            affected_skgids . extend (node . extra_ids . iter () . cloned ()); }
          if let Some (node) = candidate . nodes . get (pid) {
            affected_skgids . extend (node . extra_ids . iter () . cloned ()); }}
        let affected_recorders : HashSet<ID> = derive_affected_telescope_recorders (
          &base, &candidate, &saved_pids, &affected_skgids);
        let before : BTreeMap<ID, Vec<TelescopeViolation>> =
          warnings_by_recorder (&config, &base);
        let after : BTreeMap<ID, Vec<TelescopeViolation>> =
          warnings_by_recorder (&config, &candidate);
        let all_recorders : HashSet<ID> = before . keys () . chain (after . keys ())
          .cloned () . collect ();
        for recorder in all_recorders {
          if before . get (&recorder) != after . get (&recorder) {
            assert! (affected_recorders . contains (&recorder),
              "missed recorder {recorder}; relation={relation}, action={action}"); }}
        let expected : Vec<(ID, TelescopeViolation)> =
          validate_all_telescopes (&config, &candidate) . into_iter ()
            .filter (|(recorder, _)| affected_recorders . contains (recorder))
            .collect ();
        assert_eq! (
          affected_telescope_warnings (
            &config, &base, &candidate, &saved_pids, &affected_skgids),
          expected);
      }} }
}

#[test]
fn combined_warning_scope_reports_only_the_final_merge_state () {
  let config   : SkgConfig = two_skgrepo_config ();
  let recorder : Graphnode = recorder_with_relation (
    0, "public", "public", "X");
  let destination : Graphnode = node_at ("Y", "public");
  let base : InRustGraph =
    InRustGraph::from_graphnodes (&[recorder, destination]);
  let ordinary : Vec<NodeInstruction> = vec![NodeInstruction::Save (SaveNode (
    node_at ("X", "private")))];
  let mut intermediate : InRustGraph = base . clone ();
  apply_nodeInstructions_to_inRustGraph (&mut intermediate, &ordinary);
  assert! (! telescope_violations_of (
    &config, &intermediate, &ID::from ("recorder")) . is_empty ());

  let mut merged_destination : Graphnode = node_at ("Y", "public");
  merged_destination . extra_ids = vec![ID::from ("X")];
  let merge : Vec<NodeInstruction> = vec![
    NodeInstruction::Save (SaveNode (merged_destination)),
    NodeInstruction::Delete (DeleteNode {
      skgid : ID::from ("X"), home_skgrepo : SkgRepoName::from ("private"),
    }),
  ];
  let mut final_graph : InRustGraph = intermediate . clone ();
  apply_nodeInstructions_to_inRustGraph (&mut final_graph, &merge);
  let saved_pids : HashSet<ID> = [ID::from ("X"), ID::from ("Y")]
    . into_iter () . collect ();
  let affected_skgids : HashSet<ID> = [ID::from ("X"), ID::from ("Y")]
    . into_iter () . collect ();
  assert! (affected_telescope_warnings (
    &config, &base, &final_graph, &saved_pids, &affected_skgids) . is_empty ());
}
