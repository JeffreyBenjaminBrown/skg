//! Unit tests for the telescope invariant validator: the leak shape
//! (an edge more public than its target's home) is caught; honest
//! shapes and unknown targets are not; extra-id anchors resolve.

use super::{TelescopeViolation, telescope_violations_of, validate_all_telescopes};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{
  ID, MSV, PrivaciedMember, SkgConfig, SkgfileSource, SourceName,
  privacied_all};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

/// public < private, per source_order (dummy configs otherwise fall
/// back to ALPHABETICAL order, where "private" < "public" would
/// invert the ladder).
fn two_level_config () -> SkgConfig {
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
  let config : SkgConfig = two_level_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let private_child : NodeComplete = node_at ("secret", "private");
  let public_child  : NodeComplete = node_at ("open", "public");
  container . contains = vec! [
    // honest: public member at public level
    PrivaciedMember::at ( SourceName::from ("public"),
                          ID::new ("open") ),
    // THE LEAK: private-homed member recorded at public level
    PrivaciedMember::at ( SourceName::from ("public"),
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
  let config : SkgConfig = two_level_config ();
  let mut container : NodeComplete = node_at ("container", "private");
  let public_child  : NodeComplete = node_at ("open", "public");
  container . contains = vec! [
    PrivaciedMember::at ( SourceName::from ("private"),
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
  let config : SkgConfig = two_level_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let mut private_child : NodeComplete = node_at ("secret", "private");
  private_child . extra_ids = vec! [ ID::new ("old-name") ];
  container . contains = vec! [
    PrivaciedMember::at ( SourceName::from ("public"),
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
fn unconfigured_level_and_msv_relations_are_covered (
) {
  let config : SkgConfig = two_level_config ();
  let mut node : NodeComplete = node_at ("n", "public");
  let target : NodeComplete = node_at ("t", "private");
  node . subscribes_to = MSV::Specified ( vec! [
    // leak via a non-contains relation
    PrivaciedMember::at ( SourceName::from ("public"),
                          ID::new ("t") ) ] );
  node . hides_from_its_subscriptions = MSV::Specified (
    privacied_all ( & SourceName::from ("nonexistent-source"),
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
    v, TelescopeViolation::UnconfiguredLevel { .. } )));
}
