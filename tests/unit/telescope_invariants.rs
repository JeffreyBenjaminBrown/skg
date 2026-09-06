//! Unit tests for the telescope invariant validator.  Direct relationships
//! may intentionally disclose a later-homed target; hides retain their
//! target-side privacy floor; unknown targets are ignored; extra IDs resolve.

use super::{TelescopeViolation, telescope_violations_of, validate_all_telescopes};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::{
  ID, MSV, MemberAtSource, SkgConfig, SkgfileSource, SourceName,
  members_at_source};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

/// public < private, per the source catalog's declaration order.
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
  config . sources . set_order (vec! [
    SourceName::from ("public"),
    SourceName::from ("private") ]);
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
fn direct_relationships_may_disclose_a_later_homed_target (
) {
  let config : SkgConfig = two_source_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let private_child : NodeComplete = node_at ("secret", "private");
  let public_child  : NodeComplete = node_at ("open", "public");
  container . contains = vec! [
    // honest: public member in the public source
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("open") ),
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("secret") ) ];
  container . subscribes_to = MSV::Specified (vec! [
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("secret") ) ]);
  container . overrides_view_of = MSV::Specified (vec! [
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("secret") ) ]);
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (
      & [ container, private_child, public_child ] );
  assert! ( telescope_violations_of (
    &config, &graph, &ID::new ("container") ) . is_empty () );
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
fn hide_check_resolves_extra_ids (
) { // a hide naming a merged-away extra id judges the OWNER's home
  let config : SkgConfig = two_source_config ();
  let mut container : NodeComplete = node_at ("container", "public");
  let mut private_child : NodeComplete = node_at ("secret", "private");
  private_child . extra_ids = vec! [ ID::new ("old-name") ];
  container . hides_from_its_subscriptions = MSV::Specified (vec! [
    MemberAtSource::at_source ( SourceName::from ("public"),
                          ID::new ("old-name") ) ]);
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
    // A direct disclosure is intentional, even across the source floor.
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
  assert_eq! ( violations . len (), 1, "{:?}", violations );
  assert! ( violations . iter () . any ( |v| matches! (
    v, TelescopeViolation::UnconfiguredSource { .. } )));
}
