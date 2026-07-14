//! Unit tests for the 'edge level info' endpoint's core
//! ('edge_level_info', BUG-and-fix_make-edge-more-public.org): one
//! edge's (default, current) privacy levels, as served to the
//! client's 'skg-set-relationship-source' menu.

use super::{edge_level_info, relation_from_client_string};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::misc::{
  ID, PrivaciedMember, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut sources : HashMap<SourceName, SkgfileSource> =
    HashMap::new ();
  for name in names {
    sources . insert (
      SourceName::from (*name),
      SkgfileSource {
        name         : SourceName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSources (sources);
  config . source_order =
    names . iter () . map ( |n| SourceName::from (*n) ) . collect ();
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

fn pm (
  level  : &str,
  member : &str,
) -> PrivaciedMember<ID> {
  PrivaciedMember::at (
    SourceName::from (level), ID::new (member) ) }

#[test]
fn default_is_more_private_of_homes_and_current_is_the_recorded_level (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : NodeComplete = node_at ("child", "trusted");
  let mut owner : NodeComplete = node_at ("owner", "public");
  owner . contains = vec! [
    pm ("private", "child") ]; // recorded above its default
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes ( & [ owner, child ] );
  let (default, current) =
    edge_level_info (
      &graph, &config,
      & ID::new ("owner"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, SourceName::from ("trusted"),
               "default = more private of the endpoints' homes" );
  assert_eq! ( current, Some ( SourceName::from ("private") ),
               "current = the level the graph records" );
}

#[test]
fn current_is_none_for_an_unrecorded_edge (
) {
  // E.g. an edge just typed into a buffer and not yet saved: the
  // default is still computable from the homes.
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : NodeComplete = node_at ("child", "private");
  let owner : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes ( & [ owner, child ] );
  let (default, current) =
    edge_level_info (
      &graph, &config,
      & ID::new ("owner"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, SourceName::from ("private") );
  assert_eq! ( current, None );
}

#[test]
fn unknown_endpoints_are_errors (
) {
  let config : SkgConfig =
    config_with_order ( & ["public"] );
  let owner : NodeComplete = node_at ("owner", "public");
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes ( & [ owner ] );
  assert! ( edge_level_info (
    &graph, &config,
    & ID::new ("ghost"), & ID::new ("owner"),
    NodeRelation::Contains ) . is_err (),
    "unknown owner" );
  assert! ( edge_level_info (
    &graph, &config,
    & ID::new ("owner"), & ID::new ("ghost"),
    NodeRelation::Contains ) . is_err (),
    "unknown member: the client falls back to the full ladder" );
}

#[test]
fn only_atom_bearing_relations_are_accepted (
) {
  assert! ( relation_from_client_string ("contains") . is_ok () );
  assert! ( relation_from_client_string ("subscribes_to") . is_ok () );
  assert! ( relation_from_client_string ("overrides_view_of") . is_ok () );
  assert! ( relation_from_client_string (
    "hides_from_its_subscriptions") . is_err (),
    "hides have no explicit-level path" );
  assert! ( relation_from_client_string ("textlinks_to") . is_err () );
}
