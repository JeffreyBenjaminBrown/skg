//! Unit tests for the `relRepo info` endpoint's core
//! ('relRepo_info', BUG-and-fix_make-edge-more-public.org): one
//! relationship's (default, current) relRepos, as served to the
//! client's 'skg-set-relRepo' menu.

use super::{relRepo_info, relation_from_client_string};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::misc::{
  ID, RelPartner, SkgConfig, Skgrepo, SkgrepoName};
use crate::types::nodes::complete::{Graphnode, empty_graphnode};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut skgrepos : HashMap<SkgrepoName, Skgrepo> =
    HashMap::new ();
  for name in names {
    skgrepos . insert (
      SkgrepoName::from (*name),
      Skgrepo {
        name         : SkgrepoName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        owned        : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromSkgrepos (skgrepos);
  config . skgrepo_order =
    names . iter () . map ( |n| SkgrepoName::from (*n) ) . collect ();
  config }

fn node_at (
  pid     : &str,
  skgrepo : &str,
) -> Graphnode {
  let mut n : Graphnode = empty_graphnode ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . home_skgrepo = SkgrepoName::from (skgrepo);
  n }

fn pm (
  skgrepo : &str,
  member : &str,
) -> RelPartner<ID> {
  RelPartner::at_relRepo (
    SkgrepoName::from (skgrepo), ID::new (member) ) }

#[test]
fn default_is_more_private_of_homes_and_current_is_the_relRepo (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "trusted");
  let mut recorder : Graphnode = node_at ("recorder", "public");
  recorder . contains = vec! [
    pm ("private", "child") ]; // recorded above its default
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ recorder, child ] );
  let (default, current) =
    relRepo_info (
      &graph, &config,
      & ID::new ("recorder"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, SkgrepoName::from ("trusted"),
               "default = more private of the endpoints' homes" );
  assert_eq! ( current, Some ( SkgrepoName::from ("private") ),
               "current = the repo the graph records" );
}

#[test]
fn current_is_none_for_an_unrecorded_relationship (
) {
  // E.g. a relationship just typed into a buffer and not yet saved: the
  // default is still computable from the homes.
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : Graphnode = node_at ("child", "private");
  let recorder : Graphnode = node_at ("recorder", "public");
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ recorder, child ] );
  let (default, current) =
    relRepo_info (
      &graph, &config,
      & ID::new ("recorder"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, SkgrepoName::from ("private") );
  assert_eq! ( current, None );
}

#[test]
fn unknown_recorder_is_an_error_but_unknown_member_uses_recorder_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public"] );
  let recorder : Graphnode = node_at ("recorder", "public");
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ recorder ] );
  assert! ( relRepo_info (
    &graph, &config,
    & ID::new ("ghost"), & ID::new ("recorder"),
    NodeRelation::Contains ) . is_err (),
    "unknown recorder" );
  let (default, current) = relRepo_info (
    &graph, &config,
    & ID::new ("recorder"), & ID::new ("ghost"),
    NodeRelation::Contains ) . unwrap ();
  assert_eq! (default, SkgrepoName::from ("public"));
  assert_eq! (current, None);
}

#[test]
fn raw_unresolved_member_keeps_its_exact_relRepo (
) {
  let config : SkgConfig = config_with_order ( & ["public", "private"] );
  let mut recorder : Graphnode = node_at ("recorder", "public");
  recorder . contains = vec! [ pm ("private", "absent-raw") ];
  let graph : InRustGraph = InRustGraph::from_graphnodes ( & [recorder] );
  let (default, current) = relRepo_info (
    &graph, &config, &ID::new ("recorder"), &ID::new ("absent-raw"),
    NodeRelation::Contains ) . unwrap ();
  assert_eq! (default, SkgrepoName::from ("public"));
  assert_eq! (current, Some (SkgrepoName::from ("private")));
}

#[test]
fn only_atom_bearing_relations_are_accepted (
) {
  assert! ( relation_from_client_string ("contains") . is_ok () );
  assert! ( relation_from_client_string ("subscribesTo") . is_ok () );
  assert! ( relation_from_client_string ("overrides") . is_ok () );
  assert! ( relation_from_client_string (
    "hidesFromSubs") . is_err (),
    "hides have no explicit-repo path" );
  assert! ( relation_from_client_string ("linksTo") . is_err () );
}

#[test]
fn owned_recorder_with_foreign_member_defaults_to_recorder_home (
) {
  for (recorder_home, member_home) in
      [("public", "private"), ("private", "public")] {
    let mut config : SkgConfig =
      config_with_order ( & ["public", "private"] );
    config . skgrepos . get_mut (&SkgrepoName::from (member_home))
      . unwrap () . owned = false;
    config . skgrepos . get_mut (&SkgrepoName::from (recorder_home))
      . unwrap () . owned = true;
    let recorder  : Graphnode = node_at ("recorder", recorder_home);
    let member : Graphnode = node_at ("member", member_home);
    let graph : InRustGraph =
      InRustGraph::from_graphnodes (&[recorder, member]);
    let (default, current) = relRepo_info (
      &graph, &config, &ID::new ("recorder"), &ID::new ("member"),
      NodeRelation::Contains ) . unwrap ();
    assert_eq! (default, SkgrepoName::from (recorder_home));
    assert_eq! (current, None); }
}
