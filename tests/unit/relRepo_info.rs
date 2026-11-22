//! Unit tests for the `relRepo info` endpoint's core
//! ('relRepo_info'; 'docs/api-and-formats.org', "relRepo info"): one
//! relationship's (default, current) relRepos, as served to the
//! client's 'skg-set-relRepo' menu.

use super::{relRepo_info, relation_from_client_string};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::test_utils::{graphnode_at, owned_skgrepos_config_in_order};
use crate::types::config::SkgConfig;
use crate::types::misc::{ID, RelPartner, SkgrepoName};
use crate::types::nodes::complete::Graphnode;

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
    owned_skgrepos_config_in_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = graphnode_at ("child", "trusted");
  let mut recorder : Graphnode = graphnode_at ("recorder", "public");
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
    owned_skgrepos_config_in_order ( & ["public", "private"] );
  let child : Graphnode = graphnode_at ("child", "private");
  let recorder : Graphnode = graphnode_at ("recorder", "public");
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
    owned_skgrepos_config_in_order ( & ["public"] );
  let recorder : Graphnode = graphnode_at ("recorder", "public");
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
  let config : SkgConfig = owned_skgrepos_config_in_order ( & ["public", "private"] );
  let mut recorder : Graphnode = graphnode_at ("recorder", "public");
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
  assert! ( relation_from_client_string ("forks") . is_ok () );
  assert! ( relation_from_client_string ("subscribesTo") . is_err (),
    "a retired relation name" );
  assert! ( relation_from_client_string (
    "hides") . is_err (),
    "hides have no explicit-repo path" );
  assert! ( relation_from_client_string ("linksTo") . is_err () );
}

#[test]
fn owned_recorder_with_foreign_member_defaults_to_recorder_home (
) {
  for (recorder_home, member_home) in
      [("public", "private"), ("private", "public")] {
    let mut config : SkgConfig =
      owned_skgrepos_config_in_order ( & ["public", "private"] );
    config . skgrepos . get_mut (&SkgrepoName::from (member_home))
      . unwrap () . owned = false;
    config . skgrepos . get_mut (&SkgrepoName::from (recorder_home))
      . unwrap () . owned = true;
    let recorder  : Graphnode = graphnode_at ("recorder", recorder_home);
    let member : Graphnode = graphnode_at ("member", member_home);
    let graph : InRustGraph =
      InRustGraph::from_graphnodes (&[recorder, member]);
    let (default, current) = relRepo_info (
      &graph, &config, &ID::new ("recorder"), &ID::new ("member"),
      NodeRelation::Contains ) . unwrap ();
    assert_eq! (default, SkgrepoName::from (recorder_home));
    assert_eq! (current, None); }
}
