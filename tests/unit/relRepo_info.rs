//! Unit tests for the `relRepo info` endpoint's core
//! ('relRepo_info', BUG-and-fix_make-edge-more-public.org): one
//! edge's (default, current) relRepos, as served to the
//! client's 'skg-set-relRepo' menu.

use super::{relRepo_info, relation_from_client_string};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::types::misc::{
  ID, RelPartner, SkgConfig, SkgfileRepo, RepoName};
use crate::types::nodes::complete::{Graphnode, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config_with_order (
  names : &[&str],
) -> SkgConfig {
  let mut repos : HashMap<RepoName, SkgfileRepo> =
    HashMap::new ();
  for name in names {
    repos . insert (
      RepoName::from (*name),
      SkgfileRepo {
        name         : RepoName::from (*name),
        abbreviation : None,
        path         : PathBuf::from ( format! ("owned/{}", name) ),
        user_owns_it : true, } ); }
  let mut config : SkgConfig =
    SkgConfig::dummyFromRepos (repos);
  config . repo_order =
    names . iter () . map ( |n| RepoName::from (*n) ) . collect ();
  config }

fn node_at (
  pid    : &str,
  repo : &str,
) -> Graphnode {
  let mut n : Graphnode = empty_node_complete ();
  n . pid = ID::new (pid);
  n . title = pid . to_string ();
  n . home_repo = RepoName::from (repo);
  n }

fn pm (
  repo : &str,
  member : &str,
) -> RelPartner<ID> {
  RelPartner::at_relRepo (
    RepoName::from (repo), ID::new (member) ) }

#[test]
fn default_is_more_private_of_homes_and_current_is_the_relRepo (
) {
  let config : SkgConfig =
    config_with_order ( & ["public", "trusted", "private"] );
  let child : Graphnode = node_at ("child", "trusted");
  let mut owner : Graphnode = node_at ("owner", "public");
  owner . contains = vec! [
    pm ("private", "child") ]; // recorded above its default
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ owner, child ] );
  let (default, current) =
    relRepo_info (
      &graph, &config,
      & ID::new ("owner"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, RepoName::from ("trusted"),
               "default = more private of the endpoints' homes" );
  assert_eq! ( current, Some ( RepoName::from ("private") ),
               "current = the repo the graph records" );
}

#[test]
fn current_is_none_for_an_unrecorded_edge (
) {
  // E.g. an edge just typed into a buffer and not yet saved: the
  // default is still computable from the homes.
  let config : SkgConfig =
    config_with_order ( & ["public", "private"] );
  let child : Graphnode = node_at ("child", "private");
  let owner : Graphnode = node_at ("owner", "public");
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ owner, child ] );
  let (default, current) =
    relRepo_info (
      &graph, &config,
      & ID::new ("owner"), & ID::new ("child"),
      NodeRelation::Contains ) . unwrap ();
  assert_eq! ( default, RepoName::from ("private") );
  assert_eq! ( current, None );
}

#[test]
fn unknown_owner_is_an_error_but_unknown_member_uses_owner_home (
) {
  let config : SkgConfig =
    config_with_order ( & ["public"] );
  let owner : Graphnode = node_at ("owner", "public");
  let graph : InRustGraph =
    InRustGraph::from_graphnodes ( & [ owner ] );
  assert! ( relRepo_info (
    &graph, &config,
    & ID::new ("ghost"), & ID::new ("owner"),
    NodeRelation::Contains ) . is_err (),
    "unknown owner" );
  let (default, current) = relRepo_info (
    &graph, &config,
    & ID::new ("owner"), & ID::new ("ghost"),
    NodeRelation::Contains ) . unwrap ();
  assert_eq! (default, RepoName::from ("public"));
  assert_eq! (current, None);
}

#[test]
fn raw_unresolved_member_keeps_its_exact_relRepo (
) {
  let config : SkgConfig = config_with_order ( & ["public", "private"] );
  let mut owner : Graphnode = node_at ("owner", "public");
  owner . contains = vec! [ pm ("private", "absent-raw") ];
  let graph : InRustGraph = InRustGraph::from_graphnodes ( & [owner] );
  let (default, current) = relRepo_info (
    &graph, &config, &ID::new ("owner"), &ID::new ("absent-raw"),
    NodeRelation::Contains ) . unwrap ();
  assert_eq! (default, RepoName::from ("public"));
  assert_eq! (current, Some (RepoName::from ("private")));
}

#[test]
fn only_atom_bearing_relations_are_accepted (
) {
  assert! ( relation_from_client_string ("contains") . is_ok () );
  assert! ( relation_from_client_string ("subscribes_to") . is_ok () );
  assert! ( relation_from_client_string ("overrides_view_of") . is_ok () );
  assert! ( relation_from_client_string (
    "hides_from_its_subscriptions") . is_err (),
    "hides have no explicit-repo path" );
  assert! ( relation_from_client_string ("links_to") . is_err () );
}

#[test]
fn owned_owner_with_foreign_member_defaults_to_owner_home (
) {
  for (owner_home, member_home) in
      [("public", "private"), ("private", "public")] {
    let mut config : SkgConfig =
      config_with_order ( & ["public", "private"] );
    config . repos . get_mut (&RepoName::from (member_home))
      . unwrap () . user_owns_it = false;
    config . repos . get_mut (&RepoName::from (owner_home))
      . unwrap () . user_owns_it = true;
    let owner : Graphnode = node_at ("owner", owner_home);
    let member : Graphnode = node_at ("member", member_home);
    let graph : InRustGraph =
      InRustGraph::from_graphnodes (&[owner, member]);
    let (default, current) = relRepo_info (
      &graph, &config, &ID::new ("owner"), &ID::new ("member"),
      NodeRelation::Contains ) . unwrap ();
    assert_eq! (default, RepoName::from (owner_home));
    assert_eq! (current, None); }
}
