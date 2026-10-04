use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::override_resolution::{
  OverrideResolution,
  resolve_override,
};
use skg::repo_sets::{ActiveRepoSet, RepoSetName};
use skg::types::misc::{
  ID, MSV, RelPartner, SkgConfig, SkgfileRepo, RepoName,
  rel_partners_at_relRepo};
use skg::types::nodes::complete::{Graphnode, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromRepos (HashMap::from ([
    ( RepoName::from ("owned"),
      SkgfileRepo {
        name: RepoName::from ("owned"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned"),
        user_owns_it: true,
      }),
    ( RepoName::from ("owned2"),
      SkgfileRepo {
        name: RepoName::from ("owned2"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned2"),
        user_owns_it: true,
      }),
    ( RepoName::from ("foreign"),
      SkgfileRepo {
        name: RepoName::from ("foreign"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/foreign"),
        user_owns_it: false,
      }),
  ])) }

fn node (
  pid       : &str,
  repo    : &str,
  overrides : &[&str],
) -> Graphnode {
  let mut node : Graphnode =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . home_repo = RepoName::from (repo);
  node . overrides_view_of =
    if overrides . is_empty () {
      MSV::Unspecified
    } else {
      MSV::Specified (
        rel_partners_at_relRepo (
          &node . home_repo,
          overrides . iter ()
          . map ( |id| ID::from (*id) )
          . collect () ) )
    };
  node }

fn restricted_to (
  repos : &[&str],
) -> ActiveRepoSet {
  ActiveRepoSet {
    name    : RepoSetName ( "restricted" . to_string () ),
    repos : repos . iter ()
      . map ( |s| RepoName::from (*s) )
      . collect (),
  }}

fn resolve (
  nodes  : Vec<Graphnode>,
  active : Option<&ActiveRepoSet>,
  id     : &str,
) -> OverrideResolution {
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (&nodes);
  resolve_override (&config (), &graph, active, &ID::from (id)) }

#[test]
fn no_overrider_resolves_to_self () {
  assert_eq! (
    resolve (
      vec![ node ("target", "owned", &[]) ],
      None, "target" ),
    OverrideResolution {
      effective      : ID::from ("target"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn foreign_overriders_are_ignored () {
  assert_eq! (
    resolve (
      vec![
        node ("target", "owned", &[]),
        node ("foreign-overrider", "foreign", &["target"]),
      ],
      None, "target" ),
    OverrideResolution {
      effective      : ID::from ("target"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn a_single_owned_overrider_substitutes () {
  assert_eq! (
    resolve (
      vec![
        node ("target", "foreign", &[]),
        node ("overrider", "owned", &["target"]),
      ],
      None, "target" ),
    OverrideResolution {
      effective      : ID::from ("overrider"),
      path           : vec![ ID::from ("overrider") ],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn an_inactive_owned_overrider_does_not_substitute () {
  let active : ActiveRepoSet =
    // 'owned2' (the overrider's repo) is not in the active set.
    restricted_to ( &["owned", "foreign"] );
  assert_eq! (
    resolve (
      vec![
        node ("target", "owned", &[]),
        node ("overrider", "owned2", &["target"]),
      ],
      Some (&active), "target" ),
    OverrideResolution {
      effective      : ID::from ("target"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn an_active_owned_overrider_substitutes_under_a_restricted_set () {
  let active : ActiveRepoSet =
    restricted_to ( &["owned", "owned2"] );
  assert_eq! (
    resolve (
      vec![
        node ("target", "owned", &[]),
        node ("overrider", "owned2", &["target"]),
      ],
      Some (&active), "target" ),
    OverrideResolution {
      effective      : ID::from ("overrider"),
      path           : vec![ ID::from ("overrider") ],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn an_inactive_override_edge_between_active_nodes_does_not_substitute () {
  let active : ActiveRepoSet =
    restricted_to ( &["owned", "foreign"] );
  let mut overrider : Graphnode =
    node ("overrider", "owned", &[]);
  overrider . overrides_view_of = MSV::Specified (vec![
    RelPartner::at_relRepo (
      RepoName::from ("owned2"), ID::from ("target")) ]);
  assert_eq! (
    resolve (
      vec![ node ("target", "owned", &[]), overrider ],
      Some (&active), "target" ),
    OverrideResolution {
      effective      : ID::from ("target"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn a_chain_of_two_resolves_transitively_with_path () {
  // A user-owned chain X overrides Y overrides Z; resolves to the end
  // of the chain, carrying the full path. Linear chains are legal.
  assert_eq! (
    resolve (
      vec![
        node ("z", "owned", &[]),
        node ("y", "owned", &["z"]),
        node ("x", "owned", &["y"]),
      ],
      None, "z" ),
    OverrideResolution {
      effective      : ID::from ("x"),
      path           : vec![ ID::from ("y"), ID::from ("x") ],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn inactive_overrider_home_stops_a_chain_at_that_edge () {
  let active : ActiveRepoSet =
    // y's repo 'owned2' is inactive; x's repo 'owned' is active.
    restricted_to ( &["owned", "foreign"] );
  assert_eq! (
    resolve (
      vec![
        node ("z", "owned", &[]),
        node ("y", "owned2", &["z"]),
        node ("x", "owned", &["y"]),
      ],
      Some (&active), "z" ),
    OverrideResolution {
      effective      : ID::from ("z"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn a_cycle_is_detected_and_substitutes_nothing () {
  assert_eq! (
    resolve (
      vec![
        node ("a", "owned", &["b"]),
        node ("b", "owned", &["a"]),
      ],
      None, "a" ),
    OverrideResolution {
      effective      : ID::from ("a"),
      path           : vec![],
      cycle_detected : true,
      cycle          : vec![ ID::from ("a"), ID::from ("b") ], } ); }

#[test]
fn extra_id_input_and_extra_id_edge_both_resolve () {
  let mut target : Graphnode =
    node ("target", "foreign", &[]);
  target . extra_ids = vec![ ID::from ("target-extra") ];
  let nodes : Vec<Graphnode> = vec![
    target,
    // The override edge is written to the extra ID.
    node ("overrider", "owned", &["target-extra"]),
  ];
  let graph : InRustGraph =
    InRustGraph::from_graphnodes (&nodes);
  let by_extra_id : OverrideResolution = // input is the extra ID
    resolve_override (
      &config (), &graph, None, &ID::from ("target-extra") );
  assert_eq! (
    by_extra_id,
    OverrideResolution {
      effective      : ID::from ("overrider"),
      path           : vec![ ID::from ("overrider") ],
      cycle_detected : false,
      cycle          : vec![], } );
  let by_pid : OverrideResolution =
    resolve_override (
      &config (), &graph, None, &ID::from ("target") );
  assert_eq! (by_pid, by_extra_id); }

#[test]
fn multiple_owned_overriders_substitute_nothing () {
  // Monogamy-violating data: refuse to choose a branch.
  assert_eq! (
    resolve (
      vec![
        node ("target", "owned", &[]),
        node ("one", "owned", &["target"]),
        node ("two", "owned", &["target"]),
      ],
      None, "target" ),
    OverrideResolution {
      effective      : ID::from ("target"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }

#[test]
fn an_unknown_id_resolves_to_itself () {
  assert_eq! (
    resolve ( vec![], None, "never-heard-of-it" ),
    OverrideResolution {
      effective      : ID::from ("never-heard-of-it"),
      path           : vec![],
      cycle_detected : false,
      cycle          : vec![], } ); }
