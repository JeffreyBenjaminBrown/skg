use skg::dbs::in_rust_graph::InRustGraph;
use skg::dbs::in_rust_graph::override_resolution::{
  OverrideResolution,
  resolve_override,
};
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::types::misc::{ID, MSV, SkgConfig, SkgfileSource, SourceName, privacied_all};
use skg::types::nodes::complete::{NodeComplete, empty_node_complete};

use std::collections::HashMap;
use std::path::PathBuf;

fn config () -> SkgConfig {
  SkgConfig::dummyFromSources (HashMap::from ([
    ( SourceName::from ("owned"),
      SkgfileSource {
        name: SourceName::from ("owned"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned"),
        user_owns_it: true,
      }),
    ( SourceName::from ("owned2"),
      SkgfileSource {
        name: SourceName::from ("owned2"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/owned2"),
        user_owns_it: true,
      }),
    ( SourceName::from ("foreign"),
      SkgfileSource {
        name: SourceName::from ("foreign"),
        abbreviation: None,
        path: PathBuf::from ("/tmp/foreign"),
        user_owns_it: false,
      }),
  ])) }

fn node (
  pid       : &str,
  source    : &str,
  overrides : &[&str],
) -> NodeComplete {
  let mut node : NodeComplete =
    empty_node_complete ();
  node . pid = ID::from (pid);
  node . title = pid . to_string ();
  node . source = SourceName::from (source);
  node . overrides_view_of =
    if overrides . is_empty () {
      MSV::Unspecified
    } else {
      MSV::Specified (
        privacied_all (
          &node . source,
          overrides . iter ()
          . map ( |id| ID::from (*id) )
          . collect () ) )
    };
  node }

fn restricted_to (
  sources : &[&str],
) -> ActiveSourceSet {
  ActiveSourceSet {
    name    : SourceSetName ( "restricted" . to_string () ),
    sources : sources . iter ()
      . map ( |s| SourceName::from (*s) )
      . collect (),
  }}

fn resolve (
  nodes  : Vec<NodeComplete>,
  active : Option<&ActiveSourceSet>,
  id     : &str,
) -> OverrideResolution {
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
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
  let active : ActiveSourceSet =
    // 'owned2' (the overrider's source) is not in the active set.
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
  let active : ActiveSourceSet =
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
fn per_edge_visibility_stops_a_chain_at_an_inactive_middle () {
  let active : ActiveSourceSet =
    // y's source 'owned2' is inactive; x's source 'owned' is active.
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
  let mut target : NodeComplete =
    node ("target", "foreign", &[]);
  target . extra_ids = vec![ ID::from ("target-extra") ];
  let nodes : Vec<NodeComplete> = vec![
    target,
    // The override edge is written to the extra ID.
    node ("overrider", "owned", &["target-extra"]),
  ];
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
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
