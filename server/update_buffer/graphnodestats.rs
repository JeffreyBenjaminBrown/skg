use crate::dbs::in_rust_graph::stats::{
  fetch_all_graphnodestats,
  fetch_all_graphnodestats_with_skgrepo_set,
  graphnodestats_for_pid,
  AllGraphnodeStats};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::to_org::util::skgids_that_can_have_graphnodestats;
use crate::types::misc::{ID, SkgConfig};
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::nodes::complete::Graphnode;
use crate::types::viewnode::{GraphnodeStats, Viewnode, ViewnodeKind};
use crate::types::viewnode::{Vognode, Phantom};

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::{NodeId, Tree};

/// Enrich all nodes in a viewforest with statistics from the captured graph.
/// Also fetches and returns the containment maps, which callers
/// can pass to `set_viewnodestats_in_viewforest`.
pub fn set_graphnodestats_in_viewforest (
  viewforest : &mut Tree<Viewnode>,
  graph : &InRustGraph,
  config : &SkgConfig,
) -> Result < ( HashMap < ID, HashSet < ID > >,
               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  set_graphnodestats_in_viewforest_inner (
    viewforest, graph, config, None ) }

pub fn set_graphnodestats_in_viewforest_with_skgrepo_set (
  viewforest : &mut Tree<Viewnode>,
  graph : &InRustGraph,
  config : &SkgConfig,
  restriction : &SkgrepoRestriction,
) -> Result < ( HashMap < ID, HashSet < ID > >,
	               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  set_graphnodestats_in_viewforest_inner (
    viewforest, graph, config, Some (restriction) ) }

fn set_graphnodestats_in_viewforest_inner (
  viewforest : &mut Tree<Viewnode>,
  graph : &InRustGraph,
  config : &SkgConfig,
  restriction : Option<&SkgrepoRestriction>,
) -> Result < ( HashMap < ID, HashSet < ID > >,
	               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  let pids : Vec < ID > =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "ids_that_can_have_graphnodestats" ). entered();
      skgids_that_can_have_graphnodestats (viewforest) };
  let stats : AllGraphnodeStats =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "fetch_all_graphnodestats" ). entered();
      match restriction {
        Some (restriction) =>
          fetch_all_graphnodestats_with_skgrepo_set (
            graph, & pids, Some (restriction) ),
        None =>
          fetch_all_graphnodestats (
            graph, & pids ),
      }} ?;
  let root_treeid : NodeId = viewforest . root () . id ();
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "set_graphnodestats_recursive" ). entered();
    set_metadata_relationships_in_node_recursive (
      viewforest,
      root_treeid,
      graph,
      & stats,
      config ) };
  Ok (( stats . container_to_contents,
        stats . content_to_containers )) }

pub fn set_metadata_relationships_in_node_recursive (
  tree   : &mut Tree<Viewnode>,
  treeid : NodeId,
  graph  : &InRustGraph,
  stats  : &AllGraphnodeStats,
  config : &SkgConfig,
) {
  let new_stats : Option < GraphnodeStats > =
    { // Phantoms keep graphStats: these are node-global
      // decorations for the ID, not parent/content facts. Missing
      // current data still falls back to false rather than querying
      // historical graph context for a placeholder.
      match & tree . get (treeid) . unwrap () . value () . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => { let graphnode_opt : Option<Graphnode>
                 = graphnode_graphFirst_by_pid_and_skgrepo (
                     graph, config, &t . skgid, &t . home_skgrepo
                   ). ok ();
               Some ( graphnodestats_for_pid (
                 &t . skgid, stats, graphnode_opt . as_ref () )) },
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))
          => { let graphnode_opt : Option<Graphnode>
                 = graphnode_graphFirst_by_pid_and_skgrepo (
                     graph, config, &p . skgid, &p . home_skgrepo
                   ). ok ();
               Some ( graphnodestats_for_pid (
                 &p . skgid, stats, graphnode_opt . as_ref () )) },
        _ => None }};
  match new_stats {
    Some (gs) =>
      // Keep writeback aligned with the read arm above: both normal
      // vognodes and phantoms can display node-explicit graphStats.
      match &mut tree . get_mut (treeid)
        . unwrap () . value () . kind
        { ViewnodeKind::Vognode (Vognode::Unrestricted (t))
          => { t . graphStats = gs; },
        ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Diff (p)))
          => { p . graphStats = gs; },
        _ => {} },
    None => {} }
  let child_treeids : Vec < NodeId > =
    tree . get (treeid) . unwrap ()
    . children () . map ( | c | c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_metadata_relationships_in_node_recursive (
      tree,
      child_treeid,
      graph,
      stats,
      config ); } }
