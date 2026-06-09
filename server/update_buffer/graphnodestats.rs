use crate::dbs::typedb::search::all_graphnodestats::{
  fetch_all_graphnodestats,
  fetch_all_graphnodestats_with_source_set,
  graphnodestats_for_pid,
  AllGraphNodeStats};
use crate::source_sets::ActiveSourceSet;
use crate::to_org::util::ids_that_can_have_graphnodestats;
use crate::types::misc::{ID, SkgConfig};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{GraphNodeStats, ViewNode, ViewNodeKind};
use crate::types::viewnode::Vognode;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::{NodeId, Tree};
use typedb_driver::TypeDBDriver;

/// Enrich all nodes in a viewforest with graphStats from TypeDB.
/// Also fetches and returns the containment maps, which callers
/// can pass to `set_viewnodestats_in_viewforest`.
pub async fn set_graphnodestats_in_viewforest (
  viewforest : &mut Tree<ViewNode>,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < ( HashMap < ID, HashSet < ID > >,
               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  set_graphnodestats_in_viewforest_inner (
    viewforest, config, driver, None ) . await }

pub async fn set_graphnodestats_in_viewforest_with_source_set (
  viewforest : &mut Tree<ViewNode>,
  config : &SkgConfig,
  driver : &TypeDBDriver,
  active : &ActiveSourceSet,
) -> Result < ( HashMap < ID, HashSet < ID > >,
	               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  set_graphnodestats_in_viewforest_inner (
    viewforest, config, driver, Some (active) ) . await }

async fn set_graphnodestats_in_viewforest_inner (
  viewforest : &mut Tree<ViewNode>,
  config : &SkgConfig,
  driver : &TypeDBDriver,
  active : Option<&ActiveSourceSet>,
) -> Result < ( HashMap < ID, HashSet < ID > >,
	               HashMap < ID, HashSet < ID > > ),
	             Box<dyn Error> > {
  let pids : Vec < ID > =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "ids_that_can_have_graphnodestats" ). entered();
      ids_that_can_have_graphnodestats (viewforest) };
  let stats : AllGraphNodeStats =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "fetch_all_graphnodestats" ). entered();
      match active {
        Some (active) =>
          fetch_all_graphnodestats_with_source_set (
            & config . db_name, driver, & pids, Some (active) ) . await,
        None =>
          fetch_all_graphnodestats (
            & config . db_name, driver, & pids ) . await,
      }} ?;
  let root_treeid : NodeId = viewforest . root () . id ();
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "set_graphnodestats_recursive" ). entered();
    set_metadata_relationships_in_node_recursive (
      viewforest,
      root_treeid,
      & stats,
      config ) };
  Ok (( stats . container_to_contents,
        stats . content_to_containers )) }

pub fn set_metadata_relationships_in_node_recursive (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  stats  : &AllGraphNodeStats,
  config : &SkgConfig,
) {
  let new_stats : Option < GraphNodeStats > =
    { // Phantoms keep graphStats: these are node-global
      // decorations for the ID, not parent/content facts. Missing
      // current data still falls back to false rather than querying
      // historical graph context for a placeholder.
      match & tree . get (treeid) . unwrap () . value () . kind {
        ViewNodeKind::Vognode (Vognode::Normal (t))
          => { let nodecomplete_opt : Option<NodeComplete>
                 = nodecomplete_rustFirst_by_pid_and_source (
                     config, &t . id, &t . source
                   ). ok ();
               Some ( graphnodestats_for_pid (
                 &t . id, stats, nodecomplete_opt . as_ref () )) },
        ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
          => { let nodecomplete_opt : Option<NodeComplete>
                 = nodecomplete_rustFirst_by_pid_and_source (
                     config, &p . id, &p . source
                   ). ok ();
               Some ( graphnodestats_for_pid (
                 &p . id, stats, nodecomplete_opt . as_ref () )) },
        _ => None }};
  match new_stats {
    Some (gs) =>
      // Keep writeback aligned with the read arm above: both normal
      // vognodes and phantoms can display node-global graphStats.
      match &mut tree . get_mut (treeid)
        . unwrap () . value () . kind
        { ViewNodeKind::Vognode (Vognode::Normal (t))
          => { t . graphStats = gs; },
        ViewNodeKind::Vognode (Vognode::DiffPhantom (p))
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
      stats,
      config ); } }
