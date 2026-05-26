use crate::dbs::typedb::search::all_graphnodestats::{
  fetch_all_graphnodestats,
  fetch_all_graphnodestats_with_source_set,
  graphnodestats_for_pid,
  AllGraphNodeStats};
use crate::source_sets::ActiveSourceSet;
use crate::to_org::util::collect_ids_from_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::nodes::complete::NodeComplete;
use crate::types::viewnode::{GraphNodeStats, ViewNode, ViewNodeKind};

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
        "collect_ids_from_tree" ). entered();
      collect_ids_from_tree (viewforest) };
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
    { // PITFALL: Uses 'false' for phantom and deleted nodes. Getting 'true' where appropriate would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      match & tree . get (treeid) . unwrap () . value () . kind {
        ViewNodeKind::True (t) => {
          let nodecomplete_opt : Option<NodeComplete> =
            nodecomplete_rustFirst_by_pid_and_source (
              config, &t . id, &t . source
            ). ok ();
          Some ( graphnodestats_for_pid (
            &t . id, stats, nodecomplete_opt . as_ref () )) },
        _ => None } // Scaff, Deleted, DeletedScaff
    };
  match new_stats {
    Some (gs) => match &mut tree . get_mut (treeid)
                         . unwrap () . value () . kind {
      ViewNodeKind::True (t) =>
        { t . graphStats = gs; },
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
