use crate::dbs::typedb::search::all_graphnodestats::{
  fetch_all_graphnodestats,
  graphnodestats_for_pid,
  AllGraphNodeStats};
use crate::serve::timing_log::{timed, timed_async};
use crate::to_org::util::collect_ids_from_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::memory::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::viewnode::{GraphNodeStats, ViewNode, ViewNodeKind};

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::{NodeId, Tree};
use typedb_driver::TypeDBDriver;

/// Enrich all nodes in a forest with graphStats from TypeDB.
/// Also fetches and returns the containment maps, which callers
/// can pass to `set_viewnodestats_in_forest`.
pub async fn set_graphnodestats_in_forest (
  forest : &mut Tree<ViewNode>,
  map    : &mut SkgNodeMap,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < ( HashMap < ID, HashSet < ID > >,
               HashMap < ID, HashSet < ID > > ),
             Box<dyn Error> > {
  let pids : Vec < ID > =
    timed ( config, "collect_ids_from_tree",
            || collect_ids_from_tree (forest));
  let stats : AllGraphNodeStats =
    timed_async ( config, "fetch_all_graphnodestats",
      fetch_all_graphnodestats (
        & config . db_name, driver, & pids )) . await ?;
  let root_treeid : NodeId = forest . root () . id ();
  timed ( config, "set_graphnodestats_recursive",
          || set_metadata_relationships_in_node_recursive (
               forest,
               root_treeid,
               & stats,
               map,
               config ));
  Ok (( stats . container_to_contents,
        stats . content_to_containers )) }

fn set_metadata_relationships_in_node_recursive (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  stats  : &AllGraphNodeStats,
  map    : &mut SkgNodeMap,
  config : &SkgConfig,
) {
  // PITFALL: Disk lookup (skgnode_from_map_or_disk) mutably borrows map, so it must happen in a separate block from the mutable tree borrow below.
  let new_stats : Option < GraphNodeStats > =
    { // PITFALL: Uses 'false' for phantom and deleted nodes. Getting 'true' where appropriate would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      if let ViewNodeKind::True (t)
        = & tree . get (treeid) . unwrap () . value () . kind
        { let skgnode_opt = skgnode_from_map_or_disk (
              &t . id, &t . source, map, config
            ) . ok ();
          Some ( graphnodestats_for_pid (
            &t . id, stats, skgnode_opt )) }
        else { None } // Scaff, Deleted, DeletedScaff
    };
  if let Some (gs) = new_stats {
    if let ViewNodeKind::True (t)
      = &mut tree . get_mut (treeid) . unwrap () . value () . kind
      { t . graphStats = gs; } }
  let child_treeids : Vec < NodeId > =
    tree . get (treeid) . unwrap ()
    . children () . map ( | c | c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_metadata_relationships_in_node_recursive (
      tree,
      child_treeid,
      stats,
      map,
      config ); } }
