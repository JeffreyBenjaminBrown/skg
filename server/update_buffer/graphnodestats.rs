use crate::dbs::typedb::search::all_graphnodestats::{fetch_all_graphnodestats, AllGraphNodeStats};
use crate::serve::timing_log::{timed, timed_async};
use crate::to_org::util::collect_ids_from_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::memory::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::viewnode::{ViewNode, ViewNodeKind};

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
  // PITFALL: Aliasing and extraIDs are computed in a separate block, because skgnode_from_map_or_disk mutably borrows map, which prevents simultaneously holding a mutable borrow of tree (needed to write fields below).
  let ( aliasing, extra_ids ) : ( bool, bool ) =
    { // PITFALL: Uses 'false' for phantom and deleted nodes. Getting 'true' where appropriate would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      if let ViewNodeKind::True (t)
        = & tree . get (treeid) . unwrap () . value () . kind
        { let skgnode_opt = skgnode_from_map_or_disk (
              &t . id, &t . source, map, config
            ) . ok ();
          let aliasing = skgnode_opt . as_ref ()
            . and_then ( |n| n . aliases . as_ref () )
            . map ( |a| ! a . is_empty () )
            . unwrap_or (false);
          let extra_ids = skgnode_opt . as_ref ()
            . map ( |n| n . ids . len () > 1 )
            . unwrap_or (false);
          ( aliasing, extra_ids ) }
        else { ( false, false ) } // Scaff, Deleted, DeletedScaff
    };
  if let ViewNodeKind::True (t)
    = &mut tree . get_mut (treeid) . unwrap () . value () . kind
    { // Write all graphStats fields.
      // PITFALL: These booleans are not populated for phantom nodes. Populating those would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      let node_pid : &ID = &t . id;
      t . graphStats . aliasing = aliasing;
      t . graphStats . extraIDs = extra_ids;
      t . graphStats . overriding =
        stats . has_overrides . contains (node_pid);
      t . graphStats . subscribing =
        stats . has_subscribes . contains (node_pid);
      t . graphStats . numContainers =
        stats . num_containers . get (node_pid) . copied ();
      t . graphStats . numContents =
        stats . num_contents . get (node_pid) . copied ();
      t . graphStats . numLinksIn =
        stats . num_links_in . get (node_pid) . copied (); }
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
