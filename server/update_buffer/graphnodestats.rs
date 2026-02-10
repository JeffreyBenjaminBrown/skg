use crate::dbs::typedb::search::contains_from_pids::contains_from_pids;
use crate::dbs::typedb::search::count_relationships::{
  count_containers,
  count_contents,
  count_link_sources,
  has_subscribes,
  has_overrides};
use crate::to_org::util::collect_ids_from_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::viewnode::{ViewNode, ViewNodeKind};

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::{NodeId, Tree};
use typedb_driver::TypeDBDriver;

/// Each of these describes some kind of relationship,
/// for each of a view's nodes.
struct MapsFromIdForView {
  has_subscribes : HashSet < ID >,
  has_overrides  : HashSet < ID >,
  num_containers : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'contained' role
  num_contents : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'container' role
  num_links_in : HashMap < ID, usize >, // number of textlinks relationship for which the node plays the 'target' role
}

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
    collect_ids_from_tree ( forest );
  let rel_data : MapsFromIdForView =
    fetch_relationship_data (
      driver, & config . db_name, & pids ) . await ?;
  let ( container_to_contents, content_to_containers )
    : ( HashMap < ID, HashSet < ID > >,
        HashMap < ID, HashSet < ID > > )
    = contains_from_pids (
      & config . db_name, driver, & pids ) . await ?;
  let root_treeid : NodeId = forest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    forest,
    root_treeid,
    & rel_data,
    map,
    config );
  Ok (( container_to_contents,
        content_to_containers )) }

/// Run three batch queries to fetch all relationship data
/// for the given PIDs.
async fn fetch_relationship_data (
  driver  : &TypeDBDriver,
  db_name : &str,
  pids    : &[ID],
) -> Result < MapsFromIdForView, Box<dyn Error> > {
  let num_containers : HashMap < ID, usize > =
    count_containers ( db_name, driver, pids ) . await ?;
  let num_contents : HashMap < ID, usize > =
    count_contents ( db_name, driver, pids ) . await ?;
  let num_links_in : HashMap < ID, usize > =
    count_link_sources ( db_name, driver, pids ) . await ?;
  let has_subscribes_map : HashSet < ID > =
    has_subscribes ( db_name, driver, pids ) . await ?;
  let has_overrides_map : HashSet < ID > =
    has_overrides ( db_name, driver, pids ) . await ?;
  Ok ( MapsFromIdForView {
    has_subscribes : has_subscribes_map,
    has_overrides  : has_overrides_map,
    num_containers,
    num_contents,
    num_links_in,
  }) }

fn set_metadata_relationships_in_node_recursive (
  tree     : &mut Tree<ViewNode>,
  treeid   : NodeId,
  rel_data : &MapsFromIdForView,
  map      : &mut SkgNodeMap,
  config   : &SkgConfig,
) {
  // PITFALL: Aliasing and extraIDs are computed in a separate block, because skgnode_from_map_or_disk mutably borrows map, which prevents simultaneously holding a mutable borrow of tree (needed to write fields below).
  let ( aliasing, extra_ids ) : ( bool, bool ) =
    { // PITFALL: Uses 'false' for phantom nodes. Getting 'true' where appropriate would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      if let ViewNodeKind::True ( t )
        = & tree . get ( treeid ) . unwrap () . value () . kind
        { let skgnode_opt = skgnode_from_map_or_disk (
              &t.id, &t.source, map, config
            ). ok ();
          let aliasing = skgnode_opt . as_ref ()
            . and_then ( |n| n . aliases . as_ref () )
            . map ( |a| ! a . is_empty () )
            . unwrap_or ( false );
          let extra_ids = skgnode_opt . as_ref ()
            . map ( |n| n . ids . len () > 1 )
            . unwrap_or ( false );
          ( aliasing, extra_ids ) }
        else { ( false, false ) }};
  if let ViewNodeKind::True ( t )
    = &mut tree . get_mut ( treeid ) . unwrap () . value () . kind
    { // Write all graphStats fields.
      // PITFALL: These booleans are not populated for phantom nodes. Populating those would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      let node_pid : &ID = &t.id;
      t . graphStats . aliasing = aliasing;
      t . graphStats . extraIDs = extra_ids;
      t . graphStats . overriding =
        rel_data . has_overrides . contains ( node_pid );
      t . graphStats . subscribing =
        rel_data . has_subscribes . contains ( node_pid );
      t . graphStats . numContainers =
        rel_data . num_containers . get ( node_pid ) . copied ();
      t . graphStats . numContents =
        rel_data . num_contents . get ( node_pid ) . copied ();
      t . graphStats . numLinksIn =
        rel_data . num_links_in . get ( node_pid ) . copied (); }
  let child_treeids : Vec < NodeId > =
    tree . get ( treeid ) . unwrap ()
    . children () . map ( | c | c . id () ) . collect ();
  for child_treeid in child_treeids {
    set_metadata_relationships_in_node_recursive (
      tree,
      child_treeid,
      rel_data,
      map,
      config ); } }
