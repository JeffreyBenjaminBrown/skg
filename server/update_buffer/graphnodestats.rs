use crate::dbs::typedb::search::all_graphnodestats::{
  fetch_all_graphnodestats,
  graphnodestats_for_pid,
  AllGraphNodeStats};
use crate::to_org::util::collect_ids_from_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::memory::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::skgnode::SkgNode;
use crate::types::viewnode::{GraphNodeStats, Scaffold, ViewNode, ViewNodeKind};

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
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "collect_ids_from_tree" ). entered();
      collect_ids_from_tree (forest) };
  let stats : AllGraphNodeStats =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "fetch_all_graphnodestats" ). entered();
      fetch_all_graphnodestats (
        & config . db_name, driver, & pids ) . await } ?;
  let root_treeid : NodeId = forest . root () . id ();
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "set_graphnodestats_recursive" ). entered();
    set_metadata_relationships_in_node_recursive (
      forest,
      root_treeid,
      & stats,
      map,
      config ) };
  Ok (( stats . container_to_contents,
        stats . content_to_containers )) }

pub fn set_metadata_relationships_in_node_recursive (
  tree   : &mut Tree<ViewNode>,
  treeid : NodeId,
  stats  : &AllGraphNodeStats,
  map    : &mut SkgNodeMap,
  config : &SkgConfig,
) {
  // PITFALL: Disk lookup (skgnode_from_map_or_disk) mutably borrows map, so it must happen in a separate block from the mutable tree borrow below.
  let new_stats : Option < GraphNodeStats > =
    { // PITFALL: Uses 'false' for phantom and deleted nodes. Getting 'true' where appropriate would be expensive, requiring inquiry into the git history not just of the phantom, but also of things it was connected to.
      match & tree . get (treeid) . unwrap () . value () . kind {
        ViewNodeKind::True (t) => {
          let skgnode_opt : Option<&SkgNode> =
            skgnode_from_map_or_disk (
              &t . id, &t . source, map, config
            ). ok ();
          Some ( graphnodestats_for_pid (
            &t . id, stats, skgnode_opt )) },
        ViewNodeKind::Scaff (
          Scaffold::SearchResult { id, source, .. }
        ) => {
          let skgnode_opt : Option<&SkgNode> =
            if map . contains_key (id)
               || config . sources . contains_key (source)
              { skgnode_from_map_or_disk (
                  id, source, map, config
                ). ok ()
              } else { None };
          Some ( graphnodestats_for_pid (
            id, stats, skgnode_opt )) },
        _ => None } // Other Scaff, Deleted, DeletedScaff
    };
  match new_stats {
    Some (gs) => match &mut tree . get_mut (treeid)
                         . unwrap () . value () . kind {
      ViewNodeKind::True (t) =>
        { t . graphStats = gs; },
      ViewNodeKind::Scaff (
        Scaffold::SearchResult { graphStats, .. }
      ) => { *graphStats = gs; },
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
      map,
      config ); } }
