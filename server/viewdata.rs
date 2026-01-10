use crate::dbs::typedb::search::contains_from_pids::contains_from_pids;
use crate::dbs::typedb::search::count_relationships::{
  count_containers,
  count_contents,
  count_link_sources};
use crate::to_org::util::collect_ids_from_pair_tree;
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::OrgNodeKind;
use crate::types::tree::PairTree;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::NodeId;
use typedb_driver::TypeDBDriver;

/// Each of these describes some kind of relationship,
/// for each of a view's nodes.
struct MapsFromIdForView {
  num_containers : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'contained' role
  num_contents : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'container' role
  num_links_in : HashMap < ID, usize >, // number of textlinks relationship for which the node plays the 'target' role
  container_to_contents : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
  content_to_containers : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
}

/// Enrich all nodes in a forest with relationship metadata.
/// Fetches relationship data from TypeDB and applies it to the forest.
/// Forest is a single tree with ForestRoot at root.
pub async fn set_metadata_relationship_viewdata_in_forest (
  forest : &mut PairTree,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let rel_data : MapsFromIdForView =
    mapsFromIdForView_from_forest (
      forest, config, driver ) . await ?;
  let root_treeid : NodeId = forest . root () . id ();
  set_metadata_relationships_in_node_recursive (
    forest,
    root_treeid,
    None,
    & rel_data );
  Ok (( )) }

/// Build MapsFromIdForView from a forest
/// (a single tree with ForestRoot as root).
/// Collects all PIDs from the forest and fetches relationship data.
#[allow(non_snake_case)]
async fn mapsFromIdForView_from_forest (
  forest : &PairTree,
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < MapsFromIdForView, Box<dyn Error> > {
  let pids : Vec < ID > = (
    // This function just collects IDs,
    // but in this context we know they are specifically PIDs,
    // because they all came from 'forest_from_root_ids'.
    collect_ids_from_pair_tree ( forest ));
  fetch_relationship_data (
    driver,
    & config . db_name,
    & pids
  ) . await }

/// Run four batch queries to fetch all relationship data
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
  let ( container_to_contents, content_to_containers )
    : ( HashMap < ID, HashSet < ID > >,
        HashMap < ID, HashSet < ID > > )
    = contains_from_pids (
      db_name, driver, pids ). await ?;
  Ok ( MapsFromIdForView {
    num_containers,
    num_contents,
    num_links_in,
    container_to_contents,
    content_to_containers,
  }) }

fn set_metadata_relationships_in_node_recursive (
  tree       : &mut PairTree,
  treeid    : NodeId,
  parent_pid : Option < &ID >,
  rel_data   : &MapsFromIdForView,
) {
  let node_pid_opt : Option < ID > =
    tree . get ( treeid ) . unwrap ()
    . value () . orgnode . id_opt () . cloned ();

  if let Some ( ref node_pid ) = node_pid_opt {
    let num_containers : Option<usize> = rel_data . num_containers . get ( node_pid ) . copied ();
    let num_contents : Option<usize> = rel_data . num_contents . get ( node_pid ) . copied ();
    let num_links_in : Option<usize> = rel_data . num_links_in . get ( node_pid ) . copied ();
    let (parent_is_container, parent_is_content) : (bool, bool) =
      if let Some ( parent_skgid ) = parent_pid {
        ( rel_data . content_to_containers
            . get ( node_pid )
            . map_or ( false, | containers |
                       containers . contains ( parent_skgid )),
          rel_data . container_to_contents
            . get ( node_pid )
            . map_or ( false, | contents |
                       contents . contains ( parent_skgid )) )
      } else { (true, false) }; // default if no parent
    let mut node_mut = tree . get_mut ( treeid ) . unwrap ();
    if let OrgNodeKind::True ( t ) = &mut node_mut . value () .orgnode . kind {
      t . relationships . numContainers = num_containers;
      t . relationships . numContents = num_contents;
      t . relationships . numLinksIn = num_links_in;
      t . relationships . parentIsContainer = parent_is_container;
      t . relationships . parentIsContent = parent_is_content; }}
  { // recurse
    let child_treeids : Vec < NodeId > =
      tree . get ( treeid ) . unwrap ()
      . children () . map ( | c | c . id () ) . collect ();
    for child_treeid in child_treeids {
      set_metadata_relationships_in_node_recursive (
        tree,
        child_treeid,
        node_pid_opt . as_ref (),
        rel_data ); }} }
