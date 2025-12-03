use crate::media::tree::map_snd_over_forest;
use crate::media::typedb::search::{
  contains_from_pids,
  count_containers,
  count_contents,
  count_link_sources};
use crate::to_org::initial_view_bfs::render_initial_forest_bfs;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::to_org::util::skgnode_and_orgnode_from_id;
use crate::to_org::orgnode::orgnode_to_text;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use ego_tree::Tree;
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

/// Build a tree from a root ID and render it to org text.
pub async fn single_root_view (
  driver  : &TypeDBDriver,
  config  : &SkgConfig,
  root_id : &ID,
) -> Result < String, Box<dyn Error> > {
  multi_root_view (
    driver,
    config,
    & [ root_id . clone () ] ) . await }

/// Build a forest from multiple root IDs and render it to org text.
pub async fn multi_root_view (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  root_ids : &[ID],
) -> Result < String, Box<dyn Error> > {
  let mut paired_forest : Vec < Tree < (SkgNode, OrgNode) > > =
    stub_forest_from_root_ids (
      root_ids, config, driver ) . await ?;
  render_initial_forest_bfs (
    &mut paired_forest, config, driver ) . await ?;
  // Convert to OrgNode-only trees for metadata enrichment
  let mut forest : Vec < Tree < OrgNode > > =
    map_snd_over_forest ( paired_forest );
  set_metadata_relationship_viewdata_in_forest (
    &mut forest, config, driver ) . await ?;
  let buffer_content : String =
    render_forest_to_org ( & forest );
  Ok ( buffer_content ) }

/// Create a minimal forest containing just root nodes (no children).
/// Returns (SkgNode, OrgNode) trees to avoid multiple SkgNode lookups.
async fn stub_forest_from_root_ids (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < Tree < (SkgNode, OrgNode) > >,
              Box<dyn Error> > {
  let mut forest : Vec < Tree < (SkgNode, OrgNode) > > =
    Vec::new ();
  for root_id in root_ids {
    let (root_skgnode, root_orgnode) : ( SkgNode, OrgNode ) =
      skgnode_and_orgnode_from_id (
        config, driver, root_id ) . await ?;
    let tree : Tree < (SkgNode, OrgNode) > =
      Tree::new ( (root_skgnode, root_orgnode) );
    forest . push ( tree ); }
  Ok ( forest ) }

/// Build MapsFromIdForView from a forest of OrgNode trees.
/// Collects all PIDs from the forest and fetches relationship data.
#[allow(non_snake_case)]
async fn mapsFromIdForView_from_forest (
  forest : &[Tree < OrgNode >],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < MapsFromIdForView, Box<dyn Error> > {
  let pids : Vec < ID > = (
    // This function just collects IDs,
    // but in this context we know they are specifically PIDs,
    // because they all came from 'forest_from_root_ids'.
    collect_ids_from_forest ( forest ));
  fetch_relationship_data (
    driver,
    & config . db_name,
    & pids
  ) . await }

/// Collect all PIDs from a forest of OrgNode trees.
fn collect_ids_from_forest (
  forest : &[Tree < OrgNode >],
) -> Vec < ID > {
  let mut pids : Vec < ID > = Vec::new ();
  for tree in forest {
    for edge in tree . root () . traverse () {
      if let ego_tree::iter::Edge::Open ( node_ref ) = edge {
        if let Some ( ref pid ) =
          node_ref . value () . metadata . id {
          pids . push ( pid . clone () ); }} }}
  pids }

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

/// Enrich all nodes in a forest with relationship metadata.
/// Fetches relationship data from TypeDB and applies it to the forest.
pub async fn set_metadata_relationship_viewdata_in_forest (
  forest : &mut [Tree < OrgNode >],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let rel_data : MapsFromIdForView =
    mapsFromIdForView_from_forest (
      forest, config, driver ) . await ?;
  for tree in forest {
    let root_id : ego_tree::NodeId =
      tree . root () . id ();
    set_metadata_relationships_in_node_recursive (
      tree,
      root_id,
      None,
      & rel_data ); }
  Ok (( )) }

fn set_metadata_relationships_in_node_recursive (
  tree       : &mut Tree < OrgNode >,
  node_id    : ego_tree::NodeId,
  parent_pid : Option < &ID >,
  rel_data   : &MapsFromIdForView,
) {
  let node_pid_opt : Option < ID > =
    tree . get ( node_id ) . unwrap ()
    . value () . metadata . id . clone ();

  if let Some ( ref node_pid ) = node_pid_opt {
    // Update all relationship fields
    tree . get_mut ( node_id ) . unwrap () . value ()
      . metadata . viewData . relationships . numContainers =
      rel_data . num_containers . get ( node_pid ) . copied ();
    tree . get_mut ( node_id ) . unwrap () . value ()
      . metadata . viewData . relationships . numContents =
      rel_data . num_contents . get ( node_pid ) . copied ();
    tree . get_mut ( node_id ) . unwrap () . value ()
      . metadata . viewData . relationships . numLinksIn =
      rel_data . num_links_in . get ( node_pid ) . copied ();
    if let Some ( parent_id ) = parent_pid {
      // Set parent relationship flags if we have a parent.
      // TODO | PITFALL: If there is no parent,
      // these fields are meaningless.
      tree . get_mut ( node_id ) . unwrap () . value ()
        . metadata . viewData . relationships . parentIsContainer =
        rel_data . content_to_containers
        . get ( node_pid )
        . map_or ( false, | containers |
                   containers . contains ( parent_id ));
      tree . get_mut ( node_id ) . unwrap () . value ()
        . metadata . viewData . relationships . parentIsContent =
        rel_data . container_to_contents
        . get ( node_pid )
        . map_or ( false, | contents |
                   contents . contains ( parent_id )); }}
  { // recurse
    let child_ids : Vec < ego_tree::NodeId > =
      tree . get ( node_id ) . unwrap ()
      . children () . map ( | c | c . id () ) . collect ();
    for child_id in child_ids {
      set_metadata_relationships_in_node_recursive (
        tree,
        child_id,
        node_pid_opt . as_ref (),
        rel_data ); }} }

/// Render a forest of OrgNode trees to org-mode text.
/// Each tree's root starts at level 1.
/// Assumes metadata has already been enriched with relationship data.
pub fn render_forest_to_org (
  forest : &[Tree < OrgNode >],
) -> String {
  fn render_node_subtree_to_org (
    node_ref : ego_tree::NodeRef < OrgNode >,
    level    : usize,
  ) -> String {
    let node : &OrgNode = node_ref . value ();
    let mut out : String =
      orgnode_to_text ( level, node );
    for child in node_ref . children () {
      out . push_str (
        & render_node_subtree_to_org (
          child,
          level + 1 )); }
    out }
  let mut result : String =
    String::new ();
  for tree in forest {
    result . push_str (
      & render_node_subtree_to_org (
        tree . root (),
        1 )); }
  result }
