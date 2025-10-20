use crate::file_io::read_node;
use crate::util::path_from_pid;
use crate::typedb::search::{
  contains_from_pids,
  count_containers,
  count_contents,
  count_link_sources};
use crate::typedb::util::pid_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::default_metadata;
use crate::mk_org_text::util::newline_to_space;
use crate::mk_org_text::orgnode::render_org_node_from_text;

use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::io;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;

/// Build a tree from a root ID and render it to org text.
/// Tree-based implementation with cycle detection.
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
/// Tree-based implementation with cycle detection across all trees.
pub async fn multi_root_view (
  driver   : &TypeDBDriver,
  config   : &SkgConfig,
  root_ids : &[ID],
) -> Result < String, Box<dyn Error> > {
  let mut forest : Vec < Tree < OrgNode > > =
    forest_from_ids (
      root_ids, config, driver ) . await ?;
  set_metadata_relationships_in_forest (
    &mut forest, config, driver ) . await ?;
  Ok ( render_forest_to_org (
    & forest )) }

/// Build a forest of OrgNode trees from multiple root IDs.
/// Uses a shared visited set across all trees for cycle detection.
pub async fn forest_from_ids (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < Tree < OrgNode > >, Box<dyn Error> > {
  let mut forest : Vec < Tree < OrgNode > > = Vec::new ();
  let mut visited : HashSet < ID > = HashSet::new ();
  for root_id in root_ids {
    let tree : Tree < OrgNode > =
      orgnode_tree_from_id_with_earlier_visits (
        root_id, config, driver, &mut visited ) . await ?;
    forest . push ( tree ); }
  Ok ( forest ) }

/// Build a tree of OrgNodes from a root ID.
/// The 'visited' argument permits cycle detection
/// that accounts for any earlier trees in the forest.
pub async fn orgnode_tree_from_id_with_earlier_visits (
  root_id : &ID,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
  visited : &mut HashSet < ID >,
) -> Result < Tree < OrgNode >, Box<dyn Error> > {
  let root_pid : ID =
    pid_from_id (
      & config . db_name, driver, root_id ) . await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", root_id )) ?;
  if visited . contains ( & root_pid ) {
    let repeated_root : OrgNode =
      mk_repeated_orgnode_from_id (
        config, & root_pid ) ?;
    return Ok ( Tree::new ( repeated_root ));
  } else { // It has not been visited yet, so build normally.
    visited . insert ( root_pid . clone () );
    let ( root_orgnode, skgnode ) : ( OrgNode, SkgNode ) =
      skgnode_and_orgnode_from_pid (
        config, & root_pid ) ?;
    let mut tree : Tree < OrgNode > = (
      // a tree containing only the root node
      Tree::new ( root_orgnode ) );
    let root_node_id : ego_tree::NodeId = (
      // Appease borrow checker: Store this before mutating 'tree'.
      tree . root () . id () );
    let mut ancestor_path : Vec < ID > =
      vec! [ root_pid . clone () ];
    for child_id in & skgnode . contains { // build branches
      Box::pin (
        build_tree_recursive (
          &mut tree,
          root_node_id,
          child_id,
          config,
          driver,
          visited,
          &mut ancestor_path
        )) . await ?; }
    Ok ( tree ) }}

async fn build_tree_recursive (
  tree            : &mut Tree < OrgNode >,
  parent_node_id  : ego_tree::NodeId, // where to insert
  child_id        : &ID, // what to insert, and recurse into
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
  visited         : &mut HashSet < ID >,
  ancestor_path   : &mut Vec < ID >, // path from root to parent (for cycle detection)
) -> Result < (), Box<dyn Error> > {
  let child_pid : ID =
    pid_from_id (
      & config . db_name, driver, child_id ) . await ?
    . ok_or_else ( || format! (
      "ID '{}' not found in database", child_id )) ?;
  let is_cycle : bool =
    ancestor_path . contains ( & child_pid );
  if visited . contains ( & child_pid ) {
    // for repeated nodes, return without a repeated call
    let mut repeated_node : OrgNode =
      mk_repeated_orgnode_from_id (
        config, & child_pid ) ?;
    repeated_node . metadata . cycle = is_cycle;
    tree . get_mut ( parent_node_id ) . unwrap ()
      . append ( repeated_node );
    return Ok (( )); }
  visited . insert ( child_pid . clone () );
  let ( mut child_orgnode, child_skgnode ) : ( OrgNode, SkgNode ) =
    skgnode_and_orgnode_from_pid (
      config, & child_pid ) ?;
  child_orgnode . metadata . cycle = is_cycle;
  let new_child_id : ego_tree::NodeId = (
    // append new child to its parent
    tree . get_mut ( parent_node_id ) . unwrap ()
    . append ( child_orgnode ) . id () );
  ancestor_path . push ( child_pid . clone () );
  for grandchild_id in & child_skgnode . contains {
    Box::pin (
      build_tree_recursive ( // recurse
        tree,
        new_child_id,
        grandchild_id,
        config,
        driver,
        visited,
        ancestor_path
      )) . await ?; }
  ancestor_path . pop ();
  return Ok (( )) }

/// Fetch a SkgNode from disk.
/// Make an OrgNode from it, with validated title.
/// Return both.
pub fn skgnode_and_orgnode_from_pid (
  config : &SkgConfig,
  pid    : &ID,
) -> Result < ( OrgNode, SkgNode ), Box<dyn Error> > {
  let path : String = path_from_pid ( config, pid . clone () );
  let skgnode : SkgNode = read_node ( path ) ?;
  let orgnode : OrgNode = OrgNode {
    metadata : { let mut md = default_metadata ();
                 md . id = Some ( pid . clone () );
                 md },
    title : newline_to_space ( & skgnode . title ),
    body  : skgnode . body . clone (), };
  if orgnode . title . is_empty () { // Validate title
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 pid ),
    )) ); }
  Ok (( orgnode, skgnode )) }

/// Make an OrgNode marked 'repeated' by fetching it from disk.
pub fn mk_repeated_orgnode_from_id (
  config : &SkgConfig,
  id     : &ID,
) -> Result < OrgNode, Box<dyn Error> > {
  let path : String = path_from_pid ( config, id . clone () );
  let skgnode : SkgNode = read_node ( path ) ?;
  let mut md = default_metadata ();
  md . repeat = true;
  md . id = Some ( id . clone () );
  Ok ( OrgNode {
    metadata : md,
    title : newline_to_space ( & skgnode . title ),
    body : Some (
      "Repeated, probably above. Edit there, not here."
        . to_string () ), } ) }

/// Build MapsFromIdForView from a forest of OrgNode trees.
/// Collects all PIDs from the forest and fetches relationship data.
#[allow(non_snake_case)]
pub async fn mapsFromIdForView_from_forest (
  forest : &[Tree < OrgNode >],
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result < MapsFromIdForView, Box<dyn Error> > {
  let pids : Vec < ID > = (
    // This function just collects IDs,
    // but in this context we know they are specifically PIDs,
    // because they all came from 'forest_from_ids'.
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

/// Each of these describes some kind of relationship,
/// for each of a view's nodes.
pub struct MapsFromIdForView {
  num_containers : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'contained' role
  num_contents : HashMap < ID, usize >, // number of contains relationships for which the node plays the 'container' role
  num_links_in : HashMap < ID, usize >, // number of hyperlinks relationship for which the node plays the 'target' role
  container_to_contents : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
  content_to_containers : HashMap < ID, HashSet < ID > >, // if the value would be empty, the key is omitted
}

/// Enrich all nodes in a forest with relationship metadata.
/// Fetches relationship data from TypeDB and applies it to the forest.
pub async fn set_metadata_relationships_in_forest (
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
      . metadata . relationships . numContainers =
      rel_data . num_containers . get ( node_pid ) . copied ();
    tree . get_mut ( node_id ) . unwrap () . value ()
      . metadata . relationships . numContents =
      rel_data . num_contents . get ( node_pid ) . copied ();
    tree . get_mut ( node_id ) . unwrap () . value ()
      . metadata . relationships . numLinksIn =
      rel_data . num_links_in . get ( node_pid ) . copied ();
    if let Some ( parent_id ) = parent_pid {
      // Set parent relationship flags if we have a parent.
      // TODO | PITFALL: If there is no parent,
      // these fields are meaningless.
      tree . get_mut ( node_id ) . unwrap () . value ()
        . metadata . relationships . parentIsContainer =
        rel_data . content_to_containers
        . get ( node_pid )
        . map_or ( false, | containers |
                   containers . contains ( parent_id ));
      tree . get_mut ( node_id ) . unwrap () . value ()
        . metadata . relationships . parentIsContent =
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
      render_org_node_from_text ( level, node );
    if ! node . metadata . repeat {
      for child in node_ref . children () {
        out . push_str (
          & render_node_subtree_to_org (
            child,
            level + 1 )); }}
    out }
  let mut result : String =
    String::new ();
  for tree in forest {
    result . push_str (
      & render_node_subtree_to_org (
        tree . root (),
        1 )); }
  result }

/// Build a tree of OrgNodes from a root ID.
/// Performs cycle detection with a fresh visited set.
pub async fn orgnode_tree_from_id (
  root_id : &ID,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < Tree < OrgNode >, Box<dyn Error> > {
  let mut visited : HashSet < ID > = HashSet::new ();
  orgnode_tree_from_id_with_earlier_visits (
    root_id, config, driver, &mut visited ) . await }
