/// PURPOSE: "Integrate" a "path" into an ViewNode tree.
/// PITFALL: Both of those terms are tricky.
/// - The 'path' is actually more general than that:
///   If at the end it forks, it includes the first layer of branches,
///   and if it cycles,
///   the first node to cycle is duplicated at the end.
/// - I say 'integrate' rather than 'insert' because some of the path,
///   maybe even all of it, might already be there.

use crate::to_org::util::{
  get_id_from_treenode, skgnode_and_viewnode_from_id,
  remove_completed_view_request};
use crate::dbs::typedb::paths::{
  paths_to_first_nonlinearities,
  PathToFirstNonlinearity};
use crate::types::misc::{ID, SkgConfig};
use crate::types::viewnode::ViewRequest;
use crate::types::viewnode::{
    ViewNode, Birth, mk_indefinitive_from_viewnode };
use crate::types::memory::SkgNodeMap;
use crate::types::tree::viewnode_skgnode::{
  find_child_by_id, find_children_by_ids};

use ego_tree::{NodeId,Tree};
use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

pub async fn build_and_integrate_containerward_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  map           : &mut SkgNodeMap,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_containerward_path (
      tree, map, node_id, config, typedb_driver ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Containerward,
    "Failed to integrate containerward path",
    errors, result ) }

/// Integrate a containerward path into an ViewNode tree.
/// This is called on a specific node in the tree,
/// and integrates the containerward path from that node.
pub async fn build_and_integrate_containerward_path (
  tree      : &mut Tree<ViewNode>,
  map       : &mut SkgNodeMap,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_backpaths (
    tree, map, node_id, config, driver,
    "contains", "contained", "container",
    Birth::ContainerOf
  ) . await }

pub async fn build_and_integrate_sourceward_view_then_drop_request (
  tree          : &mut Tree<ViewNode>,
  map           : &mut SkgNodeMap,
  node_id       : NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result : Result<(), Box<dyn Error>> =
    build_and_integrate_sourceward_path (
      tree, map, node_id, config, typedb_driver ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Sourceward,
    "Failed to integrate sourceward path",
    errors, result ) }

/// Integrate a sourceward path into an ViewNode tree.
pub async fn build_and_integrate_sourceward_path (
  tree      : &mut Tree<ViewNode>,
  map       : &mut SkgNodeMap,
  node_id   : NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  build_and_integrate_backpaths (
    tree, map, node_id, config, driver,
    "textlinks_to", "dest", "source",
    Birth::LinksTo
  ) . await }

/// Plural 'backpaths' because if the origin
/// immediately forks in the backward direction,
/// this will generate a path at each fork.
/// Otherwise it will only generate one path.
async fn build_and_integrate_backpaths (
  tree        : &mut Tree<ViewNode>,
  map         : &mut SkgNodeMap,
  node_id     : NodeId,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  relation    : &str,
  input_role  : &str,
  output_role : &str,
  birth       : Birth,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    get_id_from_treenode ( tree, node_id ) ?;
  let paths : Vec<PathToFirstNonlinearity> =
    paths_to_first_nonlinearities (
      &config.db_name, driver, &terminus_pid,
      relation, input_role, output_role
    ) . await ?;
  integrate_backpaths (
    node_id, tree, paths, birth, map, config, driver
  ) . await }

/// At 'node_id' in 'tree', integrate 'paths' of homogenous birth 'birth'.
async fn integrate_backpaths (
  node_id : NodeId,
  tree    : &mut Tree<ViewNode>,
  paths   : Vec<PathToFirstNonlinearity>,
  birth   : Birth,
  map     : &mut SkgNodeMap,
  config  : &SkgConfig,
  driver  : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  for p in paths {
    integrate_path_that_might_fork_or_cycle (
      tree, map, node_id,
      p.path, p.branches, p.cycle_nodes,
      config, driver, birth
    ) . await ?; }
  Ok(()) }

/// Integrate a (maybe forked or cyclic) path into an ViewNode tree,
/// using provided backpath data.
pub async fn integrate_path_that_might_fork_or_cycle (
  tree        : &mut Tree<ViewNode>,
  map         : &mut SkgNodeMap,
  node_id     : NodeId,
  path        : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  birth       : Birth,
) -> Result < (), Box<dyn Error> > {
  let last_node_id : NodeId =
    integrate_linear_portion_of_path (
      tree, map, node_id, &path, config, driver, birth
    ). await ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      tree, map, last_node_id, branches, config, driver, birth
    ). await ?;
  } else if ! cycle_nodes . is_empty () {
    // PITFALL: If there are branches, cycle nodes are ignored.
    integrate_cycle_nodes (
      tree, map, last_node_id, cycle_nodes, config, driver, birth
    ). await ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path<'a> (
  tree    : &'a mut Tree<ViewNode>,
  map     : &'a mut SkgNodeMap,
  node_id : NodeId,
  path    : &'a [ID],
  config  : &'a SkgConfig,
  driver  : &'a TypeDBDriver,
  birth   : Birth,
) -> Pin<Box<dyn Future<Output = Result<NodeId,
                                        Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    if path . is_empty () {
      return Ok (node_id); }
    let path_head : &ID = &path[0];
    let path_tail : &[ID] = &path[1..];
    let next_node_id : NodeId =
      match find_child_by_id ( tree, node_id, path_head ) {
        Some (child_treeid) => child_treeid,
        None => { prepend_indefinitive_child (
                    tree, map, node_id, path_head, config, driver, birth
                  ) . await ? } };
    integrate_linear_portion_of_path ( // recurse
      tree,
      map,
      next_node_id, // we just found or inserted this
      path_tail,
      config,
      driver,
      birth ). await } ) }

/// Add branch nodes as children of the specified node.
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
async fn integrate_branches_in_node (
  tree     : &mut Tree<ViewNode>,
  map      : &mut SkgNodeMap,
  node_id  : NodeId,
  branches : HashSet < ID >,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  birth    : Birth,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_ids ( tree, node_id, &branches );
  let mut branches_to_add : Vec < ID > =
    branches . into_iter ()
    . filter ( | b |
                 ! found_children . contains_key (b) )
    . collect ();
  { // Simplifies testing. Not necessary in production.
    branches_to_add . sort (); }
  for branch_id in branches_to_add {
    prepend_indefinitive_child (
      tree, map, node_id, &branch_id, config, driver, birth
    ). await ?; }
  Ok (( )) }

/// Add cycle nodes as children of the specified node.
/// Cycle nodes already present as children are skipped.
async fn integrate_cycle_nodes (
  tree        : &mut Tree<ViewNode>,
  map         : &mut SkgNodeMap,
  node_id     : NodeId,
  cycle_nodes : HashSet < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
  birth       : Birth,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, NodeId > =
    find_children_by_ids ( tree, node_id, &cycle_nodes );
  let mut to_add : Vec < ID > =
    cycle_nodes . into_iter ()
    . filter ( | c |
                 ! found_children . contains_key (c) )
    . collect ();
  { to_add . sort (); }
  for cycle_id in to_add {
    prepend_indefinitive_child (
      tree, map, node_id, &cycle_id, config, driver, birth
    ). await ?; }
  Ok (( )) }

async fn prepend_indefinitive_child (
  tree           : &mut Tree<ViewNode>,
  map            : &mut SkgNodeMap,
  parent_treeid  : NodeId,
  child_skgid    : &ID,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  birth          : Birth,
) -> Result < NodeId, Box<dyn Error> > {
  let ( _, child_viewnode ) : ( _, ViewNode ) =
    skgnode_and_viewnode_from_id (
      config, driver, child_skgid, map
    ) . await ?;
  let viewnode : ViewNode =
    mk_indefinitive_from_viewnode (
      child_viewnode, birth )
    . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  let new_child_treeid : NodeId =
    tree . get_mut (parent_treeid) . unwrap ()
    . prepend (viewnode) . id ();
  Ok (new_child_treeid) }
