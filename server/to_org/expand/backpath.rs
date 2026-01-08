/// PURPOSE: "Integrate" a "path" into an OrgNode tree.
/// PITFALL: Both of those terms are tricky.
/// - The 'path' is actually more general than that:
///   If at the end it forks, it includes the first layer of branches,
///   and if it cycles,
///   the first node to cycle is duplicated at the end.
/// - I say 'integrate' rather than 'insert' because some of the path,
///   maybe even all of it, might already be there.

use crate::to_org::util::{
  get_pid_in_pairtree, skgnode_and_orgnode_from_id,
  remove_completed_view_request};
use crate::dbs::typedb::search::{
  path_containerward_to_end_cycle_and_or_branches,
  path_sourceward_to_end_cycle_and_or_branches};
use crate::types::misc::{ID, SkgConfig};
use crate::types::orgnode::{OrgNode, Interp, ViewRequest};
use crate::types::tree::{PairTree, NodePair};

use std::collections::{HashSet, HashMap};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use typedb_driver::TypeDBDriver;

pub async fn build_and_integrate_containerward_view_then_drop_request (
  tree          : &mut PairTree,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result = build_and_integrate_containerward_path (
    tree, node_id, config, typedb_driver ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Containerward,
    "Failed to integrate containerward path",
    errors, result ) }

/// Integrate a containerward path into an OrgNode tree.
/// This is called on a specific node in the tree,
/// and integrates the containerward path from that node.
pub async fn build_and_integrate_containerward_path (
  tree      : &mut PairTree,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    get_pid_in_pairtree ( tree, node_id ) ?;
  let ( path, cycle_node, branches )
    : ( Vec < ID >, Option < ID >, HashSet < ID > )
    = path_containerward_to_end_cycle_and_or_branches (
      & config . db_name,
      driver,
      & terminus_pid ) . await ?;
  integrate_path_that_might_fork_or_cycle (
    tree, node_id, path, branches, cycle_node, config, driver
  ). await }

pub async fn build_and_integrate_sourceward_view_then_drop_request (
  tree          : &mut PairTree,
  node_id       : ego_tree::NodeId,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let result = build_and_integrate_sourceward_path (
    tree, node_id, config, typedb_driver ) . await;
  remove_completed_view_request (
    tree, node_id,
    ViewRequest::Sourceward,
    "Failed to integrate sourceward path",
    errors, result ) }

/// Integrate a sourceward path into an OrgNode tree.
/// TODO ? Can this be dedup'd w/r/t
///   'build_and_integrate_containerward_path'?
pub async fn build_and_integrate_sourceward_path (
  tree      : &mut PairTree,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    get_pid_in_pairtree ( tree, node_id ) ?;
  let ( path, cycle_node, branches )
    : ( Vec < ID >, Option < ID >, HashSet < ID > )
    = path_sourceward_to_end_cycle_and_or_branches (
      & config . db_name,
      driver,
      & terminus_pid ) . await ?;
  integrate_path_that_might_fork_or_cycle (
    tree, node_id, path, branches, cycle_node, config, driver ) . await }

/// Integrate a (maybe forked or cyclic) path into an OrgNode tree,
/// using provided backpath data.
pub async fn integrate_path_that_might_fork_or_cycle (
  tree        : &mut PairTree,
  node_id     : ego_tree::NodeId,
  mut path    : Vec < ID >,
  branches    : HashSet < ID >,
  cycle_node  : Option < ID >,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    get_pid_in_pairtree ( tree, node_id ) ?;
  if ! path . is_empty () {
    // The head of the path should be the terminus. We strip it.
    if path[0] != terminus_pid {
      return Err (
        format! (
          "Path head {:?} does not match terminus {:?}",
          path[0], terminus_pid
        ) . into () ); }
    path . remove ( 0 ); }
  let last_node_id : ego_tree::NodeId =
    integrate_linear_portion_of_path (
      tree, node_id, &path, config, driver ) . await ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      tree, last_node_id, branches, config, driver ) . await ?;
  } else if let Some ( cycle_id ) = cycle_node {
    // PITFALL: If there are branches, the cycle node is ignored.
    integrate_cycle_in_node (
      tree, last_node_id, cycle_id, config, driver ) . await ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path<'a> (
  tree       : &'a mut PairTree,
  node_id    : ego_tree::NodeId,
  path       : &'a [ID],
  config     : &'a SkgConfig,
  driver     : &'a TypeDBDriver,
) -> Pin<Box<dyn Future<Output = Result<ego_tree::NodeId,
                                        Box<dyn Error>>> + 'a>> {
  Box::pin(async move {
    if path . is_empty () {
      return Ok ( node_id ); }
    let path_head : &ID = &path[0];
    let path_tail : &[ID] = &path[1..];
    let next_node_id : ego_tree::NodeId =
      match find_child_by_id ( tree, node_id, path_head ) {
        Some ( child_treeid ) => child_treeid,
        None => {
          prepend_indefinitive_child_with_parent_ignores (
            tree, node_id, path_head, config, driver ) . await ? } };
    integrate_linear_portion_of_path ( // recurse
      tree,
      next_node_id, // we just found or inserted this
      path_tail,
      config,
      driver ) . await } ) }

/// Add branch nodes as children of the specified node.
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
async fn integrate_branches_in_node (
  tree       : &mut PairTree,
  node_id    : ego_tree::NodeId,
  branches   : HashSet < ID >,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let found_children : HashMap < ID, ego_tree::NodeId > =
    find_children_by_ids ( tree, node_id, &branches );
  let mut branches_to_add : Vec < ID > =
    branches . into_iter ()
    . filter ( | b |
                 ! found_children . contains_key ( b ) )
    . collect ();
  { // TODO : This should not be necessary. It must be for testing?
    branches_to_add . sort ();
    branches_to_add . reverse (); }
  for branch_id in branches_to_add {
    prepend_indefinitive_child_with_parent_ignores (
      tree, node_id, &branch_id, config, driver ) . await ?; }
  Ok (( )) }

/// Add a cycle node as a child of the specified node.
/// The cycle node is only added if it's not already a child.
async fn integrate_cycle_in_node (
  tree       : &mut PairTree,
  node_id    : ego_tree::NodeId,
  cycle_id   : ID,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if find_child_by_id ( tree, node_id, &cycle_id ) . is_none () {
    prepend_indefinitive_child_with_parent_ignores (
      tree, node_id, &cycle_id, config, driver ) . await ?; }
  Ok (( )) }

/// Helper function to prepend a new child to a node in a tree.
/// The new child has treatment=ParentIgnores and indefinitive=true.
/// TODO: This procedure could later be improved to
/// use treatment=Content when the child is in fact content.
async fn prepend_indefinitive_child_with_parent_ignores (
  tree           : &mut PairTree,
  parent_treeid  : ego_tree::NodeId,
  child_skgid    : &ID,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < ego_tree::NodeId, Box<dyn Error> > {
  let ( _, mut child_orgnode ) : ( _, OrgNode ) =
    skgnode_and_orgnode_from_id (
      config, driver, child_skgid
    ). await ?;
  child_orgnode . metadata . code . interp =
    Interp::ParentIgnores;
  child_orgnode . metadata . code . indefinitive =
    true;
  let new_child_treeid : ego_tree::NodeId =
    tree . get_mut ( parent_treeid ) . unwrap ()
    . prepend ( NodePair { mskgnode    : None,
                           orgnode     : child_orgnode,
                           new_orgnode : None } ) . id ();
  Ok ( new_child_treeid ) }

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
fn find_child_by_id (
  tree          : & PairTree,
  parent_treeid : ego_tree::NodeId,
  target_skgid  : & ID,
) -> Option < ego_tree::NodeId > {
  for child in tree . get ( parent_treeid ) . unwrap () . children () {
    if let Some ( ref child_skgpid ) =
      child . value () . orgnode . metadata . id {
        if child_skgpid == target_skgid {
          return Some ( child . id () ); }} }
  None }

/// Find child nodes by their IDs.
/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
fn find_children_by_ids (
  tree          : & PairTree,
  parent_treeid : ego_tree::NodeId,
  target_skgids : & HashSet < ID >,
) -> HashMap < ID, ego_tree::NodeId > {
  let mut result : HashMap < ID, ego_tree::NodeId > =
    HashMap::new ();
  for child in tree . get ( parent_treeid ) . unwrap () . children () {
    if let Some ( ref child_skgpid )
      = child . value () . orgnode . metadata . id
    { if target_skgids . contains ( child_skgpid )
      { result . insert ( child_skgpid . clone (), child . id () ); }} }
  result }
