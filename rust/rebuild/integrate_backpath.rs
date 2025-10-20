/// PURPOSE: Integrate a containerward path into an OrgNode tree.
/// I say 'integrate' rather than 'insert' because some of the path,
/// even all of it, might already be there.

use crate::mk_org_text::content_view::skgnode_and_orgnode_from_pid;
use crate::typedb::search::path_containerward_to_end_cycle_and_or_branches;
use crate::types::{ID, SkgConfig, OrgNode, RelToParent};

use ego_tree::Tree;
use std::collections::{HashSet, HashMap};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Integrate a containerward path into an OrgNode tree.
/// This is called on a specific node in the tree,
/// and integrates the containerward path from that node.
pub async fn build_and_integrate_containerward_path (
  tree      : &mut Tree < OrgNode >,
  node_id   : ego_tree::NodeId,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    tree . get ( node_id ) . unwrap ()
    . value () . metadata . id . clone ()
    . ok_or ( "Node has no ID" ) ?;
  let ( path, cycle_node, branches )
    : ( Vec < ID >, Option < ID >, HashSet < ID > )
    = path_containerward_to_end_cycle_and_or_branches (
      & config . db_name,
      driver,
      & terminus_pid ) . await ?;
  integrate_containerward_path (
    tree, node_id, path, branches, cycle_node, config ) }

/// Integrate a containerward path into an OrgNode tree,
/// using provided backpath data.
pub fn integrate_containerward_path (
  tree        : &mut Tree < OrgNode >,
  node_id     : ego_tree::NodeId, // where to integrate
  mut path    : Vec < ID >,     // part of the computed path
  branches    : HashSet < ID >, // part of the computed path
  cycle_node  : Option < ID >,  // part of the computed path
  config      : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let terminus_pid : ID =
    tree . get ( node_id ) . unwrap ()
    . value () . metadata . id . clone ()
    . ok_or ( "Node has no ID" ) ?;
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
      tree, node_id, &path, config ) ?;
  if ! branches . is_empty () {
    integrate_branches_in_node (
      tree, last_node_id, branches, config ) ?;
  } else if let Some ( cycle_id ) = cycle_node {
    // PITFALL: If there are branches, the cycle node is ignored.
    integrate_cycle_in_node (
      tree, last_node_id, cycle_id, config ) ?; }
  Ok (( )) }

/// Recursively integrate the remaining path into the tree.
/// Operates on a specific node and the remaining path.
/// Returns the NodeId of the last node in the path.
fn integrate_linear_portion_of_path (
  tree       : &mut Tree < OrgNode >,
  node_id    : ego_tree::NodeId,
  path       : &[ID],
  config     : &SkgConfig,
) -> Result < ego_tree::NodeId, Box<dyn Error> > {
  if path . is_empty () {
    return Ok ( node_id ); }
  let path_head : &ID = &path[0];
  let path_tail : &[ID] = &path[1..];
  let next_node_id : ego_tree::NodeId =
    match find_child_by_id ( tree, node_id, path_head ) {
      Some ( child_id ) => child_id,
      None => {
        prepend_indefinitive_child_with_parent_ignores (
          tree, node_id, path_head, config ) ? } };
  integrate_linear_portion_of_path ( // recurse
    tree,
    next_node_id, // we just found or inserted this
    path_tail,
    config ) }

/// Add branch nodes as children of the specified node.
/// Branches are added in sorted order (reversed for prepending).
/// Branches that are already children are skipped.
fn integrate_branches_in_node (
  tree       : &mut Tree < OrgNode >,
  node_id    : ego_tree::NodeId,
  branches   : HashSet < ID >,
  config     : &SkgConfig,
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
      tree, node_id, &branch_id, config ) ?; }
  Ok (( )) }

/// Add a cycle node as a child of the specified node.
/// The cycle node is only added if it's not already a child.
fn integrate_cycle_in_node (
  tree       : &mut Tree < OrgNode >,
  node_id    : ego_tree::NodeId,
  cycle_id   : ID,
  config     : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  if find_child_by_id ( tree, node_id, &cycle_id ) . is_none () {
    prepend_indefinitive_child_with_parent_ignores (
      tree, node_id, &cycle_id, config ) ?; }
  Ok (( )) }

/// Helper function to prepend a new child to a node in a tree.
/// The new child has treatment=ParentIgnores and indefinitive=true.
/// TODO: This procedure could later be improved to
/// use treatment=Content when the child is in fact content.
fn prepend_indefinitive_child_with_parent_ignores (
  tree       : &mut Tree < OrgNode >,
  parent_id  : ego_tree::NodeId,
  child_pid  : &ID,
  config     : &SkgConfig,
) -> Result < ego_tree::NodeId, Box<dyn Error> > {
  let ( mut child_orgnode, _ ) : ( OrgNode, _ ) =
    skgnode_and_orgnode_from_pid ( config, child_pid ) ?;
  child_orgnode . metadata . code . relToParent =
    RelToParent::ParentIgnores;
  child_orgnode . metadata . code . indefinitive =
    true;
  let new_child_id : ego_tree::NodeId =
    tree . get_mut ( parent_id ) . unwrap ()
    . prepend ( child_orgnode ) . id ();
  Ok ( new_child_id ) }

/// Find a child node by its ID.
/// Returns the NodeId of the child if found, None otherwise.
fn find_child_by_id (
  tree       : & Tree < OrgNode >,
  parent_id  : ego_tree::NodeId,
  target_id  : & ID,
) -> Option < ego_tree::NodeId > {
  for child in tree . get ( parent_id ) . unwrap () . children () {
    if let Some ( ref child_pid ) = child . value () . metadata . id {
      if child_pid == target_id {
        return Some ( child . id () ); } } }
  None }

/// Find child nodes by their IDs.
/// Returns a map from ID to NodeId for children that were found.
/// IDs not found as children are not included in the result.
fn find_children_by_ids (
  tree       : & Tree < OrgNode >,
  parent_id  : ego_tree::NodeId,
  target_ids : & HashSet < ID >,
) -> HashMap < ID, ego_tree::NodeId > {
  let mut result : HashMap < ID, ego_tree::NodeId > =
    HashMap::new ();
  for child in tree . get ( parent_id ) . unwrap () . children () {
    if let Some ( ref child_pid ) = child . value () . metadata . id {
      if target_ids . contains ( child_pid ) {
        result . insert ( child_pid . clone (), child . id () ); }} }
  result }
