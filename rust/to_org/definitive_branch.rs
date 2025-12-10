/// SINGLE ENTRY POINT: 'execute_definitive_view_requests'.

use crate::to_org::bfs_shared::collect_content_children;
use crate::to_org::truncate::truncate_after_node_in_generation_in_tree;
use crate::to_org::util::{
  skgnode_and_orgnode_from_id, skgnode_and_orgnode_from_pid_and_source,
  VisitedMap, get_pid_in_pairtree, is_ancestor_id };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, RelToParent, ViewRequest};
use crate::types::trees::PairTree;

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::cmp::min;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// TODO : explain
struct ForceIndefinitive ( bool );

pub async fn execute_definitive_view_requests (
  forest        : &mut Vec < PairTree >,
  requests      : Vec < (usize, NodeId) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  for (tree_idx, node_id) in requests {
    execute_definitive_view_request (
      forest, tree_idx, node_id, config, typedb_driver,
      visited, errors ) . await ?; }
  Ok (( )) }

/// Expands a definitive view request.
///
/// This function:
/// 1. 'Indefinitizes' any conflicting (same ID) OrgNode
/// 2. Marks this OrgNode definitive
/// 3. Expands its content from disk using BFS
/// 4. Removes its ViewRequest::Definitive
async fn execute_definitive_view_request (
  forest        : &mut Vec < PairTree >,
  tree_idx      : usize,
  node_id       : NodeId, // had the request
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  _errors       : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree (
    &forest[tree_idx], node_id ) ?;
  if let Some ( &(existing_tree_idx, existing_node_id) ) =
    visited . get ( & node_pid )
  { if ( existing_tree_idx != tree_idx ||
         existing_node_id != node_id ) {
    // indefinitize the previous definitive view
    indefinitize_content_subtree ( &mut forest[existing_tree_idx],
                                    existing_node_id,
                                    visited ) ?; }}
  { // Mutate the root of the definitive view request:
    // Remove the ViewRequest, mark it definitive,
    // and rebuild from disk.
    let tree : &mut PairTree = &mut forest[tree_idx];
    let mut node_mut : NodeMut < (Option<SkgNode>, OrgNode) > =
      tree . get_mut ( node_id )
      . ok_or ( "execute_definitive_view_request: node not found" ) ?;
    node_mut . value () . 1 . metadata . code . viewRequests
      . remove ( & ViewRequest::Definitive );
    node_mut.value () . 1 . metadata.code.indefinitive = false;
    rebuild_pair_from_disk_mostly_clobbering_the_org (
      // preserves relevant orgnode fields
      &mut forest[tree_idx], node_id, config ) ?; }
  visited . insert ( node_pid.clone(),
                     (tree_idx, node_id) );
  extendDefinitiveSubtreeFromLeaf ( // populate its tree-descendents
    &mut forest[tree_idx], node_id, tree_idx,
    config.initial_node_limit, visited, config, typedb_driver
  ) . await ?;
  Ok (( )) }

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into content children (relToParent == Content).
///   Non-content children (AliasCol, ParentIgnores, etc.)
///   persist unchanged.
fn indefinitize_content_subtree (
  tree    : &mut PairTree,
  node_id : NodeId,
  visited : &mut VisitedMap,
) -> Result < (), Box<dyn Error> > {
  let node_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
    tree . get ( node_id )
    . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
  let (skgnode_opt, orgnode) : &(Option<SkgNode>, OrgNode) =
    node_ref . value ();
  let node_pid_opt : Option < ID > =
    orgnode . metadata . id . clone ();
  let content_child_ids : Vec < NodeId > =
    node_ref . children ()
    . filter ( |c| c . value () . 1 . metadata . code . relToParent
                   == RelToParent::Content )
    . map ( |c| c . id () )
    . collect ();
  if let Some(ref pid) = node_pid_opt { // remove from visited
    visited . remove ( pid ); }
  { // mark indefinitive, restore title if SkgNode present, clear body
    // TODO : This ought to call a function.
    // It, or something very like it anyway, happens elsewhere too.
    let canonical_title : Option<String> =
      skgnode_opt . as_ref () . map ( |s| s . title . clone () );
    let mut node_mut : NodeMut < (Option<SkgNode>, OrgNode) > =
      tree . get_mut ( node_id )
      . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
    node_mut . value () . 1 . metadata . code . indefinitive = true;
    if let Some(title) = canonical_title {
      node_mut . value () . 1 . title = title; }
    node_mut . value () . 1 . body = None; }
  for child_id in content_child_ids { // recurse
    indefinitize_content_subtree (
      tree, child_id, visited ) ?; }
  Ok (( )) }

/// Expands content for a node using BFS with a node limit.
///
/// This is similar to `render_initial_forest_bfs`
/// but operates on an existing PairTree.
///
/// The starting node is already definitive. This function:
/// - Reads content children from the SkgNode in the tree
/// - Adds them as children using BFS
/// - Marks repeats (nodes already in `visited`) as indefinitive
/// - Adds new definitive nodes to `visited`
/// - Truncates when the node limit is exceeded:
///   - Adds children up to the limit as indefinitive
///   - Completes the sibling group of the limit-hitting node
///   - Marks nodes after the limit-hitting node's parent as indefinitive
///
/// Generations are relative to the effective root (generation 1),
/// *not* the tree's true root. This way, truncation only affects
/// nodes within this subtree, not siblings of ancestors.
async fn extendDefinitiveSubtreeFromLeaf (
  tree           : &mut PairTree,
  effective_root : NodeId, // it contained the request
  tree_idx       : usize,
  limit          : usize,
  visited        : &mut VisitedMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  // effective_root is already in the tree and definitive.
  // Read content children from the SkgNode in the tree.
  let content_child_ids : Vec < ID > =
    collect_content_children ( tree, effective_root ) ?;
  let mut gen_with_children : Vec < (NodeId, // effective root
                                     ID) > = // one of its children
    content_child_ids
    . into_iter ()
    . map ( |child_id| (effective_root, child_id) )
    . collect ();
  let mut nodes_rendered : usize = 0;
  let mut generation : usize = 2; // children of effective root
  while ! gen_with_children . is_empty () {
    if nodes_rendered + gen_with_children . len () >= limit {
      // Limit hit. Complete this sibling group
      // and truncate later parents.
      add_last_generation_and_edit_previous (
        tree, tree_idx, generation, &gen_with_children,
        limit - nodes_rendered,
        effective_root,
        visited, config, driver ). await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_nid, child_id) in gen_with_children {
      let new_node_id : NodeId =
        add_child_to_definitive_branch (
          tree, tree_idx, parent_nid, &child_id,
          ForceIndefinitive ( false ),
          visited, config, driver ). await ?;
      nodes_rendered += 1;

      // If the new node is definitive, collect its children for next gen
      let is_indefinitive : bool = {
        let node_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
          tree . get ( new_node_id )
          . ok_or ( "extendDefinitiveSubtreeFromLeaf: new node not found" ) ?;
        node_ref . value () . 1 . metadata . code . indefinitive };
      if ! is_indefinitive {
        // Read content children from the SkgNode we just added
        let grandchild_ids : Vec < ID > =
          collect_content_children ( tree, new_node_id ) ?;
        for grandchild_id in grandchild_ids {
          next_gen . push ( (new_node_id, grandchild_id) ); }}}
    gen_with_children = next_gen;
    generation += 1; }

  Ok (( )) }

/// Returns the generation (depth) of a node relative to an effective root.
/// The effective root is generation 1.
/// If effective_root is None, uses the true tree root.
/// Returns None if effective_root is Some but node_id is not a descendant of it
/// (nor the effective root itself).
#[allow(dead_code)] // May be useful for debugging
fn node_generation (
  tree           : &PairTree,
  node_id        : NodeId,
  effective_root : Option < NodeId >,
) -> Result < Option < usize >, Box<dyn Error> > {
  let mut depth : usize = 1;
  let mut current : Option < NodeRef < (Option<SkgNode>, OrgNode) > > =
    Some ( tree . get ( node_id )
           . ok_or ( "node_generation: NodeId not in tree" ) ? );
  while let Some ( node ) = current {
    if Some ( node.id () ) == effective_root {
      return Ok ( Some ( depth ) ); }
    depth += 1;
    current = node . parent (); }
  if effective_root . is_none () {
    Ok ( Some ( depth - 1 ) ) // -1 because we incremented once after reaching the true root
  } else {
    Ok ( None ) }}

/// Add a single child during BFS expansion.
/// If `force_indefinitive.0` is true, the node is always indefinitive.
/// Otherwise, marks as indefinitive only if it's a repeat or cycle.
/// Returns the new node's NodeId.
///
/// Note: This function still fetches from disk because the child
/// doesn't exist in the tree yet. The SkgNode is stored in the
/// paired tree for future reference.
async fn add_child_to_definitive_branch (
  tree               : &mut PairTree,
  tree_idx           : usize,
  parent_nid         : NodeId,
  child_id           : &ID,
  force_indefinitive : ForceIndefinitive,
  visited            : &mut VisitedMap,
  config             : &SkgConfig,
  driver             : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let (skgnode, mut orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id ( config, driver, child_id )
    . await ?;
  let is_repeat : bool = visited . contains_key ( child_id );
  let is_cycle : bool =
    is_ancestor_id ( tree, parent_nid, child_id,
                     |n| n . 1 . metadata . id . as_ref () ) ?;
  let should_be_indefinitive : bool =
    force_indefinitive.0 || is_repeat || is_cycle;
  if should_be_indefinitive {
    orgnode . metadata . code . indefinitive = true;
    orgnode . body = None;
  } else {
    orgnode . metadata . code . indefinitive = false; }
  orgnode . metadata . viewData . cycle = is_cycle;
  let new_node_id : NodeId = {
    let mut parent_mut : NodeMut < (Option<SkgNode>, OrgNode) > =
      tree . get_mut ( parent_nid )
      . ok_or ( "add_child_to_definitive_branch: parent not found" ) ?;
    parent_mut . append ( (Some(skgnode), orgnode) ) . id () };
  if ! should_be_indefinitive {
    visited . insert ( child_id . clone(),
                       (tree_idx, new_node_id) ); }
  Ok ( new_node_id ) }

/// Add children with truncation when limit is exceeded.
/// - Adds children up to `space_left` as indefinitive nodes
/// - Completes the sibling group of the limit-hitting node
/// - Marks nodes after the limit-hitting node's parent as indefinitive
/// Generation is relative to effective_root (which is generation 1).
async fn add_last_generation_and_edit_previous (
  tree           : &mut PairTree,
  tree_idx       : usize,
  generation     : usize,
  children       : &[(NodeId, ID)],
  space_left     : usize,
  effective_root : NodeId, // it had the definitive view request
  visited        : &mut VisitedMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  if space_left < 1 { // shouldn't happen
    return Ok (( )); }
  let last_addable_index : usize =
    min ( space_left, children . len () ) - 1;
  let limit_parent_id : NodeId =
    children [last_addable_index] . 0;
  for (idx, (parent_nid, child_id))
    in children . iter () . enumerate () {
      if ( idx > last_addable_index && // past limit
           *parent_nid != limit_parent_id ) // in new sibling group
      { break; }
      else {
        add_child_to_definitive_branch (
          tree, tree_idx, *parent_nid, child_id,
          ForceIndefinitive ( true ), // important
          visited, config, driver ) . await ?; }}
  truncate_after_node_in_generation_in_tree (
    tree, generation - 1, limit_parent_id,
    effective_root, visited ) ?;
  Ok (( )) }

/// Fetches fresh (SkgNode, OrgNode) from disk
/// and replaces both in the tree.
/// Preserves OrgnodeCode fields.
/// Replaces all other OrgNode data.
fn rebuild_pair_from_disk_mostly_clobbering_the_org (
  tree    : &mut PairTree,
  node_id : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (pid, source, code) = {
    // Extract values to preserve from existing orgnode
    let node_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
      tree . get ( node_id )
      . ok_or ( "rebuild_pair_from_disk_mostly_clobbering_the_org: node not found" ) ?;
    let orgnode : &OrgNode = & node_ref . value () . 1;
    let pid : ID = orgnode . metadata . id . clone ()
      . ok_or ( "rebuild_pair_from_disk_mostly_clobbering_the_org: node has no ID" ) ?;
    let source : String = orgnode . metadata . source . clone ()
      . ok_or ( "rebuild_pair_from_disk_mostly_clobbering_the_org: node has no source" ) ?;
    ( pid,
      source,
      orgnode . metadata . code . clone () ) };
  let (skgnode, mut orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_pid_and_source (
      config, &pid, &source ) ?;
  orgnode . metadata . code = code;
  let mut node_mut : NodeMut < (Option<SkgNode>, OrgNode) > = (
    // Replace node value in place
    tree . get_mut ( node_id )
      . ok_or ( "rebuild_pair_from_disk_mostly_clobbering_the_org: node not found" )) ?;
  * node_mut . value () = ( Some ( skgnode ), orgnode );
  Ok (( )) }
