/// SINGLE ENTRY POINT: 'execute_view_requests'.

use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_edit_previous_in_tree;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{
  build_and_integrate_containerward_view_then_drop_request,
  build_and_integrate_sourceward_view_then_drop_request };
use crate::to_org::util::{
  skgnode_and_orgnode_from_pid_and_source,
  fetch_and_append_child_pair,
  mark_visited_or_repeat_or_cycle,
  get_pid_in_pairtree,
  VisitedMap, is_indefinitive,
  content_ids_if_definitive_else_empty };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, RelToParent, ViewRequest};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut Vec < PairTree >,
  requests      : Vec < (usize, NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  for (tree_idx, node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          &mut forest[tree_idx], node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request (
          &mut forest[tree_idx], node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request (
          &mut forest[tree_idx], node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Definitive => {
        execute_definitive_view_request (
          forest, tree_idx, node_id, config, typedb_driver,
          visited, errors ) . await ?; }, } }
  Ok (( )) }

/// Expands a definitive view request.
///
/// This function:
/// 1. 'Indefinitizes' any previously definitive OrgNode with that ID
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
    let mut node_mut : NodeMut < NodePair > =
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
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "indefinitize_content_subtree: NodeId not in tree" ) ?;
  let (skgnode_opt, orgnode) : &NodePair =
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
    let mut node_mut : NodeMut < NodePair > =
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
    content_ids_if_definitive_else_empty (
      tree, effective_root ) ?;
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
      add_last_generation_and_edit_previous_in_tree (
        tree, generation, &gen_with_children,
        limit - nodes_rendered,
        effective_root,
        visited, config, driver ). await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_nid, child_id) in gen_with_children {
      let new_node_id : NodeId =
        fetch_and_append_child_pair (
          tree, parent_nid, &child_id, config, driver ). await ?;
      mark_visited_or_repeat_or_cycle (
        tree, tree_idx, new_node_id, visited ) ?;
      nodes_rendered += 1;
      if ! is_indefinitive ( tree, new_node_id ) ? {
        let grandchild_ids : Vec < ID > =
          content_ids_if_definitive_else_empty (
            tree, new_node_id ) ?;
        for grandchild_id in grandchild_ids {
          next_gen . push ( (new_node_id, grandchild_id) ); }} }
    gen_with_children = next_gen;
    generation += 1; }
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
    let node_ref : NodeRef < NodePair > =
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
  let mut node_mut : NodeMut < NodePair > = (
    // Replace node value in place
    tree . get_mut ( node_id )
      . ok_or ( "rebuild_pair_from_disk_mostly_clobbering_the_org: node not found" )) ?;
  * node_mut . value () = ( Some ( skgnode ), orgnode );
  Ok (( )) }
