/// SINGLE ENTRY POINT: 'execute_view_requests'.

use crate::to_org::render::truncate_after_node_in_gen::add_last_generation_and_truncate_some_of_previous;
use crate::to_org::expand::aliases::build_and_integrate_aliases_view_then_drop_request;
use crate::to_org::expand::backpath::{
  build_and_integrate_containerward_view_then_drop_request,
  build_and_integrate_sourceward_view_then_drop_request };
use crate::to_org::util::{
  skgnode_and_orgnode_from_pid_and_source,
  build_node_branch_minus_content,
  get_pid_in_pairtree,
  VisitedMap, is_indefinitive,
  content_ids_if_definitive_else_empty };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::{OrgNode, Interp, ViewRequest};
use crate::types::trees::{NodePair, PairTree};

use ego_tree::{NodeId, NodeMut, NodeRef};
use std::collections::HashSet;
use std::error::Error;
use typedb_driver::TypeDBDriver;

pub async fn execute_view_requests (
  forest        : &mut PairTree,
  requests      : Vec < (NodeId, ViewRequest) >,
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  errors        : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  for (node_id, request) in requests {
    match request {
      ViewRequest::Aliases => {
        build_and_integrate_aliases_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Containerward => {
        build_and_integrate_containerward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Sourceward => {
        build_and_integrate_sourceward_view_then_drop_request (
          forest, node_id, config, typedb_driver, errors )
          . await ?; },
      ViewRequest::Definitive => {
        execute_definitive_view_request (
          forest, node_id, config, typedb_driver,
          visited, errors ) . await ?; }, } }
  Ok (( )) }

/// Expands a definitive view request.
///
/// This function:
/// 1. 'Indefinitizes' any previously definitive OrgNode with that ID
/// 2. Marks this OrgNode definitive
/// 3. Expands its content from disk using BFS
/// 4. Removes its ViewRequest::Definitive
///
/// For Subscribee nodes: the subscriber's 'hides_from_its_subscriptions'
/// list is used to filter out content that should be hidden.
async fn execute_definitive_view_request (
  forest        : &mut PairTree, // "forest" = tree with ForestRoot
  node_id       : NodeId, // had the request
  config        : &SkgConfig,
  typedb_driver : &TypeDBDriver,
  visited       : &mut VisitedMap,
  _errors       : &mut Vec < String >,
) -> Result < (), Box<dyn Error> > {
  let node_pid : ID = get_pid_in_pairtree (
    forest, node_id ) ?;
  let hidden_ids : HashSet < ID > =
    // If node is a subscribee, we may need to hide some content.
    get_hidden_ids_if_subscribee ( forest, node_id ) ?;
  if let Some ( &preexisting_node_id ) =
    visited . get ( & node_pid )
  { if preexisting_node_id != node_id {
    indefinitize_content_subtree ( forest,
                                   preexisting_node_id,
                                   visited ) ?; }}
  { // Mutate the root of the definitive view request:
    // Remove the ViewRequest, mark it definitive,
    // and rebuild from disk.
    let mut node_mut : NodeMut < NodePair > =
      forest . get_mut ( node_id )
      . ok_or ( "execute_definitive_view_request: node not found" ) ?;
    node_mut . value () . 1 . metadata . code . viewRequests
      . remove ( & ViewRequest::Definitive );
    node_mut.value () . 1 . metadata.code.indefinitive = false;
    rebuild_pair_from_disk_mostly_clobbering_the_org (
      // preserves relevant orgnode fields
      forest, node_id, config ) ?; }
  visited . insert ( node_pid.clone(), node_id );
  extendDefinitiveSubtreeFromLeaf ( // populate its tree-descendents
    forest, node_id,
    config.initial_node_limit, visited, config, typedb_driver,
    &hidden_ids,
  ) . await ?;
  Ok (( )) }

/// If the node is a Subscribee (child of SubscribeeCol, grandchild of Subscriber),
/// return the Subscriber's 'hides_from_its_subscriptions' as a HashSet.
/// Otherwise return an empty set.
fn get_hidden_ids_if_subscribee (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < HashSet < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "get_hidden_ids_if_subscribee: node not found" ) ?;
  let interp : &Interp =
    & node_ref . value () . 1 . metadata . code . interp;
  if *interp != Interp::Subscribee {
    return Ok ( HashSet::new () ); }
  else {
    let subscribee_col : NodeRef < NodePair > =
      node_ref . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: Subscribee has no parent (SubscribeeCol)" ) ?;
    let subscriber : NodeRef < NodePair > =
      subscribee_col . parent ()
      . ok_or ( "get_hidden_ids_if_subscribee: SubscribeeCol has no parent (Subscriber)" ) ?;
    let hidden_ids : HashSet < ID > =
      match & subscriber . value () . 0 {
        Some ( skgnode ) =>
          skgnode . hides_from_its_subscriptions . clone ()
          . unwrap_or_default () . into_iter () . collect (),
        None => HashSet::new (), };
    Ok ( hidden_ids ) }}

/// Does two things:
/// - Mark a node, and its entire content subtree, as indefinitive.
/// - Remove them from `visited`.
/// Only recurses into content children (interp == Content).
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
    . filter ( |c| c . value () . 1 . metadata . code . interp
                   == Interp::Content )
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
///   - Adds children in that last gen up to the limit as indefinitive
///   - Completes the sibling group of the limit-hitting node
///   - After limit-hitting node's parent, rewrite nodes in that second-to-last gen as indefinitive
///
/// Generations are relative to the effective root (generation 0),
/// *not* the tree's true root. This way, truncation only affects
/// nodes within this subtree, not siblings of ancestors.
///
/// `hidden_ids` contains IDs to filter from a subscribee's
/// top-level children (without recursing into grandchildren, etc.)
/// If the view was requested from a non-subscribee,
/// 'hidden_ids' will be empty.
async fn extendDefinitiveSubtreeFromLeaf (
  tree           : &mut PairTree,
  effective_root : NodeId, // it contained the request, is already in the tree, and although it was indefinitive when the request was issued, it was made definitive by 'execute_definitive_view_request'.
  limit          : usize,
  visited        : &mut VisitedMap,
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
  hidden_ids     : &HashSet < ID >,
) -> Result < (), Box<dyn Error> > {
  let mut gen_with_children : Vec < (NodeId, // effective root
                                     ID) > = // one of its children
    content_ids_if_definitive_else_empty (
      tree, effective_root ) ?
    . into_iter ()
    . filter ( |id| ! hidden_ids . contains ( id ) )
    . map ( |child_id| (effective_root, child_id) )
    . collect ();
  let mut nodes_rendered : usize = 0;
  let mut generation : usize = 1; // children of effective root
  while ! gen_with_children . is_empty () {
    if nodes_rendered + gen_with_children . len () >= limit {
      // Limit hit. Complete this sibling group
      // and truncate later parents.
      add_last_generation_and_truncate_some_of_previous (
        tree, generation, &gen_with_children,
        limit - nodes_rendered,
        effective_root,
        visited, config, driver ). await ?;
      return Ok (( )); }
    let mut next_gen : Vec < (NodeId, ID) > = Vec::new ();
    for (parent_nid, child_id) in gen_with_children {
      let (_tree, new_node_id) =
        build_node_branch_minus_content (
          Some((tree, parent_nid)),
          &child_id, config, driver, visited ). await ?;
      nodes_rendered += 1;
      if ! is_indefinitive ( tree, new_node_id ) ? {
        // No filtering here; 'hidden_ids' only applies to top-level.
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
