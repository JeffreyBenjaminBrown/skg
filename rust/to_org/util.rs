use crate::media::file_io::read_skgnode;
use crate::media::tree::collect_generation_ids;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::to_org::complete::contents::{ clobberIndefinitiveOrgnode, maybe_add_subscribee_col };
use crate::types::orgnode::{default_metadata, Interp, ViewRequest};
use crate::types::trees::{NodePair, PairTree};
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::util::path_from_pid_and_source;

use ego_tree::{Tree, NodeId, NodeMut, NodeRef};
use ego_tree::iter::Edge;
use std::collections::HashMap;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;


/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: NodeId within the forest tree
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type VisitedMap =
  HashMap < ID, NodeId >;


// ======================================================
// ForestRoot utilities
// ======================================================

/// Create a NodePair representing a ForestRoot.
/// It is never rendered; it just makes forests easier to process.
pub fn forest_root_pair () -> NodePair {
  let mut md = default_metadata ();
  md . code . interp = Interp::ForestRoot;
  NodePair { mskgnode: None,
             orgnode: OrgNode { metadata: md,
                                title: String::new (),
                                body: None }} }

/// Create a new forest (a tree with a ForestRoot root).
/// The "tree roots" will be children of this root.
pub fn new_forest () -> PairTree {
  Tree::new ( forest_root_pair () ) }

/// Check if a node is a ForestRoot.
pub fn is_forest_root (
  tree    : &PairTree,
  node_id : NodeId,
) -> bool {
  tree . get ( node_id )
    . map ( |node_ref|
             node_ref . value () . orgnode . metadata . code . interp
             == Interp::ForestRoot )
    . unwrap_or ( false ) }


// ======================================================
// Fetching, building and modifying SkgNodes and OrgNodes
// ======================================================

/// Fetch a SkgNode from disk (queries TypeDB for source).
/// Make an OrgNode from it, with validated title.
/// Return both.
pub async fn skgnode_and_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  skg_id : &ID,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, skg_id).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", skg_id ))?;
  skgnode_and_orgnode_from_pid_and_source (
    config, &pid_resolved, &source ) }

/// Fetch a SkgNode from disk given PID and source.
/// Make an OrgNode from it, with validated title.
/// Return both.
pub fn skgnode_and_orgnode_from_pid_and_source (
  config : &SkgConfig,
  pid    : &ID,
  source : &str,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let path : String =
    path_from_pid_and_source (
      config, source, pid.clone() );
  let mut skgnode : SkgNode = read_skgnode ( path ) ?;
  skgnode.source = source.to_string();
  let orgnode : OrgNode = OrgNode {
    metadata : { let mut md = default_metadata ();
                 md . id = Some ( pid . clone () );
                 md . source = Some ( source . to_string () );
                 md },
    title : ( & skgnode . title ) . replace ( '\n', " " ),
    body  : skgnode . body . clone (), };
  if orgnode . title . is_empty () { // Validate title
    return Err ( Box::new ( io::Error::new (
      io::ErrorKind::InvalidData,
      format! ( "SkgNode with ID {} has an empty title",
                 pid ), )) ); }
  Ok (( skgnode, orgnode )) }

/// Create an OrgNode with a given Interp and title.
/// Body is set to None, and all other metadata fields use defaults.
pub fn orgnode_from_title_and_rel (
  rel: Interp,
  title: String
) -> OrgNode {
  let mut md = default_metadata ();
  md . code . interp = rel;
  OrgNode { metadata: md, title, body: None }}

/// Set 'indefinitive' to true,
/// reset title and source,
/// and set body to None.
pub fn makeIndefinitiveAndClobber (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  let mut node_mut : NodeMut < NodePair > =
    tree . get_mut ( node_id )
    . ok_or ( "makeIndefinitiveAndClobber: node not found" ) ?;
  node_mut . value () . orgnode . metadata . code . indefinitive = true;
  node_mut . value () . orgnode . body = None;
  Ok (( )) }

/// This function's callers add a pristine, out-of-context
/// (skgnode, orgnode) pair to the tree.
/// Integrating the pair into the tree requires more work
/// (and later will require even more, probably),
/// which this function does:
/// - handle repeats, cycles and the visited map
/// - build a subscribee branch if needed
pub async fn complete_branch_minus_content (
  tree     : &mut PairTree,
  node_id  : NodeId,
  visited  : &mut VisitedMap,
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  mark_if_visited_or_repeat_or_cycle (
    tree, node_id, visited ) ?;
  maybe_add_subscribee_col (
    tree, node_id, config, driver ) . await ?;
  Ok (( )) }

/// Handles repetitions, cycles, and the VisitedMap.
/// Contains no surprising of complex logic.
/// - Check for cycle and mark viewData.cycle accordingly.
/// - If node is a repeat (already in visited), mark it indefinitive.
/// - If node is indefinitive, rewrite it (clear body, etc.).
///   (repeat should imply indefinitive. The reverse need not hold.)
/// - If node is definitive, add it to visited.
pub fn mark_if_visited_or_repeat_or_cycle (
  tree     : &mut PairTree,
  node_id  : NodeId,
  visited  : &mut VisitedMap,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  detect_and_mark_cycle ( tree, node_id ) ?;
  if visited . contains_key ( &pid ) {
    // Mark as indefinitive (it's a repeat).
    let mut node_mut : NodeMut < NodePair > =
      tree . get_mut ( node_id )
      . ok_or ( "mark_if_visited_or_repeat_or_cycle: node not found" ) ?;
    node_mut . value () . orgnode . metadata . code . indefinitive = true; }
  let is_indefinitive : bool = {
    let node_ref : NodeRef < NodePair > =
      tree . get ( node_id )
      . ok_or ( "mark_if_visited_or_repeat_or_cycle: node not found" ) ?;
    node_ref . value () . orgnode . metadata . code . indefinitive };
  if is_indefinitive {
    clobberIndefinitiveOrgnode ( tree, node_id ) ?;
  } else {
    visited . insert ( pid, node_id ); }
  Ok (( )) }

/// Check if the node's PID appears in its ancestors,
/// and if so, mark viewData.cycle = true.
pub fn detect_and_mark_cycle (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result<(), Box<dyn Error>> {
  let pid : ID = get_pid_in_pairtree ( tree, node_id ) ?;
  let is_cycle : bool =
    is_ancestor_id ( tree, node_id, &pid ) ?;
  let mut node_mut : NodeMut < NodePair > =
    tree . get_mut ( node_id ) . ok_or (
      "detect_and_mark_cycle: node not found" ) ?;
  node_mut . value () . orgnode . metadata . viewData . cycle =
    is_cycle;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a forest (single tree with ForestRoot at root)
/// containing just "tree root" nodes (no grandchildren yet),
/// and complete each via complete_branch_minus_content.
pub async fn stub_forest_from_root_ids (
  root_skg_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
  visited  : &mut VisitedMap,
) -> Result < PairTree, Box<dyn Error> > {
  let mut forest : PairTree = new_forest ();
  let forest_root_tree_id : NodeId = forest . root () . id ();
  for root_skg_id in root_skg_ids {
    build_node_branch_minus_content (
      Some ( (&mut forest, forest_root_tree_id) ),
      root_skg_id, config, driver, visited
    ) . await ?; }
  Ok ( forest ) }

pub fn collect_ids_from_pair_tree (
  tree : &PairTree,
) -> Vec < ID > {
  let mut pids : Vec < ID > = Vec::new ();
  for edge in tree . root () . traverse () {
    if let Edge::Open ( node_ref ) = edge {
      if let Some ( ref pid ) =
        node_ref . value () . orgnode . metadata . id {
        pids . push ( pid . clone () ); }} }
  pids }

/// Check if `target_skg_id` appears in the ancestor path of `tree_id`.
/// Used for cycle detection.
pub fn is_ancestor_id (
  tree           : &PairTree,
  origin_tree_id : NodeId, // start looking from here
  target_skg_id  : &ID,    // look for this
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( origin_tree_id )
    . ok_or ( "is_ancestor_id: NodeId not in tree" ) ?;
  let mut current : Option < NodeRef < NodePair > > =
    node_ref . parent ();
  while let Some ( parent ) = current {
    if let Some ( parent_skg_id ) =
      parent . value () . orgnode . metadata . id . as_ref () {
        if parent_skg_id == target_skg_id {
          return Ok ( true ); }}
    current = parent . parent (); }
  Ok ( false ) }

/// Extract the PID from a node in a PairTree.
/// Returns an error if the node is not found or has no ID.
pub fn get_pid_in_pairtree (
  tree    : &PairTree,
  tree_id : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( tree_id )
    . ok_or ( "get_pid_in_pairtree: NodeId not in tree" ) ?;
  node_ref . value () . orgnode . metadata . id . clone ()
    . ok_or_else ( || "get_pid_in_pairtree: node has no ID"
                       . into () ) }

/// Extract the PID from a PairTree NodeRef.
/// Returns an error if the node has no ID.
/// Use this when you already have a NodeRef to avoid redundant tree lookup.
pub fn get_pid_from_pair_using_noderef (
  node_ref : &NodeRef < NodePair >,
) -> Result < ID, Box<dyn Error> > {
  node_ref . value () . orgnode . metadata . id . clone ()
    . ok_or_else ( || "get_pid_from_pair_using_noderef: node has no ID" . into () ) }

/// Build a node from disk and
/// append it at 'parent_tree_id' as a child.
/// Returns the new node's ego_tree::NodeId.
///
/// Does *not* take its ancestors into account,
/// and does *not* build any of its descendents.
/// Those can be done later via 'complete_branch_minus_content',
/// or they can all be done at once via
/// 'build_node_branch_minus_content.
pub async fn make_and_append_child_pair (
  tree           : &mut PairTree,
  parent_tree_id : NodeId, // will parent the new node
  child_skg_id   : &ID, // how to find the new node
  config         : &SkgConfig,
  driver         : &TypeDBDriver,
) -> Result < NodeId, // the new node
              Box<dyn Error> > {
  let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, child_skg_id ) . await ?;
  let mut parent_mut : NodeMut < NodePair > =
    tree . get_mut ( parent_tree_id )
    . ok_or ( "make_and_append_child_pair: parent not found" ) ?;
  let child_tree_id : NodeId =
    parent_mut . append ( NodePair { mskgnode: Some(child_skgnode),
                                     orgnode: child_orgnode } )
    . id ();
  Ok ( child_tree_id ) }

/// Builds a pair from disk, place it in a tree,
/// complete the branch it implies except for 'content' descendents,
/// and return the root of the new branch, but in its tree context:
/// - If tree_and_parent is None:
///   creates new tree, returns (Some(tree), branch_root_nodeid)
/// - If tree_and_parent is Some:
///   appends to tree, returns (None, branch_root_nodeid)
pub async fn build_node_branch_minus_content (
  tree_and_parent : Option<(&mut PairTree, NodeId)>, // if modifying an existing tree, attach as a child here
  skg_id          : &ID, // what to fetch
  config          : &SkgConfig,
  driver          : &TypeDBDriver,
  visited         : &mut VisitedMap,
) -> Result < (Option<PairTree>, NodeId), Box<dyn Error> > {
  let (skgnode, orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id (
      config, driver, skg_id ) . await ?;
  match tree_and_parent {
    Some ( (tree, parent_tree_id) ) => {
      let child_tree_id : NodeId = {
        let mut parent_mut : NodeMut < NodePair > =
          tree . get_mut ( parent_tree_id ) . ok_or (
            "build_node_branch_minus_content: parent not found" ) ?;
        parent_mut . append ( NodePair { mskgnode: Some(skgnode),
                                         orgnode } ) . id () };
      complete_branch_minus_content (
        tree, child_tree_id, visited,
        config, driver ) . await ?;
      Ok ( (None, child_tree_id) ) },
    None => {
      let mut tree : PairTree =
        Tree::new ( NodePair { mskgnode: Some(skgnode), orgnode } );
      let root_tree_id : NodeId = tree . root () . id ();
      complete_branch_minus_content (
        &mut tree, root_tree_id, visited,
        config, driver ) . await ?;
      Ok ( (Some(tree), root_tree_id) ) }, } }

/// Collect content child IDs from a node in a PairTree.
/// Returns empty vec if the node is indefinitive or has no SkgNode.
pub fn content_ids_if_definitive_else_empty (
  tree    : &PairTree,
  tree_id : NodeId,
) -> Result < Vec < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( tree_id )
    . ok_or (
      "content_ids_if_definitive_else_empty: node not found" ) ?;
  if node_ref . value () . orgnode . metadata . code . indefinitive {
    return Ok ( Vec::new () ); }
  match & node_ref . value () . mskgnode {
    Some ( skgnode ) => Ok (
      skgnode . contains . clone () . unwrap_or_default () ),
    None => Ok ( Vec::new () ), // No SkgNode yet
  }}

/// Collect ego_tree::NodeIds after
///   some member of some generation of a tree.
/// 'effective_root' should be some ancestor.
/// It affects both the meaning of generation numbers,
/// and the scope of which nodes are collected
/// (only its descendents are collected).
/// If effective root is None,
/// the true root is used as the effective root.
pub fn nodes_after_in_generation (
  tree                     : &PairTree,
  generation               : usize,
  generation_member_treeid : NodeId,
  effective_root           : Option < NodeId >,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let tree_ids_in_gen : Vec < NodeId > =
    collect_generation_ids ( tree, generation, effective_root ) ?;
  let mut result : Vec < NodeId > = Vec::new ();
  let mut found_target : bool = false;
  for tree_id in tree_ids_in_gen {
    if found_target {
      result . push ( tree_id );
    } else if tree_id == generation_member_treeid {
      found_target = true; } }
  Ok ( result ) }


// ==============================================
// Reading from SkgNodes and OrgNodes, esp. in trees
// ==============================================

/// Check if a node in a PairTree is marked indefinitive.
/// Returns an error if the node is not found.
pub fn is_indefinitive (
  tree    : &PairTree,
  tree_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( tree_id )
    . ok_or ( "is_indefinitive: NodeId not in tree" ) ?;
  Ok ( node_ref . value () . orgnode
       . metadata . code . indefinitive ) }

/// Collect all child tree NodeIds from a node in a PairTree.
/// Returns an error if the node is not found.
pub fn collect_child_tree_ids (
  tree    : &PairTree,
  tree_id : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( tree_id )
    . ok_or ( "collect_child_tree_ids: NodeId not in tree" ) ?;
  Ok ( node_ref . children () . map ( |c| c . id () ) . collect () ) }


// ==============================================
// 'remove_completed_view_request'
// ==============================================

/// Log any error and remove the request from the node.
/// Does *not* verify that the request was completed;
/// that's just the only situation in which it would be used.
pub fn remove_completed_view_request (
  tree         : &mut PairTree,
  node_id      : NodeId,
  view_request : ViewRequest,
  error_msg    : &str,
  errors       : &mut Vec < String >,
  result       : Result < (), Box<dyn Error> >,
) -> Result < (), Box<dyn Error> > {
  if let Err ( e ) = result {
    errors . push ( format! ( "{}: {}", error_msg, e )); }
  tree . get_mut ( node_id )
    . ok_or ( "remove_completed_view_request: node not found" ) ?
    . value () . orgnode . metadata . code . viewRequests
    . remove ( &view_request );
  Ok (()) }
