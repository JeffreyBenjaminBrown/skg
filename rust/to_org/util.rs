use crate::media::file_io::read_node;
use crate::media::tree::collect_generation_ids;
use crate::media::typedb::util::pid_and_source_from_id;
use crate::types::{SkgNode, ID, SkgConfig, OrgNode};
use crate::types::orgnode::{default_metadata, RelToParent, ViewRequest};
use crate::types::trees::{NodePair, PairTree};
use crate::util::path_from_pid_and_source;

use ego_tree::{NodeId, NodeMut, NodeRef, Tree};
use std::collections::HashMap;
use std::error::Error;
use std::io;
use typedb_driver::TypeDBDriver;


/// Tracks which IDs have been rendered definitively and where.
/// - Key: the ID that was visited
/// - Value: ( index identifying a tree in the forest,
///            NodeId within that tree )
///
/// Uses:
/// - prevent duplicate definitive expansions
/// - locate the conflict when an earlier definitive view
///   conflicts with a new definitive view request
pub type VisitedMap =
  HashMap < ID, (usize, NodeId) >;


// ======================================================
// Fetching, building and modifying SkgNodes and OrgNodes
// ======================================================

/// Fetch a SkgNode from disk (queries TypeDB for source).
/// Make an OrgNode from it, with validated title.
/// Return both.
pub async fn skgnode_and_orgnode_from_id (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  id     : &ID,
) -> Result < ( SkgNode, OrgNode ), Box<dyn Error> > {
  let (pid_resolved, source) : (ID, String) =
    pid_and_source_from_id( // Query TypeDB for them
      &config.db_name, driver, id).await?
    . ok_or_else( || format!(
      "ID '{}' not found in database", id ))?;
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
  let mut skgnode : SkgNode = read_node ( path ) ?;
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

/// Create an OrgNode with a given RelToParent and title.
/// Body is set to None, and all other metadata fields use defaults.
pub fn orgnode_from_title_and_rel (
  rel: RelToParent,
  title: String
) -> OrgNode {
  let mut md = default_metadata ();
  md . code . relToParent = rel;
  OrgNode { metadata: md, title, body: None }}

/// Mark a node as indefinitive and clear its body.
pub fn rewrite_to_indefinitive (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  let mut node_mut : NodeMut < NodePair > =
    tree . get_mut ( node_id )
    . ok_or ( "rewrite_to_indefinitive: node not found" ) ?;
  node_mut . value () . 1 . metadata . code . indefinitive = true;
  node_mut . value () . 1 . body = None;
  Ok (( )) }


// ==============================================
// Reading and manipulating trees, esp. via IDs
// ==============================================

/// Create a minimal forest containing just root nodes (no children).
pub async fn stub_forest_from_root_ids (
  root_ids : &[ID],
  config   : &SkgConfig,
  driver   : &TypeDBDriver,
) -> Result < Vec < PairTree >,
              Box<dyn Error> > {
  let mut forest : Vec < PairTree > =
    Vec::new ();
  for root_id in root_ids {
    let (root_skgnode, root_orgnode) : ( SkgNode, OrgNode ) =
      skgnode_and_orgnode_from_id (
        config, driver, root_id ) . await ?;
    let tree : PairTree =
      Tree::new ( (Some(root_skgnode), root_orgnode) );
    forest . push ( tree ); }
  Ok ( forest ) }

/// Collect all PIDs from a forest of OrgNode trees.
pub fn collect_ids_from_forest (
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

/// Check if `target_id` appears in the ancestor path of `node_id`.
/// Used for cycle detection.
///
/// The `get_id` closure extracts an `Option<&ID>` from a node value,
/// allowing this to work with different tree types:
/// - `Tree<OrgNode>`: `|n| n.metadata.id.as_ref()`
/// - `Tree<(SkgNode, OrgNode)>`: `|n| n.1.metadata.id.as_ref()`
pub fn is_ancestor_id < T, F > (
  tree      : &Tree < T >,
  node_id   : NodeId,
  target_id : &ID,
  get_id    : F,
) -> Result < bool, Box<dyn Error> >
where F : Fn ( &T ) -> Option < &ID >
{
  let node_ref : NodeRef < T > =
    tree . get ( node_id )
    . ok_or ( "is_ancestor_id: NodeId not in tree" ) ?;
  let mut current : Option < NodeRef < T > > =
    node_ref . parent ();
  while let Some ( parent ) = current {
    if let Some ( parent_id ) = get_id ( parent . value () ) {
      if parent_id == target_id {
        return Ok ( true ); }}
    current = parent . parent (); }
  Ok ( false ) }

/// Extract the PID from a node in a PairTree.
/// Returns an error if the node is not found or has no ID.
pub fn get_pid_in_pairtree (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < ID, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "get_pid_in_pairtree: NodeId not in tree" ) ?;
  node_ref . value () . 1 . metadata . id . clone ()
    . ok_or_else ( || "get_pid_in_pairtree: node has no ID"
                       . into () ) }

/// Extract the PID from a PairTree NodeRef.
/// Returns an error if the node has no ID.
/// Use this when you already have a NodeRef to avoid redundant tree lookup.
pub fn get_pid_from_pair_using_noderef (
  node_ref : &NodeRef < NodePair >,
) -> Result < ID, Box<dyn Error> > {
  node_ref . value () . 1 . metadata . id . clone ()
    . ok_or_else ( || "get_pid_from_pair_using_noderef: node has no ID" . into () ) }

/// Fetch a node from disk and append it as a child.
/// Returns the new node's NodeId.
pub async fn fetch_and_append_child_pair (
  tree      : &mut PairTree,
  parent_id : NodeId,
  child_id  : &ID,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id ( config, driver, child_id ) . await ?;
  let mut parent_mut : NodeMut < NodePair > =
    tree . get_mut ( parent_id )
    . ok_or ( "fetch_and_append_child_pair: parent not found" ) ?;
  let child_node_id : NodeId =
    parent_mut . append ( (Some(child_skgnode), child_orgnode) ) . id ();
  Ok ( child_node_id ) }

/// Collect content child IDs from a node in a PairTree.
/// Returns empty vec if the node is indefinitive or has no SkgNode.
pub fn content_ids_if_definitive_else_empty (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < Vec < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or (
      "content_ids_if_definitive_else_empty: node not found" ) ?;
  if node_ref . value () . 1 . metadata . code . indefinitive {
    return Ok ( Vec::new () ); }
  match & node_ref . value () . 0 {
    Some ( skgnode ) => Ok ( content_ids_from_skgnode ( skgnode ) ),
    None => Ok ( Vec::new () ), // No SkgNode yet
  } }

/// Collect NodeIds after a target node in a generation.
/// 'effective_root' should be some ancestor.
/// It affects both the meaning of generation numbers,
/// and the scope of which nodes are collected
/// (only its descendents are collected).
/// If effective root is None,
/// the true root is used as the effective root.
pub fn nodes_after_in_generation (
  tree           : &PairTree,
  generation     : usize,
  after_node     : NodeId,
  effective_root : Option < NodeId >,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let nodes_in_gen : Vec < NodeId > =
    collect_generation_ids ( tree, generation, effective_root ) ?;
  let mut result : Vec < NodeId > = Vec::new ();
  let mut found_target : bool = false;
  for id in nodes_in_gen {
    if found_target {
      result . push ( id );
    } else if id == after_node {
      found_target = true; } }
  Ok ( result ) }


// ==============================================
// Reading from SkgNodes and OrgNodes, esp. in trees
// ==============================================

/// Check if a node in a PairTree is marked indefinitive.
/// Returns an error if the node is not found.
pub fn is_indefinitive (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < bool, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "is_indefinitive: NodeId not in tree" ) ?;
  Ok ( node_ref . value () . 1 . metadata . code . indefinitive ) }

/// Extract the content child IDs from an SkgNode.
/// Returns an empty Vec if there are no contents.
pub fn content_ids_from_skgnode (
  skgnode : &SkgNode
) -> Vec < ID > {
  skgnode . contains . clone () . unwrap_or_default () }

/// Collect all child NodeIds from a node in a PairTree.
/// Returns an error if the node is not found.
pub fn collect_child_ids (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < Vec < NodeId >, Box<dyn Error> > {
  let node_ref : NodeRef < NodePair > =
    tree . get ( node_id )
    . ok_or ( "collect_child_ids: NodeId not in tree" ) ?;
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
    . value () . 1 . metadata . code . viewRequests
    . remove ( &view_request );
  Ok (()) }
