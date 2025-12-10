/// Shared BFS rendering logic for content views.
///
/// This module provides common functionality used by both:
/// - `initial_view_bfs.rs` (rendering initial content views)
/// - `definitive_branch.rs` (expanding definitive view requests)

use crate::media::tree::collect_generation_ids;
use crate::to_org::util::{
  content_ids_from_skgnode,
  skgnode_and_orgnode_from_id };
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;
use crate::types::orgnode::OrgNode;
use crate::types::trees::PairTree;

use ego_tree::{NodeId, NodeRef, NodeMut};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Mark a node as indefinitive and clear its body.
pub fn rewrite_to_indefinitive (
  tree    : &mut PairTree,
  node_id : NodeId,
) -> Result < (), Box<dyn Error> > {
  let mut node_mut : NodeMut < (Option<SkgNode>, OrgNode) > =
    tree . get_mut ( node_id )
    . ok_or ( "rewrite_to_indefinitive: node not found" ) ?;
  node_mut . value () . 1 . metadata . code . indefinitive = true;
  node_mut . value () . 1 . body = None;
  Ok (( )) }

/// Collect content child IDs from a node in a PairTree.
/// Returns empty vec if the node is indefinitive or has no SkgNode.
pub fn collect_content_children (
  tree    : &PairTree,
  node_id : NodeId,
) -> Result < Vec < ID >, Box<dyn Error> > {
  let node_ref : NodeRef < (Option<SkgNode>, OrgNode) > =
    tree . get ( node_id )
    . ok_or ( "collect_content_children: node not found" ) ?;
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

/// Fetch a node from disk and append it as a child.
/// Returns the new node's NodeId and the OrgNode (for further mutation).
pub async fn fetch_and_append_child_pair (
  tree      : &mut PairTree,
  parent_id : NodeId,
  child_id  : &ID,
  config    : &SkgConfig,
  driver    : &TypeDBDriver,
) -> Result < NodeId, Box<dyn Error> > {
  let (child_skgnode, child_orgnode) : (SkgNode, OrgNode) =
    skgnode_and_orgnode_from_id ( config, driver, child_id ) . await ?;
  let mut parent_mut : NodeMut < (Option<SkgNode>, OrgNode) > =
    tree . get_mut ( parent_id )
    . ok_or ( "fetch_and_append_child_pair: parent not found" ) ?;
  let child_node_id : NodeId =
    parent_mut . append ( (Some(child_skgnode), child_orgnode) ) . id ();
  Ok ( child_node_id ) }
