use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::skgnode::SkgNode;
use crate::types::skgnodemap::{SkgNodeMap, skgnode_from_map_or_disk};
use crate::types::viewnode::{ViewNode, ViewNodeKind};
use crate::types::tree::generic::write_at_node_in_tree;
use crate::types::tree::viewnode_skgnode::pid_and_source_from_treenode;

use ego_tree::{NodeId, Tree};
use std::error::Error;

/// PURPOSE: Given an indefinitive node N,
/// uses SkgNodeMap or lookup on disk to:
/// - Reset title.
/// - Reset source.
/// - Set body to None.
///
/// EXPECTS: The input node is indefinitive.
pub fn clobberIndefinitiveViewnode (
  tree    : &mut Tree<ViewNode>,
  map     : &mut SkgNodeMap,
  treeid  : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_id, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, treeid, "clobberIndefinitiveViewnode" ) ?;
  let skgnode : &SkgNode =
    skgnode_from_map_or_disk ( &node_id, &source, map, config ) ?;
  let title : String = skgnode . title . clone();
  let source : SourceName = skgnode . source . clone();
  write_at_node_in_tree ( tree, treeid, |viewnode| {
    let ViewNodeKind::True ( t ) : &mut ViewNodeKind =
      &mut viewnode.kind
      else { panic! (
             "clobberIndefinitiveViewnode: expected TrueNode" ) };
    t . title = title;
    t . source = source;
    t . body = None; }
  ). map_err ( |e| -> Box<dyn Error> { e.into() } ) ?;

  Ok (( )) }
