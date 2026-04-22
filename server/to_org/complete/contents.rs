use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::memory::skgnode_from_memory_or_disk;
use crate::types::viewnode::ViewNode;
use crate::types::tree::viewnode_skgnode::{ pid_and_source_from_treenode, write_at_truenode_in_tree };

use ego_tree::{NodeId, Tree};
use std::error::Error;

/// PURPOSE: Given an indefinitive node N,
/// reads memory-or-disk to:
/// - Reset title.
/// - Reset source.
///
/// EXPECTS: The input node is indefinitive.
pub fn clobberIndefinitiveViewnode (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {
  let (node_id, source) : (ID, SourceName) =
    pid_and_source_from_treenode (
      tree, treeid, "clobberIndefinitiveViewnode" ) ?;
  let skgnode : NodeComplete =
    skgnode_from_memory_or_disk ( config, &node_id, &source ) ?;
  let title : String = skgnode . title . clone();
  let source : SourceName = skgnode . source . clone();
  write_at_truenode_in_tree ( tree, treeid, |t| {
    t . title = title;
    t . source = source; }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;

  Ok (( )) }
