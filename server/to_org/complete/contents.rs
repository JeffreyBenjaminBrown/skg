use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;
use crate::types::viewnode::ViewNode;
use crate::types::tree::viewnode_nodecomplete::{ pid_and_source_from_treenode, write_at_truenode_in_tree };

use ego_tree::{NodeId, Tree};
use std::error::Error;

/// PURPOSE: Given an indefinitive node N,
/// reads in-Rust-graph-or-disk to:
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
  let nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_source ( config, &node_id, &source ) ?;
  let title : String = nodecomplete . title . clone();
  let source : SourceName = nodecomplete . source . clone();
  write_at_truenode_in_tree ( tree, treeid, |t| {
    t . title = title;
    t . source = source; }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
