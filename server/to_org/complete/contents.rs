use crate::types::misc::{ID, SkgConfig, SkgRepoName};
use crate::types::nodes::complete::Graphnode;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::viewnode::Viewnode;
use crate::types::tree::viewnode_graphnode::{ pid_and_skgrepo_from_viewnode_at, write_at_unrestrictedVognode_in_tree };

use ego_tree::{NodeId, Tree};
use std::error::Error;

/// PURPOSE: Given a write-protected node N,
/// reads in-Rust-graph-or-disk to:
/// - Reset title.
/// - Reset skgrepo.
///
/// EXPECTS: The input node is write-protected.
pub fn clobberWriteProtectedViewnode (
  tree    : &mut Tree<Viewnode>,
  treeid  : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {

  let (node_id, skgrepo) : (ID, SkgRepoName) =
    pid_and_skgrepo_from_viewnode_at (
      tree, treeid, "clobberWriteProtectedViewnode" ) ?;
  let graphnode : Graphnode =
    graphnode_graphFirst_by_pid_and_skgrepo (
      graph, config, &node_id, &skgrepo ) ?;
  let title : String = graphnode . title . clone();
  let skgrepo : SkgRepoName = graphnode . home_skgrepo . clone();
  write_at_unrestrictedVognode_in_tree (
    tree, treeid,
    |t| { t . title = title;
          t . home_skgrepo = skgrepo; }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
