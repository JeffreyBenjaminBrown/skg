use crate::types::misc::{ID, SkgConfig, RepoName};
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_repo;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::viewnode::ViewNode;
use crate::types::tree::viewnode_nodecomplete::{ pid_and_repo_from_treenode, write_at_activeNode_in_tree };

use ego_tree::{NodeId, Tree};
use std::error::Error;

/// PURPOSE: Given a write-protected node N,
/// reads in-Rust-graph-or-disk to:
/// - Reset title.
/// - Reset repo.
///
/// EXPECTS: The input node is write-protected.
pub fn clobberWriteProtectedViewnode (
  tree    : &mut Tree<ViewNode>,
  treeid  : NodeId,
  graph   : &InRustGraph,
  config  : &SkgConfig,
) -> Result < (), Box<dyn Error> > {

  let (node_id, repo) : (ID, RepoName) =
    pid_and_repo_from_treenode (
      tree, treeid, "clobberWriteProtectedViewnode" ) ?;
  let nodecomplete : NodeComplete =
    nodecomplete_rustFirst_by_pid_and_repo (
      graph, config, &node_id, &repo ) ?;
  let title : String = nodecomplete . title . clone();
  let repo : RepoName = nodecomplete . home_repo . clone();
  write_at_activeNode_in_tree (
    tree, treeid,
    |t| { t . title = title;
          t . home_repo = repo; }
  ) . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
  Ok (( )) }
