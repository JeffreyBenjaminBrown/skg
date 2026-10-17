use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::complete_validation::validated_graph;
use crate::git_ops::git_snapshot::{
  owned_head_identity, read_owned_head_graph_snapshot_maybe_cached};
use crate::git_ops::git_snapshot_types::GraphSnapshot;
use crate::graph_pair::types::GraphPair;
use crate::telescope::invariants::TelescopeViolation;
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::Graphnode;

use std::sync::Arc;

impl GraphPair {
  /// Read the before-graph from HEAD and pair it with the live graph.
  /// Refuses when an owned skgrepo is not in a gitrepo or has no HEAD
  /// commit, and when HEAD fails the usual whole-graph validation
  /// (e.g. a duplicate ID).
  pub fn from_head_and_live (
    config : &SkgConfig,
    live   : Arc<InRustGraph>,
  ) -> Result<GraphPair, String> {
    GraphPair::from_head_identity_and_live (
      config, owned_head_identity (config) ?, live ) }

  fn from_head_identity_and_live (
    config        : &SkgConfig,
    head_identity : String,
    live          : Arc<InRustGraph>,
  ) -> Result<GraphPair, String> {
    let git_snapshot : GraphSnapshot =
      read_owned_head_graph_snapshot_maybe_cached (
        config, &head_identity ) ?;
    let nodes : Vec<Graphnode> =
      git_snapshot . nodes . into_values () . collect ();
    let (before, _telescope_warnings)
      : (InRustGraph, Vec<(ID, TelescopeViolation)>) =
      validated_graph (config, &nodes)
      . map_err ( |e| format! ("At HEAD: {}", e) ) ?;
    Ok ( GraphPair::new ( Arc::new (before), live, head_identity )) }

  /// The same before-graph, paired with a newer live graph.
  pub fn with_after (
    &self,
    live : Arc<InRustGraph>,
  ) -> GraphPair {
    GraphPair::new (
      self . before . clone (), live, self . head_identity . clone () ) }
}

/// The graph pair held in 'slot' (a connection's
/// 'ViewsState::graph_pair'), brought up to date: rebuilt when an
/// owned skgrepo's HEAD moved (which a git-commit does), re-paired
/// when the live graph changed, otherwise reused.
pub fn current_graph_pair_in_slot (
  slot   : &mut Option<Arc<GraphPair>>,
  config : &SkgConfig,
  live   : Arc<InRustGraph>,
) -> Result<Arc<GraphPair>, String> {
  let head_identity : String =
    owned_head_identity (config) ?;
  let pair : Arc<GraphPair> =
    match slot . as_ref () {
      Some (pair) if pair . head_identity == head_identity
                  && Arc::ptr_eq (&pair . after, &live) =>
        pair . clone (),
      Some (pair) if pair . head_identity == head_identity =>
        Arc::new ( pair . with_after (live) ),
      _ =>
        Arc::new ( GraphPair::from_head_identity_and_live (
          config, head_identity, live ) ? ), };
  *slot = Some (pair . clone ());
  Ok (pair) }
