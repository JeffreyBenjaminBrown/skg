//! The in-Rust partial projection of the graph.
//!
//! A single 'Graph' value holds every node (as a 'NodeRust') the
//! render / save pipeline can read from. It lives behind an
//! 'ArcSwap' so readers never block writers and writers never
//! block readers — writers clone via 'im''s structural sharing
//! (O(log n) per mutation) and atomically publish a new snapshot.
//!
//! See /home/ubuntu/.claude/plans/many-and-better-node-types.org
//! (Plan C) for the overall design.

use arc_swap::ArcSwap;
use std::sync::Arc;

use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::rust::NodeRust;

/// The Tier A in-memory projection of the graph.
///
/// Values are 'NodeRust' — everything a 'NodeComplete' has except
/// 'misc' (which no in-memory consumer reads today).
#[derive(Clone, Debug)]
pub struct Graph {
  pub nodes: im::HashMap<ID, NodeRust>,
}

impl Graph {
  pub fn new () -> Self {
    Graph { nodes: im::HashMap::new () } }

  /// Build from a slice of NodeCompletes. Typically called at
  /// startup after reading all .skg files from disk.
  pub fn from_completes (completes: &[NodeComplete]) -> Self {
    let mut nodes : im::HashMap<ID, NodeRust> =
      im::HashMap::new ();
    for c in completes {
      let node_rust : NodeRust = NodeRust::from (c);
      nodes . insert ( node_rust . pid . clone (), node_rust ); }
    Graph { nodes } }

  pub fn get (&self, pid: &ID) -> Option<&NodeRust> {
    self . nodes . get (pid) }

  pub fn len (&self) -> usize {
    self . nodes . len () }
}

/// Server-wide handle to the shared graph. Readers call
/// '.load_full()' to snap a consistent 'Arc<Graph>'; writers
/// build a new 'Arc<Graph>' (using 'im''s cheap clone +
/// structural-sharing mutations) and '.store()' it atomically.
pub type GraphHandle = Arc<ArcSwap<Graph>>;

/// Construct a fresh handle wrapping the given graph.
pub fn new_handle (graph: Graph) -> GraphHandle {
  Arc::new ( ArcSwap::from ( Arc::new (graph) )) }
