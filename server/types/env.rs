//! 'SkgEnv' bundles the per-process environment: the static config
//! and handles to the four databases that back the system. It is
//! constructed once at startup (after 'initialize_dbs') and threaded
//! through subsystems that need access to multiple databases.
//!
//! Field order is by complexity: 'config' is plain data; 'in-Rust graph' is
//! the in-process snapshot; 'tantivy_index' is in-process and indexed;
//! 'driver' talks over the wire to TypeDB.

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::telescope::invariants::TelescopeViolation;

use std::collections::HashMap;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;
use tantivy::Searcher;

#[derive(Clone)]
pub struct SkgEnv {
  pub config        : SkgConfig,
  pub in_rust_graph : InRustGraphHandle,
  pub tantivy_index : TantivyIndex,
  pub driver        : Arc<TypeDBDriver>,
  /// Actual read snapshot captured with this environment's selected graph.
  pub searcher      : Searcher,
  /// Load-time telescope warnings waiting to be presented during the
  /// connection handshake. Kept as structured data, not only a log/report.
  pub startup_warnings : Arc<Vec<(ID, TelescopeViolation)>>,
}

impl SkgEnv {
  /// Snap the current in-Rust graph.
  pub fn in_rust_graph_snapshot (&self) -> Arc<InRustGraph> {
    self . in_rust_graph . load_full () . graph . clone () }

  /// Resolve only from the supplied graph and explicitly captured Git evidence.
  pub fn find_source (
    &self,
    id : &ID,
    deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  ) -> Option<SourceName> {
    find_source (id, deleted_since_head_pid_src_map,
      &self . in_rust_graph_snapshot ()) }
}

pub fn find_source (
  id : &ID,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  graph : &InRustGraph,
) -> Option<SourceName> {
  graph . pid_and_source (id) . map (|(_pid, source)| source)
    . or_else (|| deleted_since_head_pid_src_map . get (id) . cloned ()) }
