//! Configuration and graph/search inputs for one selected publication.

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::store_state::{SelectedGraphBase, SelectedStoreState};
use crate::telescope::invariants::TelescopeViolation;

use std::collections::{BTreeSet, HashMap};
use std::sync::Arc;
use arc_swap::ArcSwap;
use tantivy::Searcher;

#[derive(Clone)]
pub struct SkgEnv {
  pub config        : SkgConfig,
  pub in_rust_graph : InRustGraphHandle,
  pub tantivy_index : TantivyIndex,
  /// Actual read snapshot captured with this environment's selected graph.
  pub searcher      : Searcher,
  /// Load-time telescope warnings waiting to be presented during the
  /// connection handshake. Kept as structured data, not only a log/report.
  pub startup_warnings : Arc<Vec<(ID, TelescopeViolation)>>,
}

/// Retained rendering and proof inputs. No live index resource is needed to
/// settle a view or finish an incident report after another publication.
#[derive(Clone)]
pub struct GraphReadSnapshot {
  pub config   : SkgConfig,
  pub selected : SelectedGraphBase,
  pub cyclic_roots : BTreeSet<ID>, }

impl GraphReadSnapshot {
  pub fn from_env
  ( env : &SkgEnv,
  ) -> Self {
    let selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
    Self {
      config: env . config . clone (),
      selected: selected . graph_base (),
      cyclic_roots: selected . cyclic_roots . clone (), } }
}

impl SkgEnv {
  /// Retained work must own the graph, manifest and matching search reader
  /// from one publication even when its caller supplied the writer handle.
  pub fn pinned (
    &self,
  ) -> Self {
    let selected : Arc<SelectedStoreState> = self . in_rust_graph . load_full ();
    let mut pinned : Self = self . clone ();
    pinned . searcher = selected . searcher . clone ()
      . expect ("a live environment has a matching Searcher");
    pinned . in_rust_graph = Arc::new (ArcSwap::from (selected));
    pinned }

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
