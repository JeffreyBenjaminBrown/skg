//! 'SkgEnv' bundles the coherent runtime generation and mutation gate. It is
//! constructed once at startup (after 'initialize_dbs') and threaded
//! through subsystems that need access to multiple databases.
//!

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::dbs::tantivy::title_and_source_by_id;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::phantom::home_from_disk;

use std::collections::HashMap;
use std::sync::Arc;
use arc_swap::ArcSwap;
use tokio::sync::Mutex;

/// Serializes authoritative mutations for one running Skg environment.
///
/// Readers continue to use immutable graph snapshots without this lock.  A
/// writer holds it across validation against its captured graph, authoritative
/// filesystem writes, graph publication, and the Tantivy enqueue. The Arc means every cloned
/// `SkgEnv` for different connections shares the same gate.
pub type MutationGate = Arc<Mutex<()>>;

pub fn new_mutation_gate () -> MutationGate {
  Arc::new ( Mutex::new (()) ) }

#[derive(Clone)]
pub struct RuntimeGeneration {
  pub config : Arc<SkgConfig>,
  pub graph : Arc<InRustGraph>,
  pub tantivy_index : TantivyIndex,
  pub generation : u64,
}

pub struct SharedRuntime {
  current : ArcSwap<RuntimeGeneration>,
  pub mutation_gate : MutationGate,
  // Shared atomic publication state for coherent runtime generations.
  legacy_graph : InRustGraphHandle,
}

impl SharedRuntime {
  pub fn new (
    config : SkgConfig,
    graph : Arc<InRustGraph>,
    tantivy_index : TantivyIndex,
  ) -> Self {
    let legacy_graph = Arc::new (ArcSwap::from (graph));
    Self::new_with_graph_handle (config, legacy_graph, tantivy_index)
  }

  fn new_with_graph_handle (
    config : SkgConfig,
    legacy_graph : InRustGraphHandle,
    tantivy_index : TantivyIndex,
  ) -> Self {
    let graph = legacy_graph . load_full ();
    SharedRuntime {
      current : ArcSwap::from_pointee (RuntimeGeneration {
        config : Arc::new (config), graph, tantivy_index, generation : 0,
      }),
      mutation_gate : new_mutation_gate (),
      legacy_graph,
    }
  }

  pub fn snapshot (&self) -> Arc<RuntimeGeneration> {
    self . current . load_full ()
  }

  pub fn publish (
    &self,
    config : Arc<SkgConfig>,
    graph : Arc<InRustGraph>,
    tantivy_index : TantivyIndex,
  ) -> Arc<RuntimeGeneration> {
    let generation = self . snapshot () . generation + 1;
    let published = Arc::new (RuntimeGeneration {
      config, graph : graph . clone (), tantivy_index, generation,
    });
    self . legacy_graph . store (graph);
    self . current . store (published . clone ());
    tracing::info! (
      generation,
      node_count = published . graph . nodes . len (),
      "published coherent runtime generation");
    published
  }

  pub fn legacy_graph_handle (&self) -> InRustGraphHandle {
    self . legacy_graph . clone ()
  }
}

#[derive(Clone)]
pub struct SkgEnv {
  pub runtime : Arc<SharedRuntime>,
}

impl SkgEnv {
  pub fn new (
    config : SkgConfig,
    graph : Arc<InRustGraph>,
    tantivy_index : TantivyIndex,
  ) -> Self {
    SkgEnv {
      runtime : Arc::new (SharedRuntime::new (config, graph, tantivy_index)),
    }
  }

  pub fn new_with_graph_handle (
    config : SkgConfig,
    graph : InRustGraphHandle,
    tantivy_index : TantivyIndex,
  ) -> Self {
    SkgEnv {
      runtime : Arc::new (SharedRuntime::new_with_graph_handle (
        config, graph, tantivy_index)),
    }
  }

  pub fn runtime_snapshot (&self) -> Arc<RuntimeGeneration> {
    let runtime = self . runtime . snapshot ();
    tracing::trace! (
      generation = runtime . generation,
      "captured runtime generation");
    runtime
  }

  pub fn mutation_gate (&self) -> MutationGate {
    self . runtime . mutation_gate . clone ()
  }

  /// Resolve an ID to its source by checking, in order:
  ///
  /// 1. The in-Rust graph snapshot (freshest; reflects in-flight
  ///    edits before they reach the indexed DBs or disk).
  /// 2. The caller's 'deleted_since_head_pid_src_map' hint (only
  ///    relevant in diff view; covers IDs whose '.skg' file has been
  ///    deleted between HEAD and now).
  /// 3. Tantivy (in-process indexed lookup; ~6us at p50).
  /// 4. Disk scan (slow last resort; covers nodes that were created
  ///    out-of-band since the last index sync).
  ///
  /// Tantivy is only an optimization here: the graph is authoritative for
  /// identity and source lookup, and this helper stays synchronous.
  pub fn find_source_in_generation (
    runtime : &RuntimeGeneration,
    id : &ID,
    deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  ) -> Option<SourceName> {
    if let Some ((_pid, src)) =
      runtime . graph . pid_and_source (id)
    { return Some (src); }
    if let Some (s) = deleted_since_head_pid_src_map . get (id)
    { return Some (s . clone ()); }
    if let Some ((_title, src)) =
      title_and_source_by_id (&runtime . tantivy_index, id)
    { return Some (src); }
    home_from_disk (id, &runtime . config) }
}

/// Source lookup against one explicit graph snapshot, with optional Tantivy
/// and disk fallbacks for deleted or out-of-band nodes.
pub fn find_source_with_optional_tantivy (
  graph                          : &InRustGraph,
  id                             : &ID,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Option<SourceName> {
  if let Some ((_pid, src)) = graph . pid_and_source (id)
    { return Some (src); }
  if let Some (s) = deleted_since_head_pid_src_map . get (id)
  { return Some (s . clone ()); }
  if let Some (idx) = tantivy_index {
    if let Some ((_title, src)) = title_and_source_by_id (idx, id)
    { return Some (src); } }
  home_from_disk (id, config) }
