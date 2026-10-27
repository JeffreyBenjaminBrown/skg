//! 'SkgEnv' bundles the coherent runtime generation and mutation gate. It is
//! constructed once at startup (after 'initialize_dbs') and threaded
//! through subsystems that need access to multiple databases.
//!

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::diff_view::readiness_store::ReadinessStore;
use crate::types::misc::{SkgConfig, TantivyIndex};

use std::sync::{Arc, OnceLock};
use std::sync::Mutex as SyncMutex;
use std::sync::MutexGuard as SyncMutexGuard;
use arc_swap::ArcSwap;
use tokio::sync::Mutex;

/// Serializes authoritative mutations for one running Skg environment.
///
/// Readers continue to use immutable graph snapshots without this lock.  A
/// writer holds it across validation against its captured graph, authoritative
/// filesystem writes, graph swap-in, and the Tantivy enqueue. The Arc means every cloned
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
  // Shared atomic swap-in state for coherent runtime generations.
  legacy_graph : InRustGraphHandle,
  /// The commit-readiness marks, read from '<data_root>' at startup
  /// and written back after every change.
  pub readiness_store : SyncMutex<ReadinessStore>,
  /// What the server warned about as it started (unknown config
  /// keys, foreign manifests whose order the config contradicts),
  /// recorded once by 'main' and served by the "server warnings"
  /// request.
  startup_warnings : OnceLock<Vec<String>>,
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
    let readiness_store : ReadinessStore =
      ReadinessStore::load (&config . data_root);
    SharedRuntime {
      current : ArcSwap::from_pointee (RuntimeGeneration {
        config : Arc::new (config), graph, tantivy_index, generation : 0,
      }),
      mutation_gate : new_mutation_gate (),
      legacy_graph,
      readiness_store : SyncMutex::new (readiness_store),
      startup_warnings : OnceLock::new (),
    }
  }

  /// Record the startup warnings. Only the first call has an effect.
  pub fn record_startup_warnings (
    &self,
    warnings : Vec<String>,
  ) {
    let _already_recorded : Result<(), Vec<String>> =
      self . startup_warnings . set (warnings); }

  /// The startup warnings; none if none were recorded.
  pub fn startup_warnings (
    &self,
  ) -> &[String] {
    self . startup_warnings . get ()
      . map ( |warnings| warnings . as_slice () )
      . unwrap_or (&[] ) }

  pub fn graph_snapshot (&self) -> Arc<RuntimeGeneration> {
    self . current . load_full ()
  }

  /// The readiness store, locked. A panic while another holder had it
  /// leaves the marks as they were, so a poisoned lock is taken
  /// anyway.
  pub fn lock_readiness_store (
    &self,
  ) -> SyncMutexGuard<'_, ReadinessStore> {
    self . readiness_store . lock ()
      . unwrap_or_else ( |poisoned| poisoned . into_inner () ) }

  pub fn swap_in (
    &self,
    config : Arc<SkgConfig>,
    graph : Arc<InRustGraph>,
    tantivy_index : TantivyIndex,
  ) -> Arc<RuntimeGeneration> {
    let generation = self . graph_snapshot () . generation + 1;
    let swapped_in = Arc::new (RuntimeGeneration {
      config, graph : graph . clone (), tantivy_index, generation,
    });
    self . legacy_graph . store (graph);
    self . current . store (swapped_in . clone ());
    tracing::info! (
      generation,
      node_count = swapped_in . graph . nodes . len (),
      "swapped in coherent runtime generation");
    swapped_in
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
    let runtime = self . runtime . graph_snapshot ();
    tracing::trace! (
      generation = runtime . generation,
      "captured runtime generation");
    runtime
  }

  pub fn mutation_gate (&self) -> MutationGate {
    self . runtime . mutation_gate . clone ()
  }
}
