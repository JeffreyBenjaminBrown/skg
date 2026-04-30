//! 'SkgEnv' bundles the per-process environment: the static config
//! and handles to the four databases that back the system. It is
//! constructed once at startup (after 'initialize_dbs') and threaded
//! through subsystems that need access to multiple databases.
//!
//! Field order is by complexity: 'config' is plain data; 'memory' is
//! the in-process snapshot; 'tantivy_index' is in-process and indexed;
//! 'driver' talks over the wire to TypeDB.

use crate::dbs::memory::{InRustGraph, InRustGraphHandle};
use crate::types::misc::{SkgConfig, TantivyIndex};

use std::sync::Arc;
use typedb_driver::TypeDBDriver;

#[derive(Clone)]
pub struct SkgEnv {
  pub config        : SkgConfig,
  pub memory        : InRustGraphHandle,
  pub tantivy_index : TantivyIndex,
  pub driver        : Arc<TypeDBDriver>,
}

impl SkgEnv {
  /// Snap the current in-Rust memory.
  pub fn memory_snapshot (&self) -> Arc<InRustGraph> {
    self . memory . load_full () }
}
