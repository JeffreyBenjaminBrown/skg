//! 'SkgEnv' bundles the per-process environment: the static config
//! and handles to the four databases that back the system. It is
//! constructed once at startup (after 'initialize_dbs') and threaded
//! through subsystems that need access to multiple databases.
//!
//! Field order is by complexity: 'config' is plain data; 'memory' is
//! the in-process snapshot; 'tantivy_index' is in-process and indexed;
//! 'driver' talks over the wire to TypeDB.

use crate::dbs::memory::{InRustGraph, InRustGraphHandle, snapshot_global};
use crate::dbs::tantivy::title_and_source_by_id;
use crate::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use crate::types::phantom::source_from_disk;

use std::collections::HashMap;
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

  /// Resolve an ID to its source by checking, in order:
  ///
  /// 1. The in-Rust memory snapshot (freshest; reflects in-flight
  ///    edits before they reach the indexed DBs or disk).
  /// 2. The caller's 'deleted_since_head_pid_src_map' hint (only
  ///    relevant in diff view; covers IDs whose '.skg' file has been
  ///    deleted between HEAD and now).
  /// 3. Tantivy (in-process indexed lookup; ~6us at p50).
  /// 4. Disk scan (slow last resort; covers nodes that were created
  ///    out-of-band since the last index sync).
  ///
  /// TypeDB is intentionally absent: production paths populate
  /// TypeDB and Tantivy together (both 'init' and 'save' write
  /// from the same node set), so any ID TypeDB knows is also
  /// Tantivy-known. Skipping TypeDB keeps this lookup synchronous.
  pub fn find_source (
    &self,
    id                             : &ID,
    deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  ) -> Option<SourceName> {
    if let Some ((_pid, src)) =
      self . memory_snapshot () . pid_and_source (id)
    { return Some (src); }
    if let Some (s) = deleted_since_head_pid_src_map . get (id)
    { return Some (s . clone ()); }
    if let Some ((_title, src)) =
      title_and_source_by_id (&self . tantivy_index, id)
    { return Some (src); }
    source_from_disk (id, &self . config) }
}

/// Free-function variant of 'SkgEnv::find_source' for callers that
/// don't have a 'SkgEnv' on hand. Uses the process-global memory
/// snapshot ('snapshot_global'); returns 'None' when no snapshot
/// has been installed (e.g. tests that bypass startup).
///
/// Layered the same way as 'SkgEnv::find_source':
/// memory → deletion-map → Tantivy → disk scan.
pub fn find_source (
  id                             : &ID,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  tantivy_index                  : &TantivyIndex,
  config                         : &SkgConfig,
) -> Option<SourceName> {
  find_source_with_optional_tantivy (
    id, deleted_since_head_pid_src_map,
    Some (tantivy_index), config ) }

/// Like 'find_source', but accepts 'None' for the Tantivy index.
/// Tests that don't populate Tantivy (and don't need it for
/// resolution) can pass 'None'; the lookup falls through to disk.
pub fn find_source_with_optional_tantivy (
  id                             : &ID,
  deleted_since_head_pid_src_map : &HashMap<ID, SourceName>,
  tantivy_index                  : Option<&TantivyIndex>,
  config                         : &SkgConfig,
) -> Option<SourceName> {
  if let Some (snap) = snapshot_global () {
    if let Some ((_pid, src)) = snap . pid_and_source (id)
    { return Some (src); } }
  if let Some (s) = deleted_since_head_pid_src_map . get (id)
  { return Some (s . clone ()); }
  if let Some (idx) = tantivy_index {
    if let Some ((_title, src)) = title_and_source_by_id (idx, id)
    { return Some (src); } }
  source_from_disk (id, config) }
