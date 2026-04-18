//! Transitional shim. The real type is 'NodeComplete' in
//! [[./nodes/complete.rs][server/types/nodes/complete.rs]].
//!
//! This module re-exports under the old 'SkgNode' name so existing
//! call sites continue to compile during Plan A's migration.
//! Later migration sweeps will update call sites to import
//! 'NodeComplete' directly from 'types::nodes::complete' and this
//! shim will be deleted.

pub use crate::types::nodes::complete::{
  FileProperty,
  NodeComplete as SkgNode,
  empty_node_complete as empty_skgnode,
};
