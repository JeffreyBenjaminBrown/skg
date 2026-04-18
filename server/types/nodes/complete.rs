//! NodeComplete: the full in-memory node.
//!
//! Currently a type alias for SkgNode, which today carries every
//! field of a node (on-disk fields plus in-memory 'source').
//!
//! This alias exists so the rest of the codebase can start referring
//! to "NodeComplete" by its intended name. Later migration steps will
//! promote this to a distinct struct (split from NodeFS), add
//! conversions to the narrower destination-specific types
//! (NodeTypedb, NodeTantivy, NodeRust), and eventually retire the
//! 'SkgNode' alias.
//!
//! See /home/ubuntu/.claude/plans/many-and-better-node-types.org
//! (Plan A) for the overall design.

pub use crate::types::skgnode::{FileProperty, SkgNode as NodeComplete};
