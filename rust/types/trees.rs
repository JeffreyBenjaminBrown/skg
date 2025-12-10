/// Type aliases for paired trees used throughout the pipeline.
///
/// Naming convention: 'mSkg' is Haskell-ese, where 'm' often means
/// 'Maybe' (which is 'Option' in Rust). So 'mSkg' = Option<SkgNode>.

use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNode;
use ego_tree::Tree;

/// Tree pairing Option<SkgNode> with OrgNode.
/// Used throughout the pipeline. The Option allows for nodes
/// that don't yet have an SkgNode fetched from disk.
pub type PairTree = Tree < (Option<SkgNode>, OrgNode) >;
