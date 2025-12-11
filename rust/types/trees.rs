/// Type aliases for paired trees used throughout the pipeline.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNode;
use ego_tree::Tree;

pub type NodePair = (Option<SkgNode>, OrgNode);
pub type PairTree = Tree < NodePair >;
