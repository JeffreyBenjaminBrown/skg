/// Type aliases for paired trees used throughout the pipeline.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNode;
use ego_tree::Tree;

/// PairTree is the primary way an org-buffer is represented.
/// Most OrgNodes have an associated SkgNode, but not all,
/// which is why the SkgNode is optional.
/// Each PairTree comes from an OrgNode tree,
/// and eventually becomes one before being sent to Emacs,
/// but carrying the SkgNode between those two phases
/// lets us avoid redundant disk lookups.
pub type PairTree = Tree < NodePair >;
pub type NodePair = (Option<SkgNode>, OrgNode);
