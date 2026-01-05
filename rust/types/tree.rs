/// Type aliases for paired trees and utilities for working with ego_tree::Tree.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

pub mod orgnode_skgnode;
pub use orgnode_skgnode::{
  pair_orgnode_forest_with_save_instructions,
};

pub mod accessors;
pub use accessors::{
  read_at_ancestor_in_tree,
  write_at_ancestor_in_tree,
  read_at_node_in_tree,
  write_at_node_in_tree,
};

pub mod generations;
pub use generations::{
  first_in_generation,
  first_in_nth_generation_of_descendents,
  next_in_generation,
  collect_generation_ids,
};

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
pub type PairTree = Tree<NodePair>;

#[derive(Clone, Debug)]
pub struct NodePair {
  pub mskgnode: Option<SkgNode>,
  pub orgnode: OrgNode,
}
