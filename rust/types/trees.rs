/// Type aliases for paired trees used throughout the pipeline.
///
/// The SkgNode is optional because in some cases
/// it hasn't been fetched yet, and in others (e.g. an AliasCol)
/// there is no SkgNode corresponding to the OrgNode.

use crate::types::orgnode::OrgNode;
use crate::types::skgnode::SkgNode;
use ego_tree::{Tree, NodeId};

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

/// Read a node's value from a tree, applying a function to it.
/// Returns an error if the node is not found.
pub fn read_at_node_in_tree<T, F, R>(
    tree: &Tree<T>,
    node_id: NodeId,
    f: F
) -> Result<R, String>
where
    F: FnOnce(&T) -> R,
{
    let node_ref = tree.get(node_id)
        .ok_or("node not found")?;
    Ok(f(node_ref.value()))
}

/// Write to a node's value in a tree, applying a mutating function to it.
/// Returns an error if the node is not found.
pub fn write_at_node_in_tree<T, F, R>(
    tree: &mut Tree<T>,
    node_id: NodeId,
    f: F
) -> Result<R, String>
where
    F: FnOnce(&mut T) -> R,
{
    let mut node_mut = tree.get_mut(node_id)
        .ok_or("node not found")?;
    Ok(f(node_mut.value()))
}
