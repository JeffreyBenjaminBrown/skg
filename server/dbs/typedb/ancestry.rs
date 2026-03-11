use std::error::Error;
use typedb_driver::TypeDBDriver;

use crate::types::misc::ID;

/// A node in the full containerward ancestry tree.
///
/// Root: a genuine root (no containers).
/// Repeated: already visited via another branch (cycle or diamond).
/// DepthTruncated: max_ancestry_depth reached; may have containers we didn't explore.
/// Inner: has at least one container; children are its containers.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AncestryNode {
  Root           ( ID ),
  Repeated       ( ID ),
  DepthTruncated ( ID ),
  Inner          ( ID, Vec<AncestryNode> ),
}

impl AncestryNode {
  pub fn id ( &self ) -> &ID {
    match self {
      AncestryNode::Root           ( id )    => id,
      AncestryNode::Repeated       ( id )    => id,
      AncestryNode::DepthTruncated ( id )    => id,
      AncestryNode::Inner          ( id, _ ) => id, } } }

/// Compute the full containerward ancestry tree for a given origin node.
///
/// BFS level-by-level: each round queries containers for every frontier node
/// (in parallel via join_all), then classifies each result as Root, Repeated,
/// DepthTruncated, or Inner.
///
/// Returns an AncestryNode rooted at the origin.
pub async fn full_containerward_ancestry(
  _db_name   : &str,
  _driver    : &TypeDBDriver,
  _origin    : &ID,
  _max_depth : usize,
) -> Result<AncestryNode, Box<dyn Error>> {
  todo!("full_containerward_ancestry not yet implemented") }
