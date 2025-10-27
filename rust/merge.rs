use crate::types::{SaveInstruction, SkgConfig, OrgNode};
use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Creates SaveInstructions for merge operations from an orgnode forest.
///
/// For each node with a merge instruction, this creates three SaveInstructions:
/// 1. A new MERGED node containing the acquiree's title and body
/// 2. An updated acquirer node with modified contents and extra IDs
/// 3. A deletion instruction for the acquiree node
///
/// TODO: This is slightly inefficient. It would be faster to collect a list
/// of orgnodes with merge instructions during one of the other walks of the forest.
pub async fn saveinstructions_from_the_merges_in_an_orgnode_forest(
    _forest: &[Tree<OrgNode>],
    _config: &SkgConfig,
    _driver: &TypeDBDriver,
) -> Result<Vec<SaveInstruction>, Box<dyn Error>> {
    // TODO: Implement
    todo!()
}
