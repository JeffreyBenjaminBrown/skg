// PURPOSE: Update Tantivy index from SaveInstructions.

use crate::dbs::tantivy::{add_documents_to_tantivy_writer, commit_with_status, delete_nodes_from_index};
use crate::types::{SkgNode, TantivyIndex, SaveInstruction, NonMerge_NodeAction};

use tantivy::IndexWriter;
use std::error::Error;


/// Updates the index with the provided SaveInstructions.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where toDelete is false.
/// Returns the number of documents processed.
pub fn update_index_from_saveinstructions (
  instructions  : &[SaveInstruction],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;
  delete_nodes_from_index(
    // Delete all IDs in the SaveInstructions from the index.
    // (Instructions that aren't toDelete are then recreated.)
    instructions.iter().map(|(node, _)| node),
    &mut writer,
    tantivy_index)?;
  { // Add documents only for non-deleted instructions.
    let nodes_to_add: Vec<&SkgNode> =
      instructions . iter()
      . filter_map(
        |(node, action)|
        if !matches!( action,
                      NonMerge_NodeAction::Delete) {
          Some(node)
        } else { None } )
      . collect();
    let processed_count: usize =
      add_documents_to_tantivy_writer(
        nodes_to_add, &mut writer, tantivy_index)?;
    commit_with_status(
      &mut writer, processed_count, "Updated")?;
    Ok (processed_count) }}
