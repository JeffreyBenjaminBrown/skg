// PURPOSE: Update Tantivy index from SaveInstructions.

use crate::media::tantivy::{create_documents_from_node, commit_with_status};
use crate::types::{ID, TantivyIndex, SaveInstruction};

use tantivy::{IndexWriter, Term, Document};
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
  { // Delete all IDs in the SaveInstructions from the index --
    // be they toDelete or otherwise.
    // (Those that aren't will come back in the next step.)
    for (node, _action) in instructions {
      if !node.ids.is_empty() {
        let primary_id: &ID = &node.ids[0];
        let term: Term = Term::from_field_text(
          tantivy_index.id_field,
          primary_id.as_str() );
        writer.delete_term(term); } } }
  { // Add documents only for non-deleted instructions.
    let mut processed_count: usize = 0;
    for (node, action) in instructions {
      if ! action . toDelete {
        let documents : Vec < Document > =
          create_documents_from_node (
            node, tantivy_index )?;
        for document in documents {
          writer.add_document (document)?;
          processed_count += 1; }} }
    commit_with_status(
      &mut writer, processed_count, "Updated")?;
    Ok (processed_count) }}
