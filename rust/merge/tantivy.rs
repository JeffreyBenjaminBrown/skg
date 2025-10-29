use crate::types::{SaveInstruction, TantivyIndex};
use crate::tantivy::update_index_from_saveinstructions;
use std::error::Error;
use std::sync::Arc;

/// Merges nodes in Tantivy by applying merge SaveInstructions.
pub(crate) fn merge_nodes_in_tantivy (
  instructions : &[SaveInstruction],
  index        : &tantivy::Index,
) -> Result < (), Box<dyn Error> > {

  // The tantivy module already has a function that handles SaveInstructions!
  // It will:
  // - Delete all IDs from the index (including the acquiree)
  // - Add back non-deleted nodes (acquiree_text_preserver and updated acquirer)
  // - Skip deleted nodes (acquiree)

  // Let me look at the schema - it should have "id" and "title_or_alias" fields
  let schema : tantivy::schema::Schema = index.schema();
  let id_field : tantivy::schema::Field =
    schema.get_field("id")
      .ok_or("Missing 'id' field in Tantivy schema")?;
  let title_or_alias_field : tantivy::schema::Field =
    schema.get_field("title_or_alias")
      .ok_or("Missing 'title_or_alias' field in Tantivy schema")?;

  let tantivy_index : TantivyIndex = TantivyIndex {
    index: Arc::new(index.clone()),
    id_field,
    title_or_alias_field,
  };

  update_index_from_saveinstructions(instructions, &tantivy_index)?;

  Ok(())
}
