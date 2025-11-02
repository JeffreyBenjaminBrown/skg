use crate::tantivy::update_index_from_saveinstructions;
use crate::types::{MergeInstructionTriple, SaveInstruction, TantivyIndex};
use std::error::Error;
use std::sync::Arc;
use tantivy::Index;
use tantivy::schema::{Schema, Field};

/// Apply each MergeInstructionTriple to the index.
pub(super) fn merge_nodes_in_tantivy (
  merge_instructions : &[MergeInstructionTriple],
  index              : &Index,
) -> Result < (), Box<dyn Error> > {
  let flat_instructions : Vec<SaveInstruction> =
    merge_instructions . iter()
    . flat_map( |m| m.to_vec() )
    . collect();
  let schema : Schema = index.schema();
  let id_field : Field =
    schema.get_field("id")
    . ok_or("Missing 'id' field in Tantivy schema")?;
  let title_or_alias_field : Field =
    schema.get_field("title_or_alias")
    . ok_or("Missing 'title_or_alias' field in Tantivy schema")?;
  let tantivy_index : TantivyIndex = TantivyIndex {
    index: Arc::new(index.clone()),
    id_field,
    title_or_alias_field, };
  update_index_from_saveinstructions(
    &flat_instructions,
    &tantivy_index)?;
  Ok (( )) }
