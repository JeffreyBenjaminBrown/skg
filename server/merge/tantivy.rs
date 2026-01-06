use crate::save::update_index_from_saveinstructions;
use crate::types::save::{MergeInstructionTriple, SaveInstruction};
use crate::types::misc::TantivyIndex;
use std::error::Error;

/// Apply each MergeInstructionTriple to the index.
pub(super) fn merge_nodes_in_tantivy (
  merge_instructions : &[MergeInstructionTriple],
  tantivy_index      : &TantivyIndex,
) -> Result < (), Box<dyn Error> > {
  update_index_from_saveinstructions(
    & { let flat_instructions : Vec<SaveInstruction> =
          merge_instructions . iter()
          . flat_map( |m| m.to_vec() )
          . collect();
        flat_instructions },
    tantivy_index)?;
  Ok (( )) }
