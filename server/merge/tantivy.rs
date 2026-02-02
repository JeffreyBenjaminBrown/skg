use crate::save::update_tantivy_from_saveinstructions;
use crate::types::save::{Merge, DefineNode};
use crate::types::misc::TantivyIndex;
use std::error::Error;

/// Apply each Merge to the index.
pub(super) fn merge_nodes_in_tantivy (
  merge_instructions : &[Merge],
  tantivy_index      : &TantivyIndex,
) -> Result < (), Box<dyn Error> > {
  update_tantivy_from_saveinstructions(
    & { let flat_instructions : Vec<DefineNode> =
          merge_instructions . iter()
          . flat_map( |m| m.to_vec() )
          . collect();
        flat_instructions },
    tantivy_index)?;
  Ok (( )) }
