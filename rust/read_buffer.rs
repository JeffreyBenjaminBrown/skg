/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'save_buffer::'
/// is the function 'buffer_to_save_instructions' defined here.

use crate::merge::instructiontriples_from_the_merges_in_an_orgnode_forest;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::SkgConfig;
use crate::types::orgnode::OrgNode;
use crate::types::save::{MergeInstructionTriple, SaveInstruction};
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;

pub use buffer_to_orgnodes::{
    org_to_uninterpreted_nodes,
    headline_to_triple,
    HeadlineInfo,
    find_inconsistent_instructions,
    find_buffer_errors_for_saving,
    add_missing_info_to_trees,
};
pub mod buffer_to_orgnodes;

pub use orgnodes_to_instructions::{
    orgnodes_to_reconciled_save_instructions,
    interpret_orgnode_forest,
    reconcile_dup_instructions,
    clobber_none_fields_with_data_from_disk,
};
pub mod orgnodes_to_instructions;

pub mod validate_foreign_nodes;
pub use validate_foreign_nodes::{
  validate_and_filter_foreign_instructions,
  validate_merges_involve_only_owned_nodes,
};

/// Builds a forest of OrgNodes:
///   - Fills in information via 'add_missing_info_to_trees'.
///   - Reconciles duplicates via 'reconcile_dup_instructions'
/// Outputs that plus a forest of SaveInstructions, plus MergeInstructionTriples.
pub async fn buffer_to_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver
) -> Result<
    ( Vec<Tree<OrgNode>>,
      Vec<SaveInstruction>,
      Vec<MergeInstructionTriple>
    ), SaveError> {
  let mut orgnode_forest : Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes ( buffer_text )
    . map_err ( SaveError::ParseError ) ?;
  add_missing_info_to_trees (
    // Should (and does) precede 'find_buffer_errors_for_saving'.
    // See that function's's header comment for why.
    & mut orgnode_forest, & config . db_name, driver
  ). await . map_err ( SaveError::DatabaseError ) ?;

  { // If saving is impossible, don't.
    let validation_errors : Vec<BufferValidationError> =
      find_buffer_errors_for_saving (
        & orgnode_forest, config, driver
      ) . await . map_err ( SaveError::DatabaseError ) ?;
    if ! validation_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors (
        validation_errors ) ); }}
  let instructions : Vec<SaveInstruction> =
    orgnodes_to_reconciled_save_instructions (
      & orgnode_forest, config, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  let instructions : Vec<SaveInstruction> =
    validate_and_filter_foreign_instructions (
      instructions, config, driver )
    . await . map_err ( SaveError::BufferValidationErrors ) ?;
  let mergeInstructions : Vec<MergeInstructionTriple> =
    instructiontriples_from_the_merges_in_an_orgnode_forest (
      & orgnode_forest, config, driver
    ) . await . map_err ( SaveError::DatabaseError ) ?;
  validate_merges_involve_only_owned_nodes (
    & mergeInstructions, config )
    . map_err ( SaveError::BufferValidationErrors ) ?;

  Ok ((orgnode_forest,
       instructions,
       mergeInstructions)) }
