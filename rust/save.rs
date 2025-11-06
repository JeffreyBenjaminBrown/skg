pub use buffer_to_orgnodes::{
    org_to_uninterpreted_nodes,
    headline_to_triple,
    HeadlineInfo,
    find_inconsistent_instructions,
    find_buffer_errors_for_saving,
    add_missing_info_to_trees,
};
pub mod buffer_to_orgnodes;

pub mod repeated_to_indefinitive;
pub use repeated_to_indefinitive::change_repeated_to_indefinitive;

pub use orgnodes_to_instructions::{
    orgnodes_to_reconciled_save_instructions,
    interpret,
    reconcile_dup_instructions,
    clobber_none_fields_with_data_from_disk,
};
pub mod orgnodes_to_instructions;

pub mod update_fs;
pub use update_fs::update_fs_from_saveinstructions;

use crate::merge::instructiontriples_from_the_merges_in_an_orgnode_forest;
use crate::types::misc::SkgConfig;
use crate::types::save::{SaveInstruction, MergeInstructionTriple};
use crate::types::orgnode::OrgNode;
use crate::types::errors::{SaveError, BufferValidationError};
use ego_tree::Tree;

use typedb_driver::TypeDBDriver;

/// Builds a forest of OrgNode2s:
///   - Futzes with repeated and indefinitive.
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
  change_repeated_to_indefinitive (
    &mut orgnode_forest );
  add_missing_info_to_trees (
    /* This should precede 'find_buffer_errors_for_saving'.
    See the latter's header comment for why. */
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

  // Generate instructions.
  let instructions : Vec<SaveInstruction> =
    orgnodes_to_reconciled_save_instructions (
      & orgnode_forest, config, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  let mergeInstructions : Vec<MergeInstructionTriple> =
    instructiontriples_from_the_merges_in_an_orgnode_forest (
      & orgnode_forest, config, driver
    ) . await . map_err ( SaveError::DatabaseError ) ?;

  Ok ((orgnode_forest,
       instructions,
       mergeInstructions)) }
