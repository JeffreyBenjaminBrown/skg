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

pub mod update;
pub use update::update_fs_from_saveinstructions;
pub use update::update_index_from_saveinstructions;
pub use update::update_typedb_from_saveinstructions;

pub mod validate_foreign_nodes;
pub use validate_foreign_nodes::{
  validate_and_filter_foreign_instructions,
  validate_foreign_merge_instructions,
};

use crate::merge::instructiontriples_from_the_merges_in_an_orgnode_forest;
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::{SaveInstruction, MergeInstructionTriple};
use crate::types::orgnode::OrgNode;
use crate::types::errors::{SaveError, BufferValidationError};
use ego_tree::Tree;
use std::error::Error;

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

  // Validate and filter foreign (read-only) node instructions
  let instructions : Vec<SaveInstruction> =
    validate_and_filter_foreign_instructions (
      instructions, config, driver )
    . await . map_err ( SaveError::BufferValidationErrors ) ?;

  let mergeInstructions : Vec<MergeInstructionTriple> =
    instructiontriples_from_the_merges_in_an_orgnode_forest (
      & orgnode_forest, config, driver
    ) . await . map_err ( SaveError::DatabaseError ) ?;

  // Validate that merge instructions don't involve foreign nodes
  validate_foreign_merge_instructions (
    & mergeInstructions, config )
    . map_err ( SaveError::BufferValidationErrors ) ?;

  Ok ((orgnode_forest,
       instructions,
       mergeInstructions)) }

/// Updates **everything** from the given `SaveInstruction`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_graph (
  instructions  : Vec<SaveInstruction>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;

  { println!( "1) Updating TypeDB database '{}' ...", db_name );
    update_typedb_from_saveinstructions (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB update complete." ); }

  { // filesystem
    let total_input : usize = instructions.len ();
    // TODO Phase 5: Print per-source write information
    println!( "2) Writing {} instruction(s) to disk ...",
               total_input );
    let (deleted_count, written_count) : (usize, usize) =
      update_fs_from_saveinstructions (
        instructions.clone (), config.clone ()) ?;
    println!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  { // Tantivy
    println!( "3) Updating Tantivy index ..." );
    let indexed_count : usize =
      update_index_from_saveinstructions (
        &instructions, tantivy_index )?;
    println!( "   Tantivy updated for {} document(s).",
                  indexed_count ); }

  println!( "All updates finished successfully." );
  Ok (( )) }
