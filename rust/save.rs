pub mod none_node_fields_are_noops;
pub use none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;

pub use buffer_to_orgnodes::*;
pub mod buffer_to_orgnodes;

pub use orgnodes_to_instructions::*;
pub mod orgnodes_to_instructions;

use crate::types::{SkgConfig, SkgNode, SaveInstruction, OrgNode, SaveError, Buffer_Cannot_Be_Saved};
use ego_tree::Tree;

use std::io;
use typedb_driver::TypeDBDriver;


/// Builds a forest of OrgNode2s:
///   - Fills in information via 'add_missing_info_to_trees'
///   - Reconciles duplicates via 'reconcile_dup_instructions'
/// Outputs that plus a forest of SaveInstructions.
pub async fn buffer_to_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver
) -> Result<
    ( Vec<Tree<OrgNode>>,
      Vec<SaveInstruction>
    ), SaveError> {

  let mut orgnode_forest : Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes ( buffer_text )
    . map_err ( SaveError::ParseError ) ?;
  add_missing_info_to_trees (
    /* Do 'add_missing_info_to_trees'
    before 'find_buffer_errors_for_saving'.
    See the latter's header comment for why. */
    & mut orgnode_forest, & config . db_name, driver
  ). await . map_err ( SaveError::DatabaseError ) ?;
  let validation_errors : Vec<Buffer_Cannot_Be_Saved> =
    find_buffer_errors_for_saving ( & orgnode_forest );
  if ! validation_errors . is_empty () {
    return Err ( SaveError::BufferValidationErrors (
      validation_errors ) ); }
  let (orgnode_forest_2, instructions)
    : (Vec<Tree<OrgNode>>, Vec<SaveInstruction>) =
    orgnodes_to_save_instructions (
      orgnode_forest, config, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  let clobbered_instructions : Vec<SaveInstruction> =
    instructions . into_iter ()
    . map ( |(node, action)| {
      let clobbered_node : SkgNode =
        clobber_none_fields_with_data_from_disk (
          config, node ) ?;
      Ok ((clobbered_node, action)) } )
    . collect::<io::Result<Vec<SaveInstruction>>>()
    . map_err ( SaveError::IoError ) ?;
  Ok ((orgnode_forest_2, clobbered_instructions)) }
