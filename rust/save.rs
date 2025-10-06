pub mod none_node_fields_are_noops;
pub use none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;

pub use buffer_to_orgnodes::*;
pub mod buffer_to_orgnodes;

pub use orgnodes_to_instructions::*;
pub mod orgnodes_to_instructions;

pub use parse_sexp::*;
pub mod parse_sexp;

use crate::types::{ID, SkgConfig, SkgNode, SaveInstruction, OrgNode, SaveError};
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

  let orgnode_forest : Vec<Tree<OrgNode>> =
    org_to_uninterpreted_nodes ( buffer_text )
    . map_err ( SaveError::ParseError ) ?;
  let ( inconsistent_deletions, multiple_definers )
    : ( Vec<ID>, Vec<ID> ) =
    find_inconsistent_instructions ( & orgnode_forest );
  if ( ! inconsistent_deletions . is_empty () ||
       ! multiple_definers . is_empty () ) {
    return Err ( SaveError::InconsistentInstructions {
      inconsistent_deletions,
      multiple_definers, } ); }
  let (orgnode_forest_2, instruction_vector)
    : (Vec<Tree<OrgNode>>, Vec<SaveInstruction>) =
    orgnodes_to_save_instructions (
      orgnode_forest, config, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  let clobbered_instruction_vector : Vec<SaveInstruction> =
    instruction_vector . into_iter ()
    . map ( |(node, action)| {
      let clobbered_node : SkgNode =
        clobber_none_fields_with_data_from_disk (
          config, node ) ?;
      Ok ((clobbered_node, action)) } )
    . collect::<io::Result<Vec<SaveInstruction>>>()
    . map_err ( SaveError::IoError ) ?;
  Ok ((orgnode_forest_2, clobbered_instruction_vector)) }
