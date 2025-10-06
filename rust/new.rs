pub use buffer_to_orgnodes::*;
pub mod buffer_to_orgnodes;

pub use orgnodes_to_instructions::*;
pub mod orgnodes_to_instructions;

use crate::types::{ID, SkgConfig, SaveInstruction, OrgNode2};
use ego_tree::Tree;

use std::error::Error;
use typedb_driver::TypeDBDriver;


#[derive(Debug)]
pub enum SaveError {
  ParseError(String),
  DatabaseError(Box<dyn Error>),
  InconsistentInstructions {
    inconsistent_deletions: Vec<ID>,
    multiple_definers: Vec<ID>, }, }

/// Builds a forest of OrgNode2s:
///   - Fills in information via 'add_missing_info_to_trees'
///   - Reconciles duplicates via 'reconcile_dup_instructions'
/// Outputs that plus a forest of SaveInstructions.
pub async fn buffer_to_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver
) -> Result<
    ( Vec<Tree<OrgNode2>>,
      Vec<SaveInstruction>
    ), SaveError> {

  let orgnode_forest : Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2 ( buffer_text )
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
    : (Vec<Tree<OrgNode2>>, Vec<SaveInstruction>) =
    orgnodes_to_save_instructions (
      orgnode_forest, config, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  Ok ((orgnode_forest_2, instruction_vector)) }
