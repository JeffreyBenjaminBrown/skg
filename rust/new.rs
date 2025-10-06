pub use buffer_to_orgnodes::*;
pub mod buffer_to_orgnodes;

pub use orgnodes_to_instructions::*;
pub mod orgnodes_to_instructions;

pub use update_fs_from_saveinstructions::*;
pub mod update_fs_from_saveinstructions;

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

pub async fn buffer_to_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver
) -> Result<Vec<SaveInstruction>, SaveError> {

  let mut trees : Vec<Tree<OrgNode2>> =
    org_to_uninterpreted_nodes2 ( buffer_text )
    . map_err ( SaveError::ParseError ) ?;
  let ( inconsistent_deletions, multiple_definers )
    : ( Vec<ID>, Vec<ID> ) =
    find_inconsistent_instructions ( & trees );
  if ( ! inconsistent_deletions . is_empty () ||
       ! multiple_definers . is_empty () ) {
    return Err ( SaveError::InconsistentInstructions {
      inconsistent_deletions,
      multiple_definers, } ); }
  add_missing_info_to_trees (
    & mut trees, & config . db_name, driver )
    . await . map_err ( SaveError::DatabaseError ) ?;
  let instructions : Vec<SaveInstruction> =
    orgnodes_to_save_instructions ( trees )
    . map_err ( SaveError::ParseError ) ?;
  let reconciled_instructions : Vec<SaveInstruction> =
    reconcile_dup_instructions (
      config, driver, instructions )
    . await . map_err ( SaveError::DatabaseError ) ?;
  Ok (reconciled_instructions) }
