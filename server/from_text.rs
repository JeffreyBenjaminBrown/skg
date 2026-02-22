/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_viewnode_forest_and_save_instructions'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod viewnodes_to_instructions;
pub mod supplement_from_disk;
pub mod validate_foreign_nodes;

use crate::merge::mergeInstructionTriple::instructiontriples_from_the_merges_in_an_viewnode_forest;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::SkgConfig;
use crate::types::viewnode::ViewNode;
use crate::types::unchecked_viewnode::{UncheckedViewNode, unchecked_to_checked_tree};
use crate::types::save::{Merge, DefineNode};

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use buffer_to_viewnodes::add_missing_info::add_missing_info_to_forest;
use buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use viewnodes_to_instructions::viewnode_forest_to_nonmerge_save_instructions;
use validate_foreign_nodes::{validate_and_filter_foreign_instructions, validate_merges_involve_only_owned_nodes};

use crate::serve::timing_log::{timed, timed_async};

use ego_tree::Tree;
use neo4rs::Graph;

pub async fn buffer_to_viewnode_forest_and_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  graph       : &Graph
) -> Result< ( Tree<ViewNode>,
               Vec<DefineNode>,
               Vec<Merge> ),
             SaveError> {
  let ( mut unchecked_forest, parsing_errors )
    : ( Tree<UncheckedViewNode>, Vec<BufferValidationError> )
    = timed ( config, "org_to_uninterpreted_nodes",
              || org_to_uninterpreted_nodes ( buffer_text ))
      . map_err ( SaveError::ParseError ) ?;
  timed_async ( config, "add_missing_info_to_forest",
    // Precedes all validation functions.
    // For why, see the header comment of one of them,
    // 'find_buffer_errors_for_saving'.
    add_missing_info_to_forest (
      & mut unchecked_forest, graph )
  ). await . map_err ( SaveError::DatabaseError ) ?;
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      timed_async ( config, "find_buffer_errors_for_saving",
        find_buffer_errors_for_saving (
          & unchecked_forest, config, graph )
      ). await . map_err ( SaveError::DatabaseError ) ?;
    validation_errors . extend ( parsing_errors );
    if ! validation_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors (
        validation_errors ) ); }}
  let viewnode_forest : Tree<ViewNode> =
    timed ( config, "unchecked_to_checked_tree",
            || unchecked_to_checked_tree ( unchecked_forest ))
      . map_err ( |e| SaveError::ParseError ( e ) ) ?;
  let nonmerge_instructions : Vec<DefineNode> =
    timed_async ( config, "viewnode_forest_to_nonmerge_save_instructions",
      viewnode_forest_to_nonmerge_save_instructions (
        & viewnode_forest, config, graph )
    ). await . map_err ( SaveError::DatabaseError ) ?;
  let nonmerge_instructions : Vec<DefineNode> =
    timed_async ( config, "validate_and_filter_foreign_instructions",
      validate_and_filter_foreign_instructions (
        nonmerge_instructions, config, graph )
    ). await . map_err ( SaveError::BufferValidationErrors ) ?;
  let merge_instructions : Vec<Merge> =
    timed_async ( config, "instructiontriples_from_the_merges_in_an_viewnode_forest",
      instructiontriples_from_the_merges_in_an_viewnode_forest (
        & viewnode_forest, config, graph )
    ). await . map_err ( SaveError::DatabaseError ) ?;
  timed ( config, "validate_merges_involve_only_owned_nodes",
          || validate_merges_involve_only_owned_nodes (
            & merge_instructions, config ))
    . map_err ( SaveError::BufferValidationErrors ) ?;

  Ok ((viewnode_forest,
       nonmerge_instructions,
       merge_instructions)) }
