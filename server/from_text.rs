/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_viewforest_and_save_instructions'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod viewnodes_to_instructions;
pub mod supplement_from_disk;
pub mod validate;

use crate::merge::mergeInstructionTriple::instructiontriples_from_the_merges_in_an_viewforest;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::SkgConfig;
use crate::types::save::{Merge, DefineNode, SourceMove};
use crate::types::unchecked_viewnode::{UncheckedViewNode, unchecked_to_checked_tree};
use crate::types::viewnode::ViewNode;

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use buffer_to_viewnodes::add_missing_info::add_missing_info_to_viewforest;
use buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use viewnodes_to_instructions::{extract_nonmerge_save_plan, NonmergeSavePlan};
use validate::{validate_and_filter_foreign_instructions, validate_merges_involve_only_owned_nodes, validate_no_simultaneous_move_and_merge};

use ego_tree::Tree;
use typedb_driver::TypeDBDriver;

#[derive(Debug)]
pub struct SavePlan {
  pub viewforest         : Tree<ViewNode>,
  pub define_nodes       : Vec<DefineNode>,
  pub merge_instructions : Vec<Merge>,
  pub source_moves       : Vec<SourceMove>,
}

pub async fn buffer_to_viewforest_and_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<SavePlan, SaveError> {
  let ( mut unchecked_viewforest, parsing_errors )
    : ( Tree<UncheckedViewNode>, Vec<BufferValidationError> )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "org_to_uninterpreted_nodes" ). entered();
        org_to_uninterpreted_nodes (buffer_text) }
      . map_err (SaveError::ParseError) ?;
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "add_missing_info_to_viewforest" ). entered();
    // Precedes all validation functions.
    // For why, see the header comment of one of them,
    // 'find_buffer_errors_for_saving'.
    add_missing_info_to_viewforest (
      & mut unchecked_viewforest, & config . db_name, driver )
    . await } . map_err (SaveError::DatabaseError) ?;
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "find_buffer_errors_for_saving" ). entered();
        find_buffer_errors_for_saving (
          & unchecked_viewforest, config, driver )
        . await } . map_err (SaveError::DatabaseError) ?;
    validation_errors . extend (parsing_errors);
    if ! validation_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors (
        validation_errors ) ); }}
  let viewforest : Tree<ViewNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "unchecked_to_checked_tree" ). entered();
      unchecked_to_checked_tree (unchecked_viewforest) }
        . map_err ( |e| SaveError::ParseError (e) ) ?;
  let nonmerge_plan : NonmergeSavePlan =
    extract_nonmerge_save_plan (
      & viewforest, config, driver )
    . await . map_err (SaveError::DatabaseError) ?;
  let define_nodes : Vec<DefineNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_plan . define_nodes, config, driver )
      . await } . map_err (SaveError::BufferValidationErrors) ?;
  let merge_instructions : Vec<Merge> =
    // PITFALL: The edit_requests consumed here remain in viewforest until cleared by complete_truenode_preorder, during complete_viewforest. Merge extraction only plans merge mutations; it does not mutate the saved viewforest.
    extract_merge_save_plan (
      & viewforest, config, driver )
    . await . map_err (SaveError::DatabaseError) ?;
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "validate_merges_involve_only_owned_nodes" ). entered();
    validate_merges_involve_only_owned_nodes (
      & merge_instructions, config ) }
    . map_err (SaveError::BufferValidationErrors) ?;
  validate_no_simultaneous_move_and_merge (
    &nonmerge_plan . source_moves, &merge_instructions )
    . map_err (SaveError::BufferValidationErrors) ?;

  Ok (SavePlan { viewforest,
                 define_nodes,
                 merge_instructions,
                 source_moves : nonmerge_plan . source_moves } ) }

async fn extract_merge_save_plan (
  viewforest : &Tree<ViewNode>,
  config     : &SkgConfig,
  driver     : &TypeDBDriver,
) -> Result<Vec<Merge>, Box<dyn std::error::Error>> {
  // PITFALL: The edit_requests consumed here remain in viewforest
  // until cleared by complete_truenode_preorder, during
  // complete_viewforest.
  let merge_instructions : Vec<Merge> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "extract_merge_save_plan" ). entered();
      instructiontriples_from_the_merges_in_an_viewforest (
        viewforest, config, driver )
      . await } ?;
  Ok (merge_instructions) }
