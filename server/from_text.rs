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
use viewnodes_to_instructions::viewforest_to_nonmerge_save_instructions;
use validate::{validate_and_filter_foreign_instructions, validate_merges_involve_only_owned_nodes, validate_no_simultaneous_move_and_merge};

use ego_tree::Tree;
use typedb_driver::TypeDBDriver;

pub async fn buffer_to_viewforest_and_save_instructions (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result< ( Tree<ViewNode>,    // the view
               Vec<DefineNode>,   // instructions
               Vec<Merge>,        // instructions
               Vec<SourceMove> ), // instructions
             SaveError> {
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
  let (nonmerge_instructions, source_moves)
    : (Vec<DefineNode>, Vec<SourceMove>) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "viewforest_to_nonmerge_save_instructions" ). entered();
      viewforest_to_nonmerge_save_instructions (
        & viewforest, config, driver )
      . await } . map_err (SaveError::DatabaseError) ?;
  let nonmerge_instructions : Vec<DefineNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_instructions, config, driver )
      . await } . map_err (SaveError::BufferValidationErrors) ?;
  let merge_instructions : Vec<Merge> =
    // PITFALL: The edit_requests consumed here remain in viewforest until cleared by complete_truenode_preorder (during complete_viewforest). While it would be more natural to clear them immediately after consumption, that would require either walking the entire viewforest of views again, or keeping a map from ID to Set<NodeId>, both of which are more complex.
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "instructiontriples_from_the_merges_in_an_viewforest"
        ). entered();
      instructiontriples_from_the_merges_in_an_viewforest (
        & viewforest, config, driver )
      . await } . map_err (SaveError::DatabaseError) ?;
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "validate_merges_involve_only_owned_nodes" ). entered();
    validate_merges_involve_only_owned_nodes (
      & merge_instructions, config ) }
    . map_err (SaveError::BufferValidationErrors) ?;
  validate_no_simultaneous_move_and_merge (
    &source_moves, &merge_instructions )
    . map_err (SaveError::BufferValidationErrors) ?;

  Ok ((viewforest,
       nonmerge_instructions,
       merge_instructions,
       source_moves)) }
