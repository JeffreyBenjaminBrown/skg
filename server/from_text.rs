/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_validated_saveplan'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod viewnodes_to_instructions;
pub mod supplement_from_disk;
pub mod validate;

use crate::merge::mergeInstructionTriple::instructiontriples_from_merge_candidates;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::SkgConfig;
use crate::types::save::{Merge, DefineNode, SaveInstructions};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::tree::forest::{MpViewForest, ViewForest};

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest,
  absent_parentIs_under_visible_parent_becomes_isContainer};
use buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use viewnodes_to_instructions::{
  extract_nonmergeSavePlan_from_authority,
  NonmergeSavePlan,
  SaveAuthority,
  validate_no_title_or_body_edit_in_subscribeeAsSuch};
use validate::{validate_and_filter_foreign_instructions, validate_no_simultaneous_move_and_merge};

use typedb_driver::TypeDBDriver;

/// The saved view plus the instructions derived from it. The two are
/// kept apart (plan_v2 §11): the graph-mutation step consumes only
/// 'instructions'; the rerender step consumes 'viewforest' (plus the
/// instruction PIDs, for collateral selection). One parse produces both.
#[derive(Debug)]
pub struct SavePlan {
  pub viewforest    : ViewForest,
  pub instructions  : SaveInstructions,
}

/// Save preparation deliberately validates at several
/// data-maturity stages:
/// - raw org parse: errors only visible before tree construction;
/// - metadata-filled maybePlaced tree: global/local buffer structure;
/// - placed, role-aware viewforest: saved-view role policy;
/// - disk-supplemented DefineNodes: foreign write policy;
/// - non-merge plus merge plan: cross-plan source-move/merge policy.
pub async fn buffer_to_validated_saveplan (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<SavePlan, SaveError> {
  let ( mut maybePlaced_viewforest, parsing_errors )
    : ( MpViewForest, Vec<BufferValidationError> )
    = { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "org_to_uninterpreted_viewforest" ). entered();
        // parse the raw buffer
        org_to_uninterpreted_viewforest (buffer_text) }
          . map_err (SaveError::ParseError) ?;
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "add_missing_info_to_viewforest" ). entered();
    // Metadata filling must precede maybePlaced-tree validation,
    // because those validators compare nodes by pid,
    // and expect sources to be inherited/resolved.
    add_missing_info_to_viewforest (
      & mut maybePlaced_viewforest, & config . db_name, driver )
    . await } . map_err (SaveError::DatabaseError) ?;
  absent_parentIs_under_visible_parent_becomes_isContainer (
    &mut maybePlaced_viewforest );
  { // If saving is impossible, don't.
    let mut validation_errors : Vec<BufferValidationError> =
      { let _span : tracing::span::EnteredSpan = tracing::info_span!(
          "find_buffer_errors_for_saving" ). entered();
        find_buffer_errors_for_saving (
          & maybePlaced_viewforest, config, driver )
        . await } . map_err (SaveError::DatabaseError) ?;
    validation_errors . extend (parsing_errors);
    if ! validation_errors . is_empty () {
      return Err ( SaveError::BufferValidationErrors (
        validation_errors ) ); }}
  let viewforest : ViewForest =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "maybePlaced_to_placed_viewforest" ). entered();
      maybePlaced_to_placed_viewforest (maybePlaced_viewforest) }
        . map_err ( |e| SaveError::ParseError (e) ) ?;
  let save_authority : SaveAuthority =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "SaveAuthority::from_viewforest" ). entered();
      SaveAuthority::from_viewforest (&viewforest) }
        . map_err ( |e| SaveError::ParseError (e) ) ?;
  validate_no_title_or_body_edit_in_subscribeeAsSuch (
    save_authority . role_viewforest (), config, driver
  ) . await . map_err (SaveError::DatabaseError) ?;
  let nonmerge_plan : NonmergeSavePlan =
    extract_nonmergeSavePlan_from_authority (
      &save_authority, config, driver )
    . await . map_err (SaveError::DatabaseError) ?;
  let merge_instructions : Vec<Merge> =
    // PITFALL: The edit_requests consumed here remain in viewforest until cleared by expand_true_content_at_truenode, during complete_viewforest. Merge extraction only plans merge mutations; it does not mutate the saved viewforest.
    extract_merge_save_plan (
      &save_authority, config, driver )
    . await . map_err (SaveError::DatabaseError) ?;
  let define_nodes : Vec<DefineNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_plan . define_nodes,
        &merge_instructions,
        config,
        driver )
      . await } . map_err (SaveError::BufferValidationErrors) ?;
  validate_no_simultaneous_move_and_merge (
    &nonmerge_plan . source_moves, &merge_instructions )
    . map_err (SaveError::BufferValidationErrors) ?;
  Ok ( SavePlan {
    viewforest,
    instructions : SaveInstructions {
      define_nodes,
      merge_instructions,
      source_moves : nonmerge_plan . source_moves } } ) }

async fn extract_merge_save_plan (
  extraction_forest : &SaveAuthority,
  config            : &SkgConfig,
  driver            : &TypeDBDriver,
) -> Result<Vec<Merge>, Box<dyn std::error::Error>> {
  // PITFALL: The edit_requests consumed here remain in viewforest
  // until cleared by expand_true_content_at_truenode, during
  // complete_viewforest.
  let merge_instructions : Vec<Merge> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "extract_merge_save_plan" ). entered();
      instructiontriples_from_merge_candidates (
        extraction_forest, config, driver )
      . await } ?;
  Ok (merge_instructions) }
