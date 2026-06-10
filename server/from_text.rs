/// The 'buffer' referred to here
/// is a Skg buffer from the Emacs client,
/// read by the Rust server when the user saves it.
/// The sole purpose of all the sub-libraries in 'from_text::'
/// is the function 'buffer_to_validated_saveplan'
/// defined here.

pub mod buffer_to_viewnodes;
pub mod local_instruction_collection;
pub mod supplement_from_disk;
pub mod validate;

use crate::nodeMerge::nodeMergeInstructionTriple::nodeMerge_instructions_from_pairs;
use crate::types::errors::{BufferValidationError, SaveError};
use crate::types::misc::{ID, SkgConfig};
use crate::types::save::{NodeMerge, DefineNode, SavePlan};
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::tree::forest::{MpViewForest, ViewForest};

use buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use buffer_to_viewnodes::add_missing_info::{
  add_missing_info_to_viewforest,
  absent_parentIs_under_visible_parent_becomes_isContainer};
use buffer_to_viewnodes::validate_tree::find_buffer_errors_for_saving;
use local_instruction_collection::{
  extract_nonmergeSavePlan_locally, NonmergeSavePlan };
use validate::{validate_and_filter_foreign_instructions, validate_no_simultaneous_move_and_nodeMerge};

use typedb_driver::TypeDBDriver;

/// Save preparation deliberately validates at several
/// data-maturity stages:
/// - raw org parse: errors only visible before tree construction;
/// - metadata-filled maybePlaced tree: global/local buffer structure;
/// - placed, role-aware viewforest: saved-view role policy;
/// - disk-supplemented DefineNodes: foreign write policy;
/// - non-nodeMerge plus nodeMerge plan: cross-plan source-move/nodeMerge policy.
///
/// Returns the saved view and the plan derived from it as a pair. The two are
/// kept apart (TODO/DONE/local-view-update/plan_v2.org §11): the graph-mutation
/// step consumes only the SavePlan; the rerender step consumes the ViewForest
/// (plus the plan's PIDs, for collateral selection). One parse produces both.
pub async fn buffer_to_validated_saveplan (
  buffer_text : &str,
  config      : &SkgConfig,
  driver      : &TypeDBDriver,
) -> Result<(ViewForest, SavePlan), SaveError> {
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
  let ( nonmerge_plan, nodeMerge_acquisitions )
    : ( NonmergeSavePlan, Vec<(ID, ID)> )
    = extract_nonmergeSavePlan_locally (
        &viewforest, config, driver )
      . await . map_err (SaveError::DatabaseError) ?;
  let nodeMerge_instructions : Vec<NodeMerge> =
    // PITFALL: The edit_requests consumed here remain in viewforest until cleared by expand_true_content_at_truenode, during complete_viewforest. NodeMerge extraction only plans nodeMerge mutations; it does not mutate the saved viewforest.
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "nodeMerge_instructions_from_pairs" ). entered();
      nodeMerge_instructions_from_pairs (
        &nodeMerge_acquisitions, config, driver )
      . await } . map_err (SaveError::DatabaseError) ?;
  let define_nodes : Vec<DefineNode> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "validate_and_filter_foreign_instructions" ). entered();
      validate_and_filter_foreign_instructions (
        nonmerge_plan . define_nodes,
        &nodeMerge_instructions,
        config,
        driver )
      . await } . map_err (SaveError::BufferValidationErrors) ?;
  validate_no_simultaneous_move_and_nodeMerge (
    &nonmerge_plan . source_moves, &nodeMerge_instructions )
    . map_err (SaveError::BufferValidationErrors) ?;
  Ok (( viewforest,
        SavePlan {
          define_nodes,
          nodeMerge_instructions,
          source_moves : nonmerge_plan . source_moves } )) }
