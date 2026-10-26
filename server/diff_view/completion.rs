//! Rendering and rerendering a whole buffer of the diff view
//! ('docs/diff-view.org', "Rendering a buffer"). Each is a refresh in
//! diff-view mode ('refresh_viewforest', 'server/refresh/'), over the
//! graph straddle, with the readiness facts read from the store: the
//! first rendering refreshes a stub of the buffer's roots
//! ('diffView_buffer_viewforest' in 'pruned_change_view.rs'); a
//! rerender ('rerender_view' in 'server/update_buffer.rs') brings the
//! straddle up to date and refreshes what the buffer holds
//! ('rerender_diffView_buffer').

use crate::diff_view::pruned_change_view::{
  append_contentOrigins_to_buffer, contentOrigins_missing_from_buffer,
  ensure_editableOccurrences_exist, root_gnids_of_buffer};
use crate::diff_view::readiness_store::ReadinessStore;
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::refresh::{Refreshed, Truth, refresh_viewforest};
use crate::refresh::buffer_corrections::BufferCorrectionPolicy;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{DiffViewBufferKind, ViewnodeIdRecord};
use crate::update_buffer::completion_mode::CompletionMode;

use std::collections::BTreeSet;
use std::error::Error;
use std::sync::{Arc, MutexGuard};

/// Refresh 'viewforest', a buffer of kind 'diffView_buffer_kind' of
/// the diff view, over 'straddle', with the readiness facts from
/// 'store'. The goal holds what HEAD alone warrants, the change
/// viewnodes are recomputed in place, and the buffer's sets choose
/// what is editable and what an extension charges to the node budget
/// ('CompletionMode::charges_node_budget'). The diff view requires the
/// skgrepo set 'all', so nothing is restricted there.
/// 'buffer_correction_policy' says which changes are no buffer corrections.
pub fn complete_viewforest_with_graph_straddle (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  restriction          : Option<&SkgrepoRestriction>,
  buffer_correction_policy    : &BufferCorrectionPolicy,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<Refreshed, Box<dyn Error>> {
  let truth : Truth = Truth { graph  : &runtime . graph,
                              config : &runtime . config,
                              restriction, };
  refresh_viewforest (
    viewforest, viewnode_id_record, &truth,
    CompletionMode::DiffView { straddle, buffer_kind : diffView_buffer_kind },
    runtime . config . initial_node_limit, Some (store), buffer_correction_policy ) }

/// Rerender a buffer of the diff view: bring the straddle in
/// 'graph_straddle_slot' up to date, and refresh the buffer over it.
/// Only for the "integrate unshown changes" request
/// ('integrate_unshown_changes') does it make occurrences editable or
/// append roots: before the refresh each node of the recomputed U
/// without an editable occurrence becomes editable at its first
/// content occurrence; after it, the content origins of the recomputed
/// D that the buffer shows nowhere are appended as stub roots, and the
/// buffer is refreshed again. Its standing roots stay. What that
/// request draws is what the user asked for, as a fresh view's is, so
/// it reports no buffer corrections.
pub fn rerender_diffView_buffer (
  graph_straddle_slot       : &mut Option<Arc<GraphStraddle>>,
  runtime                   : &RuntimeGeneration,
  env                       : &SkgEnv,
  restriction               : Option<&SkgrepoRestriction>,
  diffView_buffer_kind      : DiffViewBufferKind,
  integrate_unshown_changes : bool,
  buffer_correction_policy         : &BufferCorrectionPolicy,
  viewforest                : &mut ViewForest,
  viewnode_id_record        : &mut ViewnodeIdRecord,
) -> Result<Refreshed, Box<dyn Error>> {
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      graph_straddle_slot, &runtime . config, runtime . graph . clone () )
    . map_err ( |e| format! (
      "Cannot rerender the diff view buffer: {}", e )) ?;
  let standing_roots : BTreeSet<ID> = root_gnids_of_buffer (viewforest);
  if integrate_unshown_changes {
    ensure_editableOccurrences_exist (&straddle, diffView_buffer_kind, viewforest); }
  let store : MutexGuard<ReadinessStore> = env . runtime . lock_readiness_store ();
  let refreshed : Refreshed = complete_viewforest_with_graph_straddle (
    runtime, &straddle, diffView_buffer_kind, &store, restriction,
    buffer_correction_policy, viewforest, viewnode_id_record ) ?;
  if ! integrate_unshown_changes { return Ok (refreshed); }
  let new_roots : Vec<ID> = contentOrigins_missing_from_buffer (
    &straddle, diffView_buffer_kind, viewforest, &standing_roots );
  let text : String =
    if new_roots . is_empty () { refreshed . text }
    else {
      append_contentOrigins_to_buffer (
        runtime, &straddle, diffView_buffer_kind, viewforest, &new_roots ) ?;
      complete_viewforest_with_graph_straddle (
        runtime, &straddle, diffView_buffer_kind, &store, restriction,
        buffer_correction_policy, viewforest, viewnode_id_record ) ? . text };
  Ok ( Refreshed { text, buffer_corrections : Vec::new () } ) }
