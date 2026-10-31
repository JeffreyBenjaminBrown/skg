//! Rendering and rerendering a whole buffer of the diff view
//! ('docs/diff-view.org', "Rendering a buffer"). Each is a refresh in
//! diff-view mode ('refresh_viewforest', 'server/refresh/'), over the
//! graph straddle, with the '(ready ...)' change facts read from the store: the
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
use crate::refresh::{Truth, refresh_viewforest};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{DiffViewBufferKind, ViewnodeIdRecord};
use crate::rerender_pass::RerenderPassContext;
use crate::refresh::mode::RefreshMode;

use std::collections::BTreeSet;
use std::error::Error;
use std::sync::{Arc, MutexGuard};

/// Refresh 'viewforest', a buffer of kind 'diffView_buffer_kind' of
/// the diff view, over 'straddle', with the '(ready ...)' change facts from
/// 'store'. The goal holds what HEAD alone warrants, the change
/// viewnodes are recomputed in place, and the buffer's sets choose
/// what may be editable ('drawn_occurrence_editability'). The diff view opens only
/// under the skgrepo set 'all', but a switch rerenders its open
/// buffers under 'restriction', which the goal and the ID rule ask as
/// in a content view: a restricted node is never a ghost.
pub fn refresh_diffView_viewforest (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  restriction          : Option<&SkgrepoRestriction>,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<(), Box<dyn Error>> {
  refresh_viewforest (
    viewforest, viewnode_id_record, &Truth::of_runtime (runtime, restriction),
    RefreshMode::DiffView { straddle,
                            buffer_kind     : diffView_buffer_kind,
                            readiness_store : store },
    runtime . config . initial_node_limit ) }

/// Rerender a buffer of the diff view: bring the straddle in the
/// context's slot up to date, and refresh the buffer over it.
/// Only for the "integrate unshown changes" request
/// ('RerenderPassContext::integrates_unshown_changes') does it make
/// occurrences editable or append roots, both before the one refresh:
/// each node of the recomputed U without an editable occurrence
/// becomes editable at its first content occurrence, and the content
/// origins of the recomputed D that the buffer makes editable nowhere
/// are appended as stub roots, for the refresh to draw. Its standing
/// roots stay.
pub fn rerender_diffView_buffer (
  context              : &mut RerenderPassContext<'_>,
  diffView_buffer_kind : DiffViewBufferKind,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<(), Box<dyn Error>> {
  let integrate_unshown_changes : bool = context . integrates_unshown_changes;
  let runtime : &RuntimeGeneration = &context . runtime;
  let restriction : Option<&SkgrepoRestriction> = context . skgrepo_restriction;
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      &mut context . graph_straddle, &runtime . config, runtime . graph . clone () )
    . map_err ( |e| format! (
      "Cannot rerender the diff view buffer: {}", e )) ?;
  if integrate_unshown_changes {
    let standing_roots : BTreeSet<ID> = root_gnids_of_buffer (viewforest);
    ensure_editableOccurrences_exist (&straddle, diffView_buffer_kind, viewforest);
    let new_roots : Vec<ID> = contentOrigins_missing_from_buffer (
      &straddle, diffView_buffer_kind, viewforest, &standing_roots );
    append_contentOrigins_to_buffer (
      runtime, &straddle, diffView_buffer_kind, viewforest, &new_roots ) ?; }
  let store : MutexGuard<ReadinessStore> = context . env . runtime . lock_readiness_store ();
  refresh_diffView_viewforest (
    runtime, &straddle, diffView_buffer_kind, &store, restriction,
    viewforest, viewnode_id_record ) }
