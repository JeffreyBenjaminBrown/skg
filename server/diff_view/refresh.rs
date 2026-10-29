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
use crate::refresh::{Refreshed, Truth, refresh_viewforest};
use crate::refresh::buffer_corrections::{BufferCorrection, BufferCorrectionPolicy};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::viewnode::ViewnodeId;
use crate::types::views_state::{DiffViewBufferKind, ViewnodeIdRecord};
use crate::update_buffer::{DiffViewRerender, RerenderPassContext};
use crate::refresh::mode::RefreshMode;

use ego_tree::NodeId;
use std::collections::{BTreeSet, HashSet};
use std::error::Error;
use std::sync::{Arc, MutexGuard};

/// Refresh 'viewforest', a buffer of kind 'diffView_buffer_kind' of
/// the diff view, over 'straddle', with the '(ready ...)' change facts from
/// 'store'. The goal holds what HEAD alone warrants, the change
/// viewnodes are recomputed in place, and the buffer's sets choose
/// what is editable and what an extension charges to the node budget
/// ('RefreshMode::charges_node_budget'). The diff view opens only
/// under the skgrepo set 'all', but a switch rerenders its open
/// buffers under 'restriction', which the goal and the ID rule ask as
/// in a content view: a restricted node is never a ghost.
/// 'buffer_correction_policy' says which changes are no buffer corrections.
pub fn refresh_diffView_viewforest (
  runtime              : &RuntimeGeneration,
  straddle             : &GraphStraddle,
  diffView_buffer_kind : DiffViewBufferKind,
  store                : &ReadinessStore,
  restriction          : Option<&SkgrepoRestriction>,
  buffer_correction_policy    : &BufferCorrectionPolicy,
  viewforest           : &mut ViewForest,
  viewnode_id_record   : &mut ViewnodeIdRecord,
) -> Result<Refreshed, Box<dyn Error>> {
  refresh_viewforest (
    viewforest, viewnode_id_record, &Truth::of_runtime (runtime, restriction),
    RefreshMode::DiffView { straddle,
                            buffer_kind     : diffView_buffer_kind,
                            readiness_store : store },
    runtime . config . initial_node_limit, buffer_correction_policy ) }

/// Rerender a buffer of the diff view: bring the straddle in the
/// context's slot up to date, and refresh the buffer over it.
/// Only for the "integrate unshown changes" request
/// ('DiffViewRerender::integrate_unshown_changes') does it make occurrences editable or
/// append roots: before the refresh each node of the recomputed U
/// without an editable occurrence becomes editable at its first
/// content occurrence; after it, the content origins of the recomputed
/// D that the buffer shows nowhere are appended as stub roots, and the
/// buffer is refreshed again. Its standing roots stay. The first
/// refresh's buffer corrections go unread
/// ('BufferCorrectionPolicy::Unread'): what it draws under the
/// occurrences it made editable is what the user asked for, as a
/// fresh view's is. The second refresh reports by the context's
/// policy, so that what the user's buffer got wrong is still told,
/// but for what it draws under the roots it appended, which is a first
/// rendering of them, and unread as a first rendering is
/// ('outside_the_appended_roots').
pub fn rerender_diffView_buffer (
  context            : &mut RerenderPassContext<'_>,
  rerender           : DiffViewRerender,
  viewforest         : &mut ViewForest,
  viewnode_id_record : &mut ViewnodeIdRecord,
) -> Result<Refreshed, Box<dyn Error>> {
  let DiffViewRerender { buffer_kind : diffView_buffer_kind,
                         integrate_unshown_changes } : DiffViewRerender = rerender;
  let runtime : &RuntimeGeneration = &context . runtime;
  let restriction : Option<&SkgrepoRestriction> = context . skgrepo_restriction;
  let buffer_correction_policy : &BufferCorrectionPolicy =
    &context . buffer_correction_policy;
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      &mut context . graph_straddle, &runtime . config, runtime . graph . clone () )
    . map_err ( |e| format! (
      "Cannot rerender the diff view buffer: {}", e )) ?;
  let standing_roots : BTreeSet<ID> = root_gnids_of_buffer (viewforest);
  if integrate_unshown_changes {
    ensure_editableOccurrences_exist (&straddle, diffView_buffer_kind, viewforest); }
  let store : MutexGuard<ReadinessStore> = context . env . runtime . lock_readiness_store ();
  let refreshed : Refreshed = refresh_diffView_viewforest (
    runtime, &straddle, diffView_buffer_kind, &store, restriction,
    if integrate_unshown_changes { &BufferCorrectionPolicy::Unread }
    else { buffer_correction_policy },
    viewforest, viewnode_id_record ) ?;
  if ! integrate_unshown_changes { return Ok (refreshed); }
  let new_roots : Vec<ID> = contentOrigins_missing_from_buffer (
    &straddle, diffView_buffer_kind, viewforest, &standing_roots );
  if new_roots . is_empty () { return Ok (refreshed); }
  let appended_roots : Vec<NodeId> = append_contentOrigins_to_buffer (
    runtime, &straddle, diffView_buffer_kind, viewforest, &new_roots ) ?;
  let refreshed_again : Refreshed = refresh_diffView_viewforest (
    runtime, &straddle, diffView_buffer_kind, &store, restriction,
    buffer_correction_policy, viewforest, viewnode_id_record ) ?;
  Ok ( outside_the_appended_roots (refreshed_again, viewforest, &appended_roots) ) }

/// 'refreshed' without the buffer corrections of headlines in the
/// subtrees of 'appended_roots'.
fn outside_the_appended_roots (
  refreshed      : Refreshed,
  viewforest     : &ViewForest,
  appended_roots : &[NodeId],
) -> Refreshed {
  let appended_vnids : HashSet<&ViewnodeId> = appended_roots . iter ()
    . filter_map ( |root| viewforest . get (*root) )
    . flat_map ( |root| root . descendants () )
    . filter_map ( |node| node . value () . viewnode_id . as_ref () )
    . collect ();
  let buffer_corrections : Vec<BufferCorrection> =
    refreshed . buffer_corrections . iter ()
    . filter ( |buffer_correction| ! buffer_correction . vnid . as_ref ()
               . is_some_and ( |vnid| appended_vnids . contains (vnid) ))
    . cloned ()
    . collect ();
  Refreshed { buffer_corrections, .. refreshed } }
