//! Drawing and rerendering a whole buffer of the diff view
//! ('docs/diff-view.org', "Drawing a buffer"): the first drawing
//! completes a stub viewforest over the graph straddle; a rerender
//! ('rerender_view' in 'server/update_buffer.rs') brings the straddle
//! up to date before view completion ('prepare_diffView_rerender') and
//! sets the readiness facts after it ('finish_diffView_rerender').

use crate::diff_view::pcsvf::{
  DiffViewSource, append_contentOrigins_to_buffer, contentOrigins_missing_from_buffer,
  ensure_editableOccurrences_exist, root_skgids_of_buffer, set_diffView_facts_in_viewforest};
use crate::diff_view::readiness_store::ReadinessStore;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::graph_straddle::build::current_graph_straddle_in_slot;
use crate::graph_straddle::types::GraphStraddle;
use crate::to_org::util::EditableMap;
use crate::types::env::{RuntimeGeneration, SkgEnv};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::DiffViewBufferKind;
use crate::update_buffer::complete::{
  CompletionContext, complete_appended_roots, complete_viewforest};

use ego_tree::NodeId;
use std::collections::BTreeSet;
use std::error::Error;
use std::sync::{Arc, MutexGuard};

/// What a rerender of a buffer of the diff view draws over, from
/// 'prepare_diffView_rerender' to 'finish_diffView_rerender'.
pub struct DiffViewRerender {
  straddle                  : Arc<GraphStraddle>,
  diffView_buffer_kind      : DiffViewBufferKind,
  integrate_unshown_changes : bool,
  standing_roots            : BTreeSet<ID>, // the buffer's roots before completion
}

impl DiffViewRerender {
  /// What view completion draws the buffer from.
  pub fn source (
    &self,
  ) -> DiffViewSource<'_> {
    DiffViewSource { straddle             : &self . straddle,
                     diffView_buffer_kind : self . diffView_buffer_kind } }
}

/// Complete a stub viewforest of the diff view
/// ('server/diff_view/pcsvf.rs') as 'drawing' says: view completion with
/// 'diffView_source' set, which draws the union of the straddle's content, the change
/// folders and markers, and the former members. It creates no
/// default partner folders, spends no node budget (only an
/// editable-view request spends it, and a first drawing carries
/// none), and never restricts by skgrepo (the diff view
/// requires the set 'all').
pub fn complete_viewforest_with_graph_straddle (
  runtime    : &RuntimeGeneration,
  drawing    : DiffViewSource,
  viewforest : &mut ViewForest,
) -> Result<(), Box<dyn Error>> {
  let graph_snap : Arc<InRustGraph> = runtime . graph . clone ();
  let mut editable_map : EditableMap = EditableMap::new ();
  let mut errors : Vec<String> = Vec::new ();
  let mut context : CompletionContext = CompletionContext::for_diffView (
    runtime, &graph_snap, &mut editable_map, &mut errors, drawing );
  complete_viewforest ( viewforest, &mut context ) ?;
  if ! errors . is_empty () {
    return Err ( errors . join ("\n") . into () ); }
  Ok (( )) }

/// Before view completion of a rerender: None unless the view is a
/// buffer of the diff view ('diffView_buffer_kind' Some). Else it
/// brings the straddle in 'graph_straddle_slot' up to date, and, only
/// when 'integrate_unshown_changes' is true (the "integrate unshown
/// changes" request), makes each node of the recomputed U editable at
/// its first occurrence if it has no editable one.
pub fn prepare_diffView_rerender (
  graph_straddle_slot       : &mut Option<Arc<GraphStraddle>>,
  runtime                   : &RuntimeGeneration,
  diffView_buffer_kind      : Option<DiffViewBufferKind>,
  integrate_unshown_changes : bool,
  viewforest                : &mut ViewForest,
) -> Result<Option<DiffViewRerender>, Box<dyn Error>> {
  let Some (diffView_buffer_kind) = diffView_buffer_kind
    else { return Ok (None); };
  let straddle : Arc<GraphStraddle> =
    current_graph_straddle_in_slot (
      graph_straddle_slot, &runtime . config, runtime . graph . clone () )
    . map_err ( |e| format! (
      "Cannot redraw the diff view buffer: {}", e )) ?;
  let rerender : DiffViewRerender = DiffViewRerender {
    straddle, diffView_buffer_kind, integrate_unshown_changes,
    standing_roots : root_skgids_of_buffer (viewforest), };
  if integrate_unshown_changes {
    // Before any visit, so that a node of U is editable before its
    // parent's visit can see it.
    ensure_editableOccurrences_exist (
      rerender . source (), viewforest,
      &runtime . graph, &runtime . config ) ?; }
  Ok (Some (rerender)) }

/// After view completion of a rerender that 'prepare_diffView_rerender'
/// prepared ('rerender' Some): when integrating unshown changes, the
/// content origins of the recomputed D that the buffer does not draw
/// are appended as roots and completed, continuing
/// 'completion_context'; then the readiness facts are read from the
/// store.
pub(crate) fn finish_diffView_rerender (
  rerender           : Option<&DiffViewRerender>,
  viewforest         : &mut ViewForest,
  completion_context : &mut CompletionContext<'_>,
  runtime            : &RuntimeGeneration,
  env                : &SkgEnv,
) -> Result<(), Box<dyn Error>> {
  let Some (rerender) = rerender
    else { return Ok (( )); };
  let drawing : DiffViewSource = rerender . source ();
  if rerender . integrate_unshown_changes {
    let new_roots : Vec<ID> =
      contentOrigins_missing_from_buffer (
        drawing, viewforest, &rerender . standing_roots );
    let appended : Vec<NodeId> = append_contentOrigins_to_buffer (
      runtime, drawing, viewforest, &new_roots ) ?;
    complete_appended_roots (
      viewforest, &appended, completion_context ) ?; }
  let store : MutexGuard<ReadinessStore> =
    env . runtime . lock_readiness_store ();
  set_diffView_facts_in_viewforest (
    viewforest, drawing, &store, &runtime . graph );
  Ok (( )) }
