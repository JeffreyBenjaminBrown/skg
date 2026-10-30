//! What one rerender pass shares across the views it rerenders: a
//! save's ('update_views_after_save' in 'server/update_buffer.rs'), or
//! a request's that rerenders open views ('prepare_rerender_views' in
//! 'server/serve/rerender_stream.rs'). Both the rerender of a content
//! view ('server/update_buffer.rs') and that of a buffer of the diff
//! view ('server/diff_view/buffer_rerender.rs') read it.

use crate::graph_straddle::types::GraphStraddle;
use crate::refresh::buffer_corrections::BufferCorrectionPolicy;
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::{RuntimeGeneration, SkgEnv};

use std::sync::Arc;

/// See the module comment.
pub struct RerenderPassContext<'a> {
  pub env                       : &'a SkgEnv,
  /// The runtime generation every view of the pass is refreshed
  /// against.
  pub runtime                   : Arc<RuntimeGeneration>,
  pub errors                    : Vec<String>,
  pub warnings                  : Vec<String>,
  pub skgrepo_restriction       : Option<&'a SkgrepoRestriction>,
  /// The graph straddle a buffer of the diff view is refreshed
  /// over, brought up to date by 'current_graph_straddle_in_slot' when
  /// the first such buffer is rerendered. The caller sets the
  /// connection's, and keeps what it becomes.
  pub graph_straddle            : Option<Arc<GraphStraddle>>,
  /// Which changes the rerenders report as no buffer corrections: a
  /// save's rerender says what the save deleted and what its
  /// nodeMerge requests touched, a
  /// skgrepo-set switch whether the skgrepo restriction changed; every other
  /// rerender reports every change.
  pub buffer_correction_policy  : BufferCorrectionPolicy,
  /// True only for the "integrate unshown changes" request, which
  /// rerenders the one buffer of the diff view it names
  /// ('rerender_diffView_buffer').
  pub integrates_unshown_changes : bool,
}

impl<'a> RerenderPassContext<'a> {
  pub fn new (
    env                        : &'a SkgEnv,
    runtime                    : Arc<RuntimeGeneration>,
    skgrepo_restriction        : Option<&'a SkgrepoRestriction>,
    graph_straddle             : Option<Arc<GraphStraddle>>,
    buffer_correction_policy   : BufferCorrectionPolicy,
    integrates_unshown_changes : bool,
  ) -> RerenderPassContext<'a> {
    RerenderPassContext {
      env, runtime, skgrepo_restriction, graph_straddle,
      buffer_correction_policy, integrates_unshown_changes,
      errors   : Vec::new (),
      warnings : Vec::new (), } }
}
