//! Which kind of view a refresh refreshes: a content view, or a buffer
//! of the diff view over a graph straddle ('docs/diff-view.org',
//! "Rendering a buffer"). The mode is constant over one refresh; the
//! refresh's diff-view mode ('server/refresh/diff_view.rs') says what
//! it changes.

use crate::diff_view::readiness_store::ReadinessStore;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::views_state::DiffViewBufferKind;

#[derive(Clone, Copy)]
pub enum RefreshMode<'a> {
  /// The refresh of a content view, from the live graph.
  ContentView,
  /// The refresh of the buffer of kind 'buffer_kind' of the diff view,
  /// over 'straddle'. Its goals are the union of the straddle: the
  /// members of either graph, each tagged with the change of its node
  /// and of its relationship. 'readiness_store' gives the readiness
  /// facts.
  DiffView { straddle        : &'a GraphStraddle,
             buffer_kind     : DiffViewBufferKind,
             readiness_store : &'a ReadinessStore },
}
