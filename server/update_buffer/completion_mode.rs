//! Which kind of view a refresh refreshes: a content view, or a buffer
//! of the diff view over a graph straddle ('docs/diff-view.org',
//! "Rendering a buffer"). The mode is constant over one refresh; the
//! refresh's diff-view mode ('server/refresh/diff_view.rs') says what
//! it changes.

use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::diff_view::readiness_store::ReadinessStore;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::misc::ID;
use crate::types::views_state::DiffViewBufferKind;

#[derive(Clone, Copy)]
pub enum CompletionMode<'a> {
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

impl<'a> CompletionMode<'a> {
  /// Whether this extension of 'pid' is charged to the node budget
  /// ('server/refresh/extent.rs'). In a content view, every extension
  /// is. The diff view deviates ('docs/diff-view.org', "Editability in
  /// the diff view"): a node of U is never charged, whether the pruned
  /// change view's rule or a request made it editable, so the budget
  /// bounds only the extensions that an editable-view request makes
  /// outside U.
  pub fn charges_node_budget (
    &self,
    pid                      : &ID,
    had_editableViewRequest  : bool,
  ) -> bool {
    match self {
      CompletionMode::ContentView => true,
      CompletionMode::DiffView { straddle, buffer_kind, .. } =>
        had_editableViewRequest
        && ! prunedChangeView_sets_of_buffer (straddle, *buffer_kind)
             . u . contains (pid), } }
}
