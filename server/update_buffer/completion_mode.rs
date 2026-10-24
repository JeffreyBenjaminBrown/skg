//! Which kind of view a view completion completes: a content view, or
//! a buffer of the diff view over a graph straddle ('docs/diff-view.org',
//! "Completion over the straddle"). The mode is constant over one
//! completion; what it changes at a visit is listed in the module
//! comment of 'server/diff_view/visit.rs'.

use crate::diff_view::changes::align_content_members_with_straddle;
use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::graph_straddle::types::GraphStraddle;
use crate::types::misc::ID;
use crate::types::viewnode::Viewnode;
use crate::types::views_state::DiffViewBufferKind;

use ego_tree::{NodeId, Tree};
use std::error::Error;

#[derive(Clone, Copy)]
pub enum CompletionMode<'a> {
  /// Completion from the live graph.
  ContentView,
  /// Completion of the buffer of kind 'buffer_kind' of the diff view,
  /// over 'straddle'. Its partner folders show the union of the
  /// straddle: the members of either graph, each tagged with the change
  /// of its node and of its relationship.
  DiffView { straddle    : &'a GraphStraddle,
             buffer_kind : DiffViewBufferKind },
}

impl<'a> CompletionMode<'a> {
  /// Whether the Subscribee folder has its own reconciler. In diff-view
  /// mode the generic partner-folder reconciler fills it.
  pub fn subscribeeFolder_has_own_reconciler (
    &self,
  ) -> bool {
    matches! ( self, CompletionMode::ContentView ) }

  /// Whether content is shown with override substitution. Not in
  /// diff-view mode: it shows raw graph facts.
  pub fn substitution_enabled (
    &self,
  ) -> bool {
    matches! ( self, CompletionMode::ContentView ) }

  /// Whether default partner folders and the hidden-in-subscribee
  /// folder may be created. Not in diff-view mode.
  pub fn default_folders_allowed (
    &self,
  ) -> bool {
    matches! ( self, CompletionMode::ContentView ) }

  /// Whether this extension of 'pid' is charged to the node budget (see
  /// the module comment of 'to_org/extend/editable.rs'). In a content
  /// view, every extension is. The diff view deviates
  /// ('docs/diff-view.org', "Editability in the diff view"): a node of U
  /// is never charged, whether the pruned change view's rule or a request
  /// made it editable, so the budget bounds only the extensions that an
  /// editable-view request makes outside U.
  pub fn charges_node_budget (
    &self,
    pid                      : &ID,
    had_editableViewRequest  : bool,
  ) -> bool {
    match self {
      CompletionMode::ContentView => true,
      CompletionMode::DiffView { straddle, buffer_kind } =>
        had_editableViewRequest
        && ! prunedChangeView_sets_of_buffer (straddle, *buffer_kind)
             . u . contains (pid), } }

  /// In diff-view mode, align an editable node's members with the
  /// straddle, after the editability rule settles its editability and before
  /// the content reconciler runs ('align_content_members_with_straddle').
  pub fn align_content_members_before_extension (
    &self,
    tree   : &mut Tree<Viewnode>,
    treeid : NodeId,
  ) -> Result<(), Box<dyn Error>> {
    match self {
      CompletionMode::ContentView => Ok (( )),
      CompletionMode::DiffView { straddle, .. } =>
        align_content_members_with_straddle (tree, treeid, straddle), } }
}
