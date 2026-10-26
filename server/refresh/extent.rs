//! The extent: how much of a parent's goal the refresh draws. All of
//! it for a folder and for an editable occurrence's content; in the
//! diff view, all of a ghost's old content where content stands; none
//! of it, beyond what the buffer already shows, for a write-protected
//! occurrence's content and for the containerward chain under a
//! folder's member.
//!
//! And the node budget, which bounds how many occurrences the refresh
//! makes editable: each occurrence whose content is drawn charges it
//! once ('CompletionMode::charges_node_budget'). Among the
//! occurrences the refresh draws, the first of a node is editable only
//! when its parent is editable content and the budget allows; a repeat
//! is write-protected. In the diff view the pruned change view decides
//! instead: a drawn occurrence is editable when its node is in U, or,
//! under an occurrence an editable-view request extends (the request
//! passes on to what it draws), when the budget allows, as in a
//! content view; only that last extension is charged.

use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::refresh::{Walk, write_protect_occurrence};
use crate::types::misc::ID;
use crate::types::tree::forest::{holds_containerward_chain, in_content_position};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};
use crate::update_buffer::completion_mode::CompletionMode;

use ego_tree::{NodeId, NodeRef, Tree};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Extent {
  /// Every goal item without a child is drawn.
  All,
  /// Nothing is drawn.
  None,
}

/// The extent of the parent at 'parent'.
pub(crate) fn extent_of_parent (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> Extent {
  let node : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
  let all : bool = match &node . value () . kind {
    ViewnodeKind::Folder (_) => true,
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      ! occurrence . is_writeProtected (),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      walk . is_diff_view ()
      && in_content_position (node)
      && ! node . ancestors () . any ( |ancestor| matches! (
           &ancestor . value () . kind,
           ViewnodeKind::Vognode (Vognode::Ghost (above))
             if above . skgid == ghost . skgid )),
    _ => false, };
  if all { Extent::All } else { Extent::None } }

/// Whether the vognode children of the occurrence at 'treeid' are the
/// containerward chain ('holds_containerward_chain').
pub(crate) fn is_chain_parent (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  holds_containerward_chain ( tree . get (treeid) . unwrap () ) }

/// Whether the children of 'parent' are editable content: those of
/// an editable occurrence, and in the diff view the old content of a
/// ghost where content stands.
pub(crate) fn children_are_editable_content (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> bool {
  match &tree . get (parent) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence)) =>
      ! occurrence . is_writeProtected (),
    ViewnodeKind::Vognode (Vognode::Ghost (_)) =>
      extent_of_parent (tree, parent, walk) == Extent::All,
    _ => false, } }

/// Whether an occurrence of 'pid' the refresh draws under 'parent' is
/// editable: its parent's children are editable content, no other
/// occurrence of the node is editable, and the budget allows (in the
/// diff view: the node is in U, or an editable-view request extends
/// 'parent' and the budget allows).
pub(crate) fn drawn_occurrence_is_editable (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> bool {
  children_are_editable_content (tree, parent, walk)
  && ! walk . editable_gnids . contains (pid)
  && match walk . mode {
    CompletionMode::ContentView => walk . node_budget > 0,
    CompletionMode::DiffView { straddle, buffer_kind } =>
      prunedChangeView_sets_of_buffer (straddle, buffer_kind) . u
        . contains (pid)
      || ( walk . had_editableView_request . contains (&parent)
           && walk . node_budget > 0 ), } }

/// At the visit of an editable occurrence, before its content is
/// drawn: one the refresh drew editable, when the budget is spent and
/// the mode would charge its extension, is write-protected instead;
/// any other charges the budget, if the mode charges its extension.
pub(crate) fn settle_budget_at_visit (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  let pid : ID = match &tree . get (parent) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (occurrence))
      if ! occurrence . is_writeProtected () => occurrence . skgid . clone (),
    _ => return, };
  let charged : bool = walk . mode . charges_node_budget (
    &pid, walk . had_editableView_request . contains (&parent) );
  if walk . node_budget == 0
    && walk . drawn_editable . contains (&parent)
    && charged {
    write_protect_occurrence (tree, parent, walk);
    return; }
  if charged {
    walk . node_budget = walk . node_budget . saturating_sub (1); }}
