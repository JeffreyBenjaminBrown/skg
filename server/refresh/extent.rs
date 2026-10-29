//! The extent: how much of a parent's goal the refresh draws. All of
//! it for a folder and for an editable occurrence's content; in the
//! diff view, all of a ghost's old content where content stands; none
//! of it, beyond what the buffer already shows, for a write-protected
//! occurrence's content and for the containerward chain under a
//! folder's member.
//!
//! And the node budget, which bounds how many occurrences the refresh
//! makes editable: each editable occurrence whose content is drawn
//! charges it once, at its visit, in either mode. It need not count
//! exactly, only stop an explosion, so an occurrence the budget could
//! not have refused (one the buffer showed editable, a node of the
//! diff view's U) is charged too.
//!
//! An occurrence the refresh draws, where its parent is editable
//! content, is drawn write-protected carrying an editable-view
//! request, and so is a write-protected child that claims membership
//! under an occurrence carrying one: the request passes on to what it
//! extends. At the occurrence's own visit the request is consumed: the
//! occurrence becomes editable if no other occurrence of its node is,
//! and the budget allows, and stays write-protected otherwise
//! ('consume_editableView_request'). In the diff view the pruned
//! change view decides which drawn occurrences may be editable: one
//! whose node is in U, which is drawn editable at once, and the
//! budget never refuses it; and, under an occurrence carrying the
//! request, any other, as in a content view.

use crate::diff_view::pruned_change_view::prunedChangeView_sets_of_buffer;
use crate::refresh::{Walk, carries_editableView_request};
use crate::refresh::mode::RefreshMode;
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;
use crate::types::tree::forest::{holds_containerward_chain, in_content_position};
use crate::types::viewnode::{Editability, ViewRequest, Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef, Tree};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Extent {
  /// Every goal item without a child is drawn.
  All,
  /// Nothing is drawn.
  None,
}

/// How an occurrence the refresh draws, or re-kinds to one, stands.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum DrawnEditability {
  WriteProtected,
  /// Write-protected, carrying an editable-view request, which its
  /// visit consumes ('consume_editableView_request').
  Requested,
  /// Editable at once: in the diff view, a node of U drawn where no
  /// request extends.
  Editable,
}

/// Whether the vognode children of the occurrence at 'treeid' are the
/// containerward chain ('holds_containerward_chain').
pub(crate) fn is_chain_parent (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  holds_containerward_chain ( tree . get (treeid) . unwrap () ) }

/// How an occurrence of 'pid' the refresh draws under 'parent'
/// stands. It may be editable when its parent's children are editable
/// content, or it is a root of the view (one the ID rule makes an
/// occurrence again), and no other occurrence of the node is editable;
/// in the diff view, only when its node is in U, or the parent
/// carries an editable-view request. It is then drawn carrying the
/// request, but for a node of U under a parent that carries none,
/// which is drawn editable.
pub(crate) fn drawn_occurrence_editability (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  pid    : &ID,
  walk   : &Walk,
) -> DrawnEditability {
  let may_be_editable : bool =
    ( children_are_editable_content (tree, parent, walk)
      || matches! ( tree . get (parent) . unwrap () . value () . kind,
                    ViewnodeKind::BufferRoot ))
    && ! walk . editability_is_held (pid);
  if ! may_be_editable { return DrawnEditability::WriteProtected; }
  let requested : bool = carries_editableView_request (tree, parent);
  match walk . mode {
    RefreshMode::ContentView => DrawnEditability::Requested,
    RefreshMode::DiffView { .. } if requested => DrawnEditability::Requested,
    RefreshMode::DiffView { .. } if in_the_diffViews_u (pid, walk) =>
      DrawnEditability::Editable,
    RefreshMode::DiffView { .. } => DrawnEditability::WriteProtected, } }

/// Whether the diff view's U holds 'pid'. Never in a content view.
fn in_the_diffViews_u (
  pid  : &ID,
  walk : &Walk,
) -> bool {
  match walk . mode {
    RefreshMode::ContentView => false,
    RefreshMode::DiffView { straddle, buffer_kind, .. } =>
      prunedChangeView_sets_of_buffer (straddle, buffer_kind) . u
        . contains (pid), } }

/// Whether the children of 'parent' are editable content: those of
/// an editable occurrence, and in the diff view the old content of a
/// ghost where content stands.
pub(crate) fn children_are_editable_content (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> bool {
  match &tree . get (parent) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      ! occurrence . is_writeProtected (),
    ViewnodeKind::Vognode (Vognode::Ghost (_)) =>
      extent_of_parent (tree, parent, walk) == Extent::All,
    _ => false, } }

/// The extent of the parent at 'parent'.
pub(crate) fn extent_of_parent (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
  walk   : &Walk,
) -> Extent {
  let node : NodeRef<Viewnode> = tree . get (parent) . unwrap ();
  let all : bool = match &node . value () . kind {
    ViewnodeKind::Folder (_) => true,
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      ! occurrence . is_writeProtected (),
    ViewnodeKind::Vognode (Vognode::Ghost (ghost)) =>
      walk . is_diff_view ()
      && in_content_position (node)
      && ! node . ancestors () . any ( |ancestor| matches! (
           &ancestor . value () . kind,
           ViewnodeKind::Vognode (Vognode::Ghost (above))
             if above . gnid == ghost . gnid )),
    _ => false, };
  if all { Extent::All } else { Extent::None } }

/// At the visit of an occurrence, before its content is drawn: a
/// write-protected one carrying an editable-view request becomes
/// editable, with its body, if no other occurrence of its node is and
/// the budget allows (in the diff view, or its node is in U), and
/// otherwise stays write-protected. Then an editable one charges the
/// budget. The request stays until the visit ends
/// ('drop_editableView_request'), since what the visit draws asks
/// whether it extends.
pub(crate) fn consume_editableView_request (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
  walk   : &mut Walk,
) {
  let (pid, requested_and_writeProtected) : (ID, bool) =
    match &tree . get (parent) . unwrap () . value () . kind {
      ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
        ( occurrence . gnid . clone (),
          occurrence . is_writeProtected ()
          && occurrence . view_requests . contains (&ViewRequest::Editable) ),
      _ => return, };
  if requested_and_writeProtected
     && ( walk . node_budget > 0 || in_the_diffViews_u (&pid, walk) )
     && walk . hold_editability (&pid, parent) {
    let body : Option<String> =
      walk . truth . graph . get (&pid)
      . and_then ( |node : &GraphnodeInRust| node . body . clone () );
    if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
      &mut tree . get_mut (parent) . unwrap () . value () . kind
    { occurrence . editability = Editability::Editable { body, edit_request : None }; }}
  if tree . get (parent) . unwrap () . value () . editable_gnid () . is_some () {
    walk . node_budget = walk . node_budget . saturating_sub (1); }}

/// The visit of the occurrence at 'parent' is over: the editable-view
/// request it carried, consumed, goes.
pub(crate) fn drop_editableView_request (
  tree   : &mut Tree<Viewnode>,
  parent : NodeId,
) {
  if let ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =
    &mut tree . get_mut (parent) . unwrap () . value () . kind
  { occurrence . view_requests . remove (&ViewRequest::Editable); }}
