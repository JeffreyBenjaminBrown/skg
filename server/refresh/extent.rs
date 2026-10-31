//! The extent: how much of a parent's goal the refresh draws. All of
//! it for a folder and for an editable occurrence's content; in the
//! diff view, all of a ghost's old content where content stands; none
//! of it, beyond what the buffer already shows, for a write-protected
//! occurrence's content and for the containerward chain under a
//! folder's member.
//!
//! Which occurrences are editable, and the node budget that bounds
//! how many the refresh makes so, are editability's
//! ('editability.rs').

use crate::refresh::Walk;
use crate::types::tree::forest::{holds_containerward_chain, in_content_position};
use crate::types::viewnode::{Viewnode, ViewnodeKind, Vognode};

use ego_tree::{NodeId, NodeRef, Tree};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Extent {
  /// Every goal item without a child is drawn.
  All,
  /// Nothing is drawn.
  None,
}

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