//! The ancestry table: which ancestors each kind of descriptor may
//! stand under. A descriptor is a headline that describes its
//! viewparent rather than standing for a gnid: a folder, a property
//! member or a change viewnode. A descriptor whose ancestors are not
//! the kinds its row requires is unsayable, and the refresh
//! entombstones it ('ladder.rs', "re-kind"); its children stay under
//! the tombstone. A vognode has no row: an occurrence is never
//! unsayable.
//!
//! The stats pass reads a folder's ancestors through the table too
//! ('required_ancestor_of').

use crate::types::viewnode::{
  ChangeViewnode, Folder, PropertyMember, Viewnode, ViewnodeKind, Vognode};

use ego_tree::iter::Ancestors;
use ego_tree::{NodeId, NodeRef, Tree};
use std::mem::discriminant;

/// One position in a descriptor's required ancestry, matched against
/// the actual ancestor at that depth.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ExpectedAncestor {
  /// An occurrence ('Vognode::Occurrence').
  Occurrence,
  /// A folder of this kind (a flags folder, whatever its text).
  Folder (Folder),
  /// A ghost: the viewparent of an acquired-by folder.
  Ghost,
  /// An occurrence or a ghost, as the diff view draws change viewnodes.
  OccurrenceOrGhost,
  /// The old text of a node: the viewparent of its body diff.
  OldText,
}

// The rows, nearest ancestor first: row[i] is matched against the
// ancestor i+1 generations up.
const ANC_OCCURRENCE : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Occurrence ];
const ANC_HIDDEN_OUTSIDE : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Folder (Folder::Subscribee),
     ExpectedAncestor::Occurrence ];
const ANC_HIDDEN_IN : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Occurrence,
     ExpectedAncestor::Folder (Folder::Subscribee),
     ExpectedAncestor::Occurrence ];
const ANC_GHOST : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Ghost ];
const ANC_ALIAS_FOLDER : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Folder (Folder::Alias) ];
const ANC_ID_FOLDER : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Folder (Folder::ID) ];
const ANC_FLAGS_FOLDER : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Folder (Folder::Flags {
       title : String::new (), body : None }) ];
const ANC_OCCURRENCE_OR_GHOST : &[ExpectedAncestor] =
  &[ ExpectedAncestor::OccurrenceOrGhost ];
const ANC_OLD_TEXT : &[ExpectedAncestor] =
  &[ ExpectedAncestor::OldText ];
const ANC_NONE : &[ExpectedAncestor] = &[];

/// The required ancestry of a kind, nearest first. A vognode, a
/// tombstone and the buffer root require none.
pub fn required_ancestry (
  kind : &ViewnodeKind,
) -> &'static [ExpectedAncestor] {
  match kind {
    ViewnodeKind::Folder (folder) => match folder {
      Folder::Alias | Folder::ID | Folder::Flags { .. }
        | Folder::Subscribee | Folder::Subscriber
        | Folder::Overridden | Folder::Overrider
        | Folder::Hider | Folder::Hidden
        | Folder::Container | Folder::Mentioner
        | Folder::Acquired => ANC_OCCURRENCE,
      Folder::AcquiredBy => ANC_GHOST,
      Folder::HiddenOutsideOfSubscribee => ANC_HIDDEN_OUTSIDE,
      Folder::HiddenInSubscribee => ANC_HIDDEN_IN, },
    ViewnodeKind::PropertyMember (member) => match member {
      PropertyMember::Alias { .. } => ANC_ALIAS_FOLDER,
      PropertyMember::ID { .. }    => ANC_ID_FOLDER,
      PropertyMember::Flag { .. }  => ANC_FLAGS_FOLDER, },
    ViewnodeKind::ChangeViewnode (change) => match change {
      ChangeViewnode::BodyDiff { .. } => ANC_OLD_TEXT,
      ChangeViewnode::OldText { .. }
        | ChangeViewnode::HomeSkgrepoMove { .. }
        | ChangeViewnode::RelRepoMove { .. }
        | ChangeViewnode::ChangesetMarker { .. } => ANC_OCCURRENCE_OR_GHOST, },
    ViewnodeKind::Vognode (_)
      | ViewnodeKind::BufferRoot
      | ViewnodeKind::Tombstone => ANC_NONE, } }

/// Whether 'actual' is what 'expected' requires.
pub fn matches_expected (
  actual   : &ViewnodeKind,
  expected : &ExpectedAncestor,
) -> bool {
  match expected {
    ExpectedAncestor::Occurrence =>
      matches! ( actual, ViewnodeKind::Vognode (Vognode::Occurrence (_)) ),
    ExpectedAncestor::Folder (required) =>
      matches! ( actual, ViewnodeKind::Folder (folder)
                         if discriminant (folder) == discriminant (required) ),
    ExpectedAncestor::Ghost =>
      matches! ( actual, ViewnodeKind::Vognode (Vognode::Ghost (_)) ),
    ExpectedAncestor::OccurrenceOrGhost =>
      matches! ( actual, ViewnodeKind::Vognode (
        Vognode::Occurrence (_) | Vognode::Ghost (_) )),
    ExpectedAncestor::OldText =>
      matches! ( actual, ViewnodeKind::ChangeViewnode (
        ChangeViewnode::OldText { .. } )), } }

/// Whether the headline at 'treeid' is unsayable where it stands:
/// some ancestor its kind's row requires is of another kind, or
/// missing.
pub fn is_unsayable (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
) -> bool {
  let node : NodeRef<Viewnode> = tree . get (treeid) . unwrap ();
  let mut ancestors : Ancestors<Viewnode> = node . ancestors ();
  required_ancestry (&node . value () . kind) . iter ()
    . any ( |expected| match ancestors . next () {
      Some (ancestor) => ! matches_expected (&ancestor . value () . kind, expected),
      None => true, } ) }

/// The 'i'-th required ancestor of the descriptor at 'treeid' (0 =
/// its viewparent), if its row lists that many.
pub fn required_ancestor_of (
  tree   : &Tree<Viewnode>,
  treeid : NodeId,
  i      : usize,
) -> Option<NodeId> {
  let node : NodeRef<Viewnode> = tree . get (treeid) ?;
  if i >= required_ancestry (&node . value () . kind) . len () {
    return None; }
  node . ancestors () . nth (i) . map ( |ancestor| ancestor . id () ) }

/// Whether the save reads the claims of the children of 'parent': an
/// editable occurrence, or an editable folder whose node is an
/// editable occurrence. Under any other parent no child claims
/// membership, whatever it carries, and the printer writes no
/// 'noClaim' there.
pub fn parent_reads_claims (
  tree   : &Tree<Viewnode>,
  parent : NodeId,
) -> bool {
  match &tree . get (parent) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Occurrence (occurrence)) =>
      ! occurrence . is_writeProtected (),
    ViewnodeKind::Folder (folder) if folder . is_editable () => {
      let anchor_depth : usize = // the node the folder describes
        if matches! ( folder, Folder::HiddenOutsideOfSubscribee ) { 1 } else { 0 };
      required_ancestor_of (tree, parent, anchor_depth)
        . and_then ( |anchor| tree . get (anchor) )
        . is_some_and ( |anchor| anchor . value () . editable_gnid () . is_some () ) },
    _ => false, } }
