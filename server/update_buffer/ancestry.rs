//! Death-leafward: a folder self-checks its required ancestry at its BFS visit.
//!
//! In the
//! single-visit BFS (complete.rs), each folder (a non-vognode viewnode: PropertyFolder,
//! PartnerFolder) is reconciled at its own visit, and that reconcile reads its
//! ancestor vognode(s). If a required ancestor died this save, the read would
//! hit a missing Graphnode, return Err, and abort the whole (often
//! collateral) rerender. To prevent that, every folder first self-checks its
//! required ancestry; if it is a *generalized orphan* it entombstones itself and
//! disposes its children instead of reconciling.
//!
//! "Generalized orphan": traditionally in CS an "orphan" is a node whose
//! *immediate parent* is gone. Ours is generalized to the *whole required
//! ancestry chain*: a folder is a generalized orphan if any ancestor in its
//! required-ancestry chain -- not only the parent -- is the wrong viewnode
//! kind. Death is a view property, not a graph property: a position that must
//! be a vognode holding anything but Vognode::Unrestricted, or a position that must
//! be a specific folder holding anything but that folder, is dead. No
//! graph / Graphnode read is involved. This is sound because, in BFS order,
//! an Unrestricted node converts to Deleted at its own visit, strictly before any
//! descendant folder is visited; so by the time a folder checks, a dead ancestor
//! already shows the wrong kind in the tree.
//!
//! The required-ancestry table below ('required_ancestry') is the single source of truth. Reconciles
//! read their ancestors *through* `required_ancestor`, so a reconcile can
//! never read an ancestor the table does not list, and the death-check and the
//! reads share one spec (the per-folder *field* each reconcile then pulls --
//! subscriber.hides vs subscribee.contains -- stays per-folder).

use crate::types::misc::{ID, SkgrepoName};
use crate::types::tree::generic::{ read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree };
use crate::types::viewnode::{ PartnerFolder, Phantom, Viewnode, ViewnodeKind, Vognode };
use crate::update_buffer::util::{Unwarranted, convert_unwarranted_child};

use ego_tree::{ NodeId, NodeRef, Tree };
use std::error::Error;

/// One position in a folder's required ancestry (the table below). A position is matched
/// against the *actual ancestor at that tree depth*.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedAncestor {
  /// Must be a `Vognode::Unrestricted` (anything else at this depth means death).
  NormalVognode,
  /// Must be exactly this folder kind (a folder is a collecting non-vognode; see 'is_folder_kind'). The
  /// only intermediate-chain folder the table uses is the SubscribeeFolder.
  Folder (PartnerFolder),
  /// Must be a `Phantom::Historical` (the parent of an AcquiredBy folder).
  HistoricalPhantom,
}

// The required-ancestry table, nearest -> farthest. required_ancestry[i] is
// matched against the actual ancestor at tree depth i+1 (0 = the folder itself).
const ANC_NORMAL : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_OUTSIDE : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Folder (PartnerFolder::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_IN : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode,
     ExpectedAncestor::Folder (PartnerFolder::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_HISTORICAL : &[ExpectedAncestor] =
  &[ ExpectedAncestor::HistoricalPhantom ];
const ANC_NONE : &[ExpectedAncestor] = &[];

/// The required ancestry for a folder kind, nearest -> farthest. Non-folder kinds
/// have none (they are never orphan-checked).
fn required_ancestry (
  kind : &ViewnodeKind,
) -> &'static [ExpectedAncestor] {
  match kind {
    // PropertyFolder(Alias) and PropertyFolder(ID): parent Unrestricted vognode.
    ViewnodeKind::PropertyFolder (_) => ANC_NORMAL,
    // Subscribee folder: parent = subscriber (Unrestricted).
    // PartnerFolders (Subscriber/Overridden/Overrider/Hider/Hidden): parent
    // Unrestricted vognode.
    ViewnodeKind::PartnerFolder (PartnerFolder::Subscribee)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Subscriber)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overridden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Overrider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hider)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Hidden)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Container)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Mentioner)
      | ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) => ANC_NORMAL,
    ViewnodeKind::PartnerFolder (PartnerFolder::AcquiredBy) =>
      ANC_HISTORICAL,
    ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      ANC_HIDDEN_OUTSIDE,
    ViewnodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee) =>
      ANC_HIDDEN_IN,
    _ => ANC_NONE,
  } }

fn matches_expected (
  actual   : &ViewnodeKind,
  expected : &ExpectedAncestor,
) -> bool {
  match expected {
    ExpectedAncestor::NormalVognode =>
      matches! ( actual, ViewnodeKind::Vognode (Vognode::Unrestricted (_)) ),
    ExpectedAncestor::Folder (rc) =>
      matches! ( actual, ViewnodeKind::PartnerFolder (r) if r == rc ),
    ExpectedAncestor::HistoricalPhantom =>
      matches! ( actual,
                 ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Historical (_))) ),
  } }

/// Whether `kind` is a folder -- a folder is a
/// *collecting* non-vognode, i.e. a PropertyFolder (ID/Alias) or a PartnerFolder. (A non-vognode
/// is any non-Vognode viewnode; the non-folders -- Property leaves, BufferRoot,
/// Tombstone -- are excluded here.) Folders are the kinds that carry a required
/// ancestry and so are subject to the generalized-orphan check.
pub fn is_folder_kind (
  kind : &ViewnodeKind,
) -> bool {
  matches! ( kind,
    ViewnodeKind::PropertyFolder (_) | ViewnodeKind::PartnerFolder (_) ) }

fn ancestor_treeid (
  tree                : &Tree<Viewnode>,
  node                : NodeId,
  ancestor_generation : usize,
) -> Option<NodeId> {
  let mut node_ref : NodeRef<Viewnode> = tree . get (node) ?;
  for _ in 0 .. ancestor_generation {
    node_ref = node_ref . parent () ?; }
  Some ( node_ref . id () ) }

/// A folder is a *generalized orphan* (see module doc -- any required ancestor in
/// the full chain, not just the parent, is the wrong viewnode kind) iff,
/// walking its required ancestry nearest -> farthest, some required ancestor's
/// actual viewnode kind does not match the spec. Short-circuits on the first
/// mismatch, so we only ever look past the parent when the parent is fine. A
/// missing ancestor (chain shorter than required) also counts as orphaned.
/// Purely view-based: no graph / Graphnode read.
pub fn folder_is_generalized_orphan (
  tree : &Tree<Viewnode>,
  folder  : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let kind : ViewnodeKind =
    read_at_node_in_tree ( tree, folder, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  for (i, expected) in required_ancestry (&kind) . iter () . enumerate () {
    let depth : usize = i + 1;
    let matches : bool =
      read_at_ancestor_in_tree (
        tree, folder, depth,
        |vn : &Viewnode| matches_expected (&vn . kind, expected) )
      . unwrap_or (false); // cannot climb that high => orphaned
    if ! matches { return Ok (true); } }
  Ok (false) }

/// The NodeId of the folder's i-th required ancestor (per the required-ancestry table for its
/// kind); `None` if i is past the end of the table for this kind.
///
/// RELIES ON THE ORPHAN PRE-CHECK: it does not re-validate that each ancestor is
/// the kind the table demands. 'folder_is_generalized_orphan' runs in the BFS
/// dispatch and entombstones any folder with a wrong-kind required ancestry *before* its
/// reconcile runs, and every caller of this is inside a reconcile -- so by the
/// time we get here the chain is already known-valid, and we can read the
/// table-indexed ancestor directly. (The kind-validation lives in exactly one
/// place, 'folder_is_generalized_orphan'.)
pub fn required_ancestor (
  tree : &Tree<Viewnode>,
  folder  : NodeId,
  i    : usize,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let kind : ViewnodeKind =
    read_at_node_in_tree ( tree, folder, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let spec : &[ExpectedAncestor] = required_ancestry (&kind);
  if i >= spec . len () { return Ok (None); }
  Ok ( ancestor_treeid (tree, folder, i + 1) ) }

/// The (pid, skgrepo) of the folder's i-th required ancestor, read *through* the
/// required-ancestry table. Errors if that ancestor is absent (the folder is a generalized
/// orphan up to i) -- unreachable in practice, since the BFS dispatch runs the
/// orphan pre-check and entombstones an orphan before its reconcile is ever called,
/// but the contract is what keeps a reconcile from reading an unlisted ancestor.
pub fn pid_and_skgrepo_from_required_ancestor (
  tree   : &Tree<Viewnode>,
  folder    : NodeId,
  i      : usize,
  caller : &str,
) -> Result<(ID, SkgrepoName), Box<dyn Error>> {
  let anc : NodeId =
    required_ancestor (tree, folder, i) ?
    . ok_or_else ( || format! (
      "{}: required ancestor {} is absent (generalized orphan)",
      caller, i ) ) ?;
  read_at_node_in_tree (
    tree, anc,
    |vn : &Viewnode| match &vn . kind {
      ViewnodeKind::Vognode (v) if v . is_current_graphnode () =>
        v . pid_and_skgrepo ()
        . map ( |(pid, skgrepo)| (pid . clone (), skgrepo . clone ()) ),
      _ => None } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?
    . ok_or_else ( || format! (
      "{}: required ancestor {} has no pid/repo", caller, i ) . into () ) }

/// Entombstone a generalized-orphan folder at its BFS visit: dispose each
/// direct child, then convert the folder itself to a tombstone and skip its
/// reconcile. The BFS still visits the (disposed) children; a survivor
/// made a non-member is visited normally, a tombstone child is a no-op.
pub fn entombstone_generalized_orphan_folder (
  tree : &mut Tree<Viewnode>,
  folder  : NodeId,
) -> Result<(), Box<dyn Error>> {
  let child_skgids : Vec<NodeId> =
    tree . get (folder)
    . ok_or ("entombstone_generalized_orphan_folder: folder not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  for cid in child_skgids {
    dispose_orphaned_folder_child (tree, cid) ?; }
  write_at_node_in_tree ( tree, folder,
    |vn : &mut Viewnode| { vn . replace_kind (ViewnodeKind::Tombstone); } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

/// Dispose one direct child of an entombstoned orphan folder, by the one
/// rule ('convert_unwarranted_child'):
/// - a member (an Unrestricted member) -> convert it: a
///   view-leaf to a tombstone, which the prune removes; a branch
///   retained as non-member, so the user's subtree survives;
/// - a nested folder (PropertyFolder / PartnerFolder) -> leave it untouched: it is itself a
///   generalized orphan under this now-dead folder, so it entombstones itself -- and
///   disposes its own children -- at its own BFS visit. (Converting a nested
///   folder to Tombstone here would freeze it before it could dispose its
///   own leaf members, leaving them stranded under a dead chain. Leaving it to
///   self-entombstone is what makes leaf members at every depth die. A Property
///   leaf does not self-dispatch, so it is still converted below.)
/// - any other non-vognode child (a Property, a tombstone) -> convert
///   it to a tombstone;
/// - any other child (a non-member vognode -- a non-member Unrestricted or an
///   Restricted -- or any Phantom: Diff/Deleted/Unknown) -> keep untouched.
fn dispose_orphaned_folder_child (
  tree  : &mut Tree<Viewnode>,
  child : NodeId,
) -> Result<(), Box<dyn Error>> {
  let (affected, is_leaf, is_vognode_or_phantom, is_folder)
    : (bool, bool, bool, bool) = {
    let c : NodeRef<Viewnode> = tree . get (child)
      . ok_or ("dispose_orphaned_folder_child: child not found") ?;
    ( c . value () . is_unrestrictedVognode_and_member (),
      c . children () . next () . is_none (),
      matches! ( &c . value () . kind,
                 ViewnodeKind::Vognode (_) ),
      is_folder_kind (&c . value () . kind) ) };
  if affected || ( ! is_folder && ! is_vognode_or_phantom ) {
    convert_unwarranted_child (tree, child, is_leaf, Unwarranted::Stale) ?; }
  // else: a nested folder (it self-entombstones at its own visit; see the
  // doc above), a non-member vognode or a phantom -- kept untouched.
  Ok (( )) }

#[cfg(test)]
#[path = "../../tests/unit/ancestry.rs"]
mod tests;
