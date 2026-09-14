//! Death-leafward: a folder self-checks its required ancestry at its BFS visit.
//!
//! See TODO/local-view-update/DONE/propagate-death-leafward/plan.org. In the
//! single-visit BFS (complete.rs), each folder (a non-vognode viewnode: QualFolder,
//! PartnerFolder) is reconciled at its own visit, and that reconcile reads its
//! ancestor vognode(s). If a required ancestor died this save, the read would
//! hit a missing NodeComplete, return Err, and abort the whole (often
//! collateral) rerender. To prevent that, every folder first self-checks its
//! required ancestry; if it is a *generalized orphan* it deadens itself and
//! disposes its children instead of reconciling.
//!
//! "Generalized orphan": traditionally in CS an "orphan" is a node whose
//! *immediate parent* is gone. Ours is generalized to the *whole required
//! ancestry chain*: a folder is a generalized orphan if ANY ancestor in its
//! required-ancestry chain -- not only the parent -- is the wrong viewnode
//! kind. Death is a VIEW property, not a graph property: a position that must
//! be a vognode holding anything but Vognode::Active, or a position that must
//! be a specific folder holding anything but that folder, is dead. No
//! graph / NodeComplete read is involved. This is sound because, in BFS order,
//! an Active node converts to Deleted at its own visit, strictly before any
//! descendant folder is visited; so by the time a folder checks, a dead ancestor
//! already shows the wrong kind in the tree.
//!
//! The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required-ancestry table is the single source of truth. Reconciles
//! read their ancestors *through* `required_ancestor`, so a reconcile can
//! never read an ancestor the table does not list, and the death-check and the
//! reads share one spec (the per-folder *field* each reconcile then pulls --
//! subscriber.hides vs subscribee.contains -- stays per-folder).

use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::{ read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree };
use crate::types::tree::viewnode_nodecomplete::write_at_activeNode_in_tree;
use crate::types::viewnode::{ AffectsParent, PartnerFolder, ViewNode, ViewNodeKind, Vognode };
use crate::update_buffer::util::detach_scaffold_transferring_focus;

use ego_tree::{ NodeId, NodeRef, Tree };
use std::error::Error;

/// One position in a folder's required ancestry (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table). A position is matched
/// against the *actual ancestor at that tree depth*.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedAncestor {
  /// Must be a `Vognode::Active` (anything else at this depth means death).
  NormalVognode,
  /// Must be exactly this folder kind (TODO/DONE/local-view-update/plan_v2.org §19: a folder = a collecting scaffold). The
  /// only intermediate-chain folder the table uses is the SubscribeeFolder.
  Folder (PartnerFolder),
}

// The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required-ancestry table, nearest -> farthest. required_ancestry[i] is
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
const ANC_NONE : &[ExpectedAncestor] = &[];

/// The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required ancestry for a folder kind, nearest -> farthest. Non-folder kinds
/// have none (they are never orphan-checked).
fn required_ancestry (
  kind : &ViewNodeKind,
) -> &'static [ExpectedAncestor] {
  match kind {
    // QualFolder(Alias) and QualFolder(ID): parent Active vognode.
    ViewNodeKind::QualFolder (_) => ANC_NORMAL,
    // Subscribee folder: parent = subscriber (Normal).
    // PartnerFolders (Subscriber/Overridden/Overrider/Hider/Hidden): parent
    // Active vognode.
    ViewNodeKind::PartnerFolder (PartnerFolder::Subscribee)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Subscriber)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Overridden)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Overrider)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Hider)
      | ViewNodeKind::PartnerFolder (PartnerFolder::Hidden) => ANC_NORMAL,
    ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) =>
      ANC_HIDDEN_OUTSIDE,
    ViewNodeKind::PartnerFolder (PartnerFolder::HiddenInSubscribee) =>
      ANC_HIDDEN_IN,
    _ => ANC_NONE,
  } }

fn matches_expected (
  actual   : &ViewNodeKind,
  expected : &ExpectedAncestor,
) -> bool {
  match expected {
    ExpectedAncestor::NormalVognode =>
      matches! ( actual, ViewNodeKind::Vognode (Vognode::Active (_)) ),
    ExpectedAncestor::Folder (rc) =>
      matches! ( actual, ViewNodeKind::PartnerFolder (r) if r == rc ),
  } }

/// Whether `kind` is a FOLDER -- in Jeff's terminology (TODO/DONE/local-view-update/plan_v2.org §19) a folder is a
/// *collecting* scaffold, i.e. a QualFolder (ID/Alias) or a PartnerFolder. (A SCAFFOLD
/// is any non-Vognode viewnode; the non-folder scaffolds -- Qual leaves, BufferRoot,
/// DeadScaffold -- are excluded here.) Folders are the kinds that carry a required
/// ancestry and so are subject to the generalized-orphan check.
pub fn is_folder_kind (
  kind : &ViewNodeKind,
) -> bool {
  matches! ( kind,
    ViewNodeKind::QualFolder (_) | ViewNodeKind::PartnerFolder (_) ) }

fn ancestor_nodeid (
  tree       : &Tree<ViewNode>,
  node       : NodeId,
  generation : usize,
) -> Option<NodeId> {
  let mut node_ref : NodeRef<ViewNode> = tree . get (node) ?;
  for _ in 0 .. generation {
    node_ref = node_ref . parent () ?; }
  Some ( node_ref . id () ) }

/// A folder is a *generalized orphan* (see module doc -- ANY required ancestor in
/// the full chain, not just the parent, is the wrong viewnode kind) iff,
/// walking its required ancestry nearest -> farthest, some required ancestor's
/// actual viewnode kind does not match the spec. Short-circuits on the first
/// mismatch, so we only ever look past the parent when the parent is fine. A
/// missing ancestor (chain shorter than required) also counts as orphaned.
/// Purely view-based: no graph / NodeComplete read.
pub fn folder_is_generalized_orphan (
  tree : &Tree<ViewNode>,
  folder  : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let kind : ViewNodeKind =
    read_at_node_in_tree ( tree, folder, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  for (i, expected) in required_ancestry (&kind) . iter () . enumerate () {
    let depth : usize = i + 1;
    let matches : bool =
      read_at_ancestor_in_tree (
        tree, folder, depth,
        |vn : &ViewNode| matches_expected (&vn . kind, expected) )
      . unwrap_or (false); // cannot climb that high => orphaned
    if ! matches { return Ok (true); } }
  Ok (false) }

/// The NodeId of the folder's i-th required ancestor (per the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table for its
/// kind); `None` if i is past the end of the table for this kind.
///
/// RELIES ON THE ORPHAN PRE-CHECK: it does NOT re-validate that each ancestor is
/// the kind the table demands. 'folder_is_generalized_orphan' runs in the BFS
/// dispatch and deadens any folder with a wrong-kind required ancestry *before* its
/// reconcile runs, and every caller of this is inside a reconcile -- so by the
/// time we get here the chain is already known-valid, and we can read the
/// table-indexed ancestor directly. (The kind-validation lives in exactly one
/// place, 'folder_is_generalized_orphan'.)
pub fn required_ancestor (
  tree : &Tree<ViewNode>,
  folder  : NodeId,
  i    : usize,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let kind : ViewNodeKind =
    read_at_node_in_tree ( tree, folder, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let spec : &[ExpectedAncestor] = required_ancestry (&kind);
  if i >= spec . len () { return Ok (None); }
  Ok ( ancestor_nodeid (tree, folder, i + 1) ) }

/// The (pid, source) of the folder's i-th required ancestor, read *through* the
/// TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table. Errors if that ancestor is absent (the folder is a generalized
/// orphan up to i) -- unreachable in practice, since the BFS dispatch runs the
/// orphan pre-check and deadens an orphan before its reconcile is ever called,
/// but the contract is what keeps a reconcile from reading an unlisted ancestor.
pub fn pid_and_source_from_required_ancestor (
  tree   : &Tree<ViewNode>,
  folder    : NodeId,
  i      : usize,
  caller : &str,
) -> Result<(ID, SourceName), Box<dyn Error>> {
  let anc : NodeId =
    required_ancestor (tree, folder, i) ?
    . ok_or_else ( || format! (
      "{}: required ancestor {} is absent (generalized orphan)",
      caller, i ) ) ?;
  read_at_node_in_tree (
    tree, anc,
    |vn : &ViewNode| match &vn . kind {
      ViewNodeKind::Vognode (v) =>
        v . pid_and_source ()
        . map ( |(pid, source)| (pid . clone (), source . clone ()) ),
      _ => None } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?
    . ok_or_else ( || format! (
      "{}: required ancestor {} has no pid/source", caller, i ) . into () ) }

/// Deaden a generalized-orphan folder at its BFS visit (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5): dispose each
/// direct child, then convert the folder itself to a DeadScaffold and skip its
/// reconcile. The BFS still visits the (disposed) children; a demoted-
/// Independent survivor is visited normally, a DeadScaffold child is a no-op.
pub fn deaden_generalized_orphan_folder (
  tree : &mut Tree<ViewNode>,
  folder  : NodeId,
) -> Result<(), Box<dyn Error>> {
  let child_ids : Vec<NodeId> =
    tree . get (folder)
    . ok_or ("deaden_generalized_orphan_folder: folder not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  for cid in child_ids {
    dispose_orphaned_folder_child (tree, cid) ?; }
  write_at_node_in_tree ( tree, folder,
    |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

/// Dispose one direct child of a deadened orphan folder (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5.1.a):
/// - an Affected (affectsParent=Affected Normal) view-leaf -> delete;
/// - an Affected branch (has children) -> demote to affectsParent=Independent, so
///   the user's subtree survives;
/// - a nested folder (QualFolder / PartnerFolder) -> LEAVE it untouched: it is itself a
///   generalized orphan under this now-dead folder, so it deadens itself -- and
///   disposes ITS OWN children -- at its own BFS visit. (This DEVIATES from
///   TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5.1.a, which converted nested folders to DeadScaffold here; doing
///   so would freeze a nested folder before it could dispose its own leaf members,
///   leaving them stranded under a dead chain. Leaving it to self-deaden is
///   what the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5 note actually intends -- "a nested folder ... deadens
///   itself at its own visit too" -- and is what makes leaf members at every
///   depth die. A Qual leaf does NOT self-dispatch, so it is still converted
///   below.)
/// - any other non-vognode, non-phantom child (a Qual, a DeadScaffold) -> convert
///   to DeadScaffold;
/// - any other child (a non-Affected vognode -- an Independent Active or an
///   Inactive -- or any Phantom: Diff/Deleted/Unknown) -> keep untouched.
fn dispose_orphaned_folder_child (
  tree  : &mut Tree<ViewNode>,
  child : NodeId,
) -> Result<(), Box<dyn Error>> {
  let (affected, is_leaf, is_vognode_or_phantom, is_folder)
    : (bool, bool, bool, bool) = {
    let c : NodeRef<ViewNode> = tree . get (child)
      . ok_or ("dispose_orphaned_folder_child: child not found") ?;
    ( c . value () . is_activeNode_and_affectsParent_true (),
      c . children () . next () . is_none (),
      matches! ( &c . value () . kind,
                 ViewNodeKind::Vognode (_) | ViewNodeKind::Phantom (_) ),
      is_folder_kind (&c . value () . kind) ) };
  if affected {
    if is_leaf {
      detach_scaffold_transferring_focus (tree, child) ?;
    } else {
      write_at_activeNode_in_tree ( tree, child,
        |t| { t . affectsParent = AffectsParent::False; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  } else if is_folder {
    // Leave it: a nested folder self-deadens at its own visit (see doc above).
  } else if ! is_vognode_or_phantom {
    write_at_node_in_tree ( tree, child,
      |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  // else: a non-Affected vognode or a phantom -- kept untouched.
  Ok (( )) }

#[cfg(test)]
#[path = "../../tests/unit/ancestry.rs"]
mod tests;
