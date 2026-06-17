//! Death-leafward: a col self-checks its required ancestry at its BFS visit.
//!
//! See TODO/local-view-update/DONE/propagate-death-leafward/plan.org. In the
//! single-visit BFS (complete.rs), each col (a non-vognode viewnode: QualCol,
//! PartnerCol) is reconciled at its own visit, and that reconcile reads its
//! ancestor vognode(s). If a required ancestor died this save, the read would
//! hit a missing NodeComplete, return Err, and abort the whole (often
//! collateral) rerender. To prevent that, every col first self-checks its
//! required ancestry; if it is a *generalized orphan* it deadens itself and
//! disposes its children instead of reconciling.
//!
//! "Generalized orphan": traditionally in CS an "orphan" is a node whose
//! *immediate parent* is gone. Ours is generalized to the *whole required
//! ancestry chain*: a col is a generalized orphan if ANY ancestor in its
//! required-ancestry chain -- not only the parent -- is the wrong viewnode
//! kind. Death is a VIEW property, not a graph property: a position that must
//! be a vognode holding anything but Vognode::Active, or a position that must
//! be a specific col holding anything but that col, is dead. No
//! graph / NodeComplete read is involved. This is sound because, in BFS order,
//! an Active node converts to Deleted at its own visit, strictly before any
//! descendant col is visited; so by the time a col checks, a dead ancestor
//! already shows the wrong kind in the tree.
//!
//! The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required-ancestry table is the single source of truth. Reconciles
//! read their ancestors *through* `required_ancestor`, so a reconcile can
//! never read an ancestor the table does not list, and the death-check and the
//! reads share one spec (the per-col *field* each reconcile then pulls --
//! subscriber.hides vs subscribee.contains -- stays per-col).

use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::{ read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree };
use crate::types::tree::viewnode_nodecomplete::write_at_activeNode_in_tree;
use crate::types::viewnode::{ ParentIs, PartnerCol, ViewNode, ViewNodeKind, Vognode };
use crate::update_buffer::util::detach_scaffold_transferring_focus;

use ego_tree::{ NodeId, NodeRef, Tree };
use std::error::Error;

/// One position in a col's required ancestry (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table). A position is matched
/// against the *actual ancestor at that tree depth*.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedAncestor {
  /// Must be a `Vognode::Active` (anything else at this depth means death).
  NormalVognode,
  /// Must be exactly this col kind (TODO/DONE/local-view-update/plan_v2.org §19: a col = a collecting scaffold). The
  /// only intermediate-chain col the table uses is the SubscribeeCol.
  Col (PartnerCol),
}

// The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required-ancestry table, nearest -> farthest. required_ancestry[i] is
// matched against the actual ancestor at tree depth i+1 (0 = the col itself).
const ANC_NORMAL : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_OUTSIDE : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Col (PartnerCol::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_IN : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode,
     ExpectedAncestor::Col (PartnerCol::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_NONE : &[ExpectedAncestor] = &[];

/// The TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 required ancestry for a col kind, nearest -> farthest. Non-col kinds
/// have none (they are never orphan-checked).
fn required_ancestry (
  kind : &ViewNodeKind,
) -> &'static [ExpectedAncestor] {
  match kind {
    // QualCol(Alias) and QualCol(ID): parent Active vognode.
    ViewNodeKind::QualCol (_) => ANC_NORMAL,
    // Subscribee col: parent = subscriber (Normal).
    // PartnerCols (Subscriber/Overridden/Overrider/Hider/Hidden): parent
    // Active vognode.
    ViewNodeKind::PartnerCol (PartnerCol::Subscribee)
      | ViewNodeKind::PartnerCol (PartnerCol::Subscriber)
      | ViewNodeKind::PartnerCol (PartnerCol::Overridden)
      | ViewNodeKind::PartnerCol (PartnerCol::Overrider)
      | ViewNodeKind::PartnerCol (PartnerCol::Hider)
      | ViewNodeKind::PartnerCol (PartnerCol::Hidden) => ANC_NORMAL,
    ViewNodeKind::PartnerCol (PartnerCol::HiddenOutsideOfSubscribee) =>
      ANC_HIDDEN_OUTSIDE,
    ViewNodeKind::PartnerCol (PartnerCol::HiddenInSubscribee) =>
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
    ExpectedAncestor::Col (rc) =>
      matches! ( actual, ViewNodeKind::PartnerCol (r) if r == rc ),
  } }

/// Whether `kind` is a COL -- in Jeff's terminology (TODO/DONE/local-view-update/plan_v2.org §19) a col is a
/// *collecting* scaffold, i.e. a QualCol (ID/Alias) or a PartnerCol. (A SCAFFOLD
/// is any non-Vognode viewnode; the non-col scaffolds -- Qual leaves, BufferRoot,
/// DeadScaffold -- are excluded here.) Cols are the kinds that carry a required
/// ancestry and so are subject to the generalized-orphan check.
pub fn is_col_kind (
  kind : &ViewNodeKind,
) -> bool {
  matches! ( kind,
    ViewNodeKind::QualCol (_) | ViewNodeKind::PartnerCol (_) ) }

fn ancestor_nodeid (
  tree       : &Tree<ViewNode>,
  node       : NodeId,
  generation : usize,
) -> Option<NodeId> {
  let mut node_ref : NodeRef<ViewNode> = tree . get (node) ?;
  for _ in 0 .. generation {
    node_ref = node_ref . parent () ?; }
  Some ( node_ref . id () ) }

/// A col is a *generalized orphan* (see module doc -- ANY required ancestor in
/// the full chain, not just the parent, is the wrong viewnode kind) iff,
/// walking its required ancestry nearest -> farthest, some required ancestor's
/// actual viewnode kind does not match the spec. Short-circuits on the first
/// mismatch, so we only ever look past the parent when the parent is fine. A
/// missing ancestor (chain shorter than required) also counts as orphaned.
/// Purely view-based: no graph / NodeComplete read.
pub fn col_is_generalized_orphan (
  tree : &Tree<ViewNode>,
  col  : NodeId,
) -> Result<bool, Box<dyn Error>> {
  let kind : ViewNodeKind =
    read_at_node_in_tree ( tree, col, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  for (i, expected) in required_ancestry (&kind) . iter () . enumerate () {
    let depth : usize = i + 1;
    let matches : bool =
      read_at_ancestor_in_tree (
        tree, col, depth,
        |vn : &ViewNode| matches_expected (&vn . kind, expected) )
      . unwrap_or (false); // cannot climb that high => orphaned
    if ! matches { return Ok (true); } }
  Ok (false) }

/// The NodeId of the col's i-th required ancestor (per the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table for its
/// kind); `None` if i is past the end of the table for this kind.
///
/// RELIES ON THE ORPHAN PRE-CHECK: it does NOT re-validate that each ancestor is
/// the kind the table demands. 'col_is_generalized_orphan' runs in the BFS
/// dispatch and deadens any col with a wrong-kind required ancestry *before* its
/// reconcile runs, and every caller of this is inside a reconcile -- so by the
/// time we get here the chain is already known-valid, and we can read the
/// table-indexed ancestor directly. (The kind-validation lives in exactly one
/// place, 'col_is_generalized_orphan'.)
pub fn required_ancestor (
  tree : &Tree<ViewNode>,
  col  : NodeId,
  i    : usize,
) -> Result<Option<NodeId>, Box<dyn Error>> {
  let kind : ViewNodeKind =
    read_at_node_in_tree ( tree, col, |vn| vn . kind . clone () )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  let spec : &[ExpectedAncestor] = required_ancestry (&kind);
  if i >= spec . len () { return Ok (None); }
  Ok ( ancestor_nodeid (tree, col, i + 1) ) }

/// The (pid, source) of the col's i-th required ancestor, read *through* the
/// TODO/DONE/local-view-update/propagate-death-leafward/plan.org §3 table. Errors if that ancestor is absent (the col is a generalized
/// orphan up to i) -- unreachable in practice, since the BFS dispatch runs the
/// orphan pre-check and deadens an orphan before its reconcile is ever called,
/// but the contract is what keeps a reconcile from reading an unlisted ancestor.
pub fn pid_and_source_from_required_ancestor (
  tree   : &Tree<ViewNode>,
  col    : NodeId,
  i      : usize,
  caller : &str,
) -> Result<(ID, SourceName), Box<dyn Error>> {
  let anc : NodeId =
    required_ancestor (tree, col, i) ?
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

/// Deaden a generalized-orphan col at its BFS visit (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5): dispose each
/// direct child, then convert the col itself to a DeadScaffold and skip its
/// reconcile. The BFS still visits the (disposed) children; a demoted-
/// Independent survivor is visited normally, a DeadScaffold child is a no-op.
pub fn deaden_generalized_orphan_col (
  tree : &mut Tree<ViewNode>,
  col  : NodeId,
) -> Result<(), Box<dyn Error>> {
  let child_ids : Vec<NodeId> =
    tree . get (col)
    . ok_or ("deaden_generalized_orphan_col: col not found") ?
    . children () . map ( |c| c . id () ) . collect ();
  for cid in child_ids {
    dispose_orphaned_col_child (tree, cid) ?; }
  write_at_node_in_tree ( tree, col,
    |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; } )
    . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?;
  Ok (( )) }

/// Dispose one direct child of a deadened orphan col (TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5.1.a):
/// - an Affected (parentIs=Affected Normal) view-leaf -> delete;
/// - an Affected branch (has children) -> demote to parentIs=Independent, so
///   the user's subtree survives;
/// - a nested col (QualCol / PartnerCol) -> LEAVE it untouched: it is itself a
///   generalized orphan under this now-dead col, so it deadens itself -- and
///   disposes ITS OWN children -- at its own BFS visit. (This DEVIATES from
///   TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5.1.a, which converted nested cols to DeadScaffold here; doing
///   so would freeze a nested col before it could dispose its own leaf members,
///   leaving them stranded under a dead chain. Leaving it to self-deaden is
///   what the TODO/DONE/local-view-update/propagate-death-leafward/plan.org §5 note actually intends -- "a nested col ... deadens
///   itself at its own visit too" -- and is what makes leaf members at every
///   depth die. A Qual leaf does NOT self-dispatch, so it is still converted
///   below.)
/// - any other non-vognode, non-phantom child (a Qual, a DeadScaffold) -> convert
///   to DeadScaffold;
/// - any other child (a non-Affected vognode -- an Independent Active or an
///   Inactive -- or any Phantom: Diff/Deleted/Unknown) -> keep untouched.
fn dispose_orphaned_col_child (
  tree  : &mut Tree<ViewNode>,
  child : NodeId,
) -> Result<(), Box<dyn Error>> {
  let (affected, is_leaf, is_vognode_or_phantom, is_col)
    : (bool, bool, bool, bool) = {
    let c : NodeRef<ViewNode> = tree . get (child)
      . ok_or ("dispose_orphaned_col_child: child not found") ?;
    ( c . value () . is_activeNode_and_parentIs_affected (),
      c . children () . next () . is_none (),
      matches! ( &c . value () . kind,
                 ViewNodeKind::Vognode (_) | ViewNodeKind::Phantom (_) ),
      is_col_kind (&c . value () . kind) ) };
  if affected {
    if is_leaf {
      detach_scaffold_transferring_focus (tree, child) ?;
    } else {
      write_at_activeNode_in_tree ( tree, child,
        |t| { t . parentIs = ParentIs::Independent; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  } else if is_col {
    // Leave it: a nested col self-deadens at its own visit (see doc above).
  } else if ! is_vognode_or_phantom {
    write_at_node_in_tree ( tree, child,
      |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  // else: a non-Affected vognode or a phantom -- kept untouched.
  Ok (( )) }

#[cfg(test)]
#[path = "../../tests/unit/ancestry.rs"]
mod tests;
