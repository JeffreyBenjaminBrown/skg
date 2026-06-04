//! Death-leafward: a col self-checks its required ancestry at its BFS visit.
//!
//! See TODO/local-view-update/propagate-death-leafward/plan.org. In the
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
//! be a vognode holding anything but Vognode::Normal, or a position that must
//! be a specific scaffold holding anything but that scaffold, is dead. No
//! graph / NodeComplete read is involved. This is sound because, in BFS order,
//! a Normal node converts to Deleted at its own visit, strictly before any
//! descendant col is visited; so by the time a col checks, a dead ancestor
//! already shows the wrong kind in the tree.
//!
//! The §3 required-ancestry table is the single source of truth. Reconciles
//! read their ancestors *through* `required_ancestor`, so a reconcile can
//! never read an ancestor the table does not list, and the death-check and the
//! reads share one spec (the per-col *field* each reconcile then pulls --
//! subscriber.hides vs subscribee.contains -- stays per-col).

use crate::types::misc::{ID, SourceName};
use crate::types::tree::generic::{ read_at_ancestor_in_tree, read_at_node_in_tree, write_at_node_in_tree };
use crate::types::tree::viewnode_nodecomplete::write_at_truenode_in_tree;
use crate::types::viewnode::{ ParentIs, RoleCol, ViewNode, ViewNodeKind, Vognode };
use crate::update_buffer::util::detach_scaffold_transferring_focus;

use ego_tree::{ NodeId, NodeRef, Tree };
use std::error::Error;

/// One position in a col's required ancestry (§3 table). A position is matched
/// against the *actual ancestor at that tree depth*.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExpectedAncestor {
  /// Must be a `Vognode::Normal` (anything else at this depth means death).
  NormalVognode,
  /// Must be exactly this col kind (§19: a col = a collecting scaffold). The
  /// only intermediate-chain col the table uses is the SubscribeeCol.
  Col (RoleCol),
}

// The §3 required-ancestry table, nearest -> farthest. required_ancestry[i] is
// matched against the actual ancestor at tree depth i+1 (0 = the col itself).
const ANC_NORMAL : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_OUTSIDE : &[ExpectedAncestor] =
  &[ ExpectedAncestor::Col (RoleCol::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_HIDDEN_IN : &[ExpectedAncestor] =
  &[ ExpectedAncestor::NormalVognode,
     ExpectedAncestor::Col (RoleCol::Subscribee),
     ExpectedAncestor::NormalVognode ];
const ANC_NONE : &[ExpectedAncestor] = &[];

/// The §3 required ancestry for a col kind, nearest -> farthest. Non-col kinds
/// have none (they are never orphan-checked).
fn required_ancestry (
  kind : &ViewNodeKind,
) -> &'static [ExpectedAncestor] {
  match kind {
    // QualCol(Alias) and QualCol(ID): parent Normal vognode.
    ViewNodeKind::QualCol (_) => ANC_NORMAL,
    // Subscribee col: parent = subscriber (Normal).
    // Relation cols (Subscriber/Overridden/Overrider/Hider/Hidden): parent
    // Normal vognode.
    ViewNodeKind::PartnerCol (RoleCol::Subscribee)
      | ViewNodeKind::PartnerCol (RoleCol::Subscriber)
      | ViewNodeKind::PartnerCol (RoleCol::Overridden)
      | ViewNodeKind::PartnerCol (RoleCol::Overrider)
      | ViewNodeKind::PartnerCol (RoleCol::Hider)
      | ViewNodeKind::PartnerCol (RoleCol::Hidden) => ANC_NORMAL,
    ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee) =>
      ANC_HIDDEN_OUTSIDE,
    ViewNodeKind::PartnerCol (RoleCol::HiddenInSubscribee) =>
      ANC_HIDDEN_IN,
    _ => ANC_NONE,
  } }

fn matches_expected (
  actual   : &ViewNodeKind,
  expected : &ExpectedAncestor,
) -> bool {
  match expected {
    ExpectedAncestor::NormalVognode =>
      matches! ( actual, ViewNodeKind::Vognode (Vognode::Normal (_)) ),
    ExpectedAncestor::Col (rc) =>
      matches! ( actual, ViewNodeKind::PartnerCol (r) if r == rc ),
  } }

/// Whether `kind` is a COL -- in Jeff's terminology (plan_v2 §19) a col is a
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

/// The NodeId of the col's i-th required ancestor (per the §3 table for its
/// kind), `Some` only if the whole prefix [0..=i] of the required ancestry
/// matches the actual ancestors; `None` if any prefix position is the wrong
/// viewnode kind (i.e. the col is a generalized orphan up to depth i+1) or i is
/// past the end of the table. This is the single read path: every col reconcile
/// obtains its ancestor vognodes only through this, so a reconcile can never
/// read an ancestor the table does not list.
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
  for (j, expected) in spec [ ..= i ] . iter () . enumerate () {
    let depth : usize = j + 1;
    let matches : bool =
      read_at_ancestor_in_tree (
        tree, col, depth,
        |vn : &ViewNode| matches_expected (&vn . kind, expected) )
      . unwrap_or (false);
    if ! matches { return Ok (None); } }
  Ok ( ancestor_nodeid (tree, col, i + 1) ) }

/// The (pid, source) of the col's i-th required ancestor, read *through* the
/// §3 table. Errors if that ancestor is absent (the col is a generalized
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

/// Deaden a generalized-orphan col at its BFS visit (plan §5): dispose each
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

/// Dispose one direct child of a deadened orphan col (plan §5.1.a):
/// - an Affected (parentIs=Affected Normal) view-leaf -> delete;
/// - an Affected branch (has children) -> demote to parentIs=Independent, so
///   the user's subtree survives;
/// - a nested col (QualCol / PartnerCol) -> LEAVE it untouched: it is itself a
///   generalized orphan under this now-dead col, so it deadens itself -- and
///   disposes ITS OWN children -- at its own BFS visit. (This DEVIATES from
///   plan.org §5.1.a, which converted nested cols to DeadScaffold here; doing
///   so would freeze a nested col before it could dispose its own leaf members,
///   leaving them stranded under a dead chain. Leaving it to self-deaden is
///   what the plan's §5 note actually intends -- "a nested col ... deadens
///   itself at its own visit too" -- and is what makes leaf members at every
///   depth die. A Qual leaf does NOT self-dispatch, so it is still converted
///   below.)
/// - any other non-vognode child (a Qual, a DeadScaffold) -> convert to
///   DeadScaffold;
/// - any other child (a non-Affected vognode: an Independent Normal, a
///   DiffPhantom, an Inactive/Unknown/Deleted) -> keep untouched.
fn dispose_orphaned_col_child (
  tree  : &mut Tree<ViewNode>,
  child : NodeId,
) -> Result<(), Box<dyn Error>> {
  let (affected, is_leaf, is_vognode, is_col) : (bool, bool, bool, bool) = {
    let c : NodeRef<ViewNode> = tree . get (child)
      . ok_or ("dispose_orphaned_col_child: child not found") ?;
    ( c . value () . is_truenode_and_parentIs_affected (),
      c . children () . next () . is_none (),
      matches! ( &c . value () . kind, ViewNodeKind::Vognode (_) ),
      is_col_kind (&c . value () . kind) ) };
  if affected {
    if is_leaf {
      detach_scaffold_transferring_focus (tree, child) ?;
    } else {
      write_at_truenode_in_tree ( tree, child,
        |t| { t . parentIs = ParentIs::Independent; } )
        . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  } else if is_col {
    // Leave it: a nested col self-deadens at its own visit (see doc above).
  } else if ! is_vognode {
    write_at_node_in_tree ( tree, child,
      |vn : &mut ViewNode| { vn . kind = ViewNodeKind::DeadScaffold; } )
      . map_err ( |e| -> Box<dyn Error> { e . into () } ) ?; }
  // else: a non-Affected vognode -- kept untouched.
  Ok (( )) }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::git::MembershipAxes;
  use crate::types::viewnode::{
    mk_indefinitive_viewnode, viewforest_root_viewnode,
    DeletedNode, Qual, QualCol };

  fn sid (s : &str) -> ID { ID::from (s) }
  fn src () -> SourceName { SourceName::from ("main") }

  fn normal (title : &str, pi : ParentIs) -> ViewNode {
    mk_indefinitive_viewnode (sid (title), src (), title . to_string (), pi) }

  fn deleted (title : &str) -> ViewNode {
    ViewNode { focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::Vognode (Vognode::Deleted (DeletedNode {
        id : sid (title), source : src (),
        title : title . to_string (), body : None })) } }

  fn role_col (rc : RoleCol) -> ViewNode {
    ViewNode { focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::PartnerCol (rc) } }

  fn qual_col (qc : QualCol) -> ViewNode {
    ViewNode { focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::QualCol (qc) } }

  fn alias_qual (text : &str) -> ViewNode {
    ViewNode { focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::Qual (Qual::Alias {
        text : text . to_string (),
        membership : MembershipAxes::default () }) } }

  fn child (
    tree : &mut Tree<ViewNode>, parent : NodeId, vn : ViewNode
  ) -> NodeId {
    tree . get_mut (parent) . unwrap () . append (vn) . id () }

  fn kind_at (tree : &Tree<ViewNode>, id : NodeId) -> ViewNodeKind {
    tree . get (id) . unwrap () . value () . kind . clone () }

  fn parentis_at (tree : &Tree<ViewNode>, id : NodeId) -> Option<ParentIs> {
    match &tree . get (id) . unwrap () . value () . kind {
      ViewNodeKind::Vognode (Vognode::Normal (t)) => Some (t . parentIs),
      _ => None } }

  fn is_detached (tree : &Tree<ViewNode>, parent : NodeId, id : NodeId) -> bool {
    ! tree . get (parent) . unwrap () . children ()
      . any ( |c| c . id () == id ) }

  // ---- col_is_generalized_orphan ----

  #[test]
  fn aliascol_under_normal_is_not_orphan () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let n : NodeId = child (&mut t, root, normal ("N", ParentIs::Affected));
    let ac : NodeId = child (&mut t, n, qual_col (QualCol::Alias));
    assert! ( ! col_is_generalized_orphan (&t, ac) . unwrap () ); }

  #[test]
  fn aliascol_under_deleted_is_orphan () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let d : NodeId = child (&mut t, root, deleted ("D"));
    let ac : NodeId = child (&mut t, d, qual_col (QualCol::Alias));
    assert! ( col_is_generalized_orphan (&t, ac) . unwrap () ); }

  #[test]
  fn relation_col_under_deadscaffold_is_orphan () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let dead : NodeId = child (&mut t, root,
      ViewNode { focused : false, folded : false, body_folded : false,
        kind : ViewNodeKind::DeadScaffold });
    let rc : NodeId = child (&mut t, dead, role_col (RoleCol::Subscriber));
    assert! ( col_is_generalized_orphan (&t, rc) . unwrap () ); }

  // Multi-level: the immediate parent (subscribee) is a live Normal, but the
  // FAR ancestor (subscriber, depth 3) is dead -- the generalized (not just
  // immediate-parent) check must catch it.
  fn build_subscribee_chain (
    subscriber : ViewNode,
  ) -> (Tree<ViewNode>, NodeId, NodeId, NodeId, NodeId) {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let sber : NodeId = child (&mut t, root, subscriber);
    let scol : NodeId = child (&mut t, sber, role_col (RoleCol::Subscribee));
    let sbee : NodeId = child (&mut t, scol,
      normal ("subscribee", ParentIs::Affected));
    let hin : NodeId = child (&mut t, sbee,
      role_col (RoleCol::HiddenInSubscribee));
    (t, sber, scol, sbee, hin) }

  #[test]
  fn hiddenin_full_valid_chain_is_not_orphan () {
    let (t, _sber, _scol, _sbee, hin) =
      build_subscribee_chain (normal ("subscriber", ParentIs::Affected));
    assert! ( ! col_is_generalized_orphan (&t, hin) . unwrap () ); }

  #[test]
  fn hiddenin_dead_far_subscriber_is_orphan () {
    let (t, _sber, _scol, _sbee, hin) =
      build_subscribee_chain (deleted ("subscriber"));
    assert! ( col_is_generalized_orphan (&t, hin) . unwrap (),
      "immediate parent (subscribee) is live but the depth-3 subscriber is \
       dead -- the generalized check must report orphan" ); }

  // ---- required_ancestor (read-through) ----

  #[test]
  fn required_ancestor_walks_the_chain () {
    let (t, sber, scol, sbee, hin) =
      build_subscribee_chain (normal ("subscriber", ParentIs::Affected));
    assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
    assert_eq! ( required_ancestor (&t, hin, 1) . unwrap (), Some (scol) );
    assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber) );
    assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None ); }

  #[test]
  fn required_ancestor_none_when_chain_broken () {
    let (t, _sber, _scol, sbee, hin) =
      build_subscribee_chain (deleted ("subscriber"));
    // Prefix up to the live subscribee still resolves...
    assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
    // ...but the full chain to the dead subscriber does not.
    assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), None ); }

  // ---- deaden_generalized_orphan_col (child disposal) ----

  #[test]
  fn deaden_disposes_each_child_kind () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let d : NodeId = child (&mut t, root, deleted ("D"));
    let scol : NodeId = child (&mut t, d, role_col (RoleCol::Subscribee));
    // Affected leaf -> delete.
    let leaf : NodeId = child (&mut t, scol, normal ("L", ParentIs::Affected));
    // Affected branch (has a child) -> demote to Independent, keep.
    let branch : NodeId = child (&mut t, scol, normal ("B", ParentIs::Affected));
    let _bchild : NodeId = child (&mut t, branch, normal ("Bc", ParentIs::Affected));
    // Nested col -> leave it (self-deadens at its own visit).
    let nested : NodeId = child (&mut t, scol,
      role_col (RoleCol::HiddenOutsideOfSubscribee));
    // Non-Affected vognode -> keep untouched.
    let indep : NodeId = child (&mut t, scol, normal ("I", ParentIs::Independent));

    deaden_generalized_orphan_col (&mut t, scol) . unwrap ();

    assert! ( matches! ( kind_at (&t, scol), ViewNodeKind::DeadScaffold ),
      "the orphan col itself becomes a DeadScaffold" );
    assert! ( is_detached (&t, scol, leaf),
      "an Affected leaf is deleted" );
    assert_eq! ( parentis_at (&t, branch), Some (ParentIs::Independent),
      "an Affected branch is demoted to Independent and kept" );
    assert! ( ! is_detached (&t, scol, branch) );
    assert! ( matches! ( kind_at (&t, nested),
                         ViewNodeKind::PartnerCol (RoleCol::HiddenOutsideOfSubscribee) ),
      "a nested col is left untouched to self-deaden at its visit" );
    assert_eq! ( parentis_at (&t, indep), Some (ParentIs::Independent),
      "a non-Affected vognode is kept untouched" ); }

  #[test]
  fn deaden_converts_qual_leaf_to_deadscaffold () {
    let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
    let root : NodeId = t . root () . id ();
    let d : NodeId = child (&mut t, root, deleted ("D"));
    let ac : NodeId = child (&mut t, d, qual_col (QualCol::Alias));
    let q : NodeId = child (&mut t, ac, alias_qual ("eleven"));
    deaden_generalized_orphan_col (&mut t, ac) . unwrap ();
    assert! ( matches! ( kind_at (&t, ac), ViewNodeKind::DeadScaffold ) );
    assert! ( matches! ( kind_at (&t, q), ViewNodeKind::DeadScaffold ),
      "a Qual leaf (which does not self-dispatch) is converted to DeadScaffold" ); }
}
