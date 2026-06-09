use super::*;
use crate::types::git::MembershipAxes;
use crate::types::viewnode::{
  mk_indefinitive_viewnode, viewforest_root_viewnode,
  PhantomDeleted, Qual, QualCol };

fn sid (s : &str) -> ID { ID::from (s) }
fn src () -> SourceName { SourceName::from ("main") }

fn normal (title : &str, pi : ParentIs) -> ViewNode {
  mk_indefinitive_viewnode (sid (title), src (), title . to_string (), pi) }

fn deleted (title : &str) -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Vognode (Vognode::Deleted (PhantomDeleted {
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
fn required_ancestor_reads_without_revalidating_kind () {
  // §20.4: required_ancestor no longer re-validates each ancestor's KIND -- the
  // orphan pre-check (col_is_generalized_orphan) does that before any reconcile
  // runs. So on a broken chain (dead subscriber) it still returns the
  // table-indexed ancestor; detecting the break is the orphan check's job (see
  // hiddenin_dead_far_subscriber_is_orphan).
  let (t, sber, _scol, sbee, hin) =
    build_subscribee_chain (deleted ("subscriber"));
  assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
  assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber),
    "reads through a wrong-kind ancestor instead of returning None" );
  assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None,
    "still None past the end of the table" ); }

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
