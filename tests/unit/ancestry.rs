use super::*;
use crate::types::git::MembershipAxes;
use crate::types::viewnode::{
  mk_writeProtected_viewnode, viewforest_root_viewnode,
  Phantom, PhantomDeleted, Qual, QualFolder };

fn sid (s : &str) -> ID { ID::from (s) }
fn src () -> SourceName { SourceName::from ("main") }

fn normal (title : &str, pi : AffectsParent) -> ViewNode {
  mk_writeProtected_viewnode (sid (title), src (), title . to_string (), pi) }

fn deleted (title : &str) -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Phantom (Phantom::Deleted (PhantomDeleted {
      id : sid (title), source : src (),
      title : title . to_string (), body : None })) } }

fn role_folder (rc : PartnerFolder) -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::PartnerFolder (rc) } }

fn qual_folder (qc : QualFolder) -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::QualFolder (qc) } }

fn alias_qual (text : &str) -> ViewNode {
  ViewNode { focused : false, folded : false, body_folded : false,
    kind : ViewNodeKind::Qual (Qual::Alias {
      text : text . to_string (),
      relSource : None,
      relSource_request : None,
      membership : MembershipAxes::default () }) } }

fn child (
  tree : &mut Tree<ViewNode>, parent : NodeId, vn : ViewNode
) -> NodeId {
  tree . get_mut (parent) . unwrap () . append (vn) . id () }

fn kind_at (tree : &Tree<ViewNode>, id : NodeId) -> ViewNodeKind {
  tree . get (id) . unwrap () . value () . kind . clone () }

fn parentis_at (tree : &Tree<ViewNode>, id : NodeId) -> Option<AffectsParent> {
  match &tree . get (id) . unwrap () . value () . kind {
    ViewNodeKind::Vognode (Vognode::Active (t)) => Some (t . affectsParent),
    _ => None } }

fn is_detached (tree : &Tree<ViewNode>, parent : NodeId, id : NodeId) -> bool {
  ! tree . get (parent) . unwrap () . children ()
    . any ( |c| c . id () == id ) }

// ---- folder_is_generalized_orphan ----

#[test]
fn aliasfolder_under_normal_is_not_orphan () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let n : NodeId = child (&mut t, root, normal ("N", AffectsParent::True));
  let ac : NodeId = child (&mut t, n, qual_folder (QualFolder::Alias));
  assert! ( ! folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn aliasfolder_under_deleted_is_orphan () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let ac : NodeId = child (&mut t, d, qual_folder (QualFolder::Alias));
  assert! ( folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn relation_folder_under_deadscaffold_is_orphan () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let dead : NodeId = child (&mut t, root,
    ViewNode { focused : false, folded : false, body_folded : false,
      kind : ViewNodeKind::DeadScaffold });
  let rc : NodeId = child (&mut t, dead, role_folder (PartnerFolder::Subscriber));
  assert! ( folder_is_generalized_orphan (&t, rc) . unwrap () ); }

// Multi-level: the immediate parent (subscribee) is a live Normal, but the
// FAR ancestor (subscriber, depth 3) is dead -- the generalized (not just
// immediate-parent) check must catch it.
fn build_subscribee_chain (
  subscriber : ViewNode,
) -> (Tree<ViewNode>, NodeId, NodeId, NodeId, NodeId) {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let sber : NodeId = child (&mut t, root, subscriber);
  let subscribee_folder : NodeId = child (&mut t, sber, role_folder (PartnerFolder::Subscribee));
  let sbee : NodeId = child (&mut t, subscribee_folder,
    normal ("subscribee", AffectsParent::True));
  let hin : NodeId = child (&mut t, sbee,
    role_folder (PartnerFolder::HiddenInSubscribee));
  (t, sber, subscribee_folder, sbee, hin) }

#[test]
fn hiddenin_full_valid_chain_is_not_orphan () {
  let (t, _sber, _sfolder, _sbee, hin) =
    build_subscribee_chain (normal ("subscriber", AffectsParent::True));
  assert! ( ! folder_is_generalized_orphan (&t, hin) . unwrap () ); }

#[test]
fn hiddenin_dead_far_subscriber_is_orphan () {
  let (t, _sber, _sfolder, _sbee, hin) =
    build_subscribee_chain (deleted ("subscriber"));
  assert! ( folder_is_generalized_orphan (&t, hin) . unwrap (),
    "immediate parent (subscribee) is live but the depth-3 subscriber is \
     dead -- the generalized check must report orphan" ); }

// ---- required_ancestor (read-through) ----

#[test]
fn required_ancestor_walks_the_chain () {
  let (t, sber, subscribee_folder, sbee, hin) =
    build_subscribee_chain (normal ("subscriber", AffectsParent::True));
  assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
  assert_eq! ( required_ancestor (&t, hin, 1) . unwrap (), Some (subscribee_folder) );
  assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber) );
  assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None ); }

#[test]
fn required_ancestor_reads_without_revalidating_kind () {
  // §20.4: required_ancestor no longer re-validates each ancestor's KIND -- the
  // orphan pre-check (folder_is_generalized_orphan) does that before any reconcile
  // runs. So on a broken chain (dead subscriber) it still returns the
  // table-indexed ancestor; detecting the break is the orphan check's job (see
  // hiddenin_dead_far_subscriber_is_orphan).
  let (t, sber, _sfolder, sbee, hin) =
    build_subscribee_chain (deleted ("subscriber"));
  assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
  assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber),
    "reads through a wrong-kind ancestor instead of returning None" );
  assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None,
    "still None past the end of the table" ); }

// ---- deaden_generalized_orphan_folder (child disposal) ----

#[test]
fn deaden_disposes_each_child_kind () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let subscribee_folder : NodeId = child (&mut t, d, role_folder (PartnerFolder::Subscribee));
  // Affected leaf -> delete.
  let leaf : NodeId = child (&mut t, subscribee_folder, normal ("L", AffectsParent::True));
  // Affected branch (has a child) -> demote to Independent, keep.
  let branch : NodeId = child (&mut t, subscribee_folder, normal ("B", AffectsParent::True));
  let _bchild : NodeId = child (&mut t, branch, normal ("Bc", AffectsParent::True));
  // Nested folder -> leave it (self-deadens at its own visit).
  let nested : NodeId = child (&mut t, subscribee_folder,
    role_folder (PartnerFolder::HiddenOutsideOfSubscribee));
  // Non-Affected vognode -> keep untouched.
  let indep : NodeId = child (&mut t, subscribee_folder, normal ("I", AffectsParent::False));

  deaden_generalized_orphan_folder (&mut t, subscribee_folder) . unwrap ();

  assert! ( matches! ( kind_at (&t, subscribee_folder), ViewNodeKind::DeadScaffold ),
    "the orphan folder itself becomes a DeadScaffold" );
  assert! ( is_detached (&t, subscribee_folder, leaf),
    "an Affected leaf is deleted" );
  assert_eq! ( parentis_at (&t, branch), Some (AffectsParent::False),
    "an Affected branch is demoted to Independent and kept" );
  assert! ( ! is_detached (&t, subscribee_folder, branch) );
  assert! ( matches! ( kind_at (&t, nested),
                       ViewNodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) ),
    "a nested folder is left untouched to self-deaden at its visit" );
  assert_eq! ( parentis_at (&t, indep), Some (AffectsParent::False),
    "a non-Affected vognode is kept untouched" ); }

#[test]
fn deaden_converts_qual_leaf_to_deadscaffold () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let ac : NodeId = child (&mut t, d, qual_folder (QualFolder::Alias));
  let q : NodeId = child (&mut t, ac, alias_qual ("eleven"));
  deaden_generalized_orphan_folder (&mut t, ac) . unwrap ();
  assert! ( matches! ( kind_at (&t, ac), ViewNodeKind::DeadScaffold ) );
  assert! ( matches! ( kind_at (&t, q), ViewNodeKind::DeadScaffold ),
    "a Qual leaf (which does not self-dispatch) is converted to DeadScaffold" ); }
