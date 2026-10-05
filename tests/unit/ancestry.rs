use super::*;
use crate::types::git::RelationshipAxes;
use crate::types::viewnode::{
  mk_writeProtected_viewnode, viewforest_root_viewnode,
  Phantom, PhantomDeleted, Property, PropertyFolder };

fn sid (s : &str) -> ID { ID::from (s) }
fn src () -> SkgrepoName { SkgrepoName::from ("main") }

fn normal (title : &str, pi : AffectsParent) -> Viewnode {
  mk_writeProtected_viewnode (sid (title), src (), title . to_string (), pi) }

fn deleted (title : &str) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::Vognode (Vognode::Phantom (Phantom::Deleted (PhantomDeleted {
      skgid    : sid (title), home_skgrepo : src (),
      title : title . to_string (), body : None }))) } }

fn role_folder (rc : PartnerFolder) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::PartnerFolder (rc) } }

fn property_folder (qc : PropertyFolder) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::PropertyFolder (qc) } }

fn alias_property (text : &str) -> Viewnode {
  Viewnode { focused : false, folded : false, body_folded : false,
    kind : ViewnodeKind::Property (Property::Alias {
      text : text . to_string (),
      relRepo : None,
      relRepo_request : None,
      relationship_axes : RelationshipAxes::default () }) } }

fn child (
  tree : &mut Tree<Viewnode>, parent : NodeId, vn : Viewnode
) -> NodeId {
  tree . get_mut (parent) . unwrap () . append (vn) . id () }

fn kind_at (tree : &Tree<Viewnode>, skgid : NodeId) -> ViewnodeKind {
  tree . get (skgid) . unwrap () . value () . kind . clone () }

fn parentis_at (tree : &Tree<Viewnode>, skgid : NodeId) -> Option<AffectsParent> {
  match &tree . get (skgid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => Some (t . affectsParent),
    _ => None } }

fn is_detached (tree : &Tree<Viewnode>, parent : NodeId, skgid : NodeId) -> bool {
  ! tree . get (parent) . unwrap () . children ()
    . any ( |c| c . id () == skgid ) }

// ---- folder_is_generalized_orphan ----

#[test]
fn aliasfolder_under_normal_is_not_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let n : NodeId = child (&mut t, root, normal ("N", AffectsParent::True));
  let ac : NodeId = child (&mut t, n, property_folder (PropertyFolder::Alias));
  assert! ( ! folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn aliasfolder_under_deleted_is_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let ac : NodeId = child (&mut t, d, property_folder (PropertyFolder::Alias));
  assert! ( folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn relation_folder_under_deadviewnode_is_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let dead : NodeId = child (&mut t, root,
    Viewnode { focused : false, folded : false, body_folded : false,
      kind : ViewnodeKind::DeadViewnode });
  let rc : NodeId = child (&mut t, dead, role_folder (PartnerFolder::Subscriber));
  assert! ( folder_is_generalized_orphan (&t, rc) . unwrap () ); }

// Multi-level: the immediate parent (subscribee) is a live Unrestricted, but the
// FAR ancestor (subscriber, depth 3) is dead -- the generalized (not just
// immediate-parent) check must catch it.
fn build_subscribee_chain (
  subscriber : Viewnode,
) -> (Tree<Viewnode>, NodeId, NodeId, NodeId, NodeId) {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
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
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let subscribee_folder : NodeId = child (&mut t, d, role_folder (PartnerFolder::Subscribee));
  // Member leaf -> delete.
  let leaf : NodeId = child (&mut t, subscribee_folder, normal ("L", AffectsParent::True));
  // Member branch (has a child) -> demote to non-member, keep.
  let branch : NodeId = child (&mut t, subscribee_folder, normal ("B", AffectsParent::True));
  let _bchild : NodeId = child (&mut t, branch, normal ("Bc", AffectsParent::True));
  // Nested folder -> leave it (self-deadens at its own visit).
  let nested : NodeId = child (&mut t, subscribee_folder,
    role_folder (PartnerFolder::HiddenOutsideOfSubscribee));
  // Non-member vognode -> keep untouched.
  let indep : NodeId = child (&mut t, subscribee_folder, normal ("I", AffectsParent::False));

  deaden_generalized_orphan_folder (&mut t, subscribee_folder) . unwrap ();

  assert! ( matches! ( kind_at (&t, subscribee_folder), ViewnodeKind::DeadViewnode ),
    "the orphan folder itself becomes a DeadViewnode" );
  assert! ( is_detached (&t, subscribee_folder, leaf),
    "a member leaf is deleted" );
  assert_eq! ( parentis_at (&t, branch), Some (AffectsParent::False),
    "a member branch is demoted to non-member and kept" );
  assert! ( ! is_detached (&t, subscribee_folder, branch) );
  assert! ( matches! ( kind_at (&t, nested),
                       ViewnodeKind::PartnerFolder (PartnerFolder::HiddenOutsideOfSubscribee) ),
    "a nested folder is left untouched to self-deaden at its visit" );
  assert_eq! ( parentis_at (&t, indep), Some (AffectsParent::False),
    "a non-member vognode is kept untouched" ); }

#[test]
fn deaden_converts_property_leaf_to_deadviewnode () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, deleted ("D"));
  let ac : NodeId = child (&mut t, d, property_folder (PropertyFolder::Alias));
  let q : NodeId = child (&mut t, ac, alias_property ("eleven"));
  deaden_generalized_orphan_folder (&mut t, ac) . unwrap ();
  assert! ( matches! ( kind_at (&t, ac), ViewnodeKind::DeadViewnode ) );
  assert! ( matches! ( kind_at (&t, q), ViewnodeKind::DeadViewnode ),
    "a Property leaf (which does not self-dispatch) is converted to DeadViewnode" ); }
