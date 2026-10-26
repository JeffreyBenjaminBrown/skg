use super::*;
use crate::types::viewnode::{
  mk_writeProtected_viewnode, viewforest_root_viewnode,
  PropertyMember, Folder, viewnode_of };

fn sid (s : &str) -> ID { ID::from (s) }
fn src () -> SkgrepoName { SkgrepoName::from ("main") }

fn normal (title : &str, claimsMembership : bool) -> Viewnode {
  mk_writeProtected_viewnode (sid (title), src (), title . to_string (), claimsMembership) }

/// What stands where a node died: a tombstone.
fn dead () -> Viewnode {
  viewnode_of (ViewnodeKind::Tombstone) }

fn role_folder (rc : Folder) -> Viewnode {
  viewnode_of (ViewnodeKind::Folder (rc)) }

fn property_folder (qc : Folder) -> Viewnode {
  viewnode_of (ViewnodeKind::Folder (qc)) }

fn alias_property (text : &str) -> Viewnode {
  viewnode_of (ViewnodeKind::PropertyMember (PropertyMember::alias ( text . to_string (), None, None ))) }

fn child (
  tree : &mut Tree<Viewnode>, parent : NodeId, vn : Viewnode
) -> NodeId {
  tree . get_mut (parent) . unwrap () . append (vn) . id () }

fn kind_at (tree : &Tree<Viewnode>, skgid : NodeId) -> ViewnodeKind {
  tree . get (skgid) . unwrap () . value () . kind . clone () }

fn parentis_at (tree : &Tree<Viewnode>, skgid : NodeId) -> Option<bool> {
  match &tree . get (skgid) . unwrap () . value () . kind {
    ViewnodeKind::Vognode (Vognode::Unrestricted (t)) => Some (t . claimsMembership),
    _ => None } }

fn is_detached (tree : &Tree<Viewnode>, parent : NodeId, skgid : NodeId) -> bool {
  ! tree . get (parent) . unwrap () . children ()
    . any ( |c| c . id () == skgid ) }

// ---- folder_is_generalized_orphan ----

#[test]
fn aliasfolder_under_normal_is_not_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let n : NodeId = child (&mut t, root, normal ("N", true));
  let ac : NodeId = child (&mut t, n, property_folder (Folder::Alias));
  assert! ( ! folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn aliasfolder_under_tombstone_is_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, dead ());
  let ac : NodeId = child (&mut t, d, property_folder (Folder::Alias));
  assert! ( folder_is_generalized_orphan (&t, ac) . unwrap () ); }

#[test]
fn relation_folder_under_tombstone_is_orphan () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let dead : NodeId = child (&mut t, root,
    viewnode_of (ViewnodeKind::Tombstone));
  let rc : NodeId = child (&mut t, dead, role_folder (Folder::Subscriber));
  assert! ( folder_is_generalized_orphan (&t, rc) . unwrap () ); }

// Multi-level: the immediate parent (subscribee) is a live Unrestricted, but the
// far ancestor (subscriber, depth 3) is dead -- the generalized (not just
// immediate-parent) check must catch it.
fn build_subscribee_chain (
  subscriber : Viewnode,
) -> (Tree<Viewnode>, NodeId, NodeId, NodeId, NodeId) {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let sber : NodeId = child (&mut t, root, subscriber);
  let subscribee_folder : NodeId = child (&mut t, sber, role_folder (Folder::Subscribee));
  let sbee : NodeId = child (&mut t, subscribee_folder,
    normal ("subscribee", true));
  let hin : NodeId = child (&mut t, sbee,
    role_folder (Folder::HiddenInSubscribee));
  (t, sber, subscribee_folder, sbee, hin) }

#[test]
fn hiddenin_full_valid_chain_is_not_orphan () {
  let (t, _sber, _sfolder, _sbee, hin) =
    build_subscribee_chain (normal ("subscriber", true));
  assert! ( ! folder_is_generalized_orphan (&t, hin) . unwrap () ); }

#[test]
fn hiddenin_dead_far_subscriber_is_orphan () {
  let (t, _sber, _sfolder, _sbee, hin) =
    build_subscribee_chain (dead ());
  assert! ( folder_is_generalized_orphan (&t, hin) . unwrap (),
    "immediate parent (subscribee) is live but the depth-3 subscriber is \
     dead -- the generalized check must report orphan" ); }

// ---- required_ancestor (read-through) ----

#[test]
fn required_ancestor_walks_the_chain () {
  let (t, sber, subscribee_folder, sbee, hin) =
    build_subscribee_chain (normal ("subscriber", true));
  assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
  assert_eq! ( required_ancestor (&t, hin, 1) . unwrap (), Some (subscribee_folder) );
  assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber) );
  assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None ); }

#[test]
fn required_ancestor_reads_without_revalidating_kind () {
  // §20.4: required_ancestor no longer re-validates each ancestor's kind -- the
  // orphan pre-check (folder_is_generalized_orphan) does that before any reconcile
  // runs. So on a broken chain (dead subscriber) it still returns the
  // table-indexed ancestor; detecting the break is the orphan check's job (see
  // hiddenin_dead_far_subscriber_is_orphan).
  let (t, sber, _sfolder, sbee, hin) =
    build_subscribee_chain (dead ());
  assert_eq! ( required_ancestor (&t, hin, 0) . unwrap (), Some (sbee) );
  assert_eq! ( required_ancestor (&t, hin, 2) . unwrap (), Some (sber),
    "reads through a wrong-kind ancestor instead of returning None" );
  assert_eq! ( required_ancestor (&t, hin, 3) . unwrap (), None,
    "still None past the end of the table" ); }

// ---- entombstone_generalized_orphan_folder (child disposal) ----

#[test]
fn entombstone_disposes_each_child_kind () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, dead ());
  let subscribee_folder : NodeId = child (&mut t, d, role_folder (Folder::Subscribee));
  // Member leaf -> a tombstone, for the prune to remove.
  let leaf : NodeId = child (&mut t, subscribee_folder, normal ("L", true));
  // Member branch (has a child) -> a write-protected non-member, kept.
  let branch : NodeId = child (&mut t, subscribee_folder, normal ("B", true));
  let _bchild : NodeId = child (&mut t, branch, normal ("Bc", true));
  // Nested folder -> leave it (self-entombstones at its own visit).
  let nested : NodeId = child (&mut t, subscribee_folder,
    role_folder (Folder::HiddenOutsideOfSubscribee));
  // Non-member vognode -> keep untouched.
  let indep : NodeId = child (&mut t, subscribee_folder, normal ("I", false));

  entombstone_generalized_orphan_folder (&mut t, subscribee_folder) . unwrap ();

  assert! ( matches! ( kind_at (&t, subscribee_folder), ViewnodeKind::Tombstone ),
    "the orphan folder itself becomes a tombstone" );
  assert! ( matches! ( kind_at (&t, leaf), ViewnodeKind::Tombstone ),
    "a member leaf becomes a tombstone" );
  assert! ( ! is_detached (&t, subscribee_folder, leaf),
    "the prune, not the disposal, removes it" );
  assert_eq! ( parentis_at (&t, branch), Some (false),
    "a member branch is made a non-member and kept" );
  assert! ( matches! ( kind_at (&t, branch),
                       ViewnodeKind::Vognode (Vognode::Unrestricted (ref u))
                       if u . is_writeProtected () ),
    "the kept branch is write-protected" );
  assert! ( ! is_detached (&t, subscribee_folder, branch) );
  assert! ( matches! ( kind_at (&t, nested),
                       ViewnodeKind::Folder (Folder::HiddenOutsideOfSubscribee) ),
    "a nested folder is left untouched to self-entombstone at its visit" );
  assert_eq! ( parentis_at (&t, indep), Some (false),
    "a non-member vognode is kept untouched" ); }

#[test]
fn entombstone_converts_property_leaf_to_tombstone () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let d : NodeId = child (&mut t, root, dead ());
  let ac : NodeId = child (&mut t, d, property_folder (Folder::Alias));
  let q : NodeId = child (&mut t, ac, alias_property ("eleven"));
  entombstone_generalized_orphan_folder (&mut t, ac) . unwrap ();
  assert! ( matches! ( kind_at (&t, ac), ViewnodeKind::Tombstone ) );
  assert! ( matches! ( kind_at (&t, q), ViewnodeKind::Tombstone ),
    "a Property leaf (which does not self-dispatch) is converted to Tombstone" ); }
