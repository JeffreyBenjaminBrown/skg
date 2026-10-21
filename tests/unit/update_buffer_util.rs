use super::*;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::types::git::Sign;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::viewnode::{
  DiffPresence, PartnerFolder, mk_restricted_viewnode, mk_writeProtected_viewnode_with_birth,
  viewforest_root_viewnode, viewnode_of };
use ego_tree::Tree;

// The orderkey closure is fallible: a managed child whose kind the
// closure cannot extract an orderkey from must surface as an Err from
// 'complete_managed_children_in_viewforest', not as a panic inside
// the closure. (TODO/problems.org recorded the panic; Jeff approved
// the Err conversion 2026-06-10.)
#[test]
fn managed_child_of_wrong_kind_yields_err_not_panic () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  t . get_mut (root) . unwrap () . append (
    mk_restricted_viewnode () );
  let result : Result<RepairSummary<ID>, Box<dyn Error>> =
    complete_managed_children_in_viewforest (
      &mut t, root,
      |_vn : &Viewnode| true, // the managed predicate admits the Restricted child
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (unrestrictedVognode))
          => Ok ( unrestrictedVognode . skgid . clone () ),
        _ => Err ( "child is not an Unrestricted vognode" . to_string () ) },
      & [] as &[ID],
      |skgid : &ID| Err ( format! ( "create_child should not run for {}",
                                 skgid . 0 )) );
  assert! ( result . is_err (),
    "a managed child the orderkey closure rejects must yield Err" );
  assert! ( result . unwrap_err () . to_string ()
            . contains ("not an Unrestricted vognode") ); }

// The stale rule both reconcilers share: a stale child with something
// of the user's below it survives, an unrestricted vognode as a plain
// non-member with its diff facts cleared, a folder deadened; one with
// nothing of the user's below it is left for the caller to detach.
#[test]
fn a_stale_child_survives_only_with_the_users_below_it () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let mut graft : Viewnode = mk_writeProtected_viewnode_with_birth (
    ID::from ("g"), SkgrepoName::from ("main"), "g" . into (),
    AffectsParent::True, Birth::RoleGraft (RelationRole::CONTAINER) );
  if let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) = &mut graft . kind {
    u . change_facts . presence . rel = Some (Sign::Minus); }
  let vognode : NodeId = t . get_mut (root) . unwrap () . append (graft) . id ();
  let folder : NodeId = t . get_mut (root) . unwrap () . append (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) )) . id ();
  let childless : NodeId = t . get_mut (root) . unwrap () . append (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Container) )) . id ();
  for parent in [vognode, folder] {
    t . get_mut (parent) . unwrap () . append ( mk_restricted_viewnode () ); }
  let has_children = |node : NodeRef<Viewnode>| node . children () . next () . is_some ();
  for kept in [vognode, folder] {
    assert! ( keep_stale_child_holding_the_users (&mut t, kept, &has_children) . unwrap () ); }
  assert! ( ! keep_stale_child_holding_the_users (&mut t, childless, &has_children) . unwrap () );
  let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) =
    &t . get (vognode) . unwrap () . value () . kind
    else { panic! ("the vognode stays an unrestricted vognode") };
  assert_eq! ( u . affectsParent, AffectsParent::False );
  assert_eq! ( u . birth, Birth::Unremarkable );
  assert_eq! ( u . change_facts . presence, DiffPresence::default () );
  assert_eq! ( t . get (folder) . unwrap () . value () . kind, ViewnodeKind::DeadViewnode );
  assert_eq! ( t . get (childless) . unwrap () . value () . kind,
               ViewnodeKind::PartnerFolder (PartnerFolder::Container) ); }
