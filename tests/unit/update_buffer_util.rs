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
// the closure.
#[test]
fn managed_child_of_wrong_kind_yields_err_not_panic () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  t . get_mut (root) . unwrap () . append (
    mk_restricted_viewnode () );
  let result : Result<RepairSummary<ID>, Box<dyn Error>> =
    complete_managed_children_in_viewforest (
      &mut t, root, Owner::ContentReconciler, KeptChildOrder::FollowsGoal,
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
  graft . change_facts . presence . rel = Some (Sign::Minus);
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
  let kept_vognode : &Viewnode = t . get (vognode) . unwrap () . value ();
  let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) = &kept_vognode . kind
    else { panic! ("the vognode stays an unrestricted vognode") };
  assert_eq! ( u . affectsParent, AffectsParent::False );
  assert_eq! ( u . birth, Birth::Unremarkable );
  assert_eq! ( kept_vognode . change_facts . presence, DiffPresence::default () );
  assert_eq! ( t . get (folder) . unwrap () . value () . kind, ViewnodeKind::DeadViewnode );
  assert_eq! ( t . get (childless) . unwrap () . value () . kind,
               ViewnodeKind::PartnerFolder (PartnerFolder::Container) ); }

/// The children of 'parent', in order, after completing them against
/// 'goal' as 'kept_child_order' says. Managed children are those
/// spelled in capitals; the rest are the user's notes.
fn complete_letters (
  children         : &[&str],
  goal             : &[&str],
  kept_child_order : KeptChildOrder,
) -> Vec<String> {
  let mut t : Tree<String> = Tree::new ( String::new () );
  let root : NodeId = t . root () . id ();
  for child in children {
    t . get_mut (root) . unwrap () . append ( child . to_string () ); }
  let goal : Vec<String> = goal . iter () . map ( |s| s . to_string () ) . collect ();
  complete_managed_children (
    &mut t, root, kept_child_order,
    |s : &String| s . chars () . all ( |c| c . is_ascii_uppercase () ),
    |s : &String| Ok ( s . clone () ),
    &goal,
    |s : &String| Ok ( s . clone () ),
    |_ : &Tree<String>, _ : NodeId| Ok (false),
    |_ : &mut String| {},
    |_ : &mut Tree<String>, _ : NodeId| Ok (false) ) . unwrap ();
  t . root () . children () . map ( |c| c . value () . clone () ) . collect () }

// Unmoved, every kept child stays where it stands, the user's notes
// too, and a created one follows its predecessor in the goal, or goes
// first; following the goal, the notes go first and the managed
// children follow the goal's order.
#[test]
fn kept_children_stay_unless_the_order_follows_the_goal () {
  assert_eq! ( complete_letters ( &[ "note", "B", "A", "Z" ], &[ "C", "B", "A", "D" ],
                                  KeptChildOrder::Unmoved ),
               vec! [ "C", "note", "B", "A", "D" ] );
  assert_eq! ( complete_letters ( &[ "note", "B", "A", "Z" ], &[ "C", "B", "A", "D" ],
                                  KeptChildOrder::FollowsGoal ),
               vec! [ "note", "C", "B", "A", "D" ] );
  assert_eq! ( complete_letters ( &[ "B", "note", "A" ], &[ "A", "B" ],
                                  KeptChildOrder::FollowsGoal ),
               vec! [ "note", "A", "B" ] ); }
