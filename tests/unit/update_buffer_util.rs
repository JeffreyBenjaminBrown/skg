use super::*;
use crate::types::misc::{ID, SkgrepoName};
use crate::types::viewnode::{
  DiffChange, Existence, PartnerFolder, mk_editable_viewnode, mk_restricted_viewnode,
  mk_writeProtected_viewnode, viewforest_root_viewnode, viewnode_of };
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
      &mut t, root, KeptChildOrder::FollowsGoal,
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

// The one rule: a stale vognode with a subtree is retained as
// non-member, keeping its title, ID and editability, its facts
// cleared; a write-protected one's members become non-members, an
// editable one's stay members. A vognode leaf and every non-vognode
// become tombstones, a leaf to be pruned, one holding children to
// stay.
#[test]
fn an_unwarranted_child_is_converted_never_detached () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let mut write_protected : Viewnode = mk_writeProtected_viewnode (
    ID::from ("g"), SkgrepoName::from ("main"), "g" . into (),
    Membership::Member );
  write_protected . change_facts . change . rel = Some (Existence::Removed);
  let branch : NodeId = t . get_mut (root) . unwrap () . append (write_protected) . id ();
  let member_below : NodeId = t . get_mut (branch) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("m"), SkgrepoName::from ("main"),
                           "m" . into (), None )) . id ();
  let editable_branch : NodeId = t . get_mut (root) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("e"), SkgrepoName::from ("main"),
                           "e" . into (), None )) . id ();
  let member_below_editable : NodeId = t . get_mut (editable_branch) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("ec"), SkgrepoName::from ("main"),
                           "ec" . into (), None )) . id ();
  let vognode_leaf : NodeId = t . get_mut (root) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("l"), SkgrepoName::from ("main"),
                           "l" . into (), None )) . id ();
  let folder : NodeId = t . get_mut (root) . unwrap () . append (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Acquired) )) . id ();
  t . get_mut (folder) . unwrap () . append ( mk_restricted_viewnode () );
  let childless : NodeId = t . get_mut (root) . unwrap () . append (
    viewnode_of ( ViewnodeKind::PartnerFolder (PartnerFolder::Container) )) . id ();
  assert_eq! ( convert_unwarranted_child (
                 &mut t, branch, false, Unwarranted::Stale ) . unwrap (),
               Conversion::Retained );
  assert_eq! ( convert_unwarranted_child (
                 &mut t, editable_branch, false, Unwarranted::Stale ) . unwrap (),
               Conversion::Retained );
  assert_eq! ( convert_unwarranted_child (
                 &mut t, vognode_leaf, true, Unwarranted::Stale ) . unwrap (),
               Conversion::ChildlessTombstone );
  assert_eq! ( convert_unwarranted_child (
                 &mut t, folder, false, Unwarranted::Stale ) . unwrap (),
               Conversion::Retained );
  assert_eq! ( convert_unwarranted_child (
                 &mut t, childless, true, Unwarranted::Stale ) . unwrap (),
               Conversion::ChildlessTombstone );
  let retained : &Viewnode = t . get (branch) . unwrap () . value ();
  let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) = &retained . kind
    else { panic! ("the branch stays an unrestricted vognode") };
  assert_eq! ( u . membership, Membership::NonMember );
  assert! ( u . is_writeProtected () );
  assert_eq! ( ( u . skgid . clone (), u . title . as_str () ), ( ID::from ("g"), "g" ) );
  assert_eq! ( retained . change_facts . change, DiffChange::default () );
  assert_eq! ( membership_of (&t, member_below), Membership::NonMember,
    "a write-protected occurrence holds no member" );
  let ViewnodeKind::Vognode (Vognode::Unrestricted (e)) =
    &t . get (editable_branch) . unwrap () . value () . kind
    else { panic! ("the editable branch stays an unrestricted vognode") };
  assert_eq! ( e . membership, Membership::NonMember );
  assert! ( ! e . is_writeProtected (), "a stale copy keeps its editability" );
  assert_eq! ( membership_of (&t, member_below_editable), Membership::Member,
    "an editable retained copy keeps its members" );
  for dead in [vognode_leaf, folder, childless] {
    assert_eq! ( t . get (dead) . unwrap () . value () . kind,
                 ViewnodeKind::Tombstone ); }
  assert_eq! ( t . root () . children () . count (), 5,
    "nothing is detached" ); }

// An extra copy of a duplicate with a subtree is retained
// write-protected, whatever it was, and its members become
// non-members.
#[test]
fn an_extra_copy_of_a_duplicate_is_writeProtected () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let copy : NodeId = t . get_mut (root) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("e"), SkgrepoName::from ("main"),
                           "e" . into (), None )) . id ();
  let member_below : NodeId = t . get_mut (copy) . unwrap () . append (
    mk_editable_viewnode ( ID::from ("ec"), SkgrepoName::from ("main"),
                           "ec" . into (), None )) . id ();
  assert_eq! ( convert_unwarranted_child (
                 &mut t, copy, false, Unwarranted::Duplicate ) . unwrap (),
               Conversion::Retained );
  let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) =
    &t . get (copy) . unwrap () . value () . kind
    else { panic! ("the copy stays an unrestricted vognode") };
  assert_eq! ( u . membership, Membership::NonMember );
  assert! ( u . is_writeProtected () );
  assert_eq! ( membership_of (&t, member_below), Membership::NonMember,
    "a write-protected occurrence holds no member" ); }

/// The membership of the unrestricted vognode at 'treeid'.
fn membership_of (
  t      : &Tree<Viewnode>,
  treeid : NodeId,
) -> Membership {
  let ViewnodeKind::Vognode (Vognode::Unrestricted (u)) =
    &t . get (treeid) . unwrap () . value () . kind
    else { panic! ("not an unrestricted vognode") };
  u . membership }

/// The children of 'parent', in order, after completing them against
/// 'goal' as 'kept_child_order' says. Managed children are those
/// starting with a capital, keyed by their text without a trailing
/// '!', which marks the copy preferred among duplicates; the rest are
/// the user's notes. A converted child is lowercased.
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
    |s : &String| s . chars () . next () . is_some_and ( |c| c . is_ascii_uppercase () ),
    |s : &String| Ok ( s . trim_end_matches ('!') . to_string () ),
    |s : &String| s . ends_with ('!'),
    &goal,
    |s : &String| Ok ( s . clone () ),
    |t : &mut Tree<String>, child : NodeId, _unwarranted : Unwarranted| {
      let mut node : NodeMut<String> = t . get_mut (child) . unwrap ();
      let lowered : String = node . value () . to_lowercase ();
      *node . value () = lowered;
      Ok (Conversion::ChildlessTombstone) } ) . unwrap ();
  t . root () . children () . map ( |c| c . value () . clone () ) . collect () }

// Unmoved, every kept child stays where it stands, the user's notes
// and the converted stale child too, and a created one follows its
// predecessor in the goal, or goes first; following the goal, the notes
// and the converted child go first and the members follow the goal's
// order.
#[test]
fn kept_children_stay_unless_the_order_follows_the_goal () {
  assert_eq! ( complete_letters ( &[ "note", "B", "A", "Z" ], &[ "C", "B", "A", "D" ],
                                  KeptChildOrder::Unmoved ),
               vec! [ "C", "note", "B", "A", "D", "z" ] );
  assert_eq! ( complete_letters ( &[ "note", "B", "A", "Z" ], &[ "C", "B", "A", "D" ],
                                  KeptChildOrder::FollowsGoal ),
               vec! [ "note", "z", "C", "B", "A", "D" ] );
  assert_eq! ( complete_letters ( &[ "B", "note", "A" ], &[ "A", "B" ],
                                  KeptChildOrder::FollowsGoal ),
               vec! [ "note", "A", "B" ] ); }

// Of several copies of one key, the preferred one is kept, else the
// first; the others are converted where they stand.
#[test]
fn of_duplicates_the_preferred_copy_is_kept () {
  assert_eq! ( complete_letters ( &[ "A", "A!", "A" ], &[ "A" ],
                                  KeptChildOrder::Unmoved ),
               vec! [ "a", "A!", "a" ] );
  assert_eq! ( complete_letters ( &[ "A", "B", "A" ], &[ "A", "B" ],
                                  KeptChildOrder::Unmoved ),
               vec! [ "A", "B", "a" ] ); }
