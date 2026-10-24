use super::*;
use crate::types::viewnode::{Membership, ViewnodeKind, mk_writeProtected_viewnode, viewforest_root_viewnode, viewnode_of};

fn unrestricted_affected (id_str : &str) -> Viewnode {
  mk_writeProtected_viewnode (
    ID::from (id_str), SkgrepoName::from ("main"),
    id_str . to_string (), Membership::Member ) }

fn unrestricted_independent (id_str : &str) -> Viewnode {
  mk_writeProtected_viewnode (
    ID::from (id_str), SkgrepoName::from ("main"),
    id_str . to_string (), Membership::NonMember ) }

fn mk_folder (kind : PartnerFolder) -> Viewnode {
  viewnode_of (ViewnodeKind::PartnerFolder (kind)) }

// The user's view order of present members survives; missing graph
// members are appended in graph order; non-member children
// contribute nothing to the goal.
#[test]
fn view_order_wins_and_missing_members_append () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let folder : NodeId = t . get_mut (root) . unwrap ()
    . append (mk_folder (PartnerFolder::Subscriber)) . id ();
  { let mut folder_mut = t . get_mut (folder) . unwrap ();
    folder_mut . append (unrestricted_affected ("c"));
    folder_mut . append (unrestricted_affected ("x"));    // nonmember
    folder_mut . append (unrestricted_affected ("a"));
    folder_mut . append (unrestricted_independent ("b")); } // a non-member, not a member claim
  let graph_members : Vec<ID> =
    vec! [ ID::from ("a"), ID::from ("b"), ID::from ("c") ];
  let goal : Vec<ID> =
    view_order_preserving_goal_list (
      &t, folder, &graph_members, &InRustGraph::from_graphnodes (&[]) ) . unwrap ();
  assert_eq! ( goal,
               vec! [ ID::from ("c"), ID::from ("a"), ID::from ("b") ] ); }

// A duplicated member keeps its first view position only.
#[test]
fn duplicate_member_first_occurrence_wins () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let folder : NodeId = t . get_mut (root) . unwrap ()
    . append (mk_folder (PartnerFolder::Hider)) . id ();
  { let mut folder_mut = t . get_mut (folder) . unwrap ();
    folder_mut . append (unrestricted_affected ("c"));
    folder_mut . append (unrestricted_affected ("c"));
    folder_mut . append (unrestricted_affected ("a")); }
  let graph_members : Vec<ID> =
    vec! [ ID::from ("a"), ID::from ("c") ];
  let goal : Vec<ID> =
    view_order_preserving_goal_list (
      &t, folder, &graph_members, &InRustGraph::from_graphnodes (&[]) ) . unwrap ();
  assert_eq! ( goal,
               vec! [ ID::from ("c"), ID::from ("a") ] ); }
