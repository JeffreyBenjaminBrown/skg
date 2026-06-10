use super::*;
use crate::types::viewnode::{mk_indefinitive_viewnode, viewforest_root_viewnode};

fn active_affected (id_str : &str) -> ViewNode {
  mk_indefinitive_viewnode (
    ID::from (id_str), SourceName::from ("main"),
    id_str . to_string (), ParentIs::Affected ) }

fn active_independent (id_str : &str) -> ViewNode {
  mk_indefinitive_viewnode (
    ID::from (id_str), SourceName::from ("main"),
    id_str . to_string (), ParentIs::Independent ) }

fn mk_col (kind : PartnerCol) -> ViewNode {
  ViewNode {
    focused     : false,
    folded      : false,
    body_folded : false,
    kind        : ViewNodeKind::PartnerCol (kind) } }

// The user's view order of present members survives; missing graph
// members are appended in graph order; nonmembers and Independent
// children contribute nothing to the goal.
#[test]
fn view_order_wins_and_missing_members_append () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let col : NodeId = t . get_mut (root) . unwrap ()
    . append (mk_col (PartnerCol::Subscriber)) . id ();
  { let mut col_mut = t . get_mut (col) . unwrap ();
    col_mut . append (active_affected ("c"));
    col_mut . append (active_affected ("x"));    // nonmember
    col_mut . append (active_affected ("a"));
    col_mut . append (active_independent ("b")); } // parked, not a member claim
  let graph_members : Vec<ID> =
    vec! [ ID::from ("a"), ID::from ("b"), ID::from ("c") ];
  let goal : Vec<ID> =
    view_order_preserving_goal_list (&t, col, &graph_members) . unwrap ();
  assert_eq! ( goal,
               vec! [ ID::from ("c"), ID::from ("a"), ID::from ("b") ] ); }

// A duplicated member keeps its first view position only.
#[test]
fn duplicate_member_first_occurrence_wins () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let col : NodeId = t . get_mut (root) . unwrap ()
    . append (mk_col (PartnerCol::Hider)) . id ();
  { let mut col_mut = t . get_mut (col) . unwrap ();
    col_mut . append (active_affected ("c"));
    col_mut . append (active_affected ("c"));
    col_mut . append (active_affected ("a")); }
  let graph_members : Vec<ID> =
    vec! [ ID::from ("a"), ID::from ("c") ];
  let goal : Vec<ID> =
    view_order_preserving_goal_list (&t, col, &graph_members) . unwrap ();
  assert_eq! ( goal,
               vec! [ ID::from ("c"), ID::from ("a") ] ); }
