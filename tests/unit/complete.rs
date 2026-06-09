use super::*;
use crate::types::misc::SourceName;
use crate::types::viewnode::{ mk_inactive_viewnode, viewforest_root_viewnode };
use crate::types::git::MembershipAxes;
use ego_tree::Tree;

// §6.4/§6.6/§16: an Inactive node whose pid this save deleted converts to a
// Deleted node (so the prune sweep can later remove it if childless), while
// an Inactive node NOT deleted by this save is left untouched.
#[test]
fn inactive_deleted_by_save_becomes_deleted () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  let inact : NodeId = t . get_mut (root) . unwrap () . append (
    mk_inactive_viewnode ( ID::from ("x"), SourceName::from ("main"),
                           MembershipAxes::default () ) ) . id ();
  let kept : NodeId = t . get_mut (root) . unwrap () . append (
    mk_inactive_viewnode ( ID::from ("y"), SourceName::from ("main"),
                           MembershipAxes::default () ) ) . id ();
  let deleted : HashSet<ID> =
    [ ID::from ("x") ] . into_iter () . collect ();

  convert_inactive_to_deleted_if_deleted (&mut t, inact, &deleted) . unwrap ();
  convert_inactive_to_deleted_if_deleted (&mut t, kept,  &deleted) . unwrap ();

  assert! ( matches! ( &t . get (inact) . unwrap () . value () . kind,
    ViewNodeKind::Phantom (Phantom::Deleted (d)) if d . id == ID::from ("x") ),
    "an Inactive node deleted by this save must become Deleted" );
  assert! ( matches! ( &t . get (kept) . unwrap () . value () . kind,
    ViewNodeKind::Vognode (Vognode::Inactive (_)) ),
    "an Inactive node NOT deleted by this save must be left untouched" ); }
