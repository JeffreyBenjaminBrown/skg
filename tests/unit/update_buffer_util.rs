use super::*;
use crate::types::misc::{ID, SourceName};
use crate::types::viewnode::{ mk_inactive_viewnode, viewforest_root_viewnode };
use crate::types::git::MembershipAxes;
use ego_tree::Tree;

// The orderkey closure is fallible: a relevant child whose kind the
// closure cannot extract an orderkey from must surface as an Err from
// 'complete_relevant_children_in_viewnodetree', not as a panic inside
// the closure. (TODO/problems.org recorded the panic; Jeff approved
// the Err conversion 2026-06-10.)
#[test]
fn relevant_child_of_wrong_kind_yields_err_not_panic () {
  let mut t : Tree<ViewNode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  t . get_mut (root) . unwrap () . append (
    mk_inactive_viewnode ( ID::from ("x"), SourceName::from ("main"),
                           MembershipAxes::default () ) );
  let result : Result<RepairSummary<ID>, Box<dyn Error>> =
    complete_relevant_children_in_viewnodetree (
      &mut t, root,
      |_vn : &ViewNode| true, // relevance admits the Inactive child
      |vn : &ViewNode| match &vn . kind {
        ViewNodeKind::Vognode (Vognode::Active (truenode))
          => Ok ( truenode . id . clone () ),
        _ => Err ( "child is not an Active vognode" . to_string () ) },
      & [] as &[ID],
      |id : &ID| Err ( format! ( "create_child should not run for {}",
                                 id . 0 )) );
  assert! ( result . is_err (),
    "a relevant child the orderkey closure rejects must yield Err" );
  assert! ( result . unwrap_err () . to_string ()
            . contains ("not an Active vognode") ); }
