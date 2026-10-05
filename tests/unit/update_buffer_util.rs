use super::*;
use crate::types::misc::ID;
use crate::types::viewnode::{ mk_restricted_viewnode, viewforest_root_viewnode };
use ego_tree::Tree;

// The orderkey closure is fallible: a relevant child whose kind the
// closure cannot extract an orderkey from must surface as an Err from
// 'complete_relevant_children_in_viewforest', not as a panic inside
// the closure. (TODO/problems.org recorded the panic; Jeff approved
// the Err conversion 2026-06-10.)
#[test]
fn relevant_child_of_wrong_kind_yields_err_not_panic () {
  let mut t : Tree<Viewnode> = Tree::new (viewforest_root_viewnode ());
  let root : NodeId = t . root () . id ();
  t . get_mut (root) . unwrap () . append (
    mk_restricted_viewnode () );
  let result : Result<RepairSummary<ID>, Box<dyn Error>> =
    complete_relevant_children_in_viewforest (
      &mut t, root,
      |_vn : &Viewnode| true, // relevance admits the Restricted child
      |vn : &Viewnode| match &vn . kind {
        ViewnodeKind::Vognode (Vognode::Unrestricted (unrestrictedVognode))
          => Ok ( unrestrictedVognode . skgid . clone () ),
        _ => Err ( "child is not an Unrestricted vognode" . to_string () ) },
      & [] as &[ID],
      |skgid : &ID| Err ( format! ( "create_child should not run for {}",
                                 skgid . 0 )) );
  assert! ( result . is_err (),
    "a relevant child the orderkey closure rejects must yield Err" );
  assert! ( result . unwrap_err () . to_string ()
            . contains ("not an Unrestricted vognode") ); }
