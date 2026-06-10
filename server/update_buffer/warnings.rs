/// Typed warnings for repairs that view completion makes to
/// read-only PartnerCols. Reconcilers emit these into the
/// 'CompletionContext' warning sink; only the SAVED view's
/// completion gets a sink (de novo renders, collateral rerenders and
/// rerender-all repair silently, because their repairs do not
/// correspond to edits the user just made). The warnings are
/// rendered to strings late ('render_completion_warnings'), batched
/// per (col, owner), and delivered through 'SaveResponse.warnings'.

use crate::types::misc::ID;
use crate::types::viewnode::PartnerCol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RepairKind {
  RestoredMember,    // a generated member was missing from the buffer; completion restored it
  DemotedNonMember,  // an Affected child was not a real member; its branch was demoted to parentIs=independent
  RemovedStaleLeaf,  // an Affected child was not a real member and had no subtree; it was removed
  RemovedDuplicate,  // a member appeared more than once; the duplicate was removed
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompletionWarning {
  pub col      : PartnerCol,
  pub owner    : ID, // the node the col belongs to (its TrueNode parent)
  pub repair   : RepairKind,
  pub children : Vec<ID>,
}

/// One string per (col, owner) pair, in first-appearance order,
/// summarizing every repair to that col.
pub fn render_completion_warnings (
  warnings : &[CompletionWarning],
) -> Vec<String> {
  let mut group_order : Vec<(PartnerCol, ID)> = Vec::new ();
  for w in warnings {
    let key : (PartnerCol, ID) =
      ( w . col, w . owner . clone () );
    if ! group_order . contains (&key) {
      group_order . push (key); }}
  group_order . iter ()
    . map ( |(col, owner)| {
      let mut segments : Vec<String> = Vec::new ();
      let mut any_restored : bool = false;
      for w in warnings {
        if w . col != *col || w . owner != *owner { continue; }
        if w . children . is_empty () { continue; }
        let ids : String =
          w . children . iter ()
          . map ( |id| id . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ");
        let n : usize = w . children . len ();
        segments . push ( match w . repair {
          RepairKind::RestoredMember => {
            any_restored = true;
            format! ("restored {} member(s): {}", n, ids) },
          RepairKind::DemotedNonMember =>
            format! ("demoted {} non-member(s) to independent: {}",
                     n, ids),
          RepairKind::RemovedStaleLeaf =>
            format! ("removed {} stale member(s): {}", n, ids),
          RepairKind::RemovedDuplicate =>
            format! ("removed {} duplicate member(s): {}", n, ids),
        } ); }
      let explainer : &str =
        if any_restored {
          " (A read-only collection's membership is edited from the other side of the relationship, not by editing the collection.)"
        } else { "" };
      format! ( "Repaired {} under node {}: {}.{}",
                col . repr_in_client (),
                owner . 0,
                segments . join ("; "),
                explainer ) })
    . collect () }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/completion_warnings.rs"]
mod tests;
