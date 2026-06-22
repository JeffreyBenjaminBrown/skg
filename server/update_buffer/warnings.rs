/// Typed warnings emitted by view completion into the
/// 'CompletionContext' warning sink. One kind:
/// - 'ColRepair': repairs that completion makes to read-only
///   PartnerCols. Only the SAVED view's completion gets a sink (de
///   novo renders, collateral rerenders and rerender-all repair
///   silently, because their repairs do not correspond to edits the
///   user just made); delivered through 'SaveResponse.warnings'.
/// The warnings are rendered to strings late
/// ('render_completion_warnings'), ColRepairs batched per (col,
/// owner).

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
pub enum CompletionWarning {
  ColRepair {
    col      : PartnerCol,
    owner    : ID, // the node the col belongs to (its ActiveNode parent)
    repair   : RepairKind,
    children : Vec<ID>,
  },
}

/// ColRepairs render one string per (col, owner) pair, in
/// first-appearance order, summarizing every repair to that col.
pub fn render_completion_warnings (
  warnings : &[CompletionWarning],
) -> Vec<String> {
  let mut rendered : Vec<String> = Vec::new ();
  { // the ColRepairs, batched
    let mut group_order : Vec<(PartnerCol, ID)> = Vec::new ();
    for w in warnings {
      let CompletionWarning::ColRepair { col, owner, .. } = w;
      let key : (PartnerCol, ID) =
        ( *col, owner . clone () );
      if ! group_order . contains (&key) {
        group_order . push (key); } }
    for (col, owner) in &group_order {
      let mut segments : Vec<String> = Vec::new ();
      let mut any_restored : bool = false;
      for w in warnings {
        let CompletionWarning::ColRepair {
          col : w_col, owner : w_owner, repair, children } = w;
        if w_col != col || w_owner != owner { continue; }
        if children . is_empty () { continue; }
        let ids : String =
          children . iter ()
          . map ( |id| id . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ");
        let n : usize = children . len ();
        segments . push ( match repair {
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
      rendered . push (
        format! ( "Repaired {} under node {}: {}.{}",
                  col . repr_in_client (),
                  owner . 0,
                  segments . join ("; "),
                  explainer )); }}
  rendered }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/completion_warnings.rs"]
mod tests;
