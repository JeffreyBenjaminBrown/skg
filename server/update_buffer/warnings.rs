/// Typed warnings emitted by view completion into the
/// 'CompletionContext' warning sink. One kind:
/// - 'FolderRepair': repairs that completion makes to write-protected
///   PartnerFolders. Only the saved view's completion gets a sink (de
///   novo renders, collateral rerenders and rerender-all repair
///   silently, because their repairs do not correspond to edits the
///   user just made); delivered through 'SaveResponse.warnings'.
/// The warnings are rendered to strings late
/// ('render_completion_warnings'), ColRepairs batched per folder
/// occurrence.

use crate::types::errors::view_id_suffix;
use crate::types::misc::ID;
use crate::types::viewnode::{PartnerFolder, Vid};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RepairKind {
  RestoredMember,    // a generated member was missing from the buffer; completion restored it
  MadeNonMember,     // a child claiming membership was not a real member; the child, a branch, was made a non-member (nonMember)
  RemovedStaleLeaf,  // a child claiming membership was not a real member and had no subtree; it was removed
  RemovedDuplicate,  // a member appeared more than once; the duplicate was removed
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompletionWarning {
  FolderRepair {
    folder      : PartnerFolder,
    recorder : ID, // the node the folder belongs to (its UnrestrictedVognode parent)
    folder_vid : Option<Vid>, // the folder's view id; None for a folder the user typed
    repair   : RepairKind,
    children : Vec<ID>,
  },
}

/// ColRepairs render one string per folder occurrence (a folder kind,
/// recorder and view id), in first-appearance order, summarizing
/// every repair to that occurrence.
pub fn render_completion_warnings (
  warnings : &[CompletionWarning],
) -> Vec<String> {
  let mut rendered : Vec<String> = Vec::new ();
  { // the ColRepairs, batched
    let mut group_order : Vec<(PartnerFolder, ID, Option<Vid>)> = Vec::new ();
    for w in warnings {
      let CompletionWarning::FolderRepair { folder, recorder, folder_vid, .. } = w;
      let key : (PartnerFolder, ID, Option<Vid>) =
        ( *folder, recorder . clone (), folder_vid . clone () );
      if ! group_order . contains (&key) {
        group_order . push (key); } }
    for (folder, recorder, folder_vid) in &group_order {
      let mut segments : Vec<String> = Vec::new ();
      let mut any_restored : bool = false;
      for w in warnings {
        let CompletionWarning::FolderRepair {
          folder : w_folder, recorder : w_recorder,
          folder_vid : w_folder_vid, repair, children } = w;
        if w_folder != folder || w_recorder != recorder
          || w_folder_vid != folder_vid { continue; }
        if children . is_empty () { continue; }
        let skgids : String =
          children . iter ()
          . map ( |skgid| skgid . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ");
        let n : usize = children . len ();
        segments . push ( match repair {
          RepairKind::RestoredMember => {
            any_restored = true;
            format! ("restored {} member(s): {}", n, skgids) },
          RepairKind::MadeNonMember =>
            format! ("made {} child(ren) non-members, since they are not members: {}",
                     n, skgids),
          RepairKind::RemovedStaleLeaf =>
            format! ("removed {} stale member(s): {}", n, skgids),
          RepairKind::RemovedDuplicate =>
            format! ("removed {} duplicate member(s): {}", n, skgids),
        } ); }
      let explainer : &str =
        if any_restored {
          " (A write-protected folder's membership is edited from the other side of the relationship, not by editing the folder.)"
        } else { "" };
      rendered . push (
        format! ( "Repaired {}{} under node {}: {}.{}",
                  folder . repr_in_client (),
                  view_id_suffix ( folder_vid . as_ref () ),
                  recorder . 0,
                  segments . join ("; "),
                  explainer )); }}
  rendered }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/completion_warnings.rs"]
mod tests;
