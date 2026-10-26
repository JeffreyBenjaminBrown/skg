/// Typed warnings about the saved view, delivered through
/// 'SaveResponse.warnings'. Only the saved view gets a sink (de novo
/// renders, collateral rerenders and rerender-all correct silently,
/// because their corrections do not correspond to edits the user just
/// made). Two kinds:
/// - 'FolderRepair': repairs that the diff view's completion makes to
///   write-protected PartnerFolders;
/// - 'Correction': what the refresh of a content view changed
///   ('server/refresh/corrections.rs').
/// The warnings are rendered to strings late
/// ('render_completion_warnings'), ColRepairs batched per folder
/// occurrence, each correction on its own.

use crate::refresh::corrections::{Correction, CorrectionKind};
use crate::types::errors::{HeadlineName, with_org_subheadlines};
use crate::types::misc::ID;
use crate::types::viewnode::{Folder, ViewnodeId};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RepairKind {
  ReinstatedMember,      // a generated member was missing from the buffer; completion reinstated it
  Retained,              // a child claiming membership was not a real member; the child, a branch, was retained as non-member (noClaim)
  EntombstonedStaleLeaf, // a child claiming membership was not a real member and had no subtree; it was entombstoned, and the prune removed it
  EntombstonedDuplicate, // a member appeared more than once; the duplicate was entombstoned, and the prune removed it
}

#[derive(Debug, Clone, PartialEq)]
pub enum CompletionWarning {
  FolderRepair {
    folder      : Folder,
    recorder : ID, // the node the folder belongs to (its UnrestrictedVognode parent)
    folder_viewnode_id : Option<ViewnodeId>, // the folder's viewnode ID; None for a folder the user typed
    repair   : RepairKind,
    children : Vec<ID>,
  },
  Correction (Correction),
}

/// ColRepairs render one string per folder occurrence (a folder kind,
/// recorder and viewnode ID), in first-appearance order, summarizing
/// every repair to that occurrence.
pub fn render_completion_warnings (
  warnings : &[CompletionWarning],
) -> Vec<String> {
  let mut rendered : Vec<String> = Vec::new ();
  { // the ColRepairs, batched
    let mut group_order : Vec<(Folder, ID, Option<ViewnodeId>)> = Vec::new ();
    for w in warnings {
      let CompletionWarning::FolderRepair { folder, recorder, folder_viewnode_id, .. } = w
        else { continue; };
      let key : (Folder, ID, Option<ViewnodeId>) =
        ( folder . clone (), recorder . clone (), folder_viewnode_id . clone () );
      if ! group_order . contains (&key) {
        group_order . push (key); } }
    for (folder, recorder, folder_viewnode_id) in &group_order {
      let mut segments : Vec<String> = Vec::new ();
      let mut any_reinstated : bool = false;
      for w in warnings {
        let CompletionWarning::FolderRepair {
          folder : w_folder, recorder : w_recorder,
          folder_viewnode_id : w_folder_viewnode_id, repair, children } = w
          else { continue; };
        if w_folder != folder || w_recorder != recorder
          || w_folder_viewnode_id != folder_viewnode_id { continue; }
        if children . is_empty () { continue; }
        let skgids : String =
          children . iter ()
          . map ( |skgid| skgid . 0 . as_str () )
          . collect::<Vec<&str>> ()
          . join (", ");
        let n : usize = children . len ();
        segments . push ( match repair {
          RepairKind::ReinstatedMember => {
            any_reinstated = true;
            format! ("reinstated {} member(s): {}", n, skgids) },
          RepairKind::Retained =>
            format! ("retained {} child(ren) as non-members, since they are not members: {}",
                     n, skgids),
          RepairKind::EntombstonedStaleLeaf =>
            format! ("removed {} stale member(s): {}", n, skgids),
          RepairKind::EntombstonedDuplicate =>
            format! ("removed {} duplicate member(s): {}", n, skgids),
        } ); }
      let explainer : &str =
        if any_reinstated {
          " (A write-protected folder's membership is edited from the other side of the relationship, not by editing the folder.)"
        } else { "" };
      rendered . push ( with_org_subheadlines (
        &format! ( "Repaired {} under node {}: {}.{}",
                   folder . repr_in_client (),
                   recorder . 0,
                   segments . join ("; "),
                   explainer ),
        &[ HeadlineName::of ( folder_viewnode_id . as_ref (), None, "" ) ] )); }}
  for w in warnings {
    if let CompletionWarning::Correction (correction) = w {
      rendered . push ( rendered_correction (correction) ); }}
  rendered }

/// One correction, naming its headline as a subtree is named.
fn rendered_correction (
  correction : &Correction,
) -> String {
  let what : String = match correction . kind {
    CorrectionKind::Reinstated =>
      "Drew what the graph says belongs here." . to_string (),
    CorrectionKind::Entombstoned =>
      "Entombstoned a headline that can stand here for nothing." . to_string (),
    CorrectionKind::MarkedNoClaim =>
      "Marked noClaim a child the graph does not relate to its parent." . to_string (),
    CorrectionKind::Merged =>
      "Merged siblings standing for one node into this one." . to_string (),
    CorrectionKind::Rekinded (kind) =>
      format! ("Made a headline a {}, as the graph has it.", kind),
    CorrectionKind::EditRequestDropped =>
      "Dropped an edit request its headline has no slot for." . to_string (), };
  with_org_subheadlines (
    &what,
    &[ HeadlineName::of ( correction . vnid . as_ref (), None, &correction . title ) ] ) }

#[cfg(test)]
#[allow(non_snake_case)]
#[path = "../../tests/unit/completion_warnings.rs"]
mod tests;
