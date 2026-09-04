//! Conservative maintenance impact classification for retained editor views.

use super::candidate::ObservedDiskCandidate;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::types::maybe_placed_viewnode::{MpViewnodeKind, MpVognode, MpPhantom};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::impact_ids_from_viewforest;

use std::collections::BTreeSet;

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct ViewImpactAssessment {
  /// Parse uncertainty is itself an impact.  The separate field preserves the
  /// reason for the recovery report and prevents callers from presenting an
  /// ordinary semantic intersection as the explanation.
  pub impacted             : bool,
  pub parse_uncertain      : bool,
  pub uncertainty_reason   : Option<String>,
  pub observed_ids         : BTreeSet<ID>,
  pub resolved_primary_ids : BTreeSet<ID>,
  pub changed_primary_ids  : BTreeSet<ID>,
}

/// Classify one view against the exact semantic G0 -> G1 change.  Dirty text
/// is optional because clean buffers need only their accepted server forest.
/// When supplied, it is parsed independently and conservatively; any inability
/// to prove complete ID recovery makes the view impacted.
pub fn classify_view_impact (
  accepted_forest : &ViewForest,
  current_text    : Option<&str>,
  g0              : &InRustGraph,
  g1              : &InRustGraph,
  candidate       : &ObservedDiskCandidate,
) -> ViewImpactAssessment {
  let mut observed_ids : BTreeSet<ID> =
    impact_ids_from_viewforest (accepted_forest) . into_iter () . collect ();
  let parse_result = current_text . map (impact_ids_from_current_text);
  let (parse_uncertain, uncertainty_reason) = match parse_result {
    Some (Ok (ids)) => {
      observed_ids . extend (ids);
      (false, None)
    }
    Some (Err (reason)) => (true, Some (reason)),
    None => (false, None),
  };

  let mut resolved_primary_ids = BTreeSet::new ();
  for id in &observed_ids {
    if let Some (pid) = g0 . pid_of (id) {
      resolved_primary_ids . insert (pid); }
    if let Some (pid) = g1 . pid_of (id) {
      resolved_primary_ids . insert (pid); }
  }
  let changed_primary_ids : BTreeSet<ID> = candidate . added_primary_ids
    . union (&candidate . deleted_primary_ids) . cloned ()
    . chain (candidate . modified_primary_ids . iter () . cloned ())
    . collect ();
  let semantically_intersects = resolved_primary_ids
    . iter () . any (|pid| changed_primary_ids . contains (pid));
  ViewImpactAssessment {
    impacted: parse_uncertain || semantically_intersects,
    parse_uncertain,
    uncertainty_reason,
    observed_ids,
    resolved_primary_ids,
    changed_primary_ids,
  }
}

/// Recover all IDs represented by node metadata in exact current buffer text.
/// Syntax/validation errors and ID-less Active/diff nodes are uncertainty, not
/// permission to call the buffer orthogonal.
pub fn impact_ids_from_current_text (
  text : &str,
) -> Result<BTreeSet<ID>, String> {
  let (forest, parse_errors, _warnings) = org_to_uninterpreted_viewforest (text)
    . map_err (|error| format! ("current text could not be parsed: {}", error))?;
  if !parse_errors . is_empty () {
    return Err (format! (
      "current text has {} structural validation error(s)",
      parse_errors . len ()));
  }
  let mut ids = BTreeSet::new ();
  for node in forest . nodes () {
    match &node . value () . kind {
      MpViewnodeKind::Vognode (MpVognode::Active (active)) => {
        let id = active . id . as_ref () . ok_or_else (|| format! (
          "active node '{}' has no ID", active . title))?;
        ids . insert (id . clone ());
      }
      MpViewnodeKind::Phantom (MpPhantom::Diff (phantom)) => {
        let id = phantom . id . as_ref () . ok_or_else (|| format! (
          "diff phantom '{}' has no ID", phantom . title))?;
        ids . insert (id . clone ());
      }
      MpViewnodeKind::Phantom (MpPhantom::Deleted (phantom)) => {
        ids . insert (phantom . id . clone ());
      }
      MpViewnodeKind::Phantom (MpPhantom::Unknown (phantom)) => {
        ids . insert (phantom . id . clone ());
      }
      MpViewnodeKind::Vognode (MpVognode::Inactive (_))
      | MpViewnodeKind::QualCol (_)
      | MpViewnodeKind::Qual (_)
      | MpViewnodeKind::PartnerCol (_)
      | MpViewnodeKind::BufferRoot
      | MpViewnodeKind::DeadScaffold => {}
    }
  }
  Ok (ids)
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::viewnode::{
    Phantom, PhantomDeleted, PhantomUnknown, ViewNode, ViewNodeKind, Vognode,
    default_activeNode,
  };
  use crate::types::misc::SourceName;

  fn node (kind : ViewNodeKind) -> ViewNode {
    ViewNode {
      focused: false,
      folded: false,
      body_folded: false,
      kind,
    }
  }

  #[test]
  fn accepted_forest_impact_includes_active_and_every_phantom () {
    let source = SourceName::from ("main");
    let mut forest = ViewForest::new ();
    forest . append_root (node (ViewNodeKind::Vognode (Vognode::Active (
      default_activeNode (ID::from ("active"), source . clone (), "A" . into ())))));
    let deleted = forest . append_root (node (ViewNodeKind::Phantom (
      Phantom::Deleted (PhantomDeleted {
        id: ID::from ("deleted"), source: source . clone (),
        title: "D" . into (), body: None,
      }))));
    forest . get_mut (deleted) . unwrap () . append (node (
      ViewNodeKind::Phantom (Phantom::Unknown (PhantomUnknown {
        id: ID::from ("unknown"),
      }))));
    let active = default_activeNode (
      ID::from ("diff"), source, "Diff" . into ());
    forest . append_root (node (ViewNodeKind::Phantom (
      Phantom::Diff (crate::types::viewnode::PhantomDiff::from_activeNode (
        active)))));
    assert_eq! (
      impact_ids_from_viewforest (&forest),
      ["active", "deleted", "unknown", "diff"] . into_iter ()
        . map (ID::from) . collect ());
  }

  #[test]
  fn current_text_recovers_active_unknown_deleted_and_diff_ids () {
    let ids = impact_ids_from_current_text (concat! (
      "* (skg (node (id active) (source main))) Active\n",
      "* (skg (unknown (id unknown)))\n",
      "* (skg (deleted (id deleted) (source main))) Deleted\n",
      "* (skg (diffPhantom (id diff) (source main) indef (unstaged removedM))) Diff\n",
    )) . unwrap ();
    assert_eq! (ids, ["active", "unknown", "deleted", "diff"]
      . into_iter () . map (ID::from) . collect ());
  }

  #[test]
  fn malformed_or_idless_current_text_is_uncertain () {
    assert! (impact_ids_from_current_text (
      "* (skg (node (id x) (source main)) broken\n") . is_err ());
    assert! (impact_ids_from_current_text (
      "* (skg (node (source main))) no id\n") . is_err ());
  }
}
