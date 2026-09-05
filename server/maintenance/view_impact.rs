//! Conservative maintenance impact classification for retained editor views.

use super::candidate::ObservedDiskCandidate;
use super::archive::VerifiedInitialArchive;
use super::types::{
  ActiveMaintenance, BufferKind, ViewDisposition, ViewSettlementRecord,
  ViewSettlementRequirement,
};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_viewforest;
use crate::types::maybe_placed_viewnode::{MpViewnodeKind, MpVognode, MpPhantom};
use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::impact_ids_from_viewforest;
use crate::runtime::interactive_session::InteractiveSession;

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

/// Produce the complete, deterministic settlement inventory before asking
/// either editor to change a buffer.  Dirty current text comes only from the
/// verified initial archive; clean views use their retained accepted forest.
pub fn plan_incident_view_settlements (
  active      : &ActiveMaintenance,
  archive     : &VerifiedInitialArchive,
  interactive : &InteractiveSession,
  candidate   : &ObservedDiskCandidate,
) -> Result<Vec<ViewSettlementRecord>, String> {
  let archived : std::collections::BTreeMap<_, _> = archive . buffers . iter ()
    . map (|snapshot| (snapshot . buffer_id . as_str (), snapshot))
    . collect ();
  let mut settlements = Vec::new ();
  for (buffer_id, frozen) in &active . buffer_census {
    let archived_buffer = archived . get (buffer_id . as_str ()) . copied ();
    if frozen . dirty && archived_buffer . is_none () {
      return Err (format! (
        "dirty buffer '{}' is absent from the verified archive", buffer_id)); }
    if !frozen . dirty && archived_buffer . is_some () {
      return Err (format! (
        "clean buffer '{}' unexpectedly appears in the verified archive",
        buffer_id)); }

    let uri = frozen . view_uri . as_ref ()
      . map (|value| crate::types::views_state::ViewUri::from_client_string (
        value . clone ()));
    let state = uri . as_ref () . and_then (|uri|
      interactive . views . open_views . views . get (uri));
    let authority_current = state . map (|state|
      state . client_buffer_id . as_deref () == Some (buffer_id)
      && state . graph_generation == frozen . graph_generation
      && state . presentation_generation == frozen . presentation_generation
      && state . revision == frozen . server_revision
      && state . client_application_token == frozen . application_token)
      . unwrap_or (false);
    let assessment = match state {
      Some (state) if authority_current => classify_view_impact (
        &state . viewforest,
        archived_buffer . map (|snapshot| snapshot . current_text . as_str ()),
        &candidate . base_graph,
        &candidate . graph,
        candidate),
      _ if is_graph_view_kind (&frozen . kind) => uncertain_assessment (
        "the frozen client authority has no exact retained server forest"),
      _ => ViewImpactAssessment {
        impacted: frozen . dirty,
        parse_uncertain: false,
        uncertainty_reason: None,
        observed_ids: BTreeSet::new (),
        resolved_primary_ids: BTreeSet::new (),
        changed_primary_ids: changed_ids (candidate),
      },
    };
    let (planned_disposition, requirement) = planned_settlement (
      &frozen . kind, frozen . dirty, frozen . disposable,
      frozen . continuation_id . is_some (), &assessment, state . is_some ());
    settlements . push (ViewSettlementRecord {
      buffer_id: buffer_id . clone (),
      buffer_key: archived_buffer . map (|snapshot| snapshot . buffer_key . clone ()),
      kind: frozen . kind . clone (),
      view_uri: frozen . view_uri . clone (),
      origin_buffer_id: frozen . origin_buffer_id . clone (),
      origin_view_uri: frozen . origin_view_uri . clone (),
      origin_application_token: frozen . origin_application_token,
      origin_location: frozen . origin_location . clone (),
      dirty: frozen . dirty,
      impacted: assessment . impacted,
      parse_uncertain: assessment . parse_uncertain,
      uncertainty_reason: assessment . uncertainty_reason,
      observed_ids: assessment . observed_ids . into_iter ()
        . map (|id| id . to_string ()) . collect (),
      resolved_primary_ids: assessment . resolved_primary_ids . into_iter ()
        . map (|id| id . to_string ()) . collect (),
      base_graph_generation: frozen . graph_generation,
      base_presentation_generation: frozen . presentation_generation,
      base_server_revision: frozen . server_revision,
      base_application_token: frozen . application_token,
      planned_disposition,
      requirement,
      application: None,
      resolution: Default::default (),
      acknowledged: false,
    });
  }
  settle_attached_workflows (&mut settlements)?;
  Ok (settlements)
}

fn settle_attached_workflows (
  settlements : &mut [ViewSettlementRecord],
) -> Result<(), String> {
  let parent_results : std::collections::BTreeMap<_, _> = settlements . iter ()
    . map (|record| (record . buffer_id . clone (), (
      record . planned_disposition . clone (), record . requirement . clone (),
      record . impacted, record . parse_uncertain,
      record . uncertainty_reason . clone ())))
    . collect ();
  for record in settlements . iter_mut () {
    if !matches! (record . kind,
      BufferKind::MetadataEditor | BufferKind::ForkConfirmation
      | BufferKind::DiskConflict)
    { continue; }
    let origin_id = record . origin_buffer_id . as_ref () . ok_or_else (||
      format! ("attached workflow '{}' has no frozen origin", record . buffer_id))?;
    let (disposition, requirement, impacted, uncertain, reason) =
      parent_results . get (origin_id) . ok_or_else (|| format! (
        "attached workflow '{}' has absent settlement origin '{}'",
        record . buffer_id, origin_id))?;
    record . impacted = *impacted;
    record . parse_uncertain = *uncertain;
    record . uncertainty_reason = reason . clone ();
    match requirement {
      ViewSettlementRequirement::RetirementAck => {
        record . planned_disposition = ViewDisposition::Interrupted;
        record . requirement = ViewSettlementRequirement::RetirementAck;
      }
      ViewSettlementRequirement::ReleaseAck => {
        record . planned_disposition = ViewDisposition::ReleasedUnimpacted;
        record . requirement = ViewSettlementRequirement::ReleaseAck;
      }
      other => return Err (format! (
        "attached workflow '{}' cannot follow origin disposition {:?}/{:?}",
        record . buffer_id, disposition, other)),
    }
  }
  Ok (( ))
}

fn changed_ids (candidate : &ObservedDiskCandidate) -> BTreeSet<ID> {
  candidate . added_primary_ids
    . union (&candidate . deleted_primary_ids) . cloned ()
    . chain (candidate . modified_primary_ids . iter () . cloned ())
    . collect ()
}

fn uncertain_assessment (reason : &str) -> ViewImpactAssessment {
  ViewImpactAssessment {
    impacted: true,
    parse_uncertain: true,
    uncertainty_reason: Some (reason . into ()),
    observed_ids: BTreeSet::new (),
    resolved_primary_ids: BTreeSet::new (),
    changed_primary_ids: BTreeSet::new (),
  }
}

fn is_graph_view_kind (kind : &BufferKind) -> bool {
  matches! (kind,
    BufferKind::ContentView
    | BufferKind::NewEmptyContentView
    | BufferKind::SearchView
    | BufferKind::OverrideChoiceMenu)
}

fn planned_settlement (
  kind      : &BufferKind,
  dirty     : bool,
  disposable : bool,
  has_continuation : bool,
  impact    : &ViewImpactAssessment,
  has_forest : bool,
) -> (ViewDisposition, ViewSettlementRequirement) {
  use BufferKind::*;
  use ViewDisposition::*;
  use ViewSettlementRequirement::*;
  match kind {
    ContentView | NewEmptyContentView | SearchView if dirty && impact . impacted =>
      (Interrupted, RetirementAck),
    ContentView | NewEmptyContentView | SearchView if dirty =>
      (ReleasedUnimpacted, ReleaseAck),
    ContentView | NewEmptyContentView | SearchView
      if impact . impacted && has_forest => (Refreshed, ApplicationAck),
    ContentView | NewEmptyContentView | SearchView if impact . impacted =>
      (DetachedDerived, ReleaseAck),
    ContentView | NewEmptyContentView | SearchView =>
      (RetainedClean, ReleaseAck),
    OverrideChoiceMenu if dirty => (Interrupted, RetirementAck),
    OverrideChoiceMenu if disposable && !has_continuation =>
      (ClosedDisposable, CloseAck),
    OverrideChoiceMenu => (DetachedDerived, ReleaseAck),
    MetadataEditor | ForkConfirmation | RelationshipKindMenu if dirty =>
      (Interrupted, RetirementAck),
    RelationshipKindMenu =>
      // Its typed close path explicitly cancels the recorded continuation.
      (ClosedDisposable, CloseAck),
    MetadataEditor | ForkConfirmation | DiskConflict =>
      (Interrupted, RetirementAck),
    ReloadSelector => (DetachedDerived, ReleaseAck),
    IdStack | DerivedReport
      if dirty || !disposable || has_continuation =>
      (DetachedDerived, ReleaseAck),
    IdStack | DerivedReport =>
      (ClosedDisposable, CloseAck),
    DurableReport | RawSkgFile => (RetainedClean, ReleaseAck),
  }
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
  let changed_primary_ids = changed_ids (candidate);
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

  #[test]
  fn settlement_matrix_never_refreshes_dirty_text () {
    let orthogonal = ViewImpactAssessment {
      impacted: false, parse_uncertain: false, uncertainty_reason: None,
      observed_ids: BTreeSet::new (), resolved_primary_ids: BTreeSet::new (),
      changed_primary_ids: BTreeSet::new (),
    };
    let impacted = ViewImpactAssessment {
      impacted: true, ..orthogonal . clone ()
    };
    assert_eq! (planned_settlement (
      &BufferKind::ContentView, true, false, false, &impacted, true),
      (ViewDisposition::Interrupted,
       ViewSettlementRequirement::RetirementAck));
    assert_eq! (planned_settlement (
      &BufferKind::SearchView, true, false, false, &orthogonal, true),
      (ViewDisposition::ReleasedUnimpacted,
       ViewSettlementRequirement::ReleaseAck));
    assert_eq! (planned_settlement (
      &BufferKind::ContentView, false, false, false, &impacted, true),
      (ViewDisposition::Refreshed,
       ViewSettlementRequirement::ApplicationAck));
    assert_eq! (planned_settlement (
      &BufferKind::DerivedReport, false, false, false, &orthogonal, false),
      (ViewDisposition::DetachedDerived,
       ViewSettlementRequirement::ReleaseAck));
    assert_eq! (planned_settlement (
      &BufferKind::DerivedReport, false, true, false, &orthogonal, false),
      (ViewDisposition::ClosedDisposable,
       ViewSettlementRequirement::CloseAck));
    assert_eq! (planned_settlement (
      &BufferKind::ReloadSelector, false, true, true, &orthogonal, false),
      (ViewDisposition::DetachedDerived,
       ViewSettlementRequirement::ReleaseAck));
  }

  #[test]
  fn attached_workflow_follows_its_dirty_origins_settlement () {
    let record = |id : &str, kind : BufferKind, origin : Option<&str>,
                  disposition : ViewDisposition,
                  requirement : ViewSettlementRequirement|
      ViewSettlementRecord {
        buffer_id: id . into (), buffer_key: None, kind, view_uri: None,
        origin_buffer_id: origin . map (str::to_string),
        origin_view_uri: None, origin_application_token: origin . map (|_| 5),
        origin_location: origin . map (|_| "((scope save))" . into ()),
        dirty: true, impacted: false, parse_uncertain: false,
        uncertainty_reason: None, observed_ids: Vec::new (),
        resolved_primary_ids: Vec::new (), base_graph_generation: 1,
        base_presentation_generation: 2, base_server_revision: 3,
        base_application_token: 5, planned_disposition: disposition,
        requirement, application: None, resolution: Default::default (),
        acknowledged: false,
      };
    let mut settlements = vec![
      record ("origin", BufferKind::ContentView, None,
        ViewDisposition::ReleasedUnimpacted,
        ViewSettlementRequirement::ReleaseAck),
      record ("workflow", BufferKind::MetadataEditor, Some ("origin"),
        ViewDisposition::Interrupted,
        ViewSettlementRequirement::RetirementAck),
    ];
    settle_attached_workflows (&mut settlements) . unwrap ();
    assert_eq! (settlements[1] . planned_disposition,
      ViewDisposition::ReleasedUnimpacted);
    assert_eq! (settlements[1] . requirement,
      ViewSettlementRequirement::ReleaseAck);

    settlements[0] . planned_disposition = ViewDisposition::Interrupted;
    settlements[0] . requirement = ViewSettlementRequirement::RetirementAck;
    settle_attached_workflows (&mut settlements) . unwrap ();
    assert_eq! (settlements[1] . planned_disposition,
      ViewDisposition::Interrupted);
    assert_eq! (settlements[1] . requirement,
      ViewSettlementRequirement::RetirementAck);
  }
}
