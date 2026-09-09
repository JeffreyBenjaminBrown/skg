//! Old incident evidence supports reports without becoming current selection.

use super::{ServerRuntime, SelectedRuntimeSnapshot};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::maintenance::archive::{
  InitialArchiveExpectation, VerifiedInitialArchive, verify_initial_archive,
};
use crate::maintenance::candidate::SemanticNodeEvidence;
use crate::maintenance::evidence::{
  MaintenanceEvidenceBundle, PublishedMaintenanceEvidence, ReconstructedEvidence,
  reconstruct_evidence,
};
use crate::maintenance::view_impact::{
  ReportGraphChangeSet, plan_recovered_incident_view_settlements,
};
use crate::maintenance::{
  ActiveMaintenance, IncidentId, MaintenanceCoordinator, MaintenanceEpoch, MaintenancePhase,
  ServerEvidenceRecord, ViewSettlementRecord,
};
use crate::types::nodes::complete::NodeComplete;

use std::collections::BTreeMap;
use std::path::Path;
use std::sync::Arc;

impl ServerRuntime {
  pub(crate) fn recover_incident_report (
    &self,
    incident : &IncidentId,
    epoch : MaintenanceEpoch,
  ) -> Result<bool, String> {
    let coordinator : MaintenanceCoordinator = self . maintenance_snapshot ();
    if coordinator . terminal_incident (incident, epoch) . is_some () { return Ok (false); }
    let active : ActiveMaintenance = coordinator . incident (incident, epoch)? . clone ();
    if active . server_session_id . as_deref () == Some (self . server_session_id ())
    || active . authority_retired_by_session . is_some ()
    || active . selected_store . is_none () {
      return Ok (false); }
    if !matches! (active . phase,
        MaintenancePhase::Presenting | MaintenancePhase::FinalizingArchive) {
      return Err ("retained incident requires graph recovery before report recovery" . into ()); }
    let settlements : Option<Vec<ViewSettlementRecord>> =
      if active . phase == MaintenancePhase::Presenting
      && active . view_settlements . is_empty () {
        let archive : Arc<VerifiedInitialArchive> = self . recover_initial_archive (&active)?;
        let (bundle, publication) : (MaintenanceEvidenceBundle, PublishedMaintenanceEvidence) =
          self . maintenance_evidence . load (incident)?;
        validate_report_evidence (&active, &bundle, &publication)?;
        let reconstructed : ReconstructedEvidence = reconstruct_evidence (&bundle . recovery)?;
        let g0 : InRustGraph = graph_from_evidence (&reconstructed . g0_nodes)?;
        let g1 : InRustGraph = graph_from_evidence (&reconstructed . g1_nodes)?;
        let report : ReportGraphChangeSet = ReportGraphChangeSet {
          g0_graph: &g0, g1_graph: &g1,
          added_primary_ids: &bundle . header . added_primary_ids,
          deleted_primary_ids: &bundle . header . deleted_primary_ids,
          modified_primary_ids: &bundle . header . modified_primary_ids, };
        Some (plan_recovered_incident_view_settlements (&active, &archive, &report)?)
      } else { None };
    self . transition_maintenance (|coordinator| {
      if let Some (settlements) = settlements {
        coordinator . record_view_settlements (incident, epoch, settlements)?; }
      coordinator . retire_incident_authority (incident, epoch, self . server_session_id ())
    }) }

  fn recover_initial_archive (
    &self,
    active : &ActiveMaintenance,
  ) -> Result<Arc<VerifiedInitialArchive>, String> {
    if let Some (archive) = self . verified_archive (&active . incident_id) {
      return Ok (archive); }
    let checksum : &str = active . initial_archive_manifest_sha256 . as_deref ()
      . ok_or ("report recovery has no retained initial archive checksum")?;
    let snapshot : Arc<SelectedRuntimeSnapshot> = self . selected_snapshot ();
    let root : &Path = active . archive_root_identity . as_deref ()
      . unwrap_or (&snapshot . env . config . maintenance_archive_identity);
    let archive : VerifiedInitialArchive = verify_initial_archive (InitialArchiveExpectation {
      archive_root: root, active, manifest_sha256: checksum, })?;
    self . retain_verified_archive (active . incident_id . clone (), archive . clone ());
    Ok (Arc::new (archive)) }
}

fn validate_report_evidence (
  active : &ActiveMaintenance,
  bundle : &MaintenanceEvidenceBundle,
  publication : &PublishedMaintenanceEvidence,
) -> Result<(), String> {
  let recorded : &ServerEvidenceRecord = active . server_evidence . as_ref ()
    . ok_or ("report recovery has no retained server evidence authority")?;
  if recorded . path != publication . path
  || recorded . bundle_sha256 != publication . bundle_sha256
  || recorded . artifact_count != u64::try_from (publication . artifact_count)
    . map_err (|_| "report artifact count exceeds u64")?
  || recorded . total_file_bytes != publication . total_file_bytes
  || bundle . header . incident_id != active . incident_id
  || bundle . header . maintenance_epoch != active . epoch
  || Some (&bundle . header . candidate) != active . candidate . as_ref ()
  || bundle . header . g0_graph_generation != active . g0_graph_generation
  || bundle . header . g0_manifest_revision != active . g0_manifest_revision {
    return Err ("reconstructed report evidence disagrees with its durable incident" . into ()); }
  Ok (( )) }

fn graph_from_evidence (
  evidence : &BTreeMap<String, SemanticNodeEvidence>,
) -> Result<InRustGraph, String> {
  let nodes : Vec<NodeComplete> = evidence . iter () . map (| (pid, node) | {
    if pid != &node . pid {
      return Err ("semantic report node is stored under a different primary ID" . into ()); }
    NodeComplete::try_from (node)
  }) . collect::<Result<Vec<NodeComplete>, String>> ()?;
  Ok (InRustGraph::from_nodecompletes (&nodes)) }
