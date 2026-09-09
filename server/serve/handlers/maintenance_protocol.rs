//! Bootstrap and archive-acknowledgement endpoints for maintenance incidents.

use crate::maintenance::archive::{
  InitialArchiveExpectation,
  VerifiedInitialArchive,
  verify_initial_archive,
};
use crate::maintenance::evidence::{
  CLIENT_EVIDENCE_FORMAT_VERSION,
  ClientEvidenceArtifact,
  ClientEvidenceBundle,
};
use crate::maintenance::selection::{
  CandidateSelectionResult,
  rebuild_archived_candidate,
  select_archived_candidate,
};
use crate::maintenance::coordinator::VIEW_ENROLLMENT_PENDING;
use crate::maintenance::pull::validate_repository_mapping;
use crate::maintenance::view_impact::plan_incident_view_settlements;
use crate::maintenance::{
  CandidateId,
  ArchiveStatus,
  BufferKind,
  ClientEvidenceTransferRecord,
  CoordinatorState,
  ExternalMutationOutcome,
  ExternalMutationRecord,
  IncidentId,
  MaintenanceEpoch,
  MaintenanceOrigin,
  MaintenancePhase,
  MaintenanceTargets,
  PendingReason,
  TerminalDisposition,
  TerminalMaintenance,
  ScalarReleaseRecord,
  ViewApplicationAcknowledgement,
  ViewApplicationRecord,
  ViewSettlementRecord,
  ViewSettlementRequirement,
};
use crate::from_text::buffer_to_viewnodes::uninterpreted::
  org_to_uninterpreted_viewforest;
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::{AttachedClient, CensusDescriptor};
use crate::serve::handlers::client_census::source_inventory_field;
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  approved_pids_from_request,
  decide as decide_scalar_release,
};
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{
  send_artifact_bundle_with_length_prefix,
  send_response_with_length_prefix,
  tag_terminal_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use futures::executor::block_on;
use std::collections::{BTreeMap, HashSet};
use std::net::TcpStream;
use std::path::Path;

use crate::types::misc::ID;
use crate::types::sexp::{atom_to_string, extract_string_list_from_sexp};
use crate::types::store_state::StoreHealth;
use crate::types::maybe_placed_viewnode::maybePlaced_to_placed_viewforest;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{
  ViewState,
  ViewUri,
  pids_from_viewforest,
};
use crate::update_buffer::render_maintenance_view;

const ARCHIVE_FORMAT_VERSION : u32 = 1;

pub fn skg_save_policy_refusal (state : &CoordinatorState) -> Option<String> {
  if state . policy () . skg_saves_allowed { return None; }
  let status = match state {
    CoordinatorState::Pending (pending) => match (
      &pending . reason, &pending . candidate)
    {
      (PendingReason::ValidDiskDifference, Some (candidate)) => format! (
        "disk reconciliation is pending for candidate {} (changed primary IDs: {}); run skg-reconcile-pending-changes / :SkgReconcilePendingChanges",
        candidate . id,
        if candidate . changed_primary_ids . is_empty () {
          "none" . into ()
        } else { candidate . changed_primary_ids . join (", ") }),
      (reason, _) => format! (
        "disk reconciliation is pending ({:?}): {}; run skg-reconcile-pending-changes / :SkgReconcilePendingChanges",
        reason,
        if pending . details . is_empty () {
          "see the maintenance status report" . into ()
        } else { pending . details . join ("; ") }),
    },
    CoordinatorState::Active (active) => format! (
      "maintenance incident {} is {:?}; Skg saves remain disabled until its terminal disposition",
      active . incident_id, active . phase),
    CoordinatorState::Terminal (terminal) => format! (
      "maintenance incident {} is terminal ({}) and awaits client acknowledgement",
      terminal . incident_id, terminal . disposition . label ()),
    CoordinatorState::BlockedStoreHealth { reason } => format! (
      "Skg saves are disabled because store health is blocked: {}", reason),
    CoordinatorState::Idle | CoordinatorState::Observing =>
      "Skg save policy is temporarily unavailable" . into (),
  };
  Some (format! ("* NOTHING WAS SAVED\n\n{}\n\nThe rejected save will not be retried automatically.", status))
}

pub fn handle_maintenance_protocol_request (
  stream       : &mut TcpStream,
  request      : &str,
  runtime      : &ServerRuntime,
  request_type : RequestType,
) {
  match request_type {
    RequestType::BeginMaintenance =>
      handle_begin_maintenance_request (stream, request, runtime),
    RequestType::MaintenanceLockedCensus =>
      handle_maintenance_locked_census_request (stream, request, runtime),
    RequestType::RunMaintenanceOrigin =>
      handle_run_maintenance_origin_request (stream, request, runtime),
    RequestType::FinishMaintenanceOrigin =>
      handle_finish_maintenance_origin_request (stream, request, runtime),
    RequestType::MaintenanceArchiveReady =>
      handle_maintenance_archive_ready_request (stream, request, runtime),
    RequestType::MaintenanceArchiveFinalized =>
      handle_maintenance_archive_finalized_request (stream, request, runtime),
    RequestType::MaintenanceArchiveFailed =>
      handle_maintenance_archive_failed_request (stream, request, runtime),
    RequestType::ApproveUndoWaiver =>
      handle_approve_undo_waiver_request (stream, request, runtime),
    RequestType::ApproveMaintenanceScalarRelease =>
      handle_approve_maintenance_scalar_release_request (
        stream, request, runtime),
    RequestType::CancelMaintenance =>
      handle_cancel_maintenance_request (stream, request, runtime),
    RequestType::MaintenanceStatus =>
      handle_maintenance_status_request (stream, runtime),
    RequestType::RetryMaintenance =>
      handle_retry_maintenance_request (stream, request, runtime),
    RequestType::MaintenanceEvidence =>
      handle_maintenance_evidence_request (stream, request, runtime),
    RequestType::MaintenanceViewSettled =>
      handle_maintenance_view_settled_request (stream, request, runtime),
    RequestType::CompleteMaintenance =>
      handle_complete_maintenance_request (stream, request, runtime),
    RequestType::AcknowledgeTerminalMaintenance =>
      handle_acknowledge_terminal_maintenance_request (
        stream, request, runtime),
    _ => unreachable! ("non-maintenance request reached maintenance handler"),
  }
}

pub fn handle_begin_maintenance_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = begin_maintenance (request, runtime);
  send_result (stream, TcpToClient::MaintenanceOffer, "complete", result);
}

fn begin_maintenance (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let origin = parse_origin (&value_from_request_sexp ("origin", request)?)?;
  let parsed = sexp::parse (request)
    . map_err (|error| format! ("invalid maintenance request: {}", error))?;
  let targets = MaintenanceTargets {
    paths: optional_string_list (&parsed, "paths")?,
    ids: optional_string_list (&parsed, "ids")?,
    pull_repositories: optional_pull_repositories (&parsed)?,
  };
  let snapshot = runtime . selected_snapshot ();
  if origin == MaintenanceOrigin::ExplicitPartialReload {
    validate_partial_reload_paths (&snapshot . env . config, &targets . paths)?;
  }
  if origin == MaintenanceOrigin::Pull {
    validate_repository_mapping (
      &snapshot . env . config, &targets . pull_repositories)?;
  }
  let (client, source_set) = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let client = interactive . attached_client . clone ()
      . ok_or_else (|| "no interactive client is attached" . to_string ())?;
    if !client . census_complete {
      return Err ("client census is not complete" . into ()); }
    (client, interactive . active_source_set . name . 0 . clone ())
  };
  // Undo requirements and raw-file dirtiness belong to the later exact locked
  // census.  The format capability itself can be rejected before allocating.
  validate_client_archive_capability (&client, &[])?;
  if origin == MaintenanceOrigin::Pull && source_set != "all" {
    return Err (format! (
      "pull requires source-set 'all'; the retained source-set is '{}'",
      source_set)); }

  let candidate = match value_from_request_sexp ("candidate-id", request) {
    Ok (value) if value != "none" => {
      let id = CandidateId::parse (&value)?;
      let candidate = runtime . candidate (&id) . ok_or_else (|| format! (
        "candidate {} is no longer retained", id))?;
      Some (candidate . summary . clone ())
    }
    _ => None,
  };
  if origin == MaintenanceOrigin::PendingReconciliation && candidate . is_none () {
    return Err ("pending reconciliation requires the exact candidate ID" . into ()); }
  if let Some (candidate) = &candidate {
    if candidate . base_graph_generation != snapshot . selected . graph_generation
    || candidate . base_manifest_revision != snapshot . selected . manifest_revision
    {
      return Err (format! ("candidate {} was superseded", candidate . id)); }
  }

  let active = runtime . transition_maintenance (|coordinator|
    coordinator . begin_epoch_with_archive_contract_and_targets (
      origin,
      candidate,
      client . session_id . clone (),
      client . kind . label () . into (),
      source_set,
      snapshot . selected . graph_generation,
      snapshot . selected . manifest_revision,
      targets))?;
  Ok (maintenance_offer_payload (
    "install-maintenance-epoch-and-submit-locked-census", &active,
    &snapshot . env . config . maintenance_archive_folder . to_string_lossy (),
    &snapshot . env . config . maintenance_archive_identity . to_string_lossy ()))
}

pub fn handle_maintenance_locked_census_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = freeze_maintenance_census (request, runtime);
  send_result (stream, TcpToClient::MaintenanceOffer, "complete", result);
}

fn freeze_maintenance_census (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let active = require_archive_owner (runtime, &incident, epoch)?;
  if !matches! (active . phase,
    MaintenancePhase::AwaitingLockedCensus
    | MaintenancePhase::PreparingArchive)
  {
    return Err (format! (
      "locked census is invalid during {:?}", active . phase)); }
  let (client, census) = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let client = interactive . attached_client . clone ()
      . ok_or_else (|| "no interactive client is attached" . to_string ())?;
    if !client . census_complete {
      return Err ("locked client census is not complete" . into ()); }
    let census = interactive . live_census . values () . cloned ()
      . collect::<Vec<_>> ();
    (client, census)
  };
  validate_census_parentage (&census)?;
  validate_client_archive_capability (&client, &census)?;
  if census . iter () . any (|descriptor|
       descriptor . maintenance_epoch != Some (epoch . get ()))
  {
    return Err (format! (
      "maintenance census contains a buffer not locked for epoch {}",
      epoch . get ())); }
  let dirty_raw : Vec<_> = census . iter () . filter (|descriptor|
      descriptor . dirty && descriptor . kind == "raw-skg-file")
    . map (|descriptor| descriptor . buffer_id . clone ()) . collect ();
  if !dirty_raw . is_empty () {
    return Err (format! (
      "maintenance refuses modified raw .skg buffers: {}",
      dirty_raw . join (", "))); }
  let frozen_census = census . iter ()
    . map (CensusDescriptor::frozen_record)
    . collect::<Result<Vec<_>, _>> ()?;
  runtime . transition_maintenance (|coordinator|
    coordinator . freeze_locked_census (&incident, epoch, frozen_census))?;
  let active = matching_active (runtime, &incident, epoch)?;
  let snapshot = runtime . selected_snapshot ();
  Ok (maintenance_offer_payload (
    "locked-census-accepted-publish-initial-archive", &active,
    &snapshot . env . config . maintenance_archive_folder . to_string_lossy (),
    &snapshot . env . config . maintenance_archive_identity . to_string_lossy ()))
}

fn validate_census_parentage (census : &[CensusDescriptor])
  -> Result<(), String>
{
  let by_id : std::collections::HashMap<_, _> = census . iter ()
    . map (|descriptor| (descriptor . buffer_id . as_str (), descriptor))
    . collect ();
  for descriptor in census {
    let attached = matches! (descriptor . kind . as_str (),
      "metadata-editor" | "fork-confirmation" | "relationship-kind-menu"
      | "disk-conflict");
    let Some (origin_id) = descriptor . origin_buffer_id . as_deref () else {
      if attached {
        return Err (format! (
          "attached workflow '{}' has no origin buffer",
          descriptor . buffer_id)); }
      continue;
    };
    if !attached {
      return Err (format! (
        "non-workflow buffer '{}' carries origin authority",
        descriptor . buffer_id)); }
    let origin = by_id . get (origin_id) . ok_or_else (|| format! (
      "workflow '{}' names absent origin '{}'",
      descriptor . buffer_id, origin_id))?;
    let origin_uri = origin . view_uri . as_ref ()
      . map (ViewUri::repr_in_client);
    if descriptor . origin_view_uri != origin_uri
    || descriptor . origin_application_token != Some (origin . application_token)
    {
      return Err (format! (
        "workflow '{}' origin authority changed before census",
        descriptor . buffer_id)); }
    if matches! (descriptor . kind . as_str (),
         "metadata-editor" | "fork-confirmation" | "disk-conflict")
       && (!descriptor . dirty || !descriptor . logical_dirty
           || !origin . dirty || !origin . logical_dirty)
    {
      return Err (format! (
        "unfinished workflow '{}' did not make itself and its origin logically dirty",
        descriptor . buffer_id));
    }
    if descriptor . kind == "relationship-kind-menu"
       && (!descriptor . disposable || descriptor . continuation_id . is_none ())
    {
      return Err (format! (
        "relationship menu '{}' lacks cancellable disposable authority",
        descriptor . buffer_id));
    }
  }
  Ok (( ))
}

pub fn handle_maintenance_archive_ready_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = archive_ready (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

pub fn handle_run_maintenance_origin_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = run_maintenance_origin (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn run_maintenance_origin (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let active = require_archive_owner (runtime, &incident, epoch)?;
  match active . origin {
    MaintenanceOrigin::ExplicitPartialReload => {
      let started = runtime . transition_maintenance (|coordinator|
        coordinator . begin_target_observation (&incident, epoch))?;
      // Schedule replays too.  This is the durable restart edge: an incident
      // can journal FinalObservation before its process-local worker receives
      // the job.  Duplicate jobs drop after the first advances the incident.
      if let Err (error) = runtime . schedule_maintenance_target_observation (
          incident . clone (), epoch)
      {
        let _ = runtime . transition_maintenance (|coordinator|
          coordinator . block_invalid_disk (
            &incident, epoch, error . clone ()));
        return Err (format! (
          "could not schedule maintenance target observation: {}", error));
      }
      Ok (Sexp::List (vec![
        atom_field ("status", "origin-operation-started"),
        atom_field ("incident-id", incident . as_str ()),
        integer_field ("maintenance-epoch", epoch . get ()),
        atom_field ("phase", "final-observation"),
        atom_field ("replayed", if started { "nil" } else { "true" }),
        atom_field ("next-action", "await-maintenance-status"),
      ]) . to_string ())
    }
    MaintenanceOrigin::Pull => {
      let authorized = runtime . transition_maintenance (|coordinator|
        coordinator . authorize_external_mutation (&incident, epoch))?;
      Ok (Sexp::List (vec![
        atom_field ("status", "external-mutation-authorized"),
        atom_field ("incident-id", incident . as_str ()),
        integer_field ("maintenance-epoch", epoch . get ()),
        atom_field ("phase", "running-external-mutation"),
        atom_field ("replayed", if authorized { "nil" } else { "true" }),
        atom_field ("next-action", "run-client-pull"),
      ]) . to_string ())
    }
    MaintenanceOrigin::FullRebuild => {
      let started = runtime . transition_maintenance (|coordinator|
        coordinator . begin_full_rebuild_observation (&incident, epoch))?;
      if let Err (error) = runtime . schedule_maintenance_final_observation (
          incident . clone (), epoch)
      {
        let _ = runtime . transition_maintenance (|coordinator|
          coordinator . block_invalid_disk (
            &incident, epoch, error . clone ()));
        return Err (format! (
          "could not schedule full rebuild preflight: {}", error));
      }
      Ok (Sexp::List (vec![
        atom_field ("status", "origin-operation-started"),
        atom_field ("incident-id", incident . as_str ()),
        integer_field ("maintenance-epoch", epoch . get ()),
        atom_field ("phase", "final-observation"),
        atom_field ("replayed", if started { "nil" } else { "true" }),
        atom_field ("next-action", "await-maintenance-status"),
      ]) . to_string ())
    }
    _ => Err (format! (
      "maintenance origin '{}' has no runnable origin adapter",
      active . origin . label ())),
  }
}

pub fn handle_finish_maintenance_origin_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = finish_maintenance_origin (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn finish_maintenance_origin (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let outcome = ExternalMutationOutcome::parse (
    &value_from_request_sexp ("external-outcome", request)?)?;
  let parsed = sexp::parse (request)
    . map_err (|error| format! ("invalid maintenance request: {}", error))?;
  let record = ExternalMutationRecord {
    outcome,
    details: optional_string_list (&parsed, "external-details")?,
  };
  require_archive_owner (runtime, &incident, epoch)?;
  let recorded = runtime . transition_maintenance (|coordinator|
    coordinator . external_mutation_finished (
      &incident, epoch, record . clone ()))?;
  // The journal reaches FinalObservation before the process-local queue.  A
  // replay therefore always reschedules, while the worker accepts only the
  // still-matching incident and phase.
  if let Err (error) = runtime . schedule_maintenance_final_observation (
      incident . clone (), epoch)
  {
    let _ = runtime . transition_maintenance (|coordinator|
      coordinator . block_invalid_disk (
        &incident, epoch, error . clone ()));
    return Err (format! (
      "could not schedule final maintenance observation: {}", error));
  }
  Ok (Sexp::List (vec![
    atom_field ("status", "origin-operation-finished"),
    atom_field ("incident-id", incident . as_str ()),
    integer_field ("maintenance-epoch", epoch . get ()),
    atom_field ("phase", "final-observation"),
    atom_field ("external-outcome", record . outcome . label ()),
    list_field ("external-details", &record . details),
    atom_field ("replayed", if recorded { "nil" } else { "true" }),
    atom_field ("next-action", "await-maintenance-status"),
  ]) . to_string ())
}

pub fn handle_maintenance_archive_finalized_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = archive_finalized (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

pub fn handle_complete_maintenance_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = complete_maintenance (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn complete_maintenance (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let manifest_sha256 = sha256_request_field (request, "manifest-sha256")?;
  require_completion_owner (
    runtime, &incident, epoch, &manifest_sha256)?;
  // Close delayed watcher gaps at the terminal boundary.  The incident keeps
  // its original G1 presentation fence; a different current signature is
  // ordinary retained refresh work and remains queued until maintenance
  // unlocks.  This also upgrades an older/incomplete post-selection journal
  // which predates the explicit presentation-fence record.
  let (presentation_generation, signature_blake3) =
    runtime . exact_git_presentation_identity ()?;
  let missing_fence = matches! (
    &runtime . maintenance_snapshot ()
      . state,
    CoordinatorState::Active (active) if active . presentation_fence . is_none ());
  if missing_fence {
    runtime . transition_maintenance (|coordinator|
      coordinator . record_presentation_fence (
        &incident, epoch, signature_blake3 . clone (),
        presentation_generation))?;
  }
  let terminal = runtime . transition_maintenance (|coordinator|
    coordinator . finish (
      &incident, epoch, TerminalDisposition::Completed))?;
  Ok (terminal_payload (&terminal))
}

pub fn handle_acknowledge_terminal_maintenance_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = acknowledge_terminal_maintenance (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn acknowledge_terminal_maintenance (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let attached = attached_client (runtime)?;
  {
    let coordinator = runtime . maintenance_snapshot ();
    if let CoordinatorState::Terminal (terminal) = &coordinator . state {
      if terminal . controlling_session_id () != attached . session_id {
        return Err (
          "terminal acknowledgement came from a different controller session"
            . into ()); }
    }
  }
  let newly_acknowledged = runtime . transition_maintenance (|coordinator|
    coordinator . acknowledge_terminal (&incident, epoch))?;
  // Retain the compact idle record and its issuing epoch. An old terminal
  // ACK must never erase a newer owner's publication or reset its identity.
  let successor_queued = match runtime . schedule_full_observation (
      crate::maintenance::QueuedObservationReason::MaintenanceCompleted)
  {
    Ok (( )) => true,
    Err (error) => {
      tracing::warn! (%error,
        "could not queue post-maintenance successor observation");
      false
    }
  };
  Ok (Sexp::List (vec![
    atom_field ("status", "idle"),
    atom_field ("terminal-acknowledged",
      if newly_acknowledged { "true" } else { "already-idle" }),
    atom_field ("successor-observation-queued",
      if successor_queued { "true" } else { "nil" }),
  ]) . to_string ())
}

fn require_completion_owner (
  runtime         : &ServerRuntime,
  incident        : &IncidentId,
  epoch           : MaintenanceEpoch,
  manifest_sha256 : &str,
) -> Result<(), String> {
  let attached = attached_client (runtime)?;
  let coordinator = runtime . maintenance_snapshot ();
  match &coordinator . state {
    CoordinatorState::Active (active) => {
      if &active . incident_id != incident || active . epoch != epoch {
        return Err ("completion names another active incident" . into ()); }
      if active . controlling_session_id () != attached . session_id {
        return Err ("completion came from a different controller session"
          . into ()); }
      match &active . archive_status {
        crate::maintenance::ArchiveStatus::Finalized {
          manifest_sha256: expected,
        } if expected == manifest_sha256 => Ok (( )),
        crate::maintenance::ArchiveStatus::Finalized { .. } =>
          Err ("completion changed the final manifest checksum" . into ()),
        _ => Err ("completion precedes final archive acknowledgement"
          . into ()),
      }
    }
    CoordinatorState::Terminal (terminal) => {
      if &terminal . incident_id != incident || terminal . epoch != epoch {
        return Err ("completion names another terminal incident" . into ()); }
      if terminal . controlling_session_id () != attached . session_id {
        return Err ("completion came from a different controller session"
          . into ()); }
      if terminal . archive_manifest_sha256 . as_deref ()
         != Some (manifest_sha256)
      {
        return Err ("completion changed the final manifest checksum" . into ()); }
      Ok (( ))
    }
    _ => Err ("no finalized maintenance incident can complete" . into ()),
  }
}

fn terminal_payload (terminal : &TerminalMaintenance) -> String {
  let mut fields = vec![
    atom_field ("status", "terminal"),
    atom_field ("incident-id", terminal . incident_id . as_str ()),
    integer_field ("maintenance-epoch", terminal . epoch . get ()),
    atom_field ("disposition", terminal . disposition . label ()),
    atom_field ("archive-directory-name", &terminal . archive_directory_name),
    atom_field ("manifest-sha256", terminal . archive_manifest_sha256
      . as_deref () . unwrap_or ("none")),
    list_field ("unlock-buffer-ids", &terminal . registered_buffer_ids),
  ];
  if let Some (selected) = &terminal . selected_store {
    fields . push (integer_field (
      "selected-graph-generation", selected . graph_generation . get ()));
    fields . push (integer_field (
      "selected-manifest-revision", selected . manifest_revision . get ()));
  }
  fields . push (requested_id_outcomes_sexp (
    &terminal . requested_id_outcomes,
    terminal . disposition == TerminalDisposition::Completed));
  Sexp::List (fields) . to_string ()
}

fn archive_finalized (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let manifest_sha256 = sha256_request_field (
    request, "manifest-sha256")?;
  let transfer_manifest_sha256 = sha256_request_field (
    request, "transfer-manifest-sha256")?;
  let artifact_bytes_sha256 = sha256_request_field (
    request, "artifact-bytes-sha256")?;
  require_archive_owner (runtime, &incident, epoch)?;
  let newly_recorded = runtime . transition_maintenance (|coordinator|
    coordinator . archive_finalized (
      &incident,
      epoch,
      manifest_sha256 . clone (),
      transfer_manifest_sha256 . clone (),
      artifact_bytes_sha256 . clone ()))?;
  Ok (Sexp::List (vec![
    atom_field ("status", "archive-finalized"),
    atom_field ("manifest-sha256", &manifest_sha256),
    atom_field ("transfer-manifest-sha256", &transfer_manifest_sha256),
    atom_field ("artifact-bytes-sha256", &artifact_bytes_sha256),
    atom_field ("replayed", if newly_recorded { "nil" } else { "true" }),
    atom_field ("next-action", "complete-maintenance"),
  ]) . to_string ())
}

fn archive_ready (request : &str, runtime : &ServerRuntime)
  -> Result<String, String>
{
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let manifest_sha256 = value_from_request_sexp ("manifest-sha256", request)?;
  let lock_sha256 = value_from_request_sexp ("lock-census-sha256", request)?;
  let active = matching_active (runtime, &incident, epoch)?;
  let attached = attached_client (runtime)?;
  if attached . session_id != active . controlling_session_id () {
    return Err ("archive ACK came from a different controller session" . into ()); }
  if lock_sha256 != lock_census_sha256 (&active . registered_buffer_ids) {
    return Err ("maintenance epoch lock census checksum does not match" . into ()); }
  let snapshot = runtime . selected_snapshot ();
  if snapshot . selected . graph_generation != active . g0_graph_generation
  || snapshot . selected . manifest_revision != active . g0_manifest_revision
  {
    return Err ("selected G0 changed while the initial archive was prepared" . into ()); }
  let verified = verify_initial_archive (InitialArchiveExpectation {
    archive_root: &snapshot . env . config . maintenance_archive_identity,
    active: &active,
    manifest_sha256: &manifest_sha256,
  })?;
  runtime . retain_verified_archive (incident . clone (), verified . clone ());
  runtime . transition_maintenance (|coordinator| coordinator . archive_ready (
    &incident, epoch, manifest_sha256 . clone ()))?;
  if active . candidate . is_some () {
    return select_and_stage_candidate (runtime, &incident, epoch, &verified);
  } else {
    let mut fields = archive_verification_fields (&verified);
    fields . splice (0..0, vec![
      atom_field ("status", "archive-ready"),
      atom_field ("incident-id", active . incident_id . as_str ()),
      integer_field ("maintenance-epoch", active . epoch . get ()),
      atom_field ("phase", "archive-ready"),
    ]);
    fields . push (pull_repositories_field (
      &active . targets . pull_repositories));
    fields . push (atom_field (
      "next-action", "origin-specific-operation-required"));
    return Ok (Sexp::List (fields) . to_string ());
  }
}

/// Run the common post-archive candidate selection and durable presentation
/// staging.  Pending reconciliation calls this in its archive-ready request;
/// background origin workers call the same function after attaching their
/// exact observation to the incident.
pub(crate) fn select_and_stage_candidate (
  runtime  : &ServerRuntime,
  incident : &IncidentId,
  epoch    : MaintenanceEpoch,
  verified : &VerifiedInitialArchive,
) -> Result<String, String> {
  let mut active = matching_active (runtime, incident, epoch)?;
  if !active . pending_view_enrollments . is_empty () {
    return Ok (view_enrollment_pending_payload (&active)); }
  if active . selected_store . is_none () {
    let selection = if active . origin == MaintenanceOrigin::FullRebuild
      || active . force_full_rebuild_recovery
    {
      rebuild_archived_candidate (runtime, incident, epoch)?
    } else {
      select_archived_candidate (runtime, incident, epoch)?
    };
    if matches! (
      selection, CandidateSelectionResult::DeferredForViewEnrollment)
    {
      active = matching_active (runtime, incident, epoch)?;
      return Ok (view_enrollment_pending_payload (&active)); }
    active = matching_active (runtime, incident, epoch)?;
  } else if active . phase != MaintenancePhase::Presenting {
    return Err (format! (
      "selected maintenance candidate cannot stage views during {:?}",
      active . phase));
  }
  if !active . pending_view_enrollments . is_empty () {
    return Ok (view_enrollment_pending_payload (&active));
  }
  if active . presentation_fence . is_none () {
    let (presentation_generation, signature_blake3) =
      runtime . exact_git_presentation_identity ()?;
    runtime . transition_maintenance (|coordinator|
      coordinator . record_presentation_fence (
        incident, epoch, signature_blake3 . clone (),
        presentation_generation))?;
    active = matching_active (runtime, incident, epoch)?;
  }
  if !active . pending_view_enrollments . is_empty () {
    return Ok (view_enrollment_pending_payload (&active)); }
  let candidate_id = active . candidate . as_ref ()
    . expect ("selected incident has candidate") . id . clone ();
  let candidate = runtime . candidate (&candidate_id)
    . ok_or_else (|| "selected candidate was not retained" . to_string ())?;
  let selected_snapshot = runtime . selected_snapshot ();
  let selected_config = &selected_snapshot . env . config;
  let settlements = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    plan_incident_view_settlements (
      &active, verified, &interactive, &candidate)?
  };
  let approved_pids : HashSet<ID> = active . scalar_release . as_ref ()
    .filter (|release| release . approved)
    .into_iter ()
    .flat_map (|release| release . pids . iter ())
    .map (|pid| ID::from (pid . as_str ()))
    .collect ();
  match stage_application_settlements (
      runtime, &active, settlements, &approved_pids)?
  {
    ApplicationStaging::Ready (settlements) => {
      let recorded = runtime . transition_maintenance (|coordinator|
        coordinator . record_view_settlements (
          incident, epoch, settlements . clone ()));
      if let Err (error) = recorded {
        if error == VIEW_ENROLLMENT_PENDING {
          return matching_active (runtime, incident, epoch)
            . map (|active| view_enrollment_pending_payload (&active)); }
        return Err (error); }
      candidate_selected_payload (
        &active, selected_config, verified, &settlements)
    }
    ApplicationStaging::Challenge (challenge) => {
      let recorded = runtime . transition_maintenance (|coordinator|
        coordinator . record_scalar_challenge (
          incident, epoch, challenge . clone ()));
      if let Err (error) = recorded {
        if error == VIEW_ENROLLMENT_PENDING {
          return matching_active (runtime, incident, epoch)
            . map (|active| view_enrollment_pending_payload (&active)); }
        return Err (error); }
      scalar_challenge_payload (
        &active, selected_config, verified, &challenge)
    }
  }
}

fn view_enrollment_pending_payload (
  active : &crate::maintenance::ActiveMaintenance,
) -> String {
  Sexp::List (vec![
    atom_field ("status", "view-enrollment-pending"),
    atom_field ("incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("phase", active . phase . label ()),
    list_field ("pending-view-uris",
      &active . pending_view_enrollments . keys () . cloned ()
        . collect::<Vec<_>> ()),
    atom_field ("next-action", "submit-maintenance-census"),
  ]) . to_string ()
}

enum ApplicationStaging {
  Ready (Vec<ViewSettlementRecord>),
  Challenge (ScalarReleaseRecord),
}

struct MaintenanceRenderInput {
  settlement_index : usize,
  buffer_id        : String,
  view_uri         : ViewUri,
  viewforest       : ViewForest,
}

struct MaintenanceRenderedView {
  settlement_index : usize,
  buffer_id        : String,
  view_uri         : ViewUri,
  content          : String,
  candidate_pids   : Vec<ID>,
  warnings         : Vec<String>,
}

fn stage_application_settlements (
  runtime       : &ServerRuntime,
  active        : &crate::maintenance::ActiveMaintenance,
  settlements   : Vec<ViewSettlementRecord>,
  approved_pids : &HashSet<ID>,
) -> Result<ApplicationStaging, String> {
  if !settlements . iter () . any (|record|
      record . requirement == ViewSettlementRequirement::ApplicationAck)
  {
    return Ok (ApplicationStaging::Ready (settlements)); }

  const PRESENTATION_ADVANCED : &str =
    "maintenance Git presentation advanced while views rendered";
  for attempt in 0..3 {
    // Watcher delivery is only an optimization.  This exact check closes a
    // delayed event gap before each attempt and puts a post-G1 change into the
    // ordinary retained presentation queue.
    runtime . observe_git_presentation ()?;
    match stage_application_settlements_once (
        runtime, active, settlements . clone (), approved_pids)
    {
      Err (error) if error == PRESENTATION_ADVANCED && attempt < 2 => continue,
      Err (error) if error == PRESENTATION_ADVANCED => return Err (
        "Git presentation remained unstable across three maintenance render attempts"
          . into ()),
      result => return result,
    }
  }
  unreachable! ()
}

fn stage_application_settlements_once (
  runtime       : &ServerRuntime,
  active        : &crate::maintenance::ActiveMaintenance,
  mut settlements : Vec<ViewSettlementRecord>,
  approved_pids : &HashSet<ID>,
) -> Result<ApplicationStaging, String> {
  const PRESENTATION_ADVANCED : &str =
    "maintenance Git presentation advanced while views rendered";

  let selected = active . selected_store . as_ref ()
    . ok_or_else (|| "view rendering precedes coherent store selection"
      . to_string ())?;
  let lease = runtime . query_lease ()?;
  if lease . snapshot . selected . graph_generation
       != selected . graph_generation
  || lease . snapshot . selected . manifest_revision
       != selected . manifest_revision
  {
    return Err (
      "maintenance view rendering did not acquire the selected G1"
        . into ()); }

  let (diff_mode_enabled, active_source_set, presentation_generation,
       render_inputs) = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    if interactive . active_source_set . name . 0 != active . source_set {
      return Err (format! (
        "active source-set changed from '{}' to '{}' during maintenance",
        active . source_set,
        interactive . active_source_set . name . 0)); }
    let mut inputs = Vec::new ();
    for (settlement_index, record) in settlements . iter () . enumerate () {
      if record . requirement != ViewSettlementRequirement::ApplicationAck {
        continue; }
      let uri_text = record . view_uri . as_ref () . ok_or_else (|| format! (
        "buffer '{}' has an application settlement without a view URI",
        record . buffer_id))?;
      let view_uri = ViewUri::from_client_string (uri_text . clone ());
      let state = interactive . views . open_views . views . get (&view_uri)
        . ok_or_else (|| format! (
          "buffer '{}' lost its retained server forest before rendering",
          record . buffer_id))?;
      validate_application_base (active, record, state)?;
      inputs . push (MaintenanceRenderInput {
        settlement_index,
        buffer_id: record . buffer_id . clone (),
        view_uri,
        viewforest: state . viewforest . clone (),
      });
    }
    (
      interactive . views . diff_mode_enabled,
      interactive . active_source_set . clone (),
      interactive . collateral_scheduler . presentation_generation (),
      inputs,
    )
  };

  let mut rendered_views = Vec::new ();
  for input in render_inputs {
    let (viewforest, content, warnings) = block_on (
      render_maintenance_view (
        input . viewforest,
        &lease . snapshot . env,
        diff_mode_enabled,
        Some (&active_source_set)))?;
    let mut candidate_pids : Vec<ID> =
      pids_from_viewforest (&viewforest) . into_iter () . collect ();
    candidate_pids . sort_by (|left, right|
      left . as_str () . cmp (right . as_str ()));
    rendered_views . push (MaintenanceRenderedView {
      settlement_index: input . settlement_index,
      buffer_id: input . buffer_id,
      view_uri: input . view_uri,
      content,
      candidate_pids,
      warnings,
    });
  }

  // Recompute rather than trusting watcher timing.  A changed signature bumps
  // the presentation generation and queues ordinary post-maintenance work;
  // this attempt is discarded and repeated against the new boundary.
  runtime . observe_git_presentation ()?;

  {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    if interactive . views . diff_mode_enabled != diff_mode_enabled
    || interactive . active_source_set != active_source_set {
      return Err (
        "maintenance source-set or diff-mode inputs advanced while views rendered"
          . into ()); }
    if interactive . collateral_scheduler . presentation_generation ()
         != presentation_generation
    {
      return Err (PRESENTATION_ADVANCED . into ()); }
    for rendered in &rendered_views {
      let record = &settlements[rendered . settlement_index];
      let state = interactive . views . open_views . views
        . get (&rendered . view_uri)
        . ok_or_else (|| format! (
          "buffer '{}' closed while its maintenance view rendered",
          rendered . buffer_id))?;
      validate_application_base (active, record, state)?;
    }
  }

  let mut all_candidate_pids : Vec<ID> = rendered_views . iter ()
    . flat_map (|rendered| rendered . candidate_pids . iter () . cloned ())
    . collect ();
  all_candidate_pids . sort_by (|left, right|
    left . as_str () . cmp (right . as_str ()));
  all_candidate_pids . dedup ();
  match decide_scalar_release (
      "maintenance-presentation",
      &active_source_set,
      &all_candidate_pids,
      &lease . snapshot . selected . graph,
      approved_pids)
  {
    ScalarReleaseDecision::Challenge { operation, pids, prompt } => {
      return Ok (ApplicationStaging::Challenge (ScalarReleaseRecord {
        operation,
        pids: pids . into_iter () . map (|pid| pid . to_string ()) . collect (),
        prompt,
        approved: false,
      }));
    }
    ScalarReleaseDecision::Allow
    | ScalarReleaseDecision::AllowWithWarning { .. } => {}
  }

  for rendered in rendered_views {
    let mut warnings = rendered . warnings;
    match decide_scalar_release (
        "maintenance-presentation",
        &active_source_set,
        &rendered . candidate_pids,
        &lease . snapshot . selected . graph,
        approved_pids)
    {
      ScalarReleaseDecision::Allow => {}
      ScalarReleaseDecision::AllowWithWarning { warning } =>
        warnings . push (warning),
      ScalarReleaseDecision::Challenge { .. } => return Err (
        "an individually rendered view escaped its aggregate scalar gate"
          . into ()),
    }
    let record = &mut settlements[rendered . settlement_index];
    record . application = Some (ViewApplicationRecord {
      content_sha256: format! (
        "{:x}", Sha256::digest (rendered . content . as_bytes ())),
      content: rendered . content,
      resulting_graph_generation: selected . graph_generation . get (),
      resulting_presentation_generation: presentation_generation,
      resulting_server_revision: record . base_server_revision
        . checked_add (1)
        . ok_or_else (|| format! (
          "buffer '{}' exhausted its server revision",
          record . buffer_id))?,
      resulting_application_token: record . base_application_token
        . checked_add (1)
        . ok_or_else (|| format! (
          "buffer '{}' exhausted its application token",
          record . buffer_id))?,
      warnings,
    });
  }
  Ok (ApplicationStaging::Ready (settlements))
}

fn validate_application_base (
  active : &crate::maintenance::ActiveMaintenance,
  record : &ViewSettlementRecord,
  state  : &ViewState,
) -> Result<(), String> {
  let frozen = active . presentation_buffer (&record . buffer_id)
    . ok_or_else (|| format! (
      "buffer '{}' is absent from the frozen census", record . buffer_id))?;
  if frozen . dirty
  || record . dirty
  || frozen . view_uri != record . view_uri
  || frozen . kind != record . kind
  || frozen . graph_generation != record . base_graph_generation
  || frozen . presentation_generation != record . base_presentation_generation
  || frozen . server_revision != record . base_server_revision
  || frozen . application_token != record . base_application_token
  || state . client_buffer_id . as_deref () != Some (&record . buffer_id)
  || state . kind != record . kind
  || state . revision != frozen . server_revision
  || state . client_application_token != frozen . application_token
  || state . graph_generation != frozen . graph_generation
  || state . presentation_generation != frozen . presentation_generation
  {
    return Err (format! (
      "buffer '{}' no longer has its exact clean frozen authority",
      record . buffer_id)); }
  Ok (( ))
}

fn archive_verification_fields (
  verified : &VerifiedInitialArchive,
) -> Vec<Sexp> {
  vec![
    atom_field ("verified-manifest-sha256", &verified . manifest_sha256),
    atom_field ("archive-path", &verified . path . to_string_lossy ()),
    integer_field ("artifact-count", verified . artifact_count as u64),
    integer_field ("archive-file-bytes", verified . total_file_bytes),
  ]
}

fn append_selected_fields (
  fields : &mut Vec<Sexp>,
  active : &crate::maintenance::ActiveMaintenance,
) -> Result<(), String> {
  let selected = active . selected_store . as_ref ()
    . ok_or_else (|| "maintenance has no selected G1 record" . to_string ())?;
  let evidence = active . server_evidence . as_ref ()
    . ok_or_else (|| "maintenance has no durable server evidence" . to_string ())?;
  fields . push (integer_field (
    "g1-graph-generation", selected . graph_generation . get ()));
  fields . push (integer_field (
    "g1-manifest-revision", selected . manifest_revision . get ()));
  fields . push (integer_field (
    "tantivy-generation", selected . tantivy_generation));
  fields . push (atom_field (
    "tantivy-outcome", &selected . tantivy_outcome));
  fields . push (atom_field (
    "server-evidence-sha256", &evidence . bundle_sha256));
  fields . push (integer_field (
    "server-evidence-artifact-count", evidence . artifact_count));
  fields . push (integer_field (
    "server-evidence-bytes", evidence . total_file_bytes));
  Ok (( ))
}

fn candidate_selected_payload (
  active      : &crate::maintenance::ActiveMaintenance,
  config      : &crate::types::misc::SkgConfig,
  verified    : &VerifiedInitialArchive,
  settlements : &[ViewSettlementRecord],
) -> Result<String, String> {
  let mut fields = archive_verification_fields (verified);
  fields . splice (0..0, maintenance_selection_identity_fields (
    active, "candidate-selected"));
  append_selected_fields (&mut fields, active)?;
  append_presentation_fence_fields (&mut fields, active);
  fields . push (atom_field ("successor-observation-required",
    if active . successor_observation_required { "true" } else { "nil" }));
  fields . push (atom_field ("source-set", &active . source_set));
  fields . push (source_inventory_field (config));
  fields . push (atom_field ("maintenance-archive-folder",
    &config . maintenance_archive_folder . to_string_lossy ()));
  fields . push (atom_field ("maintenance-archive-identity",
    &config . maintenance_archive_identity . to_string_lossy ()));
  fields . push (requested_id_outcomes_sexp (
    &active . requested_id_outcomes, false));
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("view-settlements" . into ())),
    Sexp::List (settlements . iter () . map (settlement_sexp) . collect ()),
  ]));
  Ok (Sexp::List (fields) . to_string ())
}

fn scalar_challenge_payload (
  active    : &crate::maintenance::ActiveMaintenance,
  config    : &crate::types::misc::SkgConfig,
  verified  : &VerifiedInitialArchive,
  challenge : &ScalarReleaseRecord,
) -> Result<String, String> {
  let mut fields = archive_verification_fields (verified);
  fields . splice (0..0, maintenance_selection_identity_fields (
    active, "needs-scalar-authorization"));
  append_selected_fields (&mut fields, active)?;
  append_presentation_fence_fields (&mut fields, active);
  fields . push (atom_field ("source-set", &active . source_set));
  fields . push (source_inventory_field (config));
  fields . push (atom_field ("maintenance-archive-folder",
    &config . maintenance_archive_folder . to_string_lossy ()));
  fields . push (atom_field ("maintenance-archive-identity",
    &config . maintenance_archive_identity . to_string_lossy ()));
  fields . push (requested_id_outcomes_sexp (
    &active . requested_id_outcomes, false));
  fields . push (atom_field ("operation", &challenge . operation));
  fields . push (list_field ("pids", &challenge . pids));
  fields . push (atom_field ("prompt", &challenge . prompt));
  fields . push (atom_field (
    "next-action", "approve-maintenance-scalar-release"));
  Ok (Sexp::List (fields) . to_string ())
}

fn maintenance_selection_identity_fields (
  active : &crate::maintenance::ActiveMaintenance,
  status : &str,
) -> Vec<Sexp> {
  vec![
    atom_field ("status", status),
    atom_field ("incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("candidate-id", active . candidate . as_ref ()
      . map (|candidate| candidate . id . as_str ()) . unwrap_or ("none")),
    atom_field ("phase", active . phase . label ()),
    list_field ("presentation-buffer-ids",
      &active . presentation_buffer_ids ()),
  ]
}

fn append_presentation_fence_fields (
  fields : &mut Vec<Sexp>,
  active : &crate::maintenance::ActiveMaintenance,
) {
  let Some (fence) = &active . presentation_fence else { return; };
  fields . push (integer_field ("presentation-fence-observation-sequence",
    fence . candidate_observation_sequence . get ()));
  fields . push (atom_field (
    "presentation-fence-signature-blake3", &fence . signature_blake3));
  fields . push (integer_field ("presentation-fence-generation",
    fence . presentation_generation));
}

fn requested_id_outcomes_sexp (
  outcomes : &[crate::maintenance::MaintenanceIdOutcome],
  terminal : bool,
) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S ("requested-id-outcomes" . into ())),
    Sexp::List (outcomes . iter () . map (|outcome| Sexp::List (vec![
      atom_field ("requested-id", &outcome . requested_id),
      atom_field ("pid", outcome . pid . as_deref () . unwrap_or ("nil")),
      atom_field ("status", if outcome . reason . is_some () {
        "rejected"
      } else if terminal {
        "acknowledged"
      } else {
        "resolved"
      }),
      atom_field ("reason", outcome . reason . as_deref () . unwrap_or ("nil")),
      list_field ("paths", &outcome . paths),
    ])) . collect ()),
  ])
}

pub(crate) fn preselection_retirements_field (
  active : &crate::maintenance::ActiveMaintenance,
) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S ("preselection-retirements" . into ())),
    Sexp::List (active . preselection_retirements . values ()
      . map (settlement_sexp) . collect ()),
  ])
}

fn settlement_sexp (record : &crate::maintenance::ViewSettlementRecord) -> Sexp {
  let mut fields = vec![
    atom_field ("buffer-id", &record . buffer_id),
    atom_field ("buffer-key", record . buffer_key . as_deref () . unwrap_or ("none")),
    atom_field ("kind", record . kind . label ()),
    atom_field ("view-uri", record . view_uri . as_deref () . unwrap_or ("none")),
    atom_field ("origin-buffer-id",
      record . origin_buffer_id . as_deref () . unwrap_or ("none")),
    atom_field ("origin-view-uri",
      record . origin_view_uri . as_deref () . unwrap_or ("none")),
    atom_field ("origin-application-token",
      &record . origin_application_token . map (|value| value . to_string ())
        . unwrap_or_else (|| "none" . into ())),
    atom_field ("origin-location",
      record . origin_location . as_deref () . unwrap_or ("none")),
    atom_field ("dirty", if record . dirty { "true" } else { "nil" }),
    atom_field ("impacted", if record . impacted { "true" } else { "nil" }),
    atom_field ("parse-uncertain",
      if record . parse_uncertain { "true" } else { "nil" }),
    atom_field ("uncertainty-reason",
      record . uncertainty_reason . as_deref () . unwrap_or ("none")),
    list_field ("observed-ids", &record . observed_ids),
    list_field ("resolved-primary-ids", &record . resolved_primary_ids),
    integer_field ("base-graph-generation", record . base_graph_generation),
    integer_field ("base-presentation-generation",
      record . base_presentation_generation),
    integer_field ("base-server-revision", record . base_server_revision),
    integer_field ("base-application-token", record . base_application_token),
    atom_field ("planned-disposition", record . planned_disposition . label ()),
    atom_field ("required-ack", record . requirement . label ()),
    atom_field ("settlement-resolution", record . resolution . label ()),
    atom_field ("acknowledged",
      if record . acknowledged { "true" } else { "nil" }),
  ];
  if let Some (application) = &record . application {
    fields . push (Sexp::List (vec![
      Sexp::Atom (Atom::S ("application" . into ())),
      Sexp::List (vec![
        atom_field ("content", &application . content),
        atom_field ("content-sha256", &application . content_sha256),
        integer_field ("resulting-graph-generation",
          application . resulting_graph_generation),
        integer_field ("resulting-presentation-generation",
          application . resulting_presentation_generation),
        integer_field ("resulting-server-revision",
          application . resulting_server_revision),
        integer_field ("resulting-application-token",
          application . resulting_application_token),
        list_field ("warnings", &application . warnings),
      ]),
    ]));
  }
  Sexp::List (fields)
}

pub fn handle_maintenance_archive_failed_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let incident = IncidentId::parse (
      &value_from_request_sexp ("incident-id", request)?)?;
    let epoch = MaintenanceEpoch::parse (
      &value_from_request_sexp ("maintenance-epoch", request)?)?;
    let buffer_key = value_from_request_sexp ("buffer-key", request)?;
    let reason = value_from_request_sexp ("reason", request)?;
    require_archive_owner (runtime, &incident, epoch)?;
    runtime . transition_maintenance (|coordinator|
      coordinator . archive_undo_failed (
        &incident, epoch, buffer_key . clone (), reason . clone ()))?;
    Ok (Sexp::List (vec![
      atom_field ("status", "undo-waiver-required"),
      atom_field ("buffer-key", &buffer_key),
      atom_field ("reason", &reason),
    ]) . to_string ())
  })();
  send_result (stream, TcpToClient::MaintenanceStatus,
    "needs-authorization", result);
}

pub fn handle_approve_undo_waiver_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let incident = IncidentId::parse (
      &value_from_request_sexp ("incident-id", request)?)?;
    let epoch = MaintenanceEpoch::parse (
      &value_from_request_sexp ("maintenance-epoch", request)?)?;
    let buffer_key = value_from_request_sexp ("buffer-key", request)?;
    let reason = value_from_request_sexp ("reason", request)?;
    require_archive_owner (runtime, &incident, epoch)?;
    runtime . transition_maintenance (|coordinator|
      coordinator . approve_undo_waiver (
        &incident, epoch, buffer_key . clone (), reason . clone ()))?;
    Ok (Sexp::List (vec![
      atom_field ("status", "retry-initial-archive"),
      atom_field ("waived-buffer-key", &buffer_key),
      atom_field ("waived-reason", &reason),
    ]) . to_string ())
  })();
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

pub fn handle_approve_maintenance_scalar_release_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = approve_maintenance_scalar_release (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn approve_maintenance_scalar_release (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  require_archive_owner (runtime, &incident, epoch)?;
  let approved_pids = approved_pids_from_request (request);
  let approved_pid_strings : Vec<String> = approved_pids . iter ()
    . map (|pid| pid . to_string ()) . collect ();
  runtime . transition_maintenance (|coordinator|
    coordinator . approve_scalar_release (
      &incident, epoch, approved_pid_strings . clone ()))?;

  let active = matching_active (runtime, &incident, epoch)?;
  let verified = runtime . verified_archive (&incident)
    . ok_or_else (|| "verified initial archive was not retained"
      . to_string ())?;
  let selected_snapshot = runtime . selected_snapshot ();
  let selected_config = &selected_snapshot . env . config;
  if !active . pending_view_enrollments . is_empty () {
    return Ok (view_enrollment_pending_payload (&active)); }
  if !active . view_settlements . is_empty () {
    let settlements : Vec<ViewSettlementRecord> = active . view_settlements
      . values () . cloned () . collect ();
    return candidate_selected_payload (
      &active, selected_config, &verified, &settlements);
  }
  let candidate_id = active . candidate . as_ref ()
    . ok_or_else (|| "scalar authorization incident has no candidate"
      . to_string ())? . id . clone ();
  let candidate = runtime . candidate (&candidate_id)
    . ok_or_else (|| "selected candidate was not retained" . to_string ())?;
  let settlements = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    plan_incident_view_settlements (
      &active, &verified, &interactive, &candidate)?
  };
  let ApplicationStaging::Ready (settlements) =
    stage_application_settlements (
      runtime, &active, settlements, &approved_pids)?
  else {
    return Err (
      "maintenance scalar challenge changed after exact authorization"
        . into ());
  };
  let recorded = runtime . transition_maintenance (|coordinator|
    coordinator . record_view_settlements (
      &incident, epoch, settlements . clone ()));
  if let Err (error) = recorded {
    if error == VIEW_ENROLLMENT_PENDING {
      return matching_active (runtime, &incident, epoch)
        . map (|active| view_enrollment_pending_payload (&active)); }
    return Err (error); }
  candidate_selected_payload (
    &active, selected_config, &verified, &settlements)
}

pub fn handle_cancel_maintenance_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    let incident = IncidentId::parse (
      &value_from_request_sexp ("incident-id", request)?)?;
    let epoch = MaintenanceEpoch::parse (
      &value_from_request_sexp ("maintenance-epoch", request)?)?;
    require_archive_owner (runtime, &incident, epoch)?;
    runtime . transition_maintenance (|coordinator|
      coordinator . cancel_before_archive (&incident, epoch))?;
    Ok (Sexp::List (vec![
      atom_field ("status", "cancelled-before-archive"),
      integer_field ("unlock-maintenance-epoch", epoch . get ()),
    ]) . to_string ())
  })();
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

pub fn handle_maintenance_status_request (
  stream  : &mut TcpStream,
  runtime : &ServerRuntime,
) {
  let result = (|| -> Result<String, String> {
    resume_enrollment_deferred_candidate (runtime)?;
    let coordinator = runtime . maintenance_snapshot ();
    let selected_snapshot = runtime . selected_snapshot ();
    let selected_config = &selected_snapshot . env . config;
    Ok (match coordinator . state {
      CoordinatorState::Active (active) =>
        active_status_sexp (&active, Some (selected_config)),
      CoordinatorState::Pending (pending) => Sexp::List (vec![
        atom_field ("status", "pending"),
        atom_field ("pending-reason", pending . reason . label ()),
        atom_field ("candidate-id", pending . candidate . as_ref ()
          . map (|candidate| candidate . id . as_str ()) . unwrap_or ("none")),
      ]),
      CoordinatorState::Terminal (terminal) =>
        sexp::parse (&terminal_payload (&terminal))
          . expect ("terminal payload is valid"),
      other => Sexp::List (vec![
        atom_field ("status", other . label ()),
      ]),
    } . to_string ())
  })();
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn resume_enrollment_deferred_candidate (
  runtime : &ServerRuntime,
) -> Result<(), String> {
  let active = match &runtime . maintenance_snapshot ()
      . state
  {
    CoordinatorState::Active (active) => active . clone (),
    _ => return Ok (( )),
  };
  if active . candidate . is_none ()
  || !active . pending_view_enrollments . is_empty ()
  || !matches! (active . archive_status, ArchiveStatus::Ready { .. })
  {
    return Ok (( )); }
  let preselection_deferred = active . phase == MaintenancePhase::ArchiveReady
    && active . selected_store . is_none ();
  let presentation_deferred = active . phase == MaintenancePhase::Presenting
    && active . selected_store . is_some ()
    && active . view_settlements . is_empty ()
    && active . scalar_release . as_ref ()
      . map (|release| release . approved) . unwrap_or (true);
  if !preselection_deferred && !presentation_deferred {
    return Ok (( )); }
  let verified = runtime . verified_archive (&active . incident_id)
    . ok_or_else (|| "verified initial archive was not retained"
      . to_string ())?;
  select_and_stage_candidate (
    runtime, &active . incident_id, active . epoch, &verified)?;
  Ok (( ))
}

pub fn handle_retry_maintenance_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = retry_maintenance (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn retry_maintenance (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let active = require_archive_owner (runtime, &incident, epoch)?;
  let old_reason = active . blocking_reason . clone ()
    . unwrap_or_else (|| "unspecified" . into ());
  let (retry_kind, force_complete) = match active . phase {
    MaintenancePhase::BlockedInvalidAfterMutation => {
      runtime . transition_maintenance (|coordinator|
        coordinator . retry_blocked_invalid_disk (&incident, epoch))?;
      ("invalid-disk", active . force_full_rebuild_recovery)
    }
    MaintenancePhase::BlockedStoreHealth => {
      let snapshot = runtime . selected_snapshot ();
      let coherent_g0 =
        snapshot . selected . graph_generation == active . g0_graph_generation
        && snapshot . selected . manifest_revision
          == active . g0_manifest_revision
        && matches! (&snapshot . selected . tantivy_health,
          StoreHealth::Healthy);
      runtime . transition_maintenance (|coordinator|
        coordinator . retry_blocked_store_health (
          &incident, epoch, !coherent_g0))?;
      ("store-health", !coherent_g0)
    }
    ref phase => return Err (format! (
      "maintenance retry is invalid during {:?}", phase)),
  };
  let (recovery_mode, schedule_result) = if force_complete {
    ("full-rebuild", runtime . schedule_maintenance_final_observation (
      incident . clone (), epoch))
  } else if active . origin == MaintenanceOrigin::ExplicitPartialReload {
    ("targeted", runtime . schedule_maintenance_target_observation (
      incident . clone (), epoch))
  } else {
    ("complete", runtime . schedule_maintenance_final_observation (
      incident . clone (), epoch))
  };
  if let Err (error) = schedule_result {
    runtime . transition_maintenance (|coordinator| match retry_kind {
      "invalid-disk" => coordinator . block_invalid_disk (
        &incident, epoch, old_reason . clone ()),
      _ => coordinator . block_store_health (
        &incident, epoch, old_reason . clone ()),
    })?;
    return Err (format! (
      "maintenance remained blocked because recovery observation could not be queued: {}",
      error));
  }
  Ok (Sexp::List (vec![
    atom_field ("status", "maintenance-retry-queued"),
    atom_field ("incident-id", incident . as_str ()),
    integer_field ("maintenance-epoch", epoch . get ()),
    atom_field ("phase", MaintenancePhase::FinalObservation . label ()),
    atom_field ("retry-kind", retry_kind),
    atom_field ("recovery-mode", recovery_mode),
    atom_field ("previous-blocking-reason", &old_reason),
    atom_field ("next-action", "await-maintenance-status"),
  ]) . to_string ())
}

fn active_status_sexp (
  active : &crate::maintenance::ActiveMaintenance,
  config : Option<&crate::types::misc::SkgConfig>,
) -> Sexp {
  let mut fields = vec![
    atom_field ("status", "active"),
    atom_field ("active-incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("phase", active . phase . label ()),
    atom_field ("origin", active . origin . label ()),
    atom_field ("candidate-id", active . candidate . as_ref ()
      . map (|candidate| candidate . id . as_str ()) . unwrap_or ("none")),
    atom_field ("archive-directory-name", &active . archive_directory_name),
    atom_field ("archive-status", match &active . archive_status {
      ArchiveStatus::NotRequired => "not-required",
      ArchiveStatus::Preparing => "preparing",
      ArchiveStatus::UndoFailed { .. } => "undo-failed",
      ArchiveStatus::UndoWaiverApproved { .. } => "undo-waiver-approved",
      ArchiveStatus::Ready { .. } => "archive-ready",
      ArchiveStatus::Finalized { .. } => "finalized",
      ArchiveStatus::Incomplete { .. } => "incomplete",
    }),
    atom_field ("initial-manifest-sha256", active
      . initial_archive_manifest_sha256 . as_deref () . unwrap_or ("none")),
    atom_field ("archive-manifest-sha256", match &active . archive_status {
      ArchiveStatus::Ready { manifest_sha256 }
      | ArchiveStatus::Finalized { manifest_sha256 } => manifest_sha256,
      _ => "none",
    }),
    atom_field ("started-at-utc", &active . started_at_utc),
    atom_field ("source-set", &active . source_set),
    integer_field ("g0-graph-generation", active . g0_graph_generation . get ()),
    integer_field ("g0-manifest-revision", active . g0_manifest_revision . get ()),
    list_field ("requested-paths", &active . targets . paths),
    list_field ("requested-ids", &active . targets . ids),
    pull_repositories_field (&active . targets . pull_repositories),
    list_field ("registered-buffer-ids", &active . registered_buffer_ids),
    list_field ("presentation-buffer-ids",
      &active . presentation_buffer_ids ()),
    list_field ("pending-view-uris",
      &active . pending_view_enrollments . keys () . cloned ()
        . collect::<Vec<_>> ()),
    list_field ("dirty-buffer-ids", &active . dirty_buffer_ids),
    list_field (
      "undo-required-buffer-ids", &active . undo_required_buffer_ids),
    atom_field ("lock-census-sha256",
      &lock_census_sha256 (&active . registered_buffer_ids)),
  ];
  if let Some (reason) = &active . blocking_reason {
    fields . push (atom_field ("blocking-reason", reason));
  }
  if matches! (active . phase,
    MaintenancePhase::BlockedInvalidAfterMutation
    | MaintenancePhase::BlockedStoreHealth)
  {
    fields . push (atom_field ("next-action",
      if active . preselection_retirements . values ()
        . any (|record| !record . acknowledged)
      {
        "retire-invalid-dirty-buffers"
      } else {
        "retry-maintenance"
      }));
  }
  if active . selected_store . is_some () {
    let _ = append_selected_fields (&mut fields, active);
    if let Some (config) = config {
      fields . push (source_inventory_field (config));
      fields . push (atom_field ("maintenance-archive-folder",
        &config . maintenance_archive_folder . to_string_lossy ()));
      fields . push (atom_field ("maintenance-archive-identity",
        &config . maintenance_archive_identity . to_string_lossy ()));
    }
  }
  append_presentation_fence_fields (&mut fields, active);
  if let Some (scalar) = &active . scalar_release {
    fields . push (atom_field ("operation", &scalar . operation));
    fields . push (list_field ("pids", &scalar . pids));
    fields . push (atom_field ("prompt", &scalar . prompt));
    fields . push (atom_field (
      "scalar-approved", if scalar . approved { "true" } else { "nil" }));
  }
  if let Some (external) = &active . external_mutation {
    fields . push (atom_field (
      "external-outcome", external . outcome . label ()));
    fields . push (list_field ("external-details", &external . details));
  }
  if !active . preselection_retirements . is_empty () {
    fields . push (preselection_retirements_field (active));
  }
  if !active . view_settlements . is_empty () {
    fields . push (Sexp::List (vec![
      Sexp::Atom (Atom::S ("view-settlements" . into ())),
      Sexp::List (active . view_settlements . values ()
        . map (settlement_sexp) . collect ()),
    ]));
  }
  fields . push (requested_id_outcomes_sexp (
    &active . requested_id_outcomes, false));
  Sexp::List (fields)
}

pub fn handle_maintenance_evidence_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  match maintenance_evidence (request, runtime) {
    Ok ((descriptor, bytes)) => {
      let _ = send_artifact_bundle_with_length_prefix (
        stream, &descriptor, &bytes); }
    Err (error) => send_result (
      stream, TcpToClient::MaintenanceEvidence, "failed", Err (error)),
  }
}

pub fn handle_maintenance_view_settled_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = acknowledge_view_settlement (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
}

fn acknowledge_view_settlement (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<String, String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let buffer_id = value_from_request_sexp ("buffer-id", request)?;
  if buffer_id . is_empty () {
    return Err ("view settlement ACK has an empty buffer ID" . into ()); }
  let requirement = ViewSettlementRequirement::parse (
    &value_from_request_sexp ("required-ack", request)?)?;
  let view_uri = value_from_request_sexp ("view-uri", request)?;
  let view_uri = if view_uri == "none" { None } else { Some (view_uri) };
  let base_revision = unsigned_request_field (
    request, "base-server-revision")?;
  let base_graph_generation = unsigned_request_field (
    request, "base-graph-generation")?;
  let base_presentation_generation = unsigned_request_field (
    request, "base-presentation-generation")?;
  let application_token = unsigned_request_field (
    request, "base-application-token")?;
  let application_ack = if requirement
       == ViewSettlementRequirement::ApplicationAck
  {
    Some (ViewApplicationAcknowledgement {
      content_sha256: sha256_request_field (request, "content-sha256")?,
      resulting_graph_generation: unsigned_request_field (
        request, "resulting-graph-generation")?,
      resulting_presentation_generation: unsigned_request_field (
        request, "resulting-presentation-generation")?,
      resulting_server_revision: unsigned_request_field (
        request, "resulting-server-revision")?,
      resulting_application_token: unsigned_request_field (
        request, "resulting-application-token")?,
    })
  } else { None };
  let active = require_archive_owner (runtime, &incident, epoch)?;
  let preselection = active . phase
    == MaintenancePhase::BlockedInvalidAfterMutation
    && active . preselection_retirements . contains_key (&buffer_id);
  let record = if preselection {
    active . preselection_retirements . get (&buffer_id)
  } else {
    active . view_settlements . get (&buffer_id)
  } . ok_or_else (|| format! (
    "buffer '{}' has no planned settlement", buffer_id))?;
  let effect = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let state = record . view_uri . as_ref () . and_then (|uri|
      interactive . views . open_views . views . get (
        &ViewUri::from_client_string (uri . clone ())));
    prepare_server_settlement_effect (
      &active, record, state, application_ack . as_ref ())?
  };
  let all_settled = runtime . transition_maintenance (|coordinator|
    if preselection {
      coordinator . acknowledge_preselection_retirement (
        &incident, epoch, &buffer_id, requirement . clone (),
        view_uri . as_deref (), base_graph_generation,
        base_presentation_generation, base_revision, application_token)
    } else {
      coordinator . acknowledge_view_settlement (
        &incident,
        epoch,
        &buffer_id,
        requirement . clone (),
        view_uri . as_deref (),
        base_graph_generation,
        base_presentation_generation,
        base_revision,
        application_token,
        application_ack . as_ref ())
    })?;
  apply_server_settlement_effect (runtime, effect);
  Ok (Sexp::List (vec![
    atom_field ("status", if preselection && all_settled {
      "all-invalid-dirty-buffers-retired"
    } else if preselection {
      "invalid-dirty-buffer-retired"
    } else if all_settled {
      "all-views-settled"
    } else {
      "view-settlement-recorded"
    }),
    atom_field ("buffer-id", &buffer_id),
    atom_field ("required-ack", requirement . label ()),
    atom_field ("next-action", if preselection && all_settled {
      "repair-and-retry-maintenance"
    } else if preselection {
      "retire-remaining-dirty-buffers"
    } else if all_settled {
      "finalize-archive"
    } else {
      "settle-remaining-views"
    }),
  ]) . to_string ())
}

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum ServerSettlementEffect {
  None,
  Unregister (ViewUri),
  Preserve {
    uri              : ViewUri,
    graph_generation : u64,
    search_stale     : bool,
  },
  Apply {
    uri                     : ViewUri,
    base_revision           : u64,
    viewforest              : ViewForest,
    graph_generation        : u64,
    presentation_generation : u64,
    application_token       : u64,
    search_stale            : bool,
  },
}

pub(crate) fn prepare_server_settlement_effect (
  active          : &crate::maintenance::ActiveMaintenance,
  record          : &ViewSettlementRecord,
  state           : Option<&ViewState>,
  application_ack : Option<&ViewApplicationAcknowledgement>,
) -> Result<ServerSettlementEffect, String> {
  if record . acknowledged { return Ok (ServerSettlementEffect::None); }
  let uri = record . view_uri . as_ref ()
    . map (|uri| ViewUri::from_client_string (uri . clone ()));
  if record . requirement == ViewSettlementRequirement::ApplicationAck {
    let uri = uri . ok_or_else (|| format! (
      "buffer '{}' has an application offer without a view URI",
      record . buffer_id))?;
    let state = state . ok_or_else (|| format! (
      "buffer '{}' closed before application acknowledgement",
      record . buffer_id))?;
    validate_application_base (active, record, state)?;
    let offer = record . application . as_ref () . ok_or_else (|| format! (
      "buffer '{}' has no staged application offer", record . buffer_id))?;
    let ack = application_ack . ok_or_else (|| format! (
      "buffer '{}' application acknowledgement is incomplete",
      record . buffer_id))?;
    if offer . content_sha256 != ack . content_sha256
    || offer . resulting_graph_generation
         != ack . resulting_graph_generation
    || offer . resulting_presentation_generation
         != ack . resulting_presentation_generation
    || offer . resulting_server_revision
         != ack . resulting_server_revision
    || offer . resulting_application_token
         != ack . resulting_application_token
    {
      return Err (format! (
        "buffer '{}' application acknowledgement changed its offer",
        record . buffer_id)); }
    let selected = active . selected_store . as_ref ()
      . ok_or_else (|| "view application precedes coherent store selection"
        . to_string ())?;
    if offer . resulting_graph_generation
         != selected . graph_generation . get ()
    || offer . resulting_server_revision
         != record . base_server_revision . checked_add (1)
           . ok_or_else (|| "server revision exhausted" . to_string ())?
    || offer . resulting_application_token
         != record . base_application_token . checked_add (1)
           . ok_or_else (|| "application token exhausted" . to_string ())?
    || format! ("{:x}", Sha256::digest (offer . content . as_bytes ()))
         != offer . content_sha256
    {
      return Err (format! (
        "buffer '{}' staged application record is internally inconsistent",
        record . buffer_id)); }
    let (maybe_placed, parse_errors, _warnings) =
      org_to_uninterpreted_viewforest (&offer . content)
      . map_err (|error| format! (
        "could not reconstruct staged view '{}': {}",
        record . buffer_id, error))?;
    if !parse_errors . is_empty () {
      return Err (format! (
        "staged view '{}' reparsed with errors: {}",
        record . buffer_id, parse_errors . iter ()
          . map (|error| error . to_string ())
          . collect::<Vec<_>> () . join ("; "))); }
    let viewforest = maybePlaced_to_placed_viewforest (maybe_placed)
      . map_err (|error| format! (
        "could not place staged view '{}': {}",
        record . buffer_id, error))?;
    return Ok (ServerSettlementEffect::Apply {
      uri,
      base_revision: record . base_server_revision,
      viewforest,
      graph_generation: offer . resulting_graph_generation,
      presentation_generation: offer . resulting_presentation_generation,
      application_token: offer . resulting_application_token,
      search_stale: record . kind == BufferKind::SearchView,
    });
  }
  if application_ack . is_some () {
    return Err (format! (
      "buffer '{}' supplied application authority for a non-application settlement",
      record . buffer_id)); }
  if let Some (state) = state {
    let frozen = active . presentation_buffer (&record . buffer_id)
      . ok_or_else (|| "settlement is absent from the frozen census"
        . to_string ())?;
    if state . client_buffer_id . as_deref () != Some (&record . buffer_id)
    || state . revision != frozen . server_revision
    || state . client_application_token != frozen . application_token
    || state . graph_generation != frozen . graph_generation
    || state . presentation_generation != frozen . presentation_generation
    {
      return Err (format! (
        "buffer '{}' server authority advanced before settlement",
        record . buffer_id)); }
  }
  match record . requirement {
    ViewSettlementRequirement::RetirementAck
    | ViewSettlementRequirement::CloseAck => Ok (uri
      . map (ServerSettlementEffect::Unregister)
      . unwrap_or (ServerSettlementEffect::None)),
    ViewSettlementRequirement::ReleaseAck => {
      let Some (uri) = uri else { return Ok (ServerSettlementEffect::None); };
      if state . is_none () { return Ok (ServerSettlementEffect::None); }
      let selected = active . selected_store . as_ref ()
        . ok_or_else (|| "view release precedes coherent store selection"
          . to_string ())?;
      Ok (ServerSettlementEffect::Preserve {
        uri,
        graph_generation: selected . graph_generation . get (),
        search_stale: record . kind == BufferKind::SearchView,
      })
    }
    ViewSettlementRequirement::ApplicationAck => unreachable! (),
  }
}

fn apply_server_settlement_effect (
  runtime : &ServerRuntime,
  effect  : ServerSettlementEffect,
) {
  let mut interactive = runtime . interactive . lock ()
    . expect ("interactive session poisoned after settlement journal");
  match effect {
    ServerSettlementEffect::None => {}
    ServerSettlementEffect::Unregister (uri) =>
      interactive . views . open_views . unregister_view (&uri),
    ServerSettlementEffect::Preserve {
      uri, graph_generation, search_stale,
    } => {
      let state = interactive . views . open_views . views . get_mut (&uri)
        . expect ("validated settlement view disappeared during one request");
      state . graph_generation = graph_generation;
      state . presentation_stale = true;
      state . search_stale |= search_stale;
    }
    ServerSettlementEffect::Apply {
      uri,
      base_revision,
      viewforest,
      graph_generation,
      presentation_generation,
      application_token,
      search_stale,
    } => {
      assert! (interactive . views . open_views . update_view_if_revision (
        &runtime . selected_snapshot () . selected . graph, &uri, base_revision, viewforest),
        "validated maintenance application advanced during one request");
      interactive . views . open_views . set_client_application_authority (
        &uri,
        graph_generation,
        presentation_generation,
        application_token)
        . expect ("validated maintenance application view disappeared");
      let state = interactive . views . open_views . views . get_mut (&uri)
        . expect ("applied maintenance view remains registered");
      state . search_stale |= search_stale;
    }
  }
}

fn unsigned_request_field (request : &str, key : &str) -> Result<u64, String> {
  value_from_request_sexp (key, request)? . parse::<u64> ()
    . map_err (|_| format! ("{} is not an unsigned integer", key))
}

fn sha256_request_field (request : &str, key : &str) -> Result<String, String> {
  let value = value_from_request_sexp (key, request)?;
  if value . len () != 64
  || !value . bytes () . all (|byte|
       byte . is_ascii_digit () || (b'a'..=b'f') . contains (&byte))
  {
    return Err (format! ("{} is not a lowercase SHA-256", key)); }
  Ok (value)
}

fn maintenance_evidence (
  request : &str,
  runtime : &ServerRuntime,
) -> Result<(String, Vec<u8>), String> {
  let incident = IncidentId::parse (
    &value_from_request_sexp ("incident-id", request)?)?;
  let epoch = MaintenanceEpoch::parse (
    &value_from_request_sexp ("maintenance-epoch", request)?)?;
  let expected_server_sha = value_from_request_sexp (
    "server-evidence-sha256", request)?;
  let active = require_archive_owner (runtime, &incident, epoch)?;
  if !matches! (active . phase,
    MaintenancePhase::Presenting | MaintenancePhase::FinalizingArchive)
  {
    return Err (format! (
      "maintenance evidence is unavailable during {:?}", active . phase)); }
  let server_record = active . server_evidence . as_ref ()
    . ok_or_else (|| "incident has no durable server evidence" . to_string ())?;
  if expected_server_sha != server_record . bundle_sha256 {
    return Err ("maintenance evidence request names another server bundle"
      . into ()); }
  let selected = active . selected_store . as_ref ()
    . ok_or_else (|| "incident has not selected a coherent store generation"
      . to_string ())?;
  let bundle = runtime . maintenance_evidence . client_bundle (&incident)?;
  if bundle . maintenance_epoch != epoch
  || active . candidate . as_ref () != Some (&bundle . candidate)
  || bundle . server_bundle_sha256 != server_record . bundle_sha256
  || bundle . artifacts . len () as u64 != server_record . artifact_count
  {
    return Err ("durable client evidence does not match the active incident"
      . into ()); }
  let transfer = ClientEvidenceTransferRecord {
    server_bundle_sha256: bundle . server_bundle_sha256 . clone (),
    transfer_manifest_sha256: bundle . transfer_manifest_sha256 . clone (),
    artifact_bytes_sha256: bundle . artifact_bytes_sha256 . clone (),
    artifact_count: bundle . artifacts . len () as u64,
    artifact_bytes: bundle . bytes . len () as u64,
  };
  runtime . transition_maintenance (|coordinator|
    coordinator . record_client_evidence_transfer (
      &incident, epoch, transfer))?;
  let payload = maintenance_evidence_payload (&active, &bundle, selected);
  let descriptor = tag_terminal_sexp_response (
    TcpToClient::MaintenanceEvidence, "complete", &payload);
  Ok ((descriptor, bundle . bytes))
}

fn maintenance_evidence_payload (
  active   : &crate::maintenance::ActiveMaintenance,
  bundle   : &ClientEvidenceBundle,
  selected : &crate::maintenance::SelectedStoreRecord,
) -> String {
  Sexp::List (vec![
    integer_field (
      "artifact-bundle-format-version", CLIENT_EVIDENCE_FORMAT_VERSION as u64),
    atom_field ("incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("candidate-id", bundle . candidate . id . as_str ()),
    integer_field (
      "g0-graph-generation", active . g0_graph_generation . get ()),
    integer_field (
      "g0-manifest-revision", active . g0_manifest_revision . get ()),
    integer_field (
      "g1-graph-generation", selected . graph_generation . get ()),
    integer_field (
      "g1-manifest-revision", selected . manifest_revision . get ()),
    integer_field ("tantivy-generation", selected . tantivy_generation),
    atom_field ("server-evidence-sha256", &bundle . server_bundle_sha256),
    atom_field (
      "transfer-manifest-sha256", &bundle . transfer_manifest_sha256),
    atom_field ("artifact-bytes-sha256", &bundle . artifact_bytes_sha256),
    integer_field ("artifact-count", bundle . artifacts . len () as u64),
    integer_field ("artifact-bytes", bundle . bytes . len () as u64),
    Sexp::List (vec![
      Sexp::Atom (Atom::S ("artifacts" . into ())),
      Sexp::List (bundle . artifacts . iter ()
        . map (client_artifact_sexp) . collect ()),
    ]),
  ]) . to_string ()
}

fn client_artifact_sexp (artifact : &ClientEvidenceArtifact) -> Sexp {
  Sexp::List (vec![
    atom_field ("artifact-key", &artifact . key),
    atom_field ("relative-path", &artifact . relative_path),
    atom_field ("purpose", &artifact . purpose),
    integer_field ("byte-offset", artifact . byte_offset),
    integer_field ("byte-length", artifact . byte_length),
    atom_field ("sha256", &artifact . sha256),
  ])
}

fn matching_active (
  runtime  : &ServerRuntime,
  incident : &IncidentId,
  epoch    : MaintenanceEpoch,
) -> Result<crate::maintenance::ActiveMaintenance, String> {
  let coordinator = runtime . maintenance_snapshot ();
  let CoordinatorState::Active (active) = &coordinator . state else {
    return Err ("no maintenance incident is active" . into ()); };
  if &active . incident_id != incident || active . epoch != epoch {
    return Err (format! (
      "stale maintenance envelope; current incident is {} epoch {}",
      active . incident_id, active . epoch . get ())); }
  Ok (active . clone ())
}

fn attached_client (runtime : &ServerRuntime) -> Result<AttachedClient, String> {
  runtime . interactive . lock ()
    . map_err (|_| "interactive session poisoned" . to_string ())?
    . attached_client . clone ()
    . ok_or_else (|| "no interactive client is attached" . to_string ())
}

fn require_archive_owner (
  runtime  : &ServerRuntime,
  incident : &IncidentId,
  epoch    : MaintenanceEpoch,
) -> Result<crate::maintenance::ActiveMaintenance, String> {
  let active = matching_active (runtime, incident, epoch)?;
  if attached_client (runtime)? . session_id != active . controlling_session_id () {
    return Err (
      "maintenance message came from a different controller session" . into ()); }
  Ok (active)
}

fn validate_client_archive_capability (
  client : &AttachedClient,
  census : &[CensusDescriptor],
) -> Result<(), String> {
  if client . capabilities . archive_format_version != ARCHIVE_FORMAT_VERSION {
    return Err (format! (
      "client archive format {} is unsupported; server requires {}",
      client . capabilities . archive_format_version, ARCHIVE_FORMAT_VERSION)); }
  if !census . iter () . any (|descriptor|
       descriptor . dirty && descriptor . undo_required)
  {
    return Ok (( )); }
  match client . kind . label () {
    "emacs" if client . capabilities . native_undo_kind == "undo-fu-session"
      && client . capabilities . native_undo_version == "0.8" => Ok (( )),
    "neovim" if client . capabilities . native_undo_kind == "nvim-wundo"
      && client . capabilities . native_undo_version == client . version => Ok (( )),
    _ => Err (native_undo_capability_refusal (client)),
  }
}

fn native_undo_capability_refusal (client : &AttachedClient) -> String {
  let capability = &client . capabilities;
  match client . kind . label () {
    "emacs" if matches! (
      ( capability . native_undo_kind . as_str (),
        capability . native_undo_version . as_str () ),
      ("none", "not-installed") | ("unavailable", "unavailable")) =>
    {
      "dirty buffers have undo history, but this Emacs connection cannot archive it: undo-fu-session 0.8 was not available when the connection was established. Install exactly undo-fu-session 0.8, then reconnect the Skg client and retry"
        . into ()
    }
    "emacs" if capability . native_undo_kind == "undo-fu-session"
      || ( capability . native_undo_kind == "unavailable"
           && capability . native_undo_version != "unavailable" ) =>
    {
      format! (
        "dirty buffers have undo history, but this Emacs connection found undo-fu-session version '{}'; Skg requires exactly version 0.8. Install the supported version, then reconnect the Skg client and retry",
        capability . native_undo_version)
    }
    "emacs" => format! (
      "dirty buffers have undo history, but this Emacs connection advertised native undo adapter '{}' at version '{}'; Skg requires undo-fu-session 0.8. Reconnect with the supported adapter and retry",
      capability . native_undo_kind, capability . native_undo_version),
    "neovim" if capability . native_undo_kind == "nvim-wundo" => format! (
      "dirty buffers have undo history, but this Neovim connection advertised nvim-wundo version '{}' while its Neovim version is '{}'; Skg requires those versions to match. Reconnect with a compatible client and retry",
      capability . native_undo_version, client . version),
    "neovim" => format! (
      "dirty buffers have undo history, but this Neovim connection advertised native undo adapter '{}' at version '{}'; Skg requires nvim-wundo matching Neovim version '{}'. Reconnect with a compatible client and retry",
      capability . native_undo_kind, capability . native_undo_version,
      client . version),
    other => format! (
      "dirty buffers have undo history, but client '{}' advertised unsupported native undo adapter '{}' at version '{}'",
      other, capability . native_undo_kind, capability . native_undo_version),
  }
}

fn parse_origin (value : &str) -> Result<MaintenanceOrigin, String> {
  match value {
    "explicit-partial-reload" => Ok (MaintenanceOrigin::ExplicitPartialReload),
    "pending-reconciliation" => Ok (MaintenanceOrigin::PendingReconciliation),
    "pull" => Ok (MaintenanceOrigin::Pull),
    "full-rebuild" => Ok (MaintenanceOrigin::FullRebuild),
    "config-replacement" => Ok (MaintenanceOrigin::ConfigReplacement),
    "recovery" => Ok (MaintenanceOrigin::Recovery),
    other => Err (format! ("unsupported maintenance origin '{}'", other)),
  }
}

fn optional_string_list (sexp : &Sexp, key : &str)
  -> Result<Vec<String>, String>
{
  let present = match sexp {
    Sexp::List (items) => items . iter () . any (|item| match item {
      Sexp::List (parts) => matches! (parts . first (),
        Some (Sexp::Atom (Atom::S (candidate))) if candidate == key),
      _ => false,
    }),
    _ => false,
  };
  if present { extract_string_list_from_sexp (sexp, key) }
  else { Ok (Vec::new ()) }
}

fn field_values<'a> (sexp : &'a Sexp, key : &str)
  -> Option<&'a [Sexp]>
{
  let Sexp::List (items) = sexp else { return None; };
  items . iter () . find_map (|item| {
    let Sexp::List (parts) = item else { return None; };
    match parts . first () {
      Some (Sexp::Atom (Atom::S (candidate))) if candidate == key =>
        Some (&parts[1..]),
      _ => None,
    }
  })
}

fn required_record_atom (record : &Sexp, key : &str)
  -> Result<String, String>
{
  let values = field_values (record, key)
    . ok_or_else (|| format! ("pull repository has no {}", key))?;
  let value = match values {
    [value] => value,
    [Sexp::Atom (Atom::S (dot)), value] if dot == "." => value,
    _ => return Err (format! (
      "pull repository {} must contain exactly one atom", key)),
  };
  atom_to_string (value)
    . map_err (|_| format! ("pull repository {} is not an atom", key))
}

fn required_record_string_list (record : &Sexp, key : &str)
  -> Result<Vec<String>, String>
{
  let values = field_values (record, key)
    . ok_or_else (|| format! ("pull repository has no {}", key))?;
  let [Sexp::List (items)] = values else { return Err (format! (
    "pull repository {} must be one nested list", key)); };
  items . iter () . map (|item| atom_to_string (item)
    . map_err (|_| format! ("pull repository {} contains a non-atom", key)))
  . collect ()
}

fn optional_pull_repositories (sexp : &Sexp)
  -> Result<BTreeMap<String, Vec<String>>, String>
{
  let Some (values) = field_values (sexp, "pull-repositories") else {
    return Ok (BTreeMap::new ()); };
  let [Sexp::List (records)] = values else { return Err (
    "pull-repositories must be one nested list" . into ()); };
  let mut repositories = BTreeMap::new ();
  for record in records {
    let key = required_record_atom (record, "repository-key")?;
    let mut sources = required_record_string_list (record, "sources")?;
    let source_count = sources . len ();
    sources . sort ();
    sources . dedup ();
    if sources . len () != source_count {
      return Err (format! ("pull repository {} repeats a source", key)); }
    if repositories . insert (key . clone (), sources) . is_some () {
      return Err (format! ("pull repository key {} is repeated", key)); }
  }
  Ok (repositories)
}

fn validate_partial_reload_paths (
  config : &crate::types::misc::SkgConfig,
  paths  : &[String],
) -> Result<(), String> {
  for value in paths {
    let path = Path::new (value);
    let absolute = if path . is_absolute () {
      path . to_path_buf ()
    } else {
      config . data_root . join (path)
    };
    if config . sources . source_and_pid_for_direct_path (&absolute) . is_none () {
      return Err (format! (
        "partial reload path is not a direct configured .skg file: {}",
        value));
    }
  }
  Ok (( ))
}

fn maintenance_offer_payload (
  status                    : &str,
  active                    : &crate::maintenance::ActiveMaintenance,
  archive_folder            : &str,
  archive_server_identity   : &str,
) -> String {
  Sexp::List (vec![
    atom_field ("status", status),
    atom_field ("allocated-incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("origin", active . origin . label ()),
    atom_field ("started-at-utc", &active . started_at_utc),
    atom_field ("archive-directory-name", &active . archive_directory_name),
    atom_field ("maintenance-archive-folder", archive_folder),
    atom_field ("maintenance-archive-identity", archive_server_identity),
    atom_field ("source-set", &active . source_set),
    integer_field ("g0-graph-generation", active . g0_graph_generation . get ()),
    integer_field ("g0-manifest-revision", active . g0_manifest_revision . get ()),
    atom_field ("candidate-id", active . candidate . as_ref ()
      . map (|candidate| candidate . id . as_str ()) . unwrap_or ("none")),
    list_field ("requested-paths", &active . targets . paths),
    list_field ("requested-ids", &active . targets . ids),
    pull_repositories_field (&active . targets . pull_repositories),
    list_field ("registered-buffer-ids", &active . registered_buffer_ids),
    list_field ("dirty-buffer-ids", &active . dirty_buffer_ids),
    list_field ("undo-required-buffer-ids", &active . undo_required_buffer_ids),
    atom_field ("lock-census-sha256",
      &lock_census_sha256 (&active . registered_buffer_ids)),
  ]) . to_string ()
}

pub fn lock_census_sha256 (ids : &[String]) -> String {
  let mut ids = ids . to_vec ();
  ids . sort ();
  let mut digest = Sha256::new ();
  for id in ids {
    digest . update (id . as_bytes ());
    digest . update ([0]);
  }
  format! ("{:x}", digest . finalize ())
}

fn atom_field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

fn integer_field (key : &str, value : u64) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::I (value as i64)),
  ])
}

fn list_field (key : &str, values : &[String]) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::List (values . iter () . map (|value|
      Sexp::Atom (Atom::S (value . clone ()))) . collect ()),
  ])
}

fn pull_repositories_field (
  repositories : &BTreeMap<String, Vec<String>>,
) -> Sexp {
  let records = repositories . iter () . map (|(key, sources)|
    Sexp::List (vec![
      atom_field ("repository-key", key),
      list_field ("sources", sources),
    ])) . collect ();
  Sexp::List (vec![
    Sexp::Atom (Atom::S ("pull-repositories" . into ())),
    Sexp::List (records),
  ])
}

fn send_result (
  stream        : &mut TcpStream,
  response_type : TcpToClient,
  status        : &str,
  result        : Result<String, String>,
) {
  let response = match result {
    Ok (payload) => tag_terminal_sexp_response (response_type, status, &payload),
    Err (error) => tag_terminal_text_response (
      TcpToClient::Error, "failed", &error),
  };
  let _ = send_response_with_length_prefix (stream, &response);
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::maintenance::{
    CandidateSummary,
    FrozenBufferRecord,
    MaintenanceCoordinator,
    ObservationSequence,
    pull_repository_key,
    SelectedStoreRecord,
    ServerEvidenceRecord,
    ViewDisposition,
  };
  use crate::runtime::interactive_session::{
    ClientCapabilities,
    ClientKind,
  };
  use crate::types::tree::forest::ViewForest;
  use crate::types::store_state::{GraphGeneration, ManifestRevision};

  fn census_descriptor (id : &str, kind : &str) -> CensusDescriptor {
    CensusDescriptor {
      buffer_id: id . into (), kind: kind . into (),
      lifecycle: if kind == "content-view" {
        "live-view" . into ()
      } else { "attached-workflow" . into () },
      disposable: false, continuation_id: None,
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      view_uri: (kind == "content-view")
        . then_some (ViewUri::from_client_string ("view:origin" . into ())),
      recipe: "()" . into (), root_ids: Vec::new (),
      source_set: "all" . into (), graph_generation: 1,
      presentation_generation: 2, server_revision: 3,
      application_token: 5, dirty: true, logical_dirty: true,
      undo_required: false, maintenance_epoch: Some (9),
      modification_tick: 1, presentation_stale: false,
      search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "b" . repeat (64),
    }
  }

  fn attached_emacs_with_undo (
    kind    : &str,
    version : &str,
  ) -> AttachedClient {
    AttachedClient {
      kind: ClientKind::Emacs,
      version: "30.2" . into (),
      session_id: "session" . into (),
      capabilities: ClientCapabilities {
        archive_format_version: ARCHIVE_FORMAT_VERSION,
        native_undo_kind: kind . into (),
        native_undo_version: version . into (),
      },
      census_complete: true,
    }
  }

  #[test]
  fn missing_emacs_undo_capability_explains_the_fix () {
    let mut census = census_descriptor ("dirty", "content-view");
    census . undo_required = true;
    for (kind, version) in [
      ("none", "not-installed"),
      ("unavailable", "unavailable"), // pre-fix clients
    ] {
      let error = validate_client_archive_capability (
        &attached_emacs_with_undo (kind, version), &[census . clone ()])
        . unwrap_err ();
      assert! (error . contains ("undo-fu-session 0.8 was not available"),
               "{}", error);
      assert! (error . contains ("Install exactly undo-fu-session 0.8"),
               "{}", error);
      assert! (error . contains ("reconnect the Skg client"), "{}", error);
      assert! (!error . contains ("unavailable unavailable"), "{}", error);
    }
  }

  #[test]
  fn wrong_emacs_undo_version_names_found_and_required_versions () {
    let mut census = census_descriptor ("dirty", "content-view");
    census . undo_required = true;
    let error = validate_client_archive_capability (
      &attached_emacs_with_undo ("undo-fu-session", "0.9"), &[census])
      . unwrap_err ();
    assert! (error . contains ("found undo-fu-session version '0.9'"),
             "{}", error);
    assert! (error . contains ("requires exactly version 0.8"), "{}", error);
  }

  #[test]
  fn locked_census_requires_exact_attached_workflow_parentage () {
    let parent = census_descriptor ("origin", "content-view");
    let mut child = census_descriptor ("workflow", "metadata-editor");
    child . continuation_id = Some ("continuation" . into ());
    child . origin_buffer_id = Some ("origin" . into ());
    child . origin_view_uri = Some ("view:origin" . into ());
    child . origin_application_token = Some (5);
    child . origin_location = Some ("((start 1) (end 9))" . into ());
    assert! (validate_census_parentage (&[parent . clone (), child . clone ()])
      . is_ok ());
    child . origin_application_token = Some (6);
    assert! (validate_census_parentage (&[parent . clone (), child . clone ()])
      . unwrap_err () . contains ("origin authority changed"));
    child . origin_application_token = Some (5);
    let mut clean_parent = parent;
    clean_parent . logical_dirty = false;
    assert! (validate_census_parentage (&[clean_parent, child])
      . unwrap_err () . contains ("logically dirty"));
  }

  #[test]
  fn pull_repository_request_parser_normalizes_nested_source_groups () {
    let sources = vec!["one" . to_string (), "two" . to_string ()];
    let key = pull_repository_key (&sources);
    let request = sexp::parse (&format! (
      "((pull-repositories (((repository-key . \"{}\") \
       (sources (\"two\" \"one\"))))))",
      key)) . unwrap ();
    assert_eq! (
      optional_pull_repositories (&request) . unwrap (),
      BTreeMap::from ([(key, sources)]));
  }

  #[test]
  fn partial_reload_offer_and_status_repeat_the_frozen_targets () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let mut active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, Vec::new (), MaintenanceTargets {
        paths: vec!["source/node.skg" . into ()],
        ids: vec!["alias" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    active . presentation_fence = Some (
      crate::maintenance::MaintenancePresentationFence {
        candidate_observation_sequence: ObservationSequence::INITIAL,
        signature_blake3: "a" . repeat (64),
        presentation_generation: 7,
      });
    let offer = maintenance_offer_payload (
      "locked-census-accepted-publish-initial-archive",
      &active, "archive", "/archive");
    let status = active_status_sexp (&active, None) . to_string ();
    for payload in [&offer, &status] {
      assert! (payload . contains (
        "(requested-paths (source/node.skg))"), "{}", payload);
      assert! (payload . contains ("(requested-ids (alias))"), "{}", payload);
    }
    assert! (status . contains (
      "(presentation-fence-observation-sequence 0)"), "{}", status);
    assert! (status . contains (
      "(presentation-fence-signature-blake3 aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa)"),
      "{}", status);
    assert! (status . contains (
      "(presentation-fence-generation 7)"), "{}", status);
  }

  #[test]
  fn epoch_offer_precedes_the_locked_archive_offer () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let epoch = coordinator . begin_epoch_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, MaintenanceTargets {
        paths: vec!["source/node.skg" . into ()], ids: Vec::new (),
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    let first = maintenance_offer_payload (
      "install-maintenance-epoch-and-submit-locked-census",
      &epoch, "archive", "/archive");
    assert! (first . contains ("(registered-buffer-ids ())"), "{}", first);
    assert! (first . contains ("(maintenance-epoch 1)"), "{}", first);
    coordinator . freeze_locked_census (
      &epoch . incident_id, epoch . epoch, Vec::new ()) . unwrap ();
    let CoordinatorState::Active (locked) = &coordinator . state else {
      panic! ("locked census stopped being active"); };
    let second = maintenance_offer_payload (
      "locked-census-accepted-publish-initial-archive",
      locked, "archive", "/archive");
    assert! (second . contains ("locked-census-accepted"), "{}", second);
    assert_eq! (locked . phase, MaintenancePhase::PreparingArchive);
  }

  #[test]
  fn pull_status_replays_the_exact_external_mutation_result () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    coordinator . authorize_external_mutation (
      &active . incident_id, active . epoch) . unwrap ();
    coordinator . external_mutation_finished (
      &active . incident_id, active . epoch, ExternalMutationRecord {
        outcome: ExternalMutationOutcome::Failed,
        details: vec!["repo-a exited 1" . into ()],
      }) . unwrap ();
    let CoordinatorState::Active (active) = coordinator . state else {
      panic! ("pull incident stopped being active"); };
    let status = active_status_sexp (&active, None) . to_string ();
    assert! (status . contains ("(external-outcome failed)"), "{}", status);
    assert! (status . contains (
      "(external-details (\"repo-a exited 1\"))"), "{}", status);
    let key = pull_repository_key (&["test-source" . into ()]);
    assert! (status . contains (&format! (
      "(pull-repositories (((repository-key {}) (sources (test-source)))))",
      key)), "{}", status);
  }

  #[test]
  fn blocked_status_names_exact_reason_and_retry_action () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::Pull, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    coordinator . block_invalid_disk (
      &active . incident_id, active . epoch,
      "source file no longer parses" . into ()) . unwrap ();
    let CoordinatorState::Active (active) = coordinator . state else {
      panic! ("blocked incident stopped being active"); };
    let status = active_status_sexp (&active, None) . to_string ();
    assert! (status . contains (
      "(blocking-reason \"source file no longer parses\")"), "{}", status);
    assert! (status . contains ("(next-action retry-maintenance)"),
      "{}", status);
  }

  #[test]
  fn terminal_id_outcomes_acknowledge_only_ids_resolved_against_g0 () {
    let outcomes = vec![
      crate::maintenance::MaintenanceIdOutcome {
        requested_id: "alias" . into (), pid: Some ("primary" . into ()),
        reason: None, paths: vec!["/source/primary.skg" . into ()],
      },
      crate::maintenance::MaintenanceIdOutcome {
        requested_id: "unknown" . into (), pid: None,
        reason: Some ("ID is not present in the selected graph" . into ()),
        paths: Vec::new (),
      },
    ];
    let active = requested_id_outcomes_sexp (&outcomes, false) . to_string ();
    let terminal = requested_id_outcomes_sexp (&outcomes, true) . to_string ();
    assert! (active . contains ("(status resolved)"), "{}", active);
    assert! (!active . contains ("(status acknowledged)"), "{}", active);
    assert! (terminal . contains ("(status acknowledged)"), "{}", terminal);
    assert! (terminal . contains ("(status rejected)"), "{}", terminal);
    assert! (terminal . contains ("ID is not present in the selected graph"));
  }

  #[test]
  fn evidence_descriptor_names_every_exact_artifact_slice () {
    let summary = CandidateSummary {
      id: CandidateId::new (),
      base_graph_generation: GraphGeneration::INITIAL,
      base_manifest_revision: ManifestRevision::INITIAL,
      covered_sequence: ObservationSequence::INITIAL,
      changed_primary_ids: vec!["A" . into ()],
    };
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation, Some (summary . clone ()))
      . unwrap ();
    let bundle = ClientEvidenceBundle {
      incident_id: active . incident_id . clone (),
      maintenance_epoch: active . epoch,
      candidate: summary,
      server_bundle_sha256: "a" . repeat (64),
      transfer_manifest_sha256: "b" . repeat (64),
      artifact_bytes_sha256: "c" . repeat (64),
      artifacts: vec![ClientEvidenceArtifact {
        key: "artifact-00000000" . into (),
        relative_path: "modified-nodes/node-00000000-deadbeef/semantic.diff"
          . into (),
        purpose: "semantic-diff" . into (),
        byte_offset: 0,
        byte_length: 11,
        sha256: "d" . repeat (64),
      }],
      bytes: b"abcdefghijk" . to_vec (),
    };
    let selected = SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 9,
      tantivy_outcome: "committed" . into (),
    };
    let payload = maintenance_evidence_payload (&active, &bundle, &selected);
    let parsed = sexp::parse (&payload) . unwrap ();
    assert! (matches! (parsed, Sexp::List (_)));
    assert! (payload . contains ("(artifact-bundle-format-version 1)"));
    assert! (payload . contains ("(byte-offset 0)"));
    assert! (payload . contains ("(byte-length 11)"));
    assert! (payload . contains (
      "modified-nodes/node-00000000-deadbeef/semantic.diff"));
  }

  #[test]
  fn nonrendered_settlement_effects_require_exact_old_authority () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let mut active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![FrozenBufferRecord {
        buffer_id: "buffer" . into (), kind: BufferKind::SearchView,
        lifecycle: "live-view" . into (), disposable: false,
        continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
        origin_buffer_id: None, origin_view_uri: None,
        origin_application_token: None, origin_location: None,
        source_set: "all" . into (),
        view_uri: Some ("search:terms" . into ()), graph_generation: 1,
        presentation_generation: 3, server_revision: 4,
        application_token: 7, dirty: false, logical_dirty: false,
        undo_required: false, maintenance_epoch: None,
        presentation_stale: false, search_stale: false, herald_bearing: false,
        last_fetched_sha256: "a" . repeat (64),
        current_sha256: "a" . repeat (64),
      }], MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    active . selected_store = Some (SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 2, tantivy_outcome: "committed" . into (),
    });
    let record = ViewSettlementRecord {
      buffer_id: "buffer" . into (), buffer_key: None,
      kind: BufferKind::SearchView, view_uri: Some ("search:terms" . into ()),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      dirty: false, impacted: false, parse_uncertain: false,
      uncertainty_reason: None, observed_ids: Vec::new (),
      resolved_primary_ids: Vec::new (), base_graph_generation: 1,
      base_presentation_generation: 3, base_server_revision: 4,
      base_application_token: 7,
      planned_disposition: ViewDisposition::RetainedClean,
      requirement: ViewSettlementRequirement::ReleaseAck,
      application: None,
      resolution: Default::default (),
      acknowledged: false,
    };
    let state = ViewState {
      viewforest: ViewForest::new (), pids: Default::default (), revision: 4,
      root_ids: Default::default (),
      graph_generation: 1, presentation_generation: 3,
      client_application_token: 7, client_buffer_id: Some ("buffer" . into ()),
      kind: BufferKind::SearchView, recipe: Some ("terms" . into ()),
      source_set: "all" . into (),
      presentation_stale: false, search_stale: false,
    };
    assert_eq! (prepare_server_settlement_effect (
      &active, &record, Some (&state), None) . unwrap (),
      ServerSettlementEffect::Preserve {
        uri: ViewUri::SearchView ("terms" . into ()),
        graph_generation: 2,
        search_stale: true,
      });
    assert! (validate_application_base (
      &active, &record, &state) . is_ok ());
    let mut changed = state;
    changed . revision = 5;
    assert! (validate_application_base (
      &active, &record, &changed) . is_err ());
    assert! (prepare_server_settlement_effect (
      &active, &record, Some (&changed), None) . is_err ());
    let mut application = record . clone ();
    application . requirement = ViewSettlementRequirement::ApplicationAck;
    assert! (prepare_server_settlement_effect (
      &active, &application, Some (&changed), None) . is_err ());
    changed . revision = 4;
    application . impacted = true;
    application . planned_disposition = ViewDisposition::Refreshed;
    let content_sha256 = format! ("{:x}", Sha256::digest (b""));
    application . application = Some (ViewApplicationRecord {
      content: String::new (),
      content_sha256: content_sha256 . clone (),
      resulting_graph_generation: 2,
      resulting_presentation_generation: 9,
      resulting_server_revision: 5,
      resulting_application_token: 8,
      warnings: Vec::new (),
    });
    let exact = ViewApplicationAcknowledgement {
      content_sha256,
      resulting_graph_generation: 2,
      resulting_presentation_generation: 9,
      resulting_server_revision: 5,
      resulting_application_token: 8,
    };
    assert! (matches! (prepare_server_settlement_effect (
      &active, &application, Some (&changed), Some (&exact)) . unwrap (),
      ServerSettlementEffect::Apply {
        graph_generation: 2,
        presentation_generation: 9,
        application_token: 8,
        search_stale: true,
        ..
      }));
    let mut wrong = exact;
    wrong . content_sha256 = "f" . repeat (64);
    assert! (prepare_server_settlement_effect (
      &active, &application, Some (&changed), Some (&wrong)) . is_err ());
  }

  #[test]
  fn staged_application_wire_names_text_and_resulting_authority () {
    let mut record = ViewSettlementRecord {
      buffer_id: "buffer" . into (), buffer_key: None,
      kind: BufferKind::ContentView, view_uri: Some ("view" . into ()),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      dirty: false, impacted: true, parse_uncertain: false,
      uncertainty_reason: None, observed_ids: vec!["node" . into ()],
      resolved_primary_ids: vec!["node" . into ()],
      base_graph_generation: 1, base_presentation_generation: 3,
      base_server_revision: 4, base_application_token: 7,
      planned_disposition: ViewDisposition::Refreshed,
      requirement: ViewSettlementRequirement::ApplicationAck,
      application: Some (ViewApplicationRecord {
        content: "* title\nbody \"quoted\"\n" . into (),
        content_sha256: "a" . repeat (64),
        resulting_graph_generation: 2,
        resulting_presentation_generation: 9,
        resulting_server_revision: 5,
        resulting_application_token: 8,
        warnings: vec!["warning" . into ()],
      }),
      resolution: Default::default (),
      acknowledged: false,
    };
    let payload = settlement_sexp (&record) . to_string ();
    assert! (sexp::parse (&payload) . is_ok ());
    assert! (payload . contains ("* title"));
    assert! (payload . contains ("body"));
    assert! (payload . contains ("quoted"));
    assert! (payload . contains ("(content-sha256"));
    assert! (payload . contains ("(resulting-server-revision 5)"));
    assert! (payload . contains ("(resulting-application-token 8)"));
    assert! (payload . contains ("(acknowledged nil)"));
    record . acknowledged = true;
    assert! (settlement_sexp (&record) . to_string () . contains (
      "(acknowledged true)"));
  }

  #[test]
  fn scalar_challenge_response_contains_no_staged_view_text () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let mut active = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation,
      Some (CandidateSummary {
        id: CandidateId::new (),
        base_graph_generation: GraphGeneration::INITIAL,
        base_manifest_revision: ManifestRevision::INITIAL,
        covered_sequence: ObservationSequence::INITIAL,
        changed_primary_ids: vec!["node" . into ()],
      })) . unwrap ();
    active . selected_store = Some (SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 2,
      tantivy_outcome: "committed" . into (),
    });
    active . server_evidence = Some (ServerEvidenceRecord {
      path: "evidence" . into (),
      bundle_sha256: "a" . repeat (64),
      artifact_count: 1,
      total_file_bytes: 12,
    });
    let verified = VerifiedInitialArchive {
      path: "archive" . into (),
      manifest_sha256: "b" . repeat (64),
      artifact_count: 0,
      total_file_bytes: 1,
      buffers: Vec::new (),
    };
    let challenge = ScalarReleaseRecord {
      operation: "maintenance-presentation" . into (),
      pids: vec!["ugly-pid" . into ()],
      prompt: "Approve the exact PID?" . into (),
      approved: false,
    };
    let config = crate::dbs::filesystem::not_nodes::load_config (
      "tests/source_sets/fixtures/skgconfig.toml") . unwrap ();
    let payload = scalar_challenge_payload (
      &active, &config, &verified, &challenge) . unwrap ();
    assert! (payload . contains ("needs-scalar-authorization"));
    assert! (payload . contains (&format! (
      "(incident-id {})", active . incident_id)));
    assert! (payload . contains (&format! (
      "(maintenance-epoch {})", active . epoch . get ())));
    assert! (payload . contains (&format! (
      "(candidate-id {})", active . candidate . as_ref () . unwrap () . id)));
    assert! (payload . contains ("(source-set all)"));
    assert! (payload . contains ("(source-inventory ("));
    assert! (payload . contains ("(maintenance-archive-folder"));
    assert! (payload . contains ("ugly-pid"));
    assert! (!payload . contains ("view-settlements"));
    assert! (!payload . contains ("SECRET-STAGED-TEXT"));
  }
}
