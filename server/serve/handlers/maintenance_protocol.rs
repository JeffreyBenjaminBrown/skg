//! Bootstrap and archive-acknowledgement endpoints for maintenance incidents.

use crate::maintenance::archive::{
  InitialArchiveExpectation,
  verify_initial_archive,
};
use crate::maintenance::evidence::{
  CLIENT_EVIDENCE_FORMAT_VERSION,
  ClientEvidenceArtifact,
  ClientEvidenceBundle,
};
use crate::maintenance::selection::select_archived_candidate;
use crate::maintenance::view_impact::plan_incident_view_settlements;
use crate::maintenance::{
  CandidateId,
  ClientEvidenceTransferRecord,
  CoordinatorState,
  IncidentId,
  MaintenanceEpoch,
  MaintenanceOrigin,
  MaintenancePhase,
  TerminalDisposition,
  TerminalMaintenance,
  ViewSettlementRequirement,
};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::{AttachedClient, CensusDescriptor};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_artifact_bundle_with_length_prefix,
  send_response_with_length_prefix,
  tag_terminal_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};

use sexp::{Atom, Sexp};
use sha2::{Digest, Sha256};
use std::net::TcpStream;

const ARCHIVE_FORMAT_VERSION : u32 = 1;

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
  let snapshot = runtime . selected_snapshot ();
  let (client, source_set, census) = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let client = interactive . attached_client . clone ()
      . ok_or_else (|| "no interactive client is attached" . to_string ())?;
    if !client . census_complete {
      return Err ("client census is not complete" . into ()); }
    (
      client,
      interactive . active_source_set . name . 0 . clone (),
      interactive . live_census . values () . cloned () . collect::<Vec<_>> (),
    )
  };
  validate_client_archive_capability (&client, &census)?;
  let dirty_raw : Vec<_> = census . iter () . filter (|descriptor|
      descriptor . dirty && descriptor . kind == "raw-skg-file")
    . map (|descriptor| descriptor . buffer_id . clone ()) . collect ();
  if !dirty_raw . is_empty () {
    return Err (format! (
      "maintenance refuses modified raw .skg buffers: {}",
      dirty_raw . join (", "))); }
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

  let frozen_census = census . iter ()
    . map (CensusDescriptor::frozen_record)
    . collect::<Result<Vec<_>, _>> ()?;
  let active = runtime . transition_maintenance (|coordinator|
    coordinator . begin_with_archive_contract (
      origin,
      candidate,
      client . session_id . clone (),
      client . kind . label () . into (),
      source_set,
      snapshot . selected . graph_generation,
      snapshot . selected . manifest_revision,
      frozen_census))?;
  Ok (maintenance_offer_payload (&active,
    &snapshot . env . config . maintenance_archive_folder . to_string_lossy (),
    &snapshot . env . config . maintenance_archive_identity . to_string_lossy ()))
}

pub fn handle_maintenance_archive_ready_request (
  stream  : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  let result = archive_ready (request, runtime);
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", result);
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
    let coordinator = runtime . maintenance . lock ()
      . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
    if let CoordinatorState::Terminal (terminal) = &coordinator . state {
      if terminal . archive_owner_session_id != attached . session_id {
        return Err (
          "terminal acknowledgement came from a different client session"
            . into ()); }
    }
  }
  let newly_acknowledged = runtime . transition_maintenance (|coordinator|
    coordinator . acknowledge_terminal (&incident, epoch))?;
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?
    . clone ();
  if let Err (error) = runtime . maintenance_journal
    . remove_completed (&coordinator)
  {
    tracing::warn! (%error,
      "could not compact acknowledged maintenance journal"); }
  Ok (Sexp::List (vec![
    atom_field ("status", "idle"),
    atom_field ("terminal-acknowledged",
      if newly_acknowledged { "true" } else { "already-idle" }),
  ]) . to_string ())
}

fn require_completion_owner (
  runtime         : &ServerRuntime,
  incident        : &IncidentId,
  epoch           : MaintenanceEpoch,
  manifest_sha256 : &str,
) -> Result<(), String> {
  let attached = attached_client (runtime)?;
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
  match &coordinator . state {
    CoordinatorState::Active (active) => {
      if &active . incident_id != incident || active . epoch != epoch {
        return Err ("completion names another active incident" . into ()); }
      if active . archive_owner_session_id != attached . session_id {
        return Err ("completion came from a different client session"
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
      if terminal . archive_owner_session_id != attached . session_id {
        return Err ("completion came from a different client session"
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
  if attached . session_id != active . archive_owner_session_id {
    return Err ("archive ACK came from a different client session" . into ()); }
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
  let mut fields = vec![
    atom_field ("verified-manifest-sha256", &manifest_sha256),
    atom_field ("archive-path", &verified . path . to_string_lossy ()),
    integer_field ("artifact-count", verified . artifact_count as u64),
    integer_field ("archive-file-bytes", verified . total_file_bytes),
  ];
  if active . candidate . is_some () {
    let selected = select_archived_candidate (runtime, &incident, epoch)?;
    let candidate_id = active . candidate . as_ref ()
      . expect ("candidate branch has candidate") . id . clone ();
    let candidate = runtime . candidate (&candidate_id)
      . ok_or_else (|| "selected candidate was not retained" . to_string ())?;
    let settlements = {
      let interactive = runtime . interactive . lock ()
        . map_err (|_| "interactive session poisoned" . to_string ())?;
      plan_incident_view_settlements (
        &active, &verified, &interactive, &candidate)?
    };
    runtime . transition_maintenance (|coordinator|
      coordinator . record_view_settlements (
        &incident, epoch, settlements . clone ()))?;
    fields . insert (0, atom_field ("status", "candidate-selected"));
    fields . push (integer_field (
      "g1-graph-generation", selected . graph_generation));
    fields . push (integer_field (
      "g1-manifest-revision", selected . manifest_revision));
    fields . push (integer_field (
      "tantivy-generation", selected . tantivy_generation));
    fields . push (atom_field (
      "tantivy-outcome", &selected . tantivy_outcome));
    fields . push (atom_field (
      "server-evidence-sha256", &selected . evidence . bundle_sha256));
    fields . push (integer_field (
      "server-evidence-artifact-count",
      selected . evidence . artifact_count as u64));
    fields . push (integer_field (
      "server-evidence-bytes", selected . evidence . total_file_bytes));
    fields . push (Sexp::List (vec![
      Sexp::Atom (Atom::S ("view-settlements" . into ())),
      Sexp::List (settlements . iter () . map (settlement_sexp) . collect ()),
    ]));
  } else {
    fields . insert (0, atom_field ("status", "archive-ready"));
    fields . push (atom_field (
      "next-action", "origin-specific-operation-required"));
  }
  Ok (Sexp::List (fields) . to_string ())
}

fn settlement_sexp (record : &crate::maintenance::ViewSettlementRecord) -> Sexp {
  Sexp::List (vec![
    atom_field ("buffer-id", &record . buffer_id),
    atom_field ("buffer-key", record . buffer_key . as_deref () . unwrap_or ("none")),
    atom_field ("kind", record . kind . label ()),
    atom_field ("view-uri", record . view_uri . as_deref () . unwrap_or ("none")),
    atom_field ("dirty", if record . dirty { "true" } else { "nil" }),
    atom_field ("impacted", if record . impacted { "true" } else { "nil" }),
    atom_field ("parse-uncertain",
      if record . parse_uncertain { "true" } else { "nil" }),
    atom_field ("uncertainty-reason",
      record . uncertainty_reason . as_deref () . unwrap_or ("none")),
    list_field ("observed-ids", &record . observed_ids),
    list_field ("resolved-primary-ids", &record . resolved_primary_ids),
    integer_field ("base-server-revision", record . base_server_revision),
    integer_field ("base-application-token", record . base_application_token),
    atom_field ("planned-disposition", record . planned_disposition . label ()),
    atom_field ("required-ack", record . requirement . label ()),
  ])
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
  let coordinator = runtime . maintenance . lock () . unwrap () . clone ();
  let payload = match coordinator . state {
    CoordinatorState::Active (active) => Sexp::List (vec![
      atom_field ("status", "active"),
      atom_field ("active-incident-id", active . incident_id . as_str ()),
      integer_field ("maintenance-epoch", active . epoch . get ()),
      atom_field ("phase", &format! ("{:?}", active . phase)),
      atom_field ("origin", active . origin . label ()),
      atom_field ("archive-directory-name", &active . archive_directory_name),
    ]),
    CoordinatorState::Pending (pending) => Sexp::List (vec![
      atom_field ("status", "pending"),
      atom_field ("pending-reason", &format! ("{:?}", pending . reason)),
      atom_field ("candidate-id", pending . candidate . as_ref ()
        . map (|candidate| candidate . id . as_str ()) . unwrap_or ("none")),
    ]),
    CoordinatorState::Terminal (terminal) =>
      sexp::parse (&terminal_payload (&terminal))
        . expect ("terminal payload is valid"),
    other => Sexp::List (vec![
      atom_field ("status", &format! ("{:?}", other)),
    ]),
  } . to_string ();
  send_result (stream, TcpToClient::MaintenanceStatus, "complete", Ok (payload));
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
  let application_token = unsigned_request_field (
    request, "base-application-token")?;
  require_archive_owner (runtime, &incident, epoch)?;
  let all_settled = runtime . transition_maintenance (|coordinator|
    coordinator . acknowledge_view_settlement (
      &incident,
      epoch,
      &buffer_id,
      requirement . clone (),
      view_uri . as_deref (),
      base_revision,
      application_token))?;
  Ok (Sexp::List (vec![
    atom_field ("status", if all_settled {
      "all-views-settled"
    } else {
      "view-settlement-recorded"
    }),
    atom_field ("buffer-id", &buffer_id),
    atom_field ("required-ack", requirement . label ()),
    atom_field ("next-action", if all_settled {
      "finalize-archive"
    } else {
      "settle-remaining-views"
    }),
  ]) . to_string ())
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
  let coordinator = runtime . maintenance . lock ()
    . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
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
  if attached_client (runtime)? . session_id != active . archive_owner_session_id {
    return Err ("maintenance message came from a different client session" . into ()); }
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
    _ => Err (format! (
      "dirty buffers have undo history, but client advertised {} {}",
      client . capabilities . native_undo_kind,
      client . capabilities . native_undo_version)),
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

fn maintenance_offer_payload (
  active                    : &crate::maintenance::ActiveMaintenance,
  archive_folder            : &str,
  archive_server_identity   : &str,
) -> String {
  Sexp::List (vec![
    atom_field ("status", "accepted-lock-and-publish-initial-archive"),
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
    MaintenanceCoordinator,
    ObservationSequence,
    SelectedStoreRecord,
  };
  use crate::types::store_state::{GraphGeneration, ManifestRevision};

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
}
