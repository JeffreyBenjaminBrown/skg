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
use crate::maintenance::selection::select_archived_candidate;
use crate::maintenance::view_impact::plan_incident_view_settlements;
use crate::maintenance::{
  CandidateId,
  BufferKind,
  ClientEvidenceTransferRecord,
  CoordinatorState,
  IncidentId,
  MaintenanceEpoch,
  MaintenanceOrigin,
  MaintenancePhase,
  TerminalDisposition,
  TerminalMaintenance,
  ScalarReleaseRecord,
  ViewApplicationRecord,
  ViewSettlementRecord,
  ViewSettlementRequirement,
};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::{AttachedClient, CensusDescriptor};
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  approved_pids_from_request,
  decide as decide_scalar_release,
};
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
use futures::executor::block_on;
use std::collections::HashSet;
use std::net::TcpStream;

use crate::types::misc::ID;
use crate::types::tree::forest::ViewForest;
use crate::types::views_state::{
  ViewState,
  ViewUri,
  pids_from_viewforest,
};
use crate::update_buffer::render_maintenance_view;

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
  if active . candidate . is_some () {
    select_archived_candidate (runtime, &incident, epoch)?;
    let active = matching_active (runtime, &incident, epoch)?;
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
    match stage_application_settlements (
        runtime, &active, settlements, &HashSet::new ())?
    {
      ApplicationStaging::Ready (settlements) => {
        runtime . transition_maintenance (|coordinator|
          coordinator . record_view_settlements (
            &incident, epoch, settlements . clone ()))?;
        return candidate_selected_payload (
          &active, &verified, &settlements);
      }
      ApplicationStaging::Challenge (challenge) => {
        runtime . transition_maintenance (|coordinator|
          coordinator . record_scalar_challenge (
            &incident, epoch, challenge . clone ()))?;
        return scalar_challenge_payload (&active, &verified, &challenge);
      }
    }
  } else {
    let mut fields = archive_verification_fields (&verified);
    fields . insert (0, atom_field ("status", "archive-ready"));
    fields . push (atom_field (
      "next-action", "origin-specific-operation-required"));
    return Ok (Sexp::List (fields) . to_string ());
  }
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
  mut settlements : Vec<ViewSettlementRecord>,
  approved_pids : &HashSet<ID>,
) -> Result<ApplicationStaging, String> {
  if !settlements . iter () . any (|record|
      record . requirement == ViewSettlementRequirement::ApplicationAck)
  {
    return Ok (ApplicationStaging::Ready (settlements)); }

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

  {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    if interactive . views . diff_mode_enabled != diff_mode_enabled
    || interactive . active_source_set != active_source_set
    || interactive . collateral_scheduler . presentation_generation ()
         != presentation_generation
    {
      return Err (
        "maintenance presentation inputs advanced while views rendered"
          . into ()); }
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
  let frozen = active . buffer_census . get (&record . buffer_id)
    . ok_or_else (|| format! (
      "buffer '{}' is absent from the frozen census", record . buffer_id))?;
  if frozen . dirty
  || record . dirty
  || frozen . view_uri != record . view_uri
  || frozen . kind != record . kind
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
  verified    : &VerifiedInitialArchive,
  settlements : &[ViewSettlementRecord],
) -> Result<String, String> {
  let mut fields = archive_verification_fields (verified);
  fields . insert (0, atom_field ("status", "candidate-selected"));
  append_selected_fields (&mut fields, active)?;
  fields . push (Sexp::List (vec![
    Sexp::Atom (Atom::S ("view-settlements" . into ())),
    Sexp::List (settlements . iter () . map (settlement_sexp) . collect ()),
  ]));
  Ok (Sexp::List (fields) . to_string ())
}

fn scalar_challenge_payload (
  active    : &crate::maintenance::ActiveMaintenance,
  verified  : &VerifiedInitialArchive,
  challenge : &ScalarReleaseRecord,
) -> Result<String, String> {
  let mut fields = archive_verification_fields (verified);
  fields . insert (0, atom_field (
    "status", "needs-scalar-authorization"));
  append_selected_fields (&mut fields, active)?;
  fields . push (atom_field ("operation", &challenge . operation));
  fields . push (list_field ("pids", &challenge . pids));
  fields . push (atom_field ("prompt", &challenge . prompt));
  fields . push (atom_field (
    "next-action", "approve-maintenance-scalar-release"));
  Ok (Sexp::List (fields) . to_string ())
}

fn settlement_sexp (record : &crate::maintenance::ViewSettlementRecord) -> Sexp {
  let mut fields = vec![
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
  if !active . view_settlements . is_empty () {
    let settlements : Vec<ViewSettlementRecord> = active . view_settlements
      . values () . cloned () . collect ();
    return candidate_selected_payload (&active, &verified, &settlements);
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
  runtime . transition_maintenance (|coordinator|
    coordinator . record_view_settlements (
      &incident, epoch, settlements . clone ()))?;
  candidate_selected_payload (&active, &verified, &settlements)
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
    CoordinatorState::Active (active) => active_status_sexp (&active),
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

fn active_status_sexp (
  active : &crate::maintenance::ActiveMaintenance,
) -> Sexp {
  let mut fields = vec![
    atom_field ("status", "active"),
    atom_field ("active-incident-id", active . incident_id . as_str ()),
    integer_field ("maintenance-epoch", active . epoch . get ()),
    atom_field ("phase", &format! ("{:?}", active . phase)),
    atom_field ("origin", active . origin . label ()),
    atom_field ("archive-directory-name", &active . archive_directory_name),
  ];
  if active . selected_store . is_some () {
    let _ = append_selected_fields (&mut fields, active);
  }
  if let Some (scalar) = &active . scalar_release {
    fields . push (atom_field ("operation", &scalar . operation));
    fields . push (list_field ("pids", &scalar . pids));
    fields . push (atom_field ("prompt", &scalar . prompt));
    fields . push (atom_field (
      "scalar-approved", if scalar . approved { "true" } else { "nil" }));
  }
  if !active . view_settlements . is_empty () {
    fields . push (Sexp::List (vec![
      Sexp::Atom (Atom::S ("view-settlements" . into ())),
      Sexp::List (active . view_settlements . values ()
        . map (settlement_sexp) . collect ()),
    ]));
  }
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
  let application_token = unsigned_request_field (
    request, "base-application-token")?;
  let active = require_archive_owner (runtime, &incident, epoch)?;
  let record = active . view_settlements . get (&buffer_id)
    . ok_or_else (|| format! (
      "buffer '{}' has no planned settlement", buffer_id))?;
  let effect = {
    let interactive = runtime . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let state = record . view_uri . as_ref () . and_then (|uri|
      interactive . views . open_views . views . get (
        &ViewUri::from_client_string (uri . clone ())));
    prepare_server_settlement_effect (&active, record, state)?
  };
  let all_settled = runtime . transition_maintenance (|coordinator|
    coordinator . acknowledge_view_settlement (
      &incident,
      epoch,
      &buffer_id,
      requirement . clone (),
      view_uri . as_deref (),
      base_revision,
      application_token))?;
  apply_server_settlement_effect (runtime, effect);
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

#[derive(Clone, Debug, Eq, PartialEq)]
enum ServerSettlementEffect {
  None,
  Unregister (ViewUri),
  Preserve {
    uri              : ViewUri,
    graph_generation : u64,
    search_stale     : bool,
  },
}

fn prepare_server_settlement_effect (
  active : &crate::maintenance::ActiveMaintenance,
  record : &ViewSettlementRecord,
  state  : Option<&ViewState>,
) -> Result<ServerSettlementEffect, String> {
  if record . acknowledged { return Ok (ServerSettlementEffect::None); }
  if record . requirement == ViewSettlementRequirement::ApplicationAck {
    return Err (format! (
      "buffer '{}' cannot ACK application before its staged rendering",
      record . buffer_id)); }
  let uri = record . view_uri . as_ref ()
    . map (|uri| ViewUri::from_client_string (uri . clone ()));
  if let Some (state) = state {
    let frozen = active . buffer_census . get (&record . buffer_id)
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
    FrozenBufferRecord,
    MaintenanceCoordinator,
    ObservationSequence,
    SelectedStoreRecord,
    ServerEvidenceRecord,
    ViewDisposition,
  };
  use crate::types::tree::forest::ViewForest;
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

  #[test]
  fn nonrendered_settlement_effects_require_exact_old_authority () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let mut active = coordinator . begin_with_archive_contract (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![FrozenBufferRecord {
        buffer_id: "buffer" . into (), kind: BufferKind::SearchView,
        view_uri: Some ("search:terms" . into ()), graph_generation: 1,
        presentation_generation: 3, server_revision: 4,
        application_token: 7, dirty: false, undo_required: false,
        last_fetched_sha256: "a" . repeat (64),
        current_sha256: "a" . repeat (64),
      }]) . unwrap ();
    active . selected_store = Some (SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 2, tantivy_outcome: "committed" . into (),
    });
    let record = ViewSettlementRecord {
      buffer_id: "buffer" . into (), buffer_key: None,
      kind: BufferKind::SearchView, view_uri: Some ("search:terms" . into ()),
      dirty: false, impacted: false, parse_uncertain: false,
      uncertainty_reason: None, observed_ids: Vec::new (),
      resolved_primary_ids: Vec::new (), base_server_revision: 4,
      base_application_token: 7,
      planned_disposition: ViewDisposition::RetainedClean,
      requirement: ViewSettlementRequirement::ReleaseAck,
      application: None,
      acknowledged: false,
    };
    let state = ViewState {
      viewforest: ViewForest::new (), pids: Default::default (), revision: 4,
      graph_generation: 1, presentation_generation: 3,
      client_application_token: 7, client_buffer_id: Some ("buffer" . into ()),
      kind: BufferKind::SearchView, recipe: Some ("terms" . into ()),
      presentation_stale: false, search_stale: false,
    };
    assert_eq! (prepare_server_settlement_effect (
      &active, &record, Some (&state)) . unwrap (),
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
      &active, &record, Some (&changed)) . is_err ());
    let mut application = record . clone ();
    application . requirement = ViewSettlementRequirement::ApplicationAck;
    assert! (prepare_server_settlement_effect (
      &active, &application, Some (&changed)) . is_err ());
  }

  #[test]
  fn staged_application_wire_names_text_and_resulting_authority () {
    let record = ViewSettlementRecord {
      buffer_id: "buffer" . into (), buffer_key: None,
      kind: BufferKind::ContentView, view_uri: Some ("view" . into ()),
      dirty: false, impacted: true, parse_uncertain: false,
      uncertainty_reason: None, observed_ids: vec!["node" . into ()],
      resolved_primary_ids: vec!["node" . into ()],
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
    let payload = scalar_challenge_payload (
      &active, &verified, &challenge) . unwrap ();
    assert! (payload . contains ("needs-scalar-authorization"));
    assert! (payload . contains ("ugly-pid"));
    assert! (!payload . contains ("view-settlements"));
    assert! (!payload . contains ("SECRET-STAGED-TEXT"));
  }
}
