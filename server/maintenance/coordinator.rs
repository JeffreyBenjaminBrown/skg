use super::types::*;
use crate::types::store_state::{GraphGeneration, ManifestRevision};

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;
use std::time::{SystemTime, UNIX_EPOCH};

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct MaintenanceCoordinator {
  pub epoch                : MaintenanceEpoch,
  pub observation_sequence : ObservationSequence,
  pub state                : CoordinatorState,
}

impl MaintenanceCoordinator {
  pub fn new () -> Self {
    Self {
      epoch: MaintenanceEpoch::INITIAL,
      observation_sequence: ObservationSequence::INITIAL,
      state: CoordinatorState::Idle,
    }
  }

  pub fn next_observation_sequence (&mut self) -> ObservationSequence {
    self . observation_sequence = self . observation_sequence . successor ();
    self . observation_sequence
  }

  pub fn observation_started (&mut self) -> Result<(), String> {
    match self . state {
      CoordinatorState::Idle | CoordinatorState::Observing => {
        self . state = CoordinatorState::Observing;
        Ok (( )) }
      CoordinatorState::Pending (_) | CoordinatorState::Active (_)
      | CoordinatorState::Terminal (_)
      | CoordinatorState::BlockedStoreHealth { .. } =>
        Err ("observation may continue in the background but cannot replace the current coordinator state" . into ()),
    }
  }

  pub fn observation_equal (&mut self) -> Result<(), String> {
    match self . state {
      CoordinatorState::Observing | CoordinatorState::Pending (_) => {
        self . state = CoordinatorState::Idle;
        Ok (( )) }
      _ => Err ("no replaceable observation is active" . into ()),
    }
  }

  pub fn set_pending_valid (
    &mut self,
    candidate : CandidateSummary,
  ) -> Result<(), String> {
    match self . state {
      CoordinatorState::Idle
      | CoordinatorState::Observing
      | CoordinatorState::Pending (_) => {
        self . state = CoordinatorState::Pending (PendingDiskState {
          reason: PendingReason::ValidDiskDifference,
          candidate: Some (candidate),
          details: Vec::new (),
          offer_sent: false,
        });
        Ok (( )) }
      _ => Err ("active maintenance retains new observation as successor work" . into ()),
    }
  }

  pub fn set_pending_invalid (
    &mut self,
    reason  : PendingReason,
    details : Vec<String>,
  ) -> Result<(), String> {
    if !matches! (reason,
      PendingReason::InvalidDisk
      | PendingReason::UnstableDisk
      | PendingReason::ObservationFailure)
    {
      return Err ("pending invalid state requires an invalid, unstable, or infrastructure reason" . into ()); }
    match self . state {
      CoordinatorState::Idle
      | CoordinatorState::Observing
      | CoordinatorState::Pending (_) => {
        self . state = CoordinatorState::Pending (PendingDiskState {
          reason, candidate: None, details, offer_sent: false,
        });
        Ok (( )) }
      _ => Err ("active maintenance retains new observation as successor work" . into ()),
    }
  }

  pub fn mark_offer_sent (&mut self) -> Result<(), String> {
    let CoordinatorState::Pending (pending) = &mut self . state else {
      return Err ("there is no pending disk offer" . into ()); };
    pending . offer_sent = true;
    Ok (( ))
  }

  pub fn begin (
    &mut self,
    origin    : MaintenanceOrigin,
    candidate : Option<CandidateSummary>,
  ) -> Result<ActiveMaintenance, String> {
    self . begin_for_client (origin, candidate, "test-client" . into ())
  }

  pub fn begin_for_client (
    &mut self,
    origin       : MaintenanceOrigin,
    candidate    : Option<CandidateSummary>,
    client_session_id : String,
  ) -> Result<ActiveMaintenance, String> {
    let graph_generation = candidate . as_ref ()
      . map (|candidate| candidate . base_graph_generation)
      . unwrap_or (GraphGeneration::INITIAL);
    let manifest_revision = candidate . as_ref ()
      . map (|candidate| candidate . base_manifest_revision)
      . unwrap_or (ManifestRevision::INITIAL);
    self . begin_with_archive_contract (
      origin, candidate, client_session_id, "emacs" . into (), "all" . into (),
      graph_generation, manifest_revision, Vec::new ())
  }

  #[allow(clippy::too_many_arguments)]
  pub fn begin_with_archive_contract (
    &mut self,
    origin       : MaintenanceOrigin,
    candidate    : Option<CandidateSummary>,
    client_session_id : String,
    client_kind  : String,
    source_set   : String,
    g0_graph_generation : GraphGeneration,
    g0_manifest_revision : ManifestRevision,
    buffer_records : Vec<FrozenBufferRecord>,
  ) -> Result<ActiveMaintenance, String> {
    if client_session_id . is_empty () {
      return Err ("maintenance requires an owning client session" . into ()); }
    if !matches! (client_kind . as_str (), "emacs" | "neovim") {
      return Err ("maintenance requires a supported archive client" . into ()); }
    if source_set . is_empty () {
      return Err ("maintenance requires an active source-set" . into ()); }
    let mut buffer_census = std::collections::BTreeMap::new ();
    for record in buffer_records {
      if record . buffer_id . is_empty () {
        return Err ("maintenance census has an empty buffer ID" . into ()); }
      if record . undo_required && !record . dirty {
        return Err ("undo-required buffer is absent from the dirty census" . into ()); }
      let id = record . buffer_id . clone ();
      if buffer_census . insert (id . clone (), record) . is_some () {
        return Err (format! (
          "maintenance buffer census repeats '{}'", id)); }
    }
    let mut registered_buffer_ids : Vec<String> =
      buffer_census . keys () . cloned () . collect ();
    let mut dirty_buffer_ids : Vec<String> = buffer_census . values ()
      . filter (|record| record . dirty)
      . map (|record| record . buffer_id . clone ()) . collect ();
    let mut undo_required_buffer_ids : Vec<String> = buffer_census . values ()
      . filter (|record| record . undo_required)
      . map (|record| record . buffer_id . clone ()) . collect ();
    registered_buffer_ids . sort ();
    dirty_buffer_ids . sort ();
    undo_required_buffer_ids . sort ();
    undo_required_buffer_ids . dedup ();
    match &self . state {
      CoordinatorState::Idle => {}
      CoordinatorState::Pending (pending) => {
        match (pending . candidate . as_ref (), candidate . as_ref ()) {
          (Some (expected), Some (actual)) if expected . id == actual . id => {}
          (Some (expected), Some (actual)) => return Err (format! (
            "candidate {} is stale; current candidate is {}",
            actual . id, expected . id)),
          (Some (expected), None) => return Err (format! (
            "pending candidate {} must be named explicitly", expected . id)),
          (None, Some (actual)) => return Err (format! (
            "candidate {} is not the pending invalid disk state", actual . id)),
          (None, None) => {}
        }
      }
      CoordinatorState::Observing =>
        return Err ("wait for the current observation to finish" . into ()),
      CoordinatorState::Active (active) =>
        return Err (format! (
          "maintenance incident {} is already {:?}",
          active . incident_id, active . phase)),
      CoordinatorState::Terminal (terminal) =>
        return Err (format! (
          "maintenance incident {} is terminal and awaits acknowledgement",
          terminal . incident_id)),
      CoordinatorState::BlockedStoreHealth { reason } =>
        return Err (format! ("stores require repair: {}", reason)),
    }
    self . epoch = self . epoch . successor ();
    let incident_id = IncidentId::new ();
    let (started_at_utc, timestamp) = utc_incident_timestamp ();
    let active = ActiveMaintenance {
      archive_directory_name: format! ("{}_{}", timestamp, incident_id),
      incident_id,
      epoch: self . epoch,
      origin,
      phase: MaintenancePhase::PreparingArchive,
      candidate,
      archive_status: ArchiveStatus::Preparing,
      started_at_utc,
      archive_owner_session_id: client_session_id,
      archive_owner_client_kind: client_kind,
      source_set,
      g0_graph_generation,
      g0_manifest_revision,
      registered_buffer_ids,
      dirty_buffer_ids,
      undo_required_buffer_ids,
      buffer_census,
      undo_waivers: Default::default (),
      server_evidence: None,
      client_evidence_transfer: None,
      client_evidence_acknowledged: false,
      selected_store: None,
      scalar_release: None,
      view_settlements: Default::default (),
      blocking_reason: None,
      suspended_phase: None,
      client_connected: true,
      terminal: None,
    };
    self . state = CoordinatorState::Active (active . clone ());
    Ok (active)
  }

  pub fn transition (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    next        : MaintenancePhase,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !allowed_phase_transition (&active . phase, &next) {
      return Err (format! (
        "maintenance phase {:?} cannot transition to {:?}",
        active . phase, next)); }
    active . phase = next;
    Ok (( ))
  }

  pub fn archive_ready (
    &mut self,
    incident_id    : &IncidentId,
    epoch          : MaintenanceEpoch,
    manifest_sha256 : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::PreparingArchive
      | MaintenancePhase::AwaitingArchiveWaiver)
    {
      return Err (format! (
        "archive-ready is invalid during {:?}", active . phase)); }
    active . archive_status = ArchiveStatus::Ready { manifest_sha256 };
    active . phase = MaintenancePhase::ArchiveReady;
    Ok (( ))
  }

  pub fn archive_undo_failed (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    buffer_key  : String,
    reason      : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::PreparingArchive
      | MaintenancePhase::AwaitingArchiveWaiver)
    {
      return Err (format! (
        "undo failure is invalid during {:?}", active . phase)); }
    if buffer_key . is_empty () || reason . is_empty () {
      return Err ("undo failure requires a buffer key and reason" . into ()); }
    active . archive_status = ArchiveStatus::UndoFailed {
      buffer_key, reason };
    active . phase = MaintenancePhase::AwaitingArchiveWaiver;
    Ok (( ))
  }

  pub fn approve_undo_waiver (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    buffer_key  : String,
    reason      : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    let ArchiveStatus::UndoFailed {
      buffer_key: failed_key, reason: failed_reason,
    } = &active . archive_status else {
      return Err ("there is no exact undo failure awaiting waiver" . into ()); };
    if &buffer_key != failed_key || &reason != failed_reason {
      return Err ("undo waiver does not match the recorded failure" . into ()); }
    active . undo_waivers . insert (buffer_key . clone (), reason . clone ());
    active . archive_status = ArchiveStatus::UndoWaiverApproved {
      buffer_key, reason };
    Ok (( ))
  }

  pub fn archive_finalized (
    &mut self,
    incident_id    : &IncidentId,
    epoch          : MaintenanceEpoch,
    manifest_sha256 : String,
    transfer_manifest_sha256 : String,
    artifact_bytes_sha256 : String,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::FinalizingArchive {
      return Err (format! (
        "archive-finalized is invalid during {:?}", active . phase)); }
    let settlement_ids : BTreeSet<_> = active . view_settlements
      . keys () . cloned () . collect ();
    let registered_ids : BTreeSet<_> = active . registered_buffer_ids
      . iter () . cloned () . collect ();
    if settlement_ids != registered_ids
    || active . view_settlements . values ()
      . any (|record| !record . acknowledged)
    {
      return Err (
        "archive cannot finalize before every registered view is settled"
          . into ()); }
    let transfer = active . client_evidence_transfer . as_ref ()
      . ok_or_else (||
        "archive cannot finalize before evidence transfer" . to_string ())?;
    if transfer . transfer_manifest_sha256 != transfer_manifest_sha256
    || transfer . artifact_bytes_sha256 != artifact_bytes_sha256
    {
      return Err (
        "archive finalization ACK changed the journaled evidence transfer"
          . into ()); }
    if manifest_sha256 . is_empty () {
      return Err ("archive finalization ACK has no manifest checksum" . into ()); }
    if let ArchiveStatus::Finalized {
      manifest_sha256: existing,
    } = &active . archive_status
    {
      if existing != &manifest_sha256 {
        return Err (
          "incident already records a different final manifest" . into ()); }
      active . client_evidence_acknowledged = true;
      return Ok (false); }
    active . archive_status = ArchiveStatus::Finalized { manifest_sha256 };
    active . client_evidence_acknowledged = true;
    Ok (true)
  }

  pub fn record_server_evidence (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    evidence    : ServerEvidenceRecord,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::ArchiveReady {
      return Err (format! (
        "server evidence is invalid during {:?}", active . phase)); }
    if let Some (existing) = &active . server_evidence {
      if existing != &evidence {
        return Err ("incident already records different server evidence" . into ()); }
    } else {
      active . server_evidence = Some (evidence); }
    Ok (( ))
  }

  pub fn store_selected (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    selected    : SelectedStoreRecord,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::SelectingPartial {
      return Err (format! (
        "store selection completion is invalid during {:?}", active . phase)); }
    if active . server_evidence . is_none () {
      return Err ("store selection has no durable server evidence" . into ()); }
    active . selected_store = Some (selected);
    active . blocking_reason = None;
    active . phase = MaintenancePhase::Presenting;
    Ok (( ))
  }

  pub fn record_client_evidence_transfer (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    transfer    : ClientEvidenceTransferRecord,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::Presenting | MaintenancePhase::FinalizingArchive)
    {
      return Err (format! (
        "client evidence is invalid during {:?}", active . phase)); }
    let server_evidence = active . server_evidence . as_ref ()
      . ok_or_else (|| "client evidence has no durable server bundle"
        . to_string ())?;
    if active . selected_store . is_none () {
      return Err ("client evidence cannot precede store selection" . into ()); }
    if transfer . server_bundle_sha256 != server_evidence . bundle_sha256 {
      return Err ("client evidence names another server bundle" . into ()); }
    if transfer . artifact_count != server_evidence . artifact_count {
      return Err ("client evidence artifact inventory changed" . into ()); }
    if let Some (existing) = &active . client_evidence_transfer {
      if existing != &transfer {
        return Err ("incident already offered different client evidence"
          . into ()); }
    } else {
      active . client_evidence_transfer = Some (transfer); }
    Ok (( ))
  }

  pub fn record_view_settlements (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    settlements : Vec<ViewSettlementRecord>,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::Presenting {
      return Err (format! (
        "view classification is invalid during {:?}", active . phase)); }
    let mut records = std::collections::BTreeMap::new ();
    for record in settlements {
      if record . buffer_id . is_empty () {
        return Err ("view settlement has an empty buffer ID" . into ()); }
      let id = record . buffer_id . clone ();
      if records . insert (id . clone (), record) . is_some () {
        return Err (format! ("view settlement repeats buffer '{}'", id)); }
    }
    let expected : BTreeSet<_> = active . registered_buffer_ids
      . iter () . cloned () . collect ();
    let actual : BTreeSet<_> = records . keys () . cloned () . collect ();
    if actual != expected {
      return Err (format! (
        "view settlement inventory is {:?}, expected {:?}", actual, expected)); }
    if active . view_settlements . is_empty () {
      active . view_settlements = records;
    } else if active . view_settlements != records {
      return Err ("incident already records different view settlements" . into ()); }
    if active . view_settlements . is_empty () {
      active . phase = MaintenancePhase::FinalizingArchive; }
    Ok (( ))
  }

  pub fn record_scalar_challenge (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    mut challenge : ScalarReleaseRecord,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::Presenting {
      return Err (format! (
        "scalar challenge is invalid during {:?}", active . phase)); }
    if !active . view_settlements . is_empty () {
      return Err ("scalar challenge cannot replace planned view settlements"
        . into ()); }
    challenge . pids . sort ();
    challenge . pids . dedup ();
    if challenge . operation . is_empty ()
    || challenge . pids . is_empty ()
    || challenge . prompt . is_empty ()
    || challenge . approved
    {
      return Err ("scalar challenge is incomplete or already approved"
        . into ()); }
    if let Some (existing) = &active . scalar_release {
      if existing != &challenge {
        return Err ("incident already records a different scalar challenge"
          . into ()); }
      active . phase = MaintenancePhase::AwaitingScalarAuthorization;
      return Ok (false); }
    active . scalar_release = Some (challenge);
    active . phase = MaintenancePhase::AwaitingScalarAuthorization;
    Ok (true)
  }

  pub fn approve_scalar_release (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    mut pids    : Vec<String>,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    pids . sort ();
    pids . dedup ();
    let challenge = active . scalar_release . as_mut ()
      . ok_or_else (|| "incident has no scalar challenge" . to_string ())?;
    if challenge . pids != pids {
      return Err ("scalar approval does not match the exact challenged PIDs"
        . into ()); }
    if challenge . approved {
      if active . phase != MaintenancePhase::Presenting {
        return Err ("approved scalar challenge has an invalid phase"
          . into ()); }
      return Ok (false); }
    if active . phase != MaintenancePhase::AwaitingScalarAuthorization {
      return Err (format! (
        "scalar approval is invalid during {:?}", active . phase)); }
    challenge . approved = true;
    active . phase = MaintenancePhase::Presenting;
    Ok (true)
  }

  #[allow(clippy::too_many_arguments)]
  pub fn acknowledge_view_settlement (
    &mut self,
    incident_id      : &IncidentId,
    epoch            : MaintenanceEpoch,
    buffer_id        : &str,
    requirement      : ViewSettlementRequirement,
    view_uri         : Option<&str>,
    base_revision    : u64,
    application_token : u64,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::Presenting | MaintenancePhase::FinalizingArchive)
    {
      return Err (format! (
        "view settlement ACK is invalid during {:?}", active . phase)); }
    let record = active . view_settlements . get_mut (buffer_id)
      . ok_or_else (|| format! (
        "buffer '{}' has no planned settlement", buffer_id))?;
    if record . requirement != requirement
    || record . view_uri . as_deref () != view_uri
    || record . base_server_revision != base_revision
    || record . base_application_token != application_token
    {
      return Err (format! (
        "buffer '{}' settlement ACK changed its frozen authority", buffer_id)); }
    if record . acknowledged {
      return Ok (active . view_settlements . values ()
        . all (|record| record . acknowledged)); }
    if active . phase == MaintenancePhase::FinalizingArchive {
      return Err (
        "finalizing archive contains an unacknowledged view settlement"
          . into ()); }
    record . acknowledged = true;
    let complete = active . view_settlements . values ()
      . all (|record| record . acknowledged);
    if complete { active . phase = MaintenancePhase::FinalizingArchive; }
    Ok (complete)
  }

  pub fn block_store_health (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    reason      : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    active . blocking_reason = Some (reason);
    active . phase = MaintenancePhase::BlockedStoreHealth;
    Ok (( ))
  }

  pub fn selection_superseded (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    reason      : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::ArchiveReady | MaintenancePhase::SelectingPartial)
    {
      return Err (format! (
        "candidate supersession is invalid during {:?}", active . phase)); }
    active . blocking_reason = Some (reason);
    active . phase = MaintenancePhase::FinalObservation;
    Ok (( ))
  }

  pub fn cancel_before_archive (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)? . clone ();
    if !matches! (active . phase,
      MaintenancePhase::PreparingArchive
      | MaintenancePhase::AwaitingArchiveWaiver)
    {
      return Err (format! (
        "maintenance cannot be cancelled without finalizing its ready archive during {:?}",
        active . phase)); }
    self . state = if let Some (candidate) = active . candidate {
      CoordinatorState::Pending (PendingDiskState {
        reason: PendingReason::ValidDiskDifference,
        candidate: Some (candidate),
        details: vec!["maintenance was cancelled before archive publication" . into ()],
        offer_sent: true,
      })
    } else { CoordinatorState::Idle };
    Ok (( ))
  }

  pub fn disconnected (&mut self) {
    if let CoordinatorState::Active (active) = &mut self . state {
      active . client_connected = false;
      if !matches! (active . phase,
        MaintenancePhase::RunningExternalMutation
        | MaintenancePhase::BlockedInvalidAfterMutation
        | MaintenancePhase::BlockedStoreHealth)
      {
        if active . phase != MaintenancePhase::AwaitingClient {
          active . suspended_phase = Some (active . phase . clone ());
          active . phase = MaintenancePhase::AwaitingClient; }}}
  }

  pub fn reconnected (&mut self) {
    if let CoordinatorState::Active (active) = &mut self . state {
      active . client_connected = true;
      if active . phase == MaintenancePhase::AwaitingClient {
        if let Some (phase) = active . suspended_phase . take () {
          active . phase = phase; }}}
  }

  pub fn finish (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    disposition : TerminalDisposition,
  ) -> Result<TerminalMaintenance, String> {
    if let CoordinatorState::Terminal (terminal) = &self . state {
      if &terminal . incident_id != incident_id || terminal . epoch != epoch {
        return Err ("terminal maintenance identity changed" . into ()); }
      if terminal . disposition != disposition {
        return Err ("terminal maintenance disposition changed" . into ()); }
      return Ok (terminal . clone ()); }
    let active = self . matching_active_mut (incident_id, epoch)?;
    if disposition == TerminalDisposition::Completed
    && (!matches! (active . archive_status, ArchiveStatus::Finalized { .. })
        || !active . client_evidence_acknowledged)
    {
      return Err (
        "completed maintenance requires an acknowledged finalized archive"
          . into ()); }
    let manifest_sha256 = match &active . archive_status {
      ArchiveStatus::Finalized { manifest_sha256 } =>
        Some (manifest_sha256 . clone ()),
      _ => None,
    };
    let terminal = TerminalMaintenance {
      incident_id: active . incident_id . clone (),
      epoch: active . epoch,
      disposition,
      archive_owner_session_id: active . archive_owner_session_id . clone (),
      archive_manifest_sha256: manifest_sha256,
      registered_buffer_ids: active . registered_buffer_ids . clone (),
      selected_store: active . selected_store . clone (),
    };
    self . state = CoordinatorState::Terminal (terminal . clone ());
    Ok (terminal)
  }

  pub fn acknowledge_terminal (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<bool, String> {
    match &self . state {
      CoordinatorState::Idle => Ok (false),
      CoordinatorState::Terminal (terminal) => {
        if &terminal . incident_id != incident_id || terminal . epoch != epoch {
          return Err ("terminal acknowledgement names another incident"
            . into ()); }
        self . state = CoordinatorState::Idle;
        Ok (true)
      }
      _ => Err ("maintenance is not ready for terminal acknowledgement"
        . into ()),
    }
  }

  fn matching_active_mut (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<&mut ActiveMaintenance, String> {
    let CoordinatorState::Active (active) = &mut self . state else {
      return Err ("no maintenance incident is active" . into ()); };
    if &active . incident_id != incident_id {
      return Err (format! (
        "stale incident {}; current incident is {}",
        incident_id, active . incident_id)); }
    if active . epoch != epoch {
      return Err (format! (
        "stale maintenance epoch {}; current epoch is {}",
        epoch . get (), active . epoch . get ())); }
    Ok (active)
  }
}

fn utc_incident_timestamp () -> (String, String) {
  let duration = SystemTime::now () . duration_since (UNIX_EPOCH)
    . unwrap_or_default ();
  let seconds = duration . as_secs ();
  let micros = duration . subsec_micros ();
  let days = seconds / 86_400;
  let within_day = seconds % 86_400;
  let hour = within_day / 3_600;
  let minute = (within_day % 3_600) / 60;
  let second = within_day % 60;
  let (year, month, day) = civil_date_from_epoch_days (days as i64);
  (
    format! (
      "{:04}-{:02}-{:02}T{:02}:{:02}:{:02}.{:06}Z",
      year, month, day, hour, minute, second, micros),
    format! (
      "{:04}{:02}{:02}T{:02}{:02}{:02}.{:06}Z",
      year, month, day, hour, minute, second, micros),
  )
}

fn civil_date_from_epoch_days (days : i64) -> (i64, i64, i64) {
  let days_from_2000_03_01 = days - 11_017;
  let mut year = 2000;
  let mut remainder = days_from_2000_03_01;
  let cycles_400 = remainder . div_euclid (146_097);
  year += 400 * cycles_400;
  remainder -= 146_097 * cycles_400;
  let cycles_100 = (remainder / 36_524) . min (3);
  year += 100 * cycles_100;
  remainder -= 36_524 * cycles_100;
  let cycles_4 = remainder / 1_461;
  year += 4 * cycles_4;
  remainder -= 1_461 * cycles_4;
  let years = (remainder / 365) . min (3);
  year += years;
  remainder -= 365 * years;
  let month_lengths = [31,30,31,30,31,31,30,31,30,31,31,29];
  let mut march_month = 0usize;
  while march_month < 11 && remainder >= month_lengths[march_month] {
    remainder -= month_lengths[march_month];
    march_month += 1; }
  let month = ((march_month as i64 + 2) % 12) + 1;
  let year = if month <= 2 { year + 1 } else { year };
  (year, month, remainder + 1)
}

fn allowed_phase_transition (
  current : &MaintenancePhase,
  next    : &MaintenancePhase,
) -> bool {
  use MaintenancePhase::*;
  matches! ((current, next),
    (ArchiveReady, RunningExternalMutation)
    | (ArchiveReady, FinalObservation)
    | (ArchiveReady, SelectingPartial)
    | (ArchiveReady, FullRebuildExclusive)
    | (RunningExternalMutation, FinalObservation)
    | (FinalObservation, SelectingPartial)
    | (SelectingPartial, Presenting)
    | (FullRebuildExclusive, Presenting)
    | (Presenting, AwaitingScalarAuthorization)
    | (AwaitingScalarAuthorization, Presenting)
    | (Presenting, FinalizingArchive)
    | (_, BlockedInvalidAfterMutation)
    | (_, BlockedStoreHealth))
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::store_state::{GraphGeneration, ManifestRevision};

  fn candidate () -> CandidateSummary {
    CandidateSummary {
      id: CandidateId::new (),
      base_graph_generation: GraphGeneration::INITIAL,
      base_manifest_revision: ManifestRevision::INITIAL,
      covered_sequence: ObservationSequence::INITIAL,
      changed_primary_ids: vec!["A" . into ()],
    }
  }

  #[test]
  fn pending_disk_blocks_saves_but_not_editing_or_queries () {
    let mut coordinator = MaintenanceCoordinator::new ();
    coordinator . set_pending_valid (candidate ()) . unwrap ();
    let policy = coordinator . state . policy ();
    assert! (policy . edits_allowed);
    assert! (policy . queries_allowed);
    assert! (!policy . skg_saves_allowed);
  }

  #[test]
  fn stale_candidate_cannot_begin_maintenance () {
    let mut coordinator = MaintenanceCoordinator::new ();
    coordinator . set_pending_valid (candidate ()) . unwrap ();
    let error = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation, Some (candidate ()))
      . unwrap_err ();
    assert! (error . contains ("stale"), "{}", error);
    assert! (matches! (coordinator . state, CoordinatorState::Pending (_)));
  }

  #[test]
  fn happy_path_requires_exact_incident_epoch_and_final_archive () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "server" . into (),
        artifact_count: 1, total_file_bytes: 2,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    coordinator . store_selected (
      &active . incident_id, active . epoch,
      SelectedStoreRecord {
        graph_generation: GraphGeneration::INITIAL . successor (),
        manifest_revision: ManifestRevision::INITIAL . successor (),
        tantivy_generation: 1, tantivy_outcome: "committed" . into (),
      }) . unwrap ();
    coordinator . record_client_evidence_transfer (
      &active . incident_id, active . epoch, ClientEvidenceTransferRecord {
        server_bundle_sha256: "server" . into (),
        transfer_manifest_sha256: "transfer" . into (),
        artifact_bytes_sha256: "bytes" . into (),
        artifact_count: 1, artifact_bytes: 2,
      }) . unwrap ();
    coordinator . record_view_settlements (
      &active . incident_id, active . epoch, Vec::new ()) . unwrap ();
    assert! (coordinator . finish (
      &active . incident_id, active . epoch,
      TerminalDisposition::Completed) . is_err ());
    coordinator . archive_finalized (
      &active . incident_id, active . epoch, "final" . into (),
      "transfer" . into (), "bytes" . into ()) . unwrap ();
    let terminal = coordinator . finish (
      &active . incident_id, active . epoch,
      TerminalDisposition::Completed) . unwrap ();
    assert_eq! (terminal . archive_manifest_sha256,
      Some ("final" . into ()));
    assert_eq! (coordinator . finish (
      &active . incident_id, active . epoch,
      TerminalDisposition::Completed) . unwrap (), terminal);
    assert! (matches! (
      coordinator . state, CoordinatorState::Terminal (_)));
    assert! (coordinator . state . policy () . skg_saves_allowed);
    assert! (coordinator . acknowledge_terminal (
      &active . incident_id, active . epoch) . unwrap ());
    assert_eq! (coordinator . state, CoordinatorState::Idle);
    assert! (!coordinator . acknowledge_terminal (
      &active . incident_id, active . epoch) . unwrap ());
  }

  #[test]
  fn pre_archive_failure_terminal_claims_no_archive_and_can_unlock () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    let terminal = coordinator . finish (
      &active . incident_id, active . epoch,
      TerminalDisposition::FailedBeforeArchive) . unwrap ();
    assert_eq! (terminal . archive_manifest_sha256, None);
    assert! (!coordinator . state . policy () . maintenance_locked);
    assert! (coordinator . state . policy () . skg_saves_allowed);
  }

  #[test]
  fn scalar_release_requires_the_exact_durable_challenge () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "server" . into (),
        artifact_count: 1, total_file_bytes: 2,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    coordinator . store_selected (
      &active . incident_id, active . epoch, SelectedStoreRecord {
        graph_generation: GraphGeneration::INITIAL . successor (),
        manifest_revision: ManifestRevision::INITIAL . successor (),
        tantivy_generation: 1, tantivy_outcome: "committed" . into (),
      }) . unwrap ();
    let challenge = ScalarReleaseRecord {
      operation: "maintenance-presentation" . into (),
      pids: vec!["b" . into (), "a" . into ()],
      prompt: "Release protected text?" . into (),
      approved: false,
    };
    assert! (coordinator . record_scalar_challenge (
      &active . incident_id, active . epoch, challenge . clone ())
      . unwrap ());
    let CoordinatorState::Active (retained) = &coordinator . state else {
      panic! ("challenge discarded incident"); };
    assert_eq! (retained . phase,
      MaintenancePhase::AwaitingScalarAuthorization);
    assert_eq! (retained . scalar_release . as_ref () . unwrap () . pids,
      vec!["a" . to_string (), "b" . to_string ()]);
    assert! (coordinator . approve_scalar_release (
      &active . incident_id, active . epoch, vec!["a" . into ()]) . is_err ());
    assert! (coordinator . approve_scalar_release (
      &active . incident_id, active . epoch,
      vec!["b" . into (), "a" . into ()]) . unwrap ());
    assert! (!coordinator . approve_scalar_release (
      &active . incident_id, active . epoch,
      vec!["a" . into (), "b" . into ()]) . unwrap ());
  }

  #[test]
  fn disconnect_keeps_the_incident_and_lock_policy () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    coordinator . disconnected ();
    let CoordinatorState::Active (retained) = &coordinator . state else {
      panic! ("disconnect discarded incident"); };
    assert_eq! (retained . incident_id, active . incident_id);
    assert_eq! (retained . phase, MaintenancePhase::AwaitingClient);
    assert! (coordinator . state . policy () . maintenance_locked);
  }

  #[test]
  fn reconnect_restores_the_exact_suspended_phase () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    coordinator . disconnected ();
    coordinator . reconnected ();
    let CoordinatorState::Active (retained) = &coordinator . state else {
      panic! ("reconnect discarded incident"); };
    assert_eq! (retained . phase, MaintenancePhase::SelectingPartial);
    assert_eq! (retained . suspended_phase, None);
    assert! (retained . client_connected);
  }

  #[test]
  fn undo_waiver_must_match_the_recorded_failure_exactly () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_undo_failed (
      &active . incident_id, active . epoch,
      "view-1_deadbeef" . into (), "round trip failed" . into ())
      . unwrap ();
    assert! (coordinator . approve_undo_waiver (
      &active . incident_id, active . epoch,
      "view-1_deadbeef" . into (), "different" . into ()) . is_err ());
    coordinator . approve_undo_waiver (
      &active . incident_id, active . epoch,
      "view-1_deadbeef" . into (), "round trip failed" . into ())
      . unwrap ();
    let CoordinatorState::Active (retained) = &coordinator . state else {
      panic! ("waiver discarded incident"); };
    assert_eq! (retained . undo_waivers . get ("view-1_deadbeef"),
      Some (&"round trip failed" . to_string ()));
    assert_eq! (retained . phase, MaintenancePhase::AwaitingArchiveWaiver);
  }

  #[test]
  fn begin_allocates_strict_archive_identity () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_for_client (
      MaintenanceOrigin::Pull, None, "client-session" . into ()) . unwrap ();
    assert_eq! (active . archive_owner_session_id, "client-session");
    assert! (active . started_at_utc . ends_with ('Z'));
    assert_eq! (active . started_at_utc . len (), 27);
    let suffix = format! ("_{}", active . incident_id);
    assert! (active . archive_directory_name . ends_with (&suffix));
    assert_eq! (active . archive_directory_name . len (), 60);
    assert_eq! (&active . archive_directory_name[8..9], "T");
    assert_eq! (&active . archive_directory_name[15..16], ".");
    assert_eq! (&active . archive_directory_name[22..24], "Z_");
  }

  fn settlement (id : &str) -> ViewSettlementRecord {
    ViewSettlementRecord {
      buffer_id: id . into (),
      buffer_key: Some (format! ("{}_key", id)),
      kind: BufferKind::ContentView,
      view_uri: Some (format! ("uri-{}", id)),
      dirty: true,
      impacted: true,
      parse_uncertain: false,
      uncertainty_reason: None,
      observed_ids: vec!["node" . into ()],
      resolved_primary_ids: vec!["node" . into ()],
      base_server_revision: 4,
      base_application_token: 9,
      planned_disposition: ViewDisposition::Interrupted,
      requirement: ViewSettlementRequirement::RetirementAck,
      application: None,
      acknowledged: false,
    }
  }

  #[test]
  fn exact_view_settlement_inventory_and_acks_gate_finalization () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let frozen = |id : &str| FrozenBufferRecord {
      buffer_id: id . into (), kind: BufferKind::ContentView,
      view_uri: Some (format! ("uri-{}", id)), graph_generation: 1,
      presentation_generation: 0, server_revision: 4,
      application_token: 9, dirty: true, undo_required: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "b" . repeat (64),
    };
    let active = coordinator . begin_with_archive_contract (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![frozen ("one"), frozen ("two")])
      . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "hash" . into (),
        artifact_count: 1, total_file_bytes: 2,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    coordinator . store_selected (
      &active . incident_id, active . epoch, SelectedStoreRecord {
        graph_generation: GraphGeneration::INITIAL . successor (),
        manifest_revision: ManifestRevision::INITIAL . successor (),
        tantivy_generation: 1, tantivy_outcome: "committed" . into (),
      }) . unwrap ();
    let transfer = ClientEvidenceTransferRecord {
      server_bundle_sha256: "hash" . into (),
      transfer_manifest_sha256: "transfer" . into (),
      artifact_bytes_sha256: "bytes" . into (),
      artifact_count: 1,
      artifact_bytes: 2,
    };
    coordinator . record_client_evidence_transfer (
      &active . incident_id, active . epoch, transfer . clone ()) . unwrap ();
    coordinator . record_client_evidence_transfer (
      &active . incident_id, active . epoch, transfer . clone ()) . unwrap ();
    let mut changed_transfer = transfer;
    changed_transfer . artifact_bytes_sha256 = "different" . into ();
    assert! (coordinator . record_client_evidence_transfer (
      &active . incident_id, active . epoch, changed_transfer) . is_err ());
    assert! (coordinator . record_view_settlements (
      &active . incident_id, active . epoch, vec![settlement ("one")])
      . is_err ());
    coordinator . record_view_settlements (
      &active . incident_id, active . epoch,
      vec![settlement ("one"), settlement ("two")]) . unwrap ();
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("wrong"), 4, 9)
      . is_err ());
    assert! (!coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("uri-one"), 4, 9)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "two",
      ViewSettlementRequirement::RetirementAck, Some ("uri-two"), 4, 9)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("uri-one"), 4, 9)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("wrong"), 4, 9)
      . is_err ());
    assert! (coordinator . archive_finalized (
      &active . incident_id, active . epoch, "final" . into (),
      "wrong" . into (), "bytes" . into ()) . is_err ());
    assert! (coordinator . archive_finalized (
      &active . incident_id, active . epoch, "final" . into (),
      "transfer" . into (), "bytes" . into ()) . unwrap ());
    assert! (!coordinator . archive_finalized (
      &active . incident_id, active . epoch, "final" . into (),
      "transfer" . into (), "bytes" . into ()) . unwrap ());
    assert! (coordinator . archive_finalized (
      &active . incident_id, active . epoch, "changed-final" . into (),
      "transfer" . into (), "bytes" . into ()) . is_err ());
    let CoordinatorState::Active (active) = &coordinator . state else {
      panic! ("incident vanished"); };
    assert_eq! (active . phase, MaintenancePhase::FinalizingArchive);
    assert! (active . client_evidence_acknowledged);
    assert_eq! (active . client_evidence_transfer . as_ref ()
      . unwrap () . transfer_manifest_sha256, "transfer");
  }
}
