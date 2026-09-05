use super::types::*;
use crate::types::store_state::{GraphGeneration, ManifestRevision};

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, BTreeSet};
use std::time::{SystemTime, UNIX_EPOCH};

pub(crate) const VIEW_ENROLLMENT_PENDING : &str =
  "view classification awaits pending URI enrollment";

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

  /// Ordinary watcher work cannot publish over a serialized maintenance or
  /// terminal state.  Retain its monotonic sequence and make the active
  /// incident report that a complete successor sweep is owed instead of
  /// spending work on a result the state machine must reject.
  pub fn defer_ordinary_observation (&mut self) -> Option<ObservationSequence> {
    if !matches! (self . state,
      CoordinatorState::Active (_)
      | CoordinatorState::Terminal (_)
      | CoordinatorState::BlockedStoreHealth { .. })
    {
      return None;
    }
    self . observation_sequence = self . observation_sequence . successor ();
    if let CoordinatorState::Active (active) = &mut self . state {
      active . successor_observation_required = true; }
    Some (self . observation_sequence)
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

  #[cfg(test)]
  pub fn begin (
    &mut self,
    origin    : MaintenanceOrigin,
    candidate : Option<CandidateSummary>,
  ) -> Result<ActiveMaintenance, String> {
    self . begin_for_client (origin, candidate, "test-client" . into ())
  }

  #[cfg(test)]
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
    let targets = match origin {
      MaintenanceOrigin::ExplicitPartialReload => MaintenanceTargets {
        paths: Vec::new (), ids: vec!["test-target" . into ()],
        ..MaintenanceTargets::default ()
      },
      MaintenanceOrigin::Pull => MaintenanceTargets {
        pull_repositories: std::collections::BTreeMap::from ([
          (pull_repository_key (&["test-source" . into ()]),
           vec!["test-source" . into ()])]),
        ..MaintenanceTargets::default ()
      },
      _ => MaintenanceTargets::default (),
    };
    self . begin_with_archive_contract_and_targets (
      origin, candidate, client_session_id, "emacs" . into (), "all" . into (),
      graph_generation, manifest_revision, Vec::new (), targets)
  }

  #[allow(clippy::too_many_arguments)]
  pub fn begin_with_archive_contract_and_targets (
    &mut self,
    origin       : MaintenanceOrigin,
    candidate    : Option<CandidateSummary>,
    client_session_id : String,
    client_kind  : String,
    source_set   : String,
    g0_graph_generation : GraphGeneration,
    g0_manifest_revision : ManifestRevision,
    buffer_records : Vec<FrozenBufferRecord>,
    targets       : MaintenanceTargets,
  ) -> Result<ActiveMaintenance, String> {
    let active = self . begin_epoch_with_archive_contract_and_targets (
      origin,
      candidate,
      client_session_id,
      client_kind,
      source_set,
      g0_graph_generation,
      g0_manifest_revision,
      targets)?;
    self . freeze_locked_census (
      &active . incident_id, active . epoch, buffer_records)?;
    let CoordinatorState::Active (active) = &self . state else {
      unreachable! ("freezing a new incident preserves active maintenance"); };
    Ok (active . clone ())
  }

  #[allow(clippy::too_many_arguments)]
  pub fn begin_epoch_with_archive_contract_and_targets (
    &mut self,
    origin       : MaintenanceOrigin,
    candidate    : Option<CandidateSummary>,
    client_session_id : String,
    client_kind  : String,
    source_set   : String,
    g0_graph_generation : GraphGeneration,
    g0_manifest_revision : ManifestRevision,
    mut targets  : MaintenanceTargets,
  ) -> Result<ActiveMaintenance, String> {
    targets . paths . sort ();
    targets . paths . dedup ();
    targets . ids . sort ();
    targets . ids . dedup ();
    for (repository_key, sources) in &mut targets . pull_repositories {
      if repository_key . is_empty () {
        return Err ("pull repository key cannot be empty" . into ()); }
      if sources . is_empty () || sources . iter () . any (String::is_empty) {
        return Err (format! (
          "pull repository {} has no complete source inventory",
          repository_key)); }
      let original_len = sources . len ();
      sources . sort ();
      sources . dedup ();
      if sources . len () != original_len {
        return Err (format! (
          "pull repository {} repeats a source", repository_key)); }
      if repository_key != &pull_repository_key (sources) {
        return Err (format! (
          "pull repository key {} does not match its source inventory",
          repository_key)); }
    }
    match origin {
      MaintenanceOrigin::ExplicitPartialReload => {
        if targets . paths . is_empty () && targets . ids . is_empty () {
          return Err (
            "explicit partial reload requires at least one path or ID"
              . into ()); }
        if !targets . pull_repositories . is_empty () {
          return Err (
            "explicit partial reload does not accept pull repositories"
              . into ()); }
      }
      MaintenanceOrigin::Pull => {
        if !targets . paths . is_empty () || !targets . ids . is_empty () {
          return Err ("pull does not accept partial-reload targets" . into ()); }
        if targets . pull_repositories . is_empty () {
          return Err ("pull requires a verified repository mapping" . into ()); }
      }
      _ if !targets . paths . is_empty () || !targets . ids . is_empty ()
             || !targets . pull_repositories . is_empty () =>
        return Err (format! (
          "maintenance origin '{}' does not accept explicit targets",
          origin . label ())),
      _ => {}
    }
    if client_session_id . is_empty () {
      return Err ("maintenance requires an owning client session" . into ()); }
    if !matches! (client_kind . as_str (), "emacs" | "neovim") {
      return Err ("maintenance requires a supported archive client" . into ()); }
    if source_set . is_empty () {
      return Err ("maintenance requires an active source-set" . into ()); }
    match &self . state {
      CoordinatorState::Idle => {}
      CoordinatorState::Pending (pending) => {
        if origin == MaintenanceOrigin::FullRebuild && candidate . is_none () {
          // A full rebuild deliberately supersedes an incidental watcher
          // candidate with its own post-archive complete observation.
        } else {
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
      }
      CoordinatorState::Observing
        if origin == MaintenanceOrigin::FullRebuild => {}
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
      phase: MaintenancePhase::AwaitingLockedCensus,
      candidate,
      archive_status: ArchiveStatus::Preparing,
      started_at_utc,
      archive_owner_session_id: client_session_id,
      archive_owner_client_kind: client_kind,
      initial_archive_manifest_sha256: None,
      controller_session_id: String::new (),
      source_set,
      g0_graph_generation,
      g0_manifest_revision,
      registered_buffer_ids: Vec::new (),
      dirty_buffer_ids: Vec::new (),
      undo_required_buffer_ids: Vec::new (),
      buffer_census: Default::default (),
      presentation_buffer_census: Default::default (),
      pending_view_enrollments: Default::default (),
      targets,
      requested_id_outcomes: Vec::new (),
      external_mutation: None,
      undo_waivers: Default::default (),
      server_evidence: None,
      client_evidence_transfer: None,
      client_evidence_acknowledged: false,
      selected_store: None,
      presentation_fence: None,
      successor_observation_required: false,
      force_full_rebuild_recovery: false,
      scalar_release: None,
      preselection_retirements: Default::default (),
      view_settlements: Default::default (),
      blocking_reason: None,
      suspended_phase: None,
      client_connected: true,
      terminal: None,
    };
    self . state = CoordinatorState::Active (active . clone ());
    Ok (active)
  }

  /// Freeze the exact client registry only after the client has installed the
  /// server-owned epoch on every current buffer.  An exact retry is a no-op.
  pub fn freeze_locked_census (
    &mut self,
    incident_id   : &IncidentId,
    epoch         : MaintenanceEpoch,
    buffer_records : Vec<FrozenBufferRecord>,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    let mut buffer_census = std::collections::BTreeMap::new ();
    for record in buffer_records {
      if record . buffer_id . is_empty () {
        return Err ("maintenance census has an empty buffer ID" . into ()); }
      if record . undo_required && !record . dirty {
        return Err (
          "undo-required buffer is absent from the dirty census" . into ()); }
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
    if active . phase == MaintenancePhase::PreparingArchive {
      if active . buffer_census != buffer_census {
        return Err ("maintenance locked-census retry changed its exact records"
          . into ()); }
      return Ok (false);
    }
    if active . phase != MaintenancePhase::AwaitingLockedCensus {
      return Err (format! (
        "locked census is invalid during {:?}", active . phase)); }
    active . registered_buffer_ids = registered_buffer_ids;
    active . dirty_buffer_ids = dirty_buffer_ids;
    active . undo_required_buffer_ids = undo_required_buffer_ids;
    active . presentation_buffer_census = buffer_census . clone ();
    active . buffer_census = buffer_census;
    active . phase = MaintenancePhase::PreparingArchive;
    Ok (true)
  }

  /// Journal a successful query result before its response is allowed onto
  /// the wire.  The URI is the identity available at this boundary; a later
  /// client census supplies the editor-owned buffer ID.
  pub fn enroll_pending_view (
    &mut self,
    enrollment : PendingViewEnrollment,
  ) -> Result<bool, String> {
    let CoordinatorState::Active (active) = &mut self . state else {
      return Ok (false); };
    if enrollment . view_uri . is_empty () {
      return Err ("pending maintenance view has an empty URI" . into ()); }
    if active . client_evidence_transfer . is_some ()
       || matches! (active . archive_status, ArchiveStatus::Finalized { .. })
    {
      return Err (
        "a new view cannot enter maintenance after final evidence transfer"
          . into ()); }
    if let Some (record) = active . presentation_census () . values ()
      . find (|record| record . view_uri . as_deref ()
        == Some (&enrollment . view_uri))
    {
      if record . graph_generation == enrollment . graph_generation
        && record . presentation_generation
             == enrollment . presentation_generation
        && record . server_revision == enrollment . server_revision
        && record . application_token == enrollment . application_token
      { return Ok (false); }
      return Err (format! (
        "view '{}' changed authority during maintenance enrollment",
        enrollment . view_uri));
    }
    if let Some (existing) = active . pending_view_enrollments
      . get (&enrollment . view_uri)
    {
      if existing == &enrollment { return Ok (false); }
      return Err (format! (
        "view '{}' changed pending maintenance authority",
        enrollment . view_uri));
    }
    active . pending_view_enrollments
      . insert (enrollment . view_uri . clone (), enrollment);
    Ok (true)
  }

  /// Bind clean buffers born after the immutable initial archive census.
  /// An incident-qualified census is a monotonic enrollment barrier; an
  /// ordinary reconnect census additionally proves that an unbound response
  /// never became a client buffer and may resolve it absent.
  pub fn enroll_presentation_census (
    &mut self,
    records         : Vec<FrozenBufferRecord>,
    requested_epoch : Option<u64>,
  ) -> Result<Vec<String>, String> {
    let CoordinatorState::Active (active) = &mut self . state else {
      return Ok (Vec::new ()); };
    if let Some (requested_epoch) = requested_epoch {
      if requested_epoch != active . epoch . get () {
        return Err (format! (
          "client census names maintenance epoch {}, current epoch is {}",
          requested_epoch, active . epoch . get ())); }
    }
    // The bootstrap census is about to become the immutable initial census;
    // it predates the distinction between old buffers and late enrollment.
    // `freeze_locked_census` validates and installs it in the next request.
    if active . phase == MaintenancePhase::AwaitingLockedCensus {
      return Ok (Vec::new ()); }
    if active . presentation_buffer_census . is_empty ()
       && !active . buffer_census . is_empty ()
    {
      active . presentation_buffer_census = active . buffer_census . clone (); }
    let census_uris : BTreeSet<String> = records . iter ()
      . filter_map (|record| record . view_uri . clone ()) . collect ();
    let mut added = Vec::new ();
    for record in records {
      if active . presentation_buffer_census . contains_key (&record . buffer_id) {
        continue; }
      if record . maintenance_epoch != Some (active . epoch . get ()) {
        return Err (format! (
          "new buffer '{}' is not locked for maintenance epoch {}",
          record . buffer_id, active . epoch . get ())); }
      if record . dirty || record . logical_dirty || record . undo_required {
        return Err (format! (
          "new maintenance buffer '{}' is dirty but absent from the initial archive",
          record . buffer_id)); }
      if active . presentation_buffer_census . values () . any (|existing|
          existing . view_uri . is_some ()
          && existing . view_uri == record . view_uri)
      {
        return Err (format! (
          "new buffer '{}' repeats a maintenance view URI", record . buffer_id)); }
      if let Some (uri) = &record . view_uri {
        if let Some (pending) = active . pending_view_enrollments . get (uri) {
          if pending . graph_generation != record . graph_generation
          || pending . presentation_generation
               != record . presentation_generation
          || pending . server_revision != record . server_revision
          || pending . application_token != record . application_token
          {
            return Err (format! (
              "buffer '{}' does not match pending authority for view '{}'",
              record . buffer_id, uri)); }
        } else if record . kind != BufferKind::NewEmptyContentView {
          return Err (format! (
            "buffer '{}' has no pending maintenance view enrollment",
            record . buffer_id)); }
      }
      if active . selected_store . is_some ()
         && !active . view_settlements . is_empty ()
      {
        let selected_generation = active . selected_store . as_ref ()
          . expect ("checked selected store") . graph_generation . get ();
        if record . graph_generation != selected_generation {
          return Err (format! (
            "G0 view '{}' reached the client after settlement planning",
            record . buffer_id)); }
        active . view_settlements . insert (
          record . buffer_id . clone (), current_generation_settlement (&record));
        if active . phase == MaintenancePhase::FinalizingArchive {
          active . phase = MaintenancePhase::Presenting; }
      }
      if let Some (uri) = &record . view_uri {
        active . pending_view_enrollments . remove (uri); }
      added . push (record . buffer_id . clone ());
      active . presentation_buffer_census
        . insert (record . buffer_id . clone (), record);
    }
    if requested_epoch . is_none () {
      active . pending_view_enrollments
        . retain (|uri, _| census_uris . contains (uri)); }
    Ok (added)
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

  /// Give a selected-store transition back its archive-ready phase when an
  /// old-generation query completed while the generation gate was draining.
  /// The gate caller invokes this while new queries are still excluded, so a
  /// false result is also proof that selection may cross the G0 boundary.
  pub fn defer_selection_for_view_enrollment (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::SelectingPartial
      | MaintenancePhase::FullRebuildExclusive)
    {
      return Err (format! (
        "view-enrollment selection barrier is invalid during {:?}",
        active . phase)); }
    if active . pending_view_enrollments . is_empty () {
      return Ok (false); }
    if active . selected_store . is_some () {
      return Err (
        "view-enrollment selection barrier followed store selection" . into ()); }
    active . phase = MaintenancePhase::ArchiveReady;
    Ok (true)
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
    active . initial_archive_manifest_sha256 = Some (manifest_sha256 . clone ());
    active . archive_status = ArchiveStatus::Ready { manifest_sha256 };
    active . phase = MaintenancePhase::ArchiveReady;
    Ok (( ))
  }

  /// Journal the pull point of no return before the client launches Git.
  /// A lost response is replayable but never grants authority to a different
  /// origin or to an incident whose archive is not already durable.
  pub fn authorize_external_mutation (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::Pull {
      return Err ("external mutation authorization currently belongs only to pull"
        . into ()); }
    if !matches! (active . archive_status, ArchiveStatus::Ready { .. }) {
      return Err ("external mutation requires an acknowledged initial archive"
        . into ()); }
    if active . phase == MaintenancePhase::RunningExternalMutation {
      return Ok (false); }
    if active . phase != MaintenancePhase::ArchiveReady
    || active . candidate . is_some ()
    || active . external_mutation . is_some ()
    {
      return Err (format! (
        "external mutation authorization is invalid during {:?}",
        active . phase)); }
    active . phase = MaintenancePhase::RunningExternalMutation;
    Ok (true)
  }

  /// Record how the client-owned Git chain ended before observing actual disk.
  /// Exit status is diagnostic only: every outcome advances to the same exact
  /// final observation because a failed or lost child may still have written.
  pub fn external_mutation_finished (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    record      : ExternalMutationRecord,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::Pull {
      return Err ("external mutation completion currently belongs only to pull"
        . into ()); }
    if active . phase == MaintenancePhase::FinalObservation {
      if active . external_mutation . as_ref () == Some (&record) {
        return Ok (false); }
      return Err ("external mutation completion changed its durable outcome"
        . into ());
    }
    if active . phase != MaintenancePhase::RunningExternalMutation {
      return Err (format! (
        "external mutation completion is invalid during {:?}",
        active . phase)); }
    active . external_mutation = Some (record);
    active . phase = MaintenancePhase::FinalObservation;
    Ok (true)
  }

  /// Begin the server-owned observation for an archive-ready explicit reload.
  /// Replays are harmless; no other origin may borrow this transition.
  pub fn begin_target_observation (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::ExplicitPartialReload {
      return Err ("target observation belongs only to an explicit partial reload"
        . into ()); }
    if !matches! (active . archive_status, ArchiveStatus::Ready { .. }) {
      return Err ("target observation requires an acknowledged initial archive"
        . into ()); }
    if active . phase == MaintenancePhase::FinalObservation {
      return Ok (false); }
    if active . phase != MaintenancePhase::ArchiveReady
       || active . candidate . is_some ()
    {
      return Err (format! (
        "target observation is invalid during {:?}", active . phase)); }
    active . phase = MaintenancePhase::FinalObservation;
    Ok (true)
  }

  /// Begin the complete preflight observation for an archive-ready rebuild.
  /// This is still before the exclusive/destructive interval: invalid disk
  /// therefore leaves the selected G0 stores untouched and queryable.
  pub fn begin_full_rebuild_observation (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::FullRebuild {
      return Err ("full rebuild observation belongs only to a full rebuild"
        . into ()); }
    if !matches! (active . archive_status, ArchiveStatus::Ready { .. }) {
      return Err (
        "full rebuild observation requires an acknowledged initial archive"
          . into ()); }
    if active . phase == MaintenancePhase::FinalObservation {
      return Ok (false); }
    if active . phase != MaintenancePhase::ArchiveReady
       || active . candidate . is_some ()
    {
      return Err (format! (
        "full rebuild observation is invalid during {:?}", active . phase)); }
    active . phase = MaintenancePhase::FinalObservation;
    Ok (true)
  }

  /// Bind one immutable observation to the already-authorized incident.  The
  /// candidate returns to ArchiveReady so the common evidence/selection path
  /// can close every precondition before touching a derived store.
  pub fn record_observed_candidate (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    candidate   : CandidateSummary,
  ) -> Result<bool, String> {
    let sequence = self . observation_sequence;
    let active = self . matching_active_mut (incident_id, epoch)?;
    if candidate . base_graph_generation != active . g0_graph_generation
    || candidate . base_manifest_revision != active . g0_manifest_revision
    || candidate . covered_sequence != sequence
    {
      return Err (
        "observed candidate does not share the incident's exact G0/fence"
          . into ()); }
    if let Some (existing) = &active . candidate {
      if existing != &candidate {
        return Err ("incident already names another observed candidate"
          . into ()); }
      if active . phase == MaintenancePhase::ArchiveReady {
        return Ok (false); }
    }
    if active . phase != MaintenancePhase::FinalObservation {
      return Err (format! (
        "observed candidate is invalid during {:?}", active . phase)); }
    active . candidate = Some (candidate);
    active . successor_observation_required = false;
    active . blocking_reason = None;
    active . phase = MaintenancePhase::ArchiveReady;
    Ok (true)
  }

  /// Journal how each requested ID resolved before an explicit target is
  /// selected.  Replays must reproduce the exact same resolution and paths.
  pub fn record_requested_id_outcomes (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    mut outcomes : Vec<MaintenanceIdOutcome>,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::ExplicitPartialReload
    || active . phase != MaintenancePhase::FinalObservation
    {
      return Err (format! (
        "requested-ID resolution is invalid during {:?}", active . phase)); }
    for outcome in &mut outcomes {
      outcome . paths . sort ();
      outcome . paths . dedup ();
      if outcome . requested_id . is_empty ()
      || outcome . pid . as_deref () == Some ("")
      || outcome . pid . is_some () == outcome . reason . is_some ()
      {
        return Err ("requested-ID resolution record is inconsistent" . into ()); }
    }
    outcomes . sort_by (|left, right|
      left . requested_id . cmp (&right . requested_id));
    let actual : Vec<&str> = outcomes . iter ()
      . map (|outcome| outcome . requested_id . as_str ()) . collect ();
    let expected : Vec<&str> = active . targets . ids . iter ()
      . map (String::as_str) . collect ();
    if actual != expected {
      return Err (format! (
        "requested-ID resolution inventory is {:?}, expected {:?}",
        actual, expected)); }
    if !active . requested_id_outcomes . is_empty () {
      if active . requested_id_outcomes != outcomes {
        return Err (
          "incident already records another requested-ID resolution" . into ()); }
      return Ok (false);
    }
    active . requested_id_outcomes = outcomes;
    Ok (true)
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
    let registered_ids : BTreeSet<_> = active . presentation_census ()
      . keys () . cloned () . collect ();
    if settlement_ids != registered_ids
    || !active . pending_view_enrollments . is_empty ()
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

  pub fn store_rebuilt (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    selected    : SelectedStoreRecord,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::FullRebuild
    && !active . force_full_rebuild_recovery
    {
      return Err (
        "full rebuild completion has no exclusive rebuild authority" . into ()); }
    if active . phase != MaintenancePhase::FullRebuildExclusive
    {
      return Err (format! (
        "full rebuild completion is invalid during {:?}", active . phase)); }
    if active . server_evidence . is_none () {
      return Err ("full rebuild has no durable server evidence" . into ()); }
    active . selected_store = Some (selected);
    active . blocking_reason = None;
    active . force_full_rebuild_recovery = false;
    active . phase = MaintenancePhase::Presenting;
    Ok (( ))
  }

  /// Bind the selected G1 candidate's disk observation to the exact Git
  /// presentation seen before maintenance view classification/rendering.
  /// A later Git signature is deliberately not folded into this record: it is
  /// ordinary retained presentation debt against the already-selected G1.
  pub fn record_presentation_fence (
    &mut self,
    incident_id              : &IncidentId,
    epoch                    : MaintenanceEpoch,
    signature_blake3         : String,
    presentation_generation : u64,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if !matches! (active . phase,
      MaintenancePhase::Presenting
      | MaintenancePhase::AwaitingScalarAuthorization
      | MaintenancePhase::FinalizingArchive)
    {
      return Err (format! (
        "presentation fence is invalid during {:?}", active . phase)); }
    let candidate = active . candidate . as_ref ()
      . ok_or_else (|| "presentation fence has no selected candidate"
        . to_string ())?;
    if active . selected_store . is_none () {
      return Err ("presentation fence precedes coherent G1 selection" . into ()); }
    if signature_blake3 . len () != 64
    || !signature_blake3 . bytes () . all (|byte| byte . is_ascii_hexdigit ())
    {
      return Err ("presentation fence has an invalid BLAKE3 signature" . into ());
    }
    let fence = MaintenancePresentationFence {
      candidate_observation_sequence: candidate . covered_sequence,
      signature_blake3: signature_blake3 . to_ascii_lowercase (),
      presentation_generation,
    };
    if let Some (existing) = &active . presentation_fence {
      if existing != &fence {
        return Err ("incident already records another presentation fence"
          . into ()); }
      return Ok (false);
    }
    active . presentation_fence = Some (fence);
    Ok (true)
  }

  pub fn replace_full_rebuild_source_set (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    source_set  : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . origin != MaintenanceOrigin::FullRebuild
    && !active . force_full_rebuild_recovery
    {
      return Err (
        "source-set replacement has no exclusive rebuild authority" . into ()); }
    if active . phase != MaintenancePhase::FullRebuildExclusive
    {
      return Err (format! (
        "source-set replacement is invalid during {:?}", active . phase)); }
    if source_set . is_empty () {
      return Err ("replacement source-set may not be empty" . into ()); }
    active . source_set = source_set;
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
    if !active . pending_view_enrollments . is_empty () {
      return Err (VIEW_ENROLLMENT_PENDING . into ()); }
    let expected : BTreeSet<_> = active . presentation_census ()
      . keys () . cloned () . collect ();
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
    if !active . pending_view_enrollments . is_empty () {
      return Err (VIEW_ENROLLMENT_PENDING . into ()); }
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
    base_graph_generation : u64,
    base_presentation_generation : u64,
    base_revision    : u64,
    application_token : u64,
    application_ack  : Option<&ViewApplicationAcknowledgement>,
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
    || record . base_graph_generation != base_graph_generation
    || record . base_presentation_generation != base_presentation_generation
    || record . base_server_revision != base_revision
    || record . base_application_token != application_token
    {
      return Err (format! (
        "buffer '{}' settlement ACK changed its frozen authority", buffer_id)); }
    match (&record . application, application_ack) {
      (Some (offer), Some (ack))
        if record . requirement == ViewSettlementRequirement::ApplicationAck
        && offer . content_sha256 == ack . content_sha256
        && offer . resulting_graph_generation
             == ack . resulting_graph_generation
        && offer . resulting_presentation_generation
             == ack . resulting_presentation_generation
        && offer . resulting_server_revision
             == ack . resulting_server_revision
        && offer . resulting_application_token
             == ack . resulting_application_token => {}
      (None, None)
        if record . requirement != ViewSettlementRequirement::ApplicationAck => {}
      _ => return Err (format! (
        "buffer '{}' application ACK changed its staged authority", buffer_id)),
    }
    if record . acknowledged {
      return Ok (active . view_settlements . values ()
        . all (|record| record . acknowledged)); }
    if active . phase == MaintenancePhase::FinalizingArchive {
      return Err (
        "finalizing archive contains an unacknowledged view settlement"
          . into ()); }
    record . resolution = ViewSettlementResolution::ClientAcknowledged;
    record . acknowledged = true;
    let complete = active . view_settlements . values ()
      . all (|record| record . acknowledged);
    if complete { active . phase = MaintenancePhase::FinalizingArchive; }
    Ok (complete)
  }

  /// Before an external mutation yields invalid disk there is no G1 against
  /// which dirty views can prove orthogonality.  Record their conservative
  /// retirements separately from the eventual complete G1 settlement set.
  pub fn record_preselection_retirements (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    retirements : Vec<ViewSettlementRecord>,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::BlockedInvalidAfterMutation
    || active . external_mutation . is_none ()
    {
      return Err (format! (
        "preselection retirement is invalid during {:?}", active . phase)); }
    let mut records = BTreeMap::new ();
    for record in retirements {
      if !record . dirty || !record . impacted
      || record . planned_disposition != ViewDisposition::Interrupted
      || record . requirement != ViewSettlementRequirement::RetirementAck
      || record . application . is_some ()
      {
        return Err (format! (
          "buffer '{}' is not a conservative dirty retirement",
          record . buffer_id)); }
      let id = record . buffer_id . clone ();
      if records . insert (id . clone (), record) . is_some () {
        return Err (format! (
          "preselection retirement repeats buffer '{}'", id)); }
    }
    let expected : BTreeSet<_> = active . dirty_buffer_ids
      . iter () . cloned () . collect ();
    let actual : BTreeSet<_> = records . keys () . cloned () . collect ();
    if actual != expected {
      return Err (format! (
        "preselection retirement inventory is {:?}, expected {:?}",
        actual, expected)); }
    if active . preselection_retirements . is_empty () {
      active . preselection_retirements = records;
    } else if active . preselection_retirements != records {
      return Err (
        "incident already records different preselection retirements" . into ()); }
    Ok (( ))
  }

  #[allow(clippy::too_many_arguments)]
  pub fn acknowledge_preselection_retirement (
    &mut self,
    incident_id      : &IncidentId,
    epoch            : MaintenanceEpoch,
    buffer_id        : &str,
    requirement      : ViewSettlementRequirement,
    view_uri         : Option<&str>,
    base_graph_generation : u64,
    base_presentation_generation : u64,
    base_revision    : u64,
    application_token : u64,
  ) -> Result<bool, String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::BlockedInvalidAfterMutation {
      return Err (format! (
        "preselection retirement ACK is invalid during {:?}", active . phase)); }
    let record = active . preselection_retirements . get_mut (buffer_id)
      . ok_or_else (|| format! (
        "buffer '{}' has no preselection retirement", buffer_id))?;
    if requirement != ViewSettlementRequirement::RetirementAck
    || record . requirement != requirement
    || record . view_uri . as_deref () != view_uri
    || record . base_graph_generation != base_graph_generation
    || record . base_presentation_generation != base_presentation_generation
    || record . base_server_revision != base_revision
    || record . base_application_token != application_token
    {
      return Err (format! (
        "buffer '{}' retirement ACK changed its frozen authority", buffer_id)); }
    if !record . acknowledged {
      record . resolution = ViewSettlementResolution::ClientAcknowledged;
      record . acknowledged = true; }
    Ok (active . preselection_retirements . values ()
      . all (|record| record . acknowledged))
  }

  pub fn reconcile_absent_preselection_retirements (
    &mut self,
    live_buffer_ids : &BTreeSet<String>,
  ) -> Vec<String> {
    let CoordinatorState::Active (active) = &mut self . state else {
      return Vec::new (); };
    let mut resolved = Vec::new ();
    for (buffer_id, record) in &mut active . preselection_retirements {
      if record . acknowledged || live_buffer_ids . contains (buffer_id) {
        continue; }
      record . resolution = ViewSettlementResolution::CensusAbsent;
      record . acknowledged = true;
      resolved . push (buffer_id . clone ());
    }
    resolved
  }

  /// Resolve presentation work for frozen buffers which no longer exist in
  /// the replacement editor's complete census.  The original disposition and
  /// staged application remain journaled as evidence; only the obligation to
  /// receive an impossible client ACK is closed.
  pub fn reconcile_absent_view_settlements (
    &mut self,
    live_buffer_ids : &BTreeSet<String>,
  ) -> Result<Vec<String>, String> {
    let CoordinatorState::Active (active) = &mut self . state else {
      return Ok (Vec::new ()); };
    if active . view_settlements . is_empty () {
      return Ok (Vec::new ()); }
    if !matches! (active . phase,
      MaintenancePhase::Presenting | MaintenancePhase::FinalizingArchive)
    {
      return Err (format! (
        "view census reconciliation is invalid during {:?}", active . phase)); }
    let mut resolved = Vec::new ();
    for (buffer_id, record) in &mut active . view_settlements {
      if record . acknowledged || live_buffer_ids . contains (buffer_id) {
        continue; }
      record . resolution = ViewSettlementResolution::CensusAbsent;
      record . acknowledged = true;
      resolved . push (buffer_id . clone ());
    }
    if active . view_settlements . values ()
      . all (|record| record . acknowledged)
    {
      active . phase = MaintenancePhase::FinalizingArchive; }
    Ok (resolved)
  }

  /// Record that a complete census proved the client already installed the
  /// exact staged application whose ordinary ACK was lost.
  pub fn acknowledge_view_application_from_census (
    &mut self,
    buffer_id       : &str,
    application_ack : &ViewApplicationAcknowledgement,
  ) -> Result<bool, String> {
    let (incident_id, epoch, requirement, view_uri, base_graph,
         base_presentation, base_revision, base_token) = {
      let CoordinatorState::Active (active) = &self . state else {
        return Err ("census application has no active maintenance" . into ()); };
      let record = active . view_settlements . get (buffer_id)
        . ok_or_else (|| format! (
          "buffer '{}' has no planned settlement", buffer_id))?;
      if record . acknowledged {
        return if record . resolution == ViewSettlementResolution::CensusApplied {
          Ok (active . view_settlements . values ()
            . all (|record| record . acknowledged))
        } else {
          Err (format! (
            "buffer '{}' settlement was resolved by another authority",
            buffer_id))
        }; }
      (
        active . incident_id . clone (), active . epoch,
        record . requirement . clone (), record . view_uri . clone (),
        record . base_graph_generation,
        record . base_presentation_generation,
        record . base_server_revision,
        record . base_application_token,
      )
    };
    if requirement != ViewSettlementRequirement::ApplicationAck {
      return Err (format! (
        "buffer '{}' has no staged application to confirm", buffer_id)); }
    let complete = self . acknowledge_view_settlement (
      &incident_id, epoch, buffer_id, requirement, view_uri . as_deref (),
      base_graph, base_presentation, base_revision, base_token,
      Some (application_ack))?;
    let active = self . matching_active_mut (&incident_id, epoch)?;
    active . view_settlements . get_mut (buffer_id)
      . expect ("acknowledged census application remains journaled")
      . resolution = ViewSettlementResolution::CensusApplied;
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

  pub fn block_invalid_disk (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
    reason      : String,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if reason . is_empty () {
      return Err ("invalid-disk block requires an exact reason" . into ()); }
    if active . phase == MaintenancePhase::BlockedInvalidAfterMutation {
      if active . blocking_reason . as_deref () == Some (&reason) {
        return Ok (( )); }
      return Err ("incident already records another invalid-disk reason"
        . into ());
    }
    if !matches! (active . phase,
      MaintenancePhase::ArchiveReady
      | MaintenancePhase::RunningExternalMutation
      | MaintenancePhase::FinalObservation)
    {
      return Err (format! (
        "invalid-disk block is invalid during {:?}", active . phase)); }
    active . blocking_reason = Some (reason);
    active . phase = MaintenancePhase::BlockedInvalidAfterMutation;
    Ok (( ))
  }

  /// Retry an invalid final observation without repeating the external origin
  /// operation or weakening its archive/epoch boundary.
  pub fn retry_blocked_invalid_disk (
    &mut self,
    incident_id : &IncidentId,
    epoch       : MaintenanceEpoch,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::BlockedInvalidAfterMutation {
      return Err (format! (
        "invalid-disk retry is invalid during {:?}", active . phase)); }
    if active . selected_store . is_some ()
    || active . client_evidence_transfer . is_some ()
    || !active . view_settlements . is_empty ()
    {
      return Err (
        "invalid-disk retry cannot replace selected or presented authority"
          . into ());
    }
    if active . preselection_retirements . values ()
      . any (|record| !record . acknowledged)
    {
      return Err (
        "invalid-disk retry awaits every dirty-buffer retirement" . into ()); }
    active . candidate = None;
    active . server_evidence = None;
    active . presentation_fence = None;
    active . scalar_release = None;
    active . blocking_reason = None;
    active . successor_observation_required = false;
    active . phase = MaintenancePhase::FinalObservation;
    Ok (( ))
  }

  /// A failed store transition is retried only from a fresh observation.  A
  /// poisoned/unqueryable selected triple escalates to a complete candidate
  /// and full reconstruction even when the original incident was targeted.
  pub fn retry_blocked_store_health (
    &mut self,
    incident_id       : &IncidentId,
    epoch             : MaintenanceEpoch,
    force_full_rebuild : bool,
  ) -> Result<(), String> {
    let active = self . matching_active_mut (incident_id, epoch)?;
    if active . phase != MaintenancePhase::BlockedStoreHealth {
      return Err (format! (
        "store-health retry is invalid during {:?}", active . phase)); }
    if active . selected_store . is_some ()
    || active . client_evidence_transfer . is_some ()
    || !active . view_settlements . is_empty ()
    {
      return Err (
        "store-health retry cannot replace selected or presented authority"
          . into ());
    }
    if active . preselection_retirements . values ()
      . any (|record| !record . acknowledged)
    {
      return Err (
        "store-health retry awaits every dirty-buffer retirement" . into ()); }
    active . candidate = None;
    active . server_evidence = None;
    active . presentation_fence = None;
    active . scalar_release = None;
    active . blocking_reason = None;
    active . successor_observation_required = false;
    active . force_full_rebuild_recovery = force_full_rebuild;
    active . phase = MaintenancePhase::FinalObservation;
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
      MaintenancePhase::ArchiveReady
      | MaintenancePhase::SelectingPartial
      | MaintenancePhase::FullRebuildExclusive)
    {
      return Err (format! (
        "candidate supersession is invalid during {:?}", active . phase)); }
    if active . client_evidence_transfer . is_some ()
    || !active . view_settlements . is_empty ()
    {
      return Err (
        "candidate cannot be superseded after client presentation began"
          . into ());
    }
    active . candidate = None;
    active . server_evidence = None;
    active . presentation_fence = None;
    active . scalar_release = None;
    active . successor_observation_required = true;
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
      MaintenancePhase::AwaitingLockedCensus
      | MaintenancePhase::PreparingArchive
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

  /// Resume the owning editor, or abandon a different editor's transaction
  /// while it is still before the archive/risky-work boundary.  This is not
  /// incident adoption: the replacement receives ordinary idle/pending state
  /// and must explicitly request any still-wanted maintenance again.
  pub fn reconnected_for_session (
    &mut self,
    session_id : &str,
  ) -> Option<(IncidentId, String)> {
    self . reconnected ();
    let CoordinatorState::Active (active) = &self . state else {
      return None; };
    if active . controlling_session_id () == session_id
    || !matches! (active . phase,
      MaintenancePhase::AwaitingLockedCensus
      | MaintenancePhase::PreparingArchive
      | MaintenancePhase::AwaitingArchiveWaiver)
    {
      return None; }
    let abandoned = (
      active . incident_id . clone (), active . origin . label () . to_string ());
    let candidate = active . candidate . clone ();
    self . state = if let Some (candidate) = candidate {
      CoordinatorState::Pending (PendingDiskState {
        reason: PendingReason::ValidDiskDifference,
        candidate: Some (candidate),
        details: vec![
          "a replacement editor abandoned pre-archive maintenance" . into ()],
        // The replacement never saw the old offer, so it needs a fresh one.
        offer_sent: false,
      })
    } else { CoordinatorState::Idle };
    Some (abandoned)
  }

  /// Transfer an already archived incident to the sole replacement editor.
  /// The caller independently reverifies the initial archive before invoking
  /// this transition.  Session IDs identify resumptions; they are not archive
  /// provenance and therefore never rewrite `archive_owner_session_id`.
  pub fn adopt_attached_session (
    &mut self,
    session_id : &str,
  ) -> Result<bool, String> {
    if session_id . is_empty () {
      return Err ("replacement session ID may not be empty" . into ()); }
    match &mut self . state {
      CoordinatorState::Active (active) => {
        if active . controlling_session_id () == session_id {
          return Ok (false); }
        if active . initial_archive_manifest_sha256 . is_none ()
        || !matches! (active . archive_status,
          ArchiveStatus::Ready { .. } | ArchiveStatus::Finalized { .. })
        {
          return Err (
            "a replacement editor cannot adopt maintenance before archive-ready"
              . into ()); }
        active . controller_session_id = session_id . to_string ();
        Ok (true)
      }
      CoordinatorState::Terminal (terminal) => {
        if terminal . controlling_session_id () == session_id {
          return Ok (false); }
        if terminal . archive_manifest_sha256 . is_none () {
          return Err (
            "a replacement editor cannot adopt terminal maintenance without a final archive"
              . into ()); }
        terminal . controller_session_id = session_id . to_string ();
        Ok (true)
      }
      _ => Ok (false),
    }
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
        || !active . client_evidence_acknowledged
        || active . presentation_fence . is_none ()
        || !active . pending_view_enrollments . is_empty ())
    {
      return Err (
        "completed maintenance requires an acknowledged finalized archive and presentation fence"
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
      controller_session_id: active . controlling_session_id () . to_string (),
      archive_directory_name: active . archive_directory_name . clone (),
      archive_manifest_sha256: manifest_sha256,
      registered_buffer_ids: active . presentation_buffer_ids (),
      selected_store: active . selected_store . clone (),
      requested_id_outcomes: active . requested_id_outcomes . clone (),
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

fn current_generation_settlement (
  record : &FrozenBufferRecord,
) -> ViewSettlementRecord {
  ViewSettlementRecord {
    buffer_id: record . buffer_id . clone (),
    buffer_key: None,
    kind: record . kind . clone (),
    view_uri: record . view_uri . clone (),
    origin_buffer_id: record . origin_buffer_id . clone (),
    origin_view_uri: record . origin_view_uri . clone (),
    origin_application_token: record . origin_application_token,
    origin_location: record . origin_location . clone (),
    dirty: false,
    impacted: false,
    parse_uncertain: false,
    uncertainty_reason: None,
    observed_ids: Vec::new (),
    resolved_primary_ids: Vec::new (),
    base_graph_generation: record . graph_generation,
    base_presentation_generation: record . presentation_generation,
    base_server_revision: record . server_revision,
    base_application_token: record . application_token,
    planned_disposition: ViewDisposition::RetainedClean,
    requirement: ViewSettlementRequirement::ReleaseAck,
    application: None,
    resolution: Default::default (),
    acknowledged: false,
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

  fn frozen_dirty_view (buffer_id : &str) -> FrozenBufferRecord {
    FrozenBufferRecord {
      buffer_id: buffer_id . into (), kind: BufferKind::ContentView,
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      source_set: "all" . into (), view_uri: Some ("view" . into ()),
      graph_generation: 1, presentation_generation: 2, server_revision: 3,
      application_token: 4, dirty: true, logical_dirty: false,
      undo_required: true, maintenance_epoch: None,
      presentation_stale: false, search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "b" . repeat (64),
    }
  }

  fn late_clean_view (
    buffer_id : &str,
    view_uri  : &str,
    epoch     : MaintenanceEpoch,
  ) -> FrozenBufferRecord {
    FrozenBufferRecord {
      buffer_id: buffer_id . into (), kind: BufferKind::ContentView,
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      source_set: "all" . into (), view_uri: Some (view_uri . into ()),
      graph_generation: 1, presentation_generation: 2, server_revision: 3,
      application_token: 4, dirty: false, logical_dirty: false,
      undo_required: false, maintenance_epoch: Some (epoch . get ()),
      presentation_stale: false, search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "a" . repeat (64),
    }
  }

  fn dirty_retirement (buffer_id : &str) -> ViewSettlementRecord {
    ViewSettlementRecord {
      buffer_id: buffer_id . into (), buffer_key: Some ("view-key" . into ()),
      kind: BufferKind::ContentView, view_uri: Some ("view" . into ()),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      dirty: true, impacted: true, parse_uncertain: true,
      uncertainty_reason: Some ("invalid final disk" . into ()),
      observed_ids: Vec::new (), resolved_primary_ids: Vec::new (),
      base_graph_generation: 1, base_presentation_generation: 2,
      base_server_revision: 3, base_application_token: 4,
      planned_disposition: ViewDisposition::Interrupted,
      requirement: ViewSettlementRequirement::RetirementAck,
      application: None, resolution: ViewSettlementResolution::Pending,
      acknowledged: false,
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
  fn invalid_preflight_keeps_the_selected_generation_queryable () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . begin_full_rebuild_observation (
      &active . incident_id, active . epoch) . unwrap ();
    coordinator . block_invalid_disk (
      &active . incident_id, active . epoch, "invalid candidate" . into ())
      . unwrap ();
    let policy = coordinator . state . policy ();
    assert! (policy . queries_allowed);
    assert! (!policy . skg_saves_allowed);
    assert! (policy . maintenance_locked);
  }

  #[test]
  fn blocked_invalid_disk_retries_without_repeating_origin_authority () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . block_invalid_disk (
      &active . incident_id, active . epoch, "invalid candidate" . into ())
      . unwrap ();
    coordinator . retry_blocked_invalid_disk (
      &active . incident_id, active . epoch) . unwrap ();
    let CoordinatorState::Active (retried) = &coordinator . state else {
      panic! ("invalid-disk retry stopped being active"); };
    assert_eq! (retried . phase, MaintenancePhase::FinalObservation);
    assert_eq! (retried . origin, MaintenanceOrigin::ExplicitPartialReload);
    assert! (retried . blocking_reason . is_none ());
    assert! (!retried . force_full_rebuild_recovery);
  }

  #[test]
  fn poisoned_store_retry_records_exclusive_rebuild_authority () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::ExplicitPartialReload, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . block_store_health (
      &active . incident_id, active . epoch, "stores poisoned" . into ())
      . unwrap ();
    coordinator . retry_blocked_store_health (
      &active . incident_id, active . epoch, true) . unwrap ();
    let CoordinatorState::Active (retried) = &coordinator . state else {
      panic! ("store-health retry stopped being active"); };
    assert_eq! (retried . phase, MaintenancePhase::FinalObservation);
    assert_eq! (retried . origin, MaintenanceOrigin::ExplicitPartialReload);
    assert! (retried . force_full_rebuild_recovery);
    coordinator . record_observed_candidate (
      &active . incident_id, active . epoch, candidate ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "server" . into (),
        artifact_count: 1, total_file_bytes: 2,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::FullRebuildExclusive) . unwrap ();
    coordinator . replace_full_rebuild_source_set (
      &active . incident_id, active . epoch, "all" . into ()) . unwrap ();
    coordinator . store_rebuilt (
      &active . incident_id, active . epoch, SelectedStoreRecord {
        graph_generation: GraphGeneration::INITIAL . successor (),
        manifest_revision: ManifestRevision::INITIAL . successor (),
        tantivy_generation: 0,
        tantivy_outcome: "synchronous-full-rebuild" . into (),
      }) . unwrap ();
    let CoordinatorState::Active (rebuilt) = &coordinator . state else {
      panic! ("recovered stores stopped being active"); };
    assert_eq! (rebuilt . phase, MaintenancePhase::Presenting);
    assert! (!rebuilt . force_full_rebuild_recovery);
  }

  #[test]
  fn invalid_post_pull_disk_requires_exact_dirty_retirement_before_retry () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let sources = vec!["main" . to_string ()];
    let active = coordinator . begin_epoch_with_archive_contract_and_targets (
      MaintenanceOrigin::Pull, None, "session" . into (), "emacs" . into (),
      "all" . into (), GraphGeneration::INITIAL, ManifestRevision::INITIAL,
      MaintenanceTargets {
        pull_repositories: BTreeMap::from ([
          (pull_repository_key (&sources), sources)]),
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    coordinator . freeze_locked_census (
      &active . incident_id, active . epoch,
      vec![frozen_dirty_view ("dirty")]) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . authorize_external_mutation (
      &active . incident_id, active . epoch) . unwrap ();
    coordinator . external_mutation_finished (
      &active . incident_id, active . epoch, ExternalMutationRecord {
        outcome: ExternalMutationOutcome::Failed,
        details: vec!["pull exited 1" . into ()],
      }) . unwrap ();
    coordinator . block_invalid_disk (
      &active . incident_id, active . epoch, "invalid final disk" . into ())
      . unwrap ();
    coordinator . record_preselection_retirements (
      &active . incident_id, active . epoch,
      vec![dirty_retirement ("dirty")]) . unwrap ();
    assert! (coordinator . retry_blocked_invalid_disk (
      &active . incident_id, active . epoch) . unwrap_err ()
      . contains ("awaits every dirty-buffer retirement"));
    assert! (coordinator . acknowledge_preselection_retirement (
      &active . incident_id, active . epoch, "dirty",
      ViewSettlementRequirement::RetirementAck, Some ("view"), 1, 2, 3, 4)
      . unwrap ());
    coordinator . retry_blocked_invalid_disk (
      &active . incident_id, active . epoch) . unwrap ();
    let CoordinatorState::Active (retried) = &coordinator . state else {
      panic! ("post-pull invalid retry stopped being active"); };
    assert_eq! (retried . phase, MaintenancePhase::FinalObservation);
    assert! (retried . preselection_retirements["dirty"] . acknowledged);
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
  fn full_rebuild_replaces_a_pending_candidate_with_its_own_observation () {
    let mut coordinator = MaintenanceCoordinator::new ();
    coordinator . set_pending_valid (candidate ()) . unwrap ();
    let active = coordinator . begin (MaintenanceOrigin::FullRebuild, None)
      . unwrap ();
    assert_eq! (active . origin, MaintenanceOrigin::FullRebuild);
    assert! (active . candidate . is_none ());
    assert_eq! (active . phase, MaintenancePhase::PreparingArchive);
  }

  #[test]
  fn full_rebuild_supersedes_an_in_progress_ordinary_observation () {
    let mut coordinator = MaintenanceCoordinator::new ();
    coordinator . observation_started () . unwrap ();
    let active = coordinator . begin (MaintenanceOrigin::FullRebuild, None)
      . unwrap ();
    assert_eq! (active . origin, MaintenanceOrigin::FullRebuild);
    assert! (matches! (coordinator . state, CoordinatorState::Active (_)));
  }

  #[test]
  fn epoch_precedes_the_exact_replay_safe_locked_census () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_epoch_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    assert_eq! (active . phase, MaintenancePhase::AwaitingLockedCensus);
    assert! (active . buffer_census . is_empty ());
    assert! (coordinator . archive_ready (
      &active . incident_id, active . epoch, "too-early" . into ()) . is_err ());
    let frozen = FrozenBufferRecord {
      buffer_id: "view" . into (), kind: BufferKind::ContentView,
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      source_set: "all" . into (),
      view_uri: Some ("uri-view" . into ()), graph_generation: 1,
      presentation_generation: 0, server_revision: 4,
      application_token: 9, dirty: true, logical_dirty: false,
      undo_required: true, maintenance_epoch: None,
      presentation_stale: false, search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "b" . repeat (64),
    };
    let mut bootstrap = frozen . clone ();
    bootstrap . maintenance_epoch = Some (active . epoch . get ());
    assert! (coordinator . enroll_presentation_census (
      vec![bootstrap], Some (active . epoch . get ()))
      . unwrap () . is_empty ());
    assert! (coordinator . freeze_locked_census (
      &active . incident_id, active . epoch, vec![frozen . clone ()])
      . unwrap ());
    let CoordinatorState::Active (locked) = &coordinator . state else {
      panic! ("locked census stopped being active"); };
    assert_eq! (locked . phase, MaintenancePhase::PreparingArchive);
    assert_eq! (locked . registered_buffer_ids, ["view"]);
    assert_eq! (locked . dirty_buffer_ids, ["view"]);
    assert_eq! (locked . undo_required_buffer_ids, ["view"]);
    assert! (!coordinator . freeze_locked_census (
      &active . incident_id, active . epoch, vec![frozen . clone ()])
      . unwrap ());
    let mut changed = frozen;
    changed . current_sha256 = "c" . repeat (64);
    assert! (coordinator . freeze_locked_census (
      &active . incident_id, active . epoch, vec![changed]) . is_err ());
  }

  #[test]
  fn server_view_uri_binds_a_late_clean_client_buffer_monotonically () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    coordinator . block_invalid_disk (
      &active . incident_id, active . epoch, "invalid candidate" . into ())
      . unwrap ();
    let pending = PendingViewEnrollment {
      view_uri: "late-view" . into (), graph_generation: 1,
      presentation_generation: 2, server_revision: 3, application_token: 4,
    };
    assert! (coordinator . enroll_pending_view (pending . clone ()) . unwrap ());
    assert! (!coordinator . enroll_pending_view (pending) . unwrap ());
    assert_eq! (coordinator . enroll_presentation_census (
      vec![late_clean_view ("late-buffer", "late-view", active . epoch)],
      Some (active . epoch . get ()))
      . unwrap (), ["late-buffer"]);
    let CoordinatorState::Active (enrolled) = &coordinator . state else {
      panic! ("late view stopped being active"); };
    assert! (enrolled . buffer_census . is_empty ());
    assert! (enrolled . registered_buffer_ids . is_empty ());
    assert_eq! (enrolled . presentation_buffer_ids (), ["late-buffer"]);
    assert! (enrolled . pending_view_enrollments . is_empty ());
  }

  #[test]
  fn selection_waits_for_a_query_to_bind_its_client_buffer () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let candidate = candidate ();
    coordinator . set_pending_valid (candidate . clone ()) . unwrap ();
    let active = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation, Some (candidate)) . unwrap ();
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
    coordinator . enroll_pending_view (PendingViewEnrollment {
      view_uri: "late-view" . into (), graph_generation: 1,
      presentation_generation: 2, server_revision: 3, application_token: 4,
    }) . unwrap ();
    assert! (coordinator . defer_selection_for_view_enrollment (
      &active . incident_id, active . epoch) . unwrap ());
    let CoordinatorState::Active (deferred) = &coordinator . state else {
      panic! ("deferred selection stopped being active"); };
    assert_eq! (deferred . phase, MaintenancePhase::ArchiveReady);
    assert! (deferred . server_evidence . is_some ());

    coordinator . enroll_presentation_census (
      vec![late_clean_view ("late-buffer", "late-view", active . epoch)],
      Some (active . epoch . get ())) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    assert! (!coordinator . defer_selection_for_view_enrollment (
      &active . incident_id, active . epoch) . unwrap ());
    let CoordinatorState::Active (ready) = &coordinator . state else {
      panic! ("ready selection stopped being active"); };
    assert_eq! (ready . phase, MaintenancePhase::SelectingPartial);
  }

  #[test]
  fn reconnect_resolves_an_unconstructed_pending_view_but_not_dirty_work () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . unwrap ();
    coordinator . enroll_pending_view (PendingViewEnrollment {
      view_uri: "lost-response" . into (), graph_generation: 1,
      presentation_generation: 0, server_revision: 1, application_token: 1,
    }) . unwrap ();
    coordinator . enroll_presentation_census (Vec::new (), None) . unwrap ();
    let mut dirty = late_clean_view ("dirty", "client-created", active . epoch);
    dirty . kind = BufferKind::NewEmptyContentView;
    dirty . dirty = true;
    assert! (coordinator . enroll_presentation_census (
      vec![dirty], Some (active . epoch . get ()))
      . unwrap_err () . contains ("absent from the initial archive"));
    let CoordinatorState::Active (enrolled) = &coordinator . state else {
      panic! ("late view stopped being active"); };
    assert! (enrolled . pending_view_enrollments . is_empty ());
    assert! (enrolled . presentation_census () . is_empty ());
  }

  #[test]
  fn happy_path_requires_exact_incident_epoch_and_final_archive () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let candidate = candidate ();
    coordinator . set_pending_valid (candidate . clone ()) . unwrap ();
    let active = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation, Some (candidate)) . unwrap ();
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
    assert! (coordinator . record_presentation_fence (
      &active . incident_id, active . epoch, "a" . repeat (64), 3)
      . unwrap ());
    assert! (!coordinator . record_presentation_fence (
      &active . incident_id, active . epoch, "a" . repeat (64), 3)
      . unwrap ());
    assert! (coordinator . record_presentation_fence (
      &active . incident_id, active . epoch, "b" . repeat (64), 4)
      . unwrap_err () . contains ("another presentation fence"));
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
  fn full_rebuild_has_distinct_preflight_and_exclusive_edges () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (
      MaintenanceOrigin::FullRebuild, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    assert! (coordinator . begin_full_rebuild_observation (
      &active . incident_id, active . epoch) . unwrap ());
    assert! (!coordinator . begin_full_rebuild_observation (
      &active . incident_id, active . epoch) . unwrap ());
    assert! (coordinator . record_observed_candidate (
      &active . incident_id, active . epoch, candidate ()) . unwrap ());
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "evidence" . into (), bundle_sha256: "server" . into (),
        artifact_count: 1, total_file_bytes: 2,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::FullRebuildExclusive) . unwrap ();
    assert! (!coordinator . state . policy () . queries_allowed);
    coordinator . store_rebuilt (
      &active . incident_id, active . epoch, SelectedStoreRecord {
        graph_generation: GraphGeneration::INITIAL . successor (),
        manifest_revision: ManifestRevision::INITIAL . successor (),
        tantivy_generation: 0,
        tantivy_outcome: "synchronous-full-rebuild" . into (),
      }) . unwrap ();
    let CoordinatorState::Active (rebuilt) = &coordinator . state else {
      panic! ("full rebuild stopped being active"); };
    assert_eq! (rebuilt . phase, MaintenancePhase::Presenting);
    assert_eq! (rebuilt . selected_store . as_ref () . unwrap ()
      . tantivy_outcome, "synchronous-full-rebuild");
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
  fn replacement_abandons_but_never_adopts_prearchive_maintenance () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_for_client (
      MaintenanceOrigin::ExplicitPartialReload, None, "owner" . into ())
      . unwrap ();
    coordinator . disconnected ();
    assert_eq! (
      coordinator . reconnected_for_session ("replacement"),
      Some ((active . incident_id, "explicit-partial-reload" . into ())));
    assert! (matches! (coordinator . state, CoordinatorState::Idle));
  }

  #[test]
  fn owner_resumes_and_archived_incidents_remain_adoptable () {
    let mut owner = MaintenanceCoordinator::new ();
    owner . begin_for_client (
      MaintenanceOrigin::ExplicitPartialReload, None, "owner" . into ())
      . unwrap ();
    owner . disconnected ();
    assert_eq! (owner . reconnected_for_session ("owner"), None);
    assert! (matches! (owner . state, CoordinatorState::Active (_)));

    let mut archived = MaintenanceCoordinator::new ();
    let active = archived . begin_for_client (
      MaintenanceOrigin::ExplicitPartialReload, None, "owner" . into ())
      . unwrap ();
    archived . archive_ready (
      &active . incident_id, active . epoch, "initial" . into ()) . unwrap ();
    archived . disconnected ();
    assert_eq! (archived . reconnected_for_session ("replacement"), None);
    let CoordinatorState::Active (retained) = &archived . state else {
      panic! ("archived incident was abandoned"); };
    assert_eq! (retained . controlling_session_id (), "owner");
  }

  #[test]
  fn replacement_gets_a_fresh_offer_for_an_abandoned_candidate () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let candidate = candidate ();
    coordinator . set_pending_valid (candidate . clone ()) . unwrap ();
    coordinator . begin_for_client (
      MaintenanceOrigin::PendingReconciliation, Some (candidate . clone ()),
      "owner" . into ()) . unwrap ();
    coordinator . disconnected ();
    assert! (coordinator . reconnected_for_session ("replacement") . is_some ());
    let CoordinatorState::Pending (pending) = &coordinator . state else {
      panic! ("abandoned candidate was not returned to pending"); };
    assert_eq! (pending . candidate . as_ref (), Some (&candidate));
    assert! (!pending . offer_sent);
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

  #[test]
  fn explicit_partial_reload_freezes_one_normalized_target_set () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      GraphGeneration::INITIAL, ManifestRevision::INITIAL, Vec::new (),
      MaintenanceTargets {
        paths: vec!["z.skg" . into (), "a.skg" . into (), "z.skg" . into ()],
        ids: vec!["extra" . into (), "node" . into (), "extra" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    assert_eq! (active . targets . paths, vec!["a.skg", "z.skg"]);
    assert_eq! (active . targets . ids, vec!["extra", "node"]);

    let mut empty = MaintenanceCoordinator::new ();
    assert! (empty . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      GraphGeneration::INITIAL, ManifestRevision::INITIAL, Vec::new (),
      MaintenanceTargets::default ()) . is_err ());
    let mut pull = MaintenanceCoordinator::new ();
    assert! (pull . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::Pull, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      GraphGeneration::INITIAL, ManifestRevision::INITIAL, Vec::new (),
      MaintenanceTargets {
        paths: Vec::new (), ids: vec!["not-a-pull-target" . into ()],
        pull_repositories: std::collections::BTreeMap::from ([
          (pull_repository_key (&["source" . into ()]),
           vec!["source" . into ()])]),
      }) . is_err ());
    let mut missing = MaintenanceCoordinator::new ();
    assert! (missing . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::Pull, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      GraphGeneration::INITIAL, ManifestRevision::INITIAL, Vec::new (),
      MaintenanceTargets::default ()) . is_err ());
    let mut false_key = MaintenanceCoordinator::new ();
    assert! (false_key . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::Pull, None,
      "client-session" . into (), "emacs" . into (), "all" . into (),
      GraphGeneration::INITIAL, ManifestRevision::INITIAL, Vec::new (),
      MaintenanceTargets {
        pull_repositories: std::collections::BTreeMap::from ([
          ("not-the-source-hash" . into (), vec!["source" . into ()])]),
        ..MaintenanceTargets::default ()
      }) . is_err ());
  }

  #[test]
  fn exact_target_observation_attaches_one_candidate_after_archive () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, Vec::new (), MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    assert! (coordinator . begin_target_observation (
      &active . incident_id, active . epoch) . unwrap ());
    assert! (!coordinator . begin_target_observation (
      &active . incident_id, active . epoch) . unwrap ());
    let outcomes = vec![MaintenanceIdOutcome {
      requested_id: "node" . into (), pid: Some ("node" . into ()),
      reason: None, paths: vec!["/source/node.skg" . into ()],
    }];
    assert! (coordinator . record_requested_id_outcomes (
      &active . incident_id, active . epoch, outcomes . clone ()) . unwrap ());
    assert! (!coordinator . record_requested_id_outcomes (
      &active . incident_id, active . epoch, outcomes . clone ()) . unwrap ());
    let sequence = coordinator . defer_ordinary_observation () . unwrap ();
    let CoordinatorState::Active (deferred) = &coordinator . state else {
      panic! ("deferred incident stopped being active"); };
    assert! (deferred . successor_observation_required);
    let observed = CandidateSummary {
      id: CandidateId::new (),
      base_graph_generation: GraphGeneration::INITIAL,
      base_manifest_revision: ManifestRevision::INITIAL,
      covered_sequence: sequence,
      changed_primary_ids: vec!["node" . into ()],
    };
    assert! (coordinator . record_observed_candidate (
      &active . incident_id, active . epoch, observed . clone ()) . unwrap ());
    assert! (!coordinator . record_observed_candidate (
      &active . incident_id, active . epoch, observed . clone ()) . unwrap ());
    let CoordinatorState::Active (recorded) = &coordinator . state else {
      panic! ("incident stopped being active"); };
    assert_eq! (recorded . phase, MaintenancePhase::ArchiveReady);
    assert_eq! (recorded . candidate, Some (observed));
    assert_eq! (recorded . requested_id_outcomes, outcomes);
    assert! (!recorded . successor_observation_required);
  }

  #[test]
  fn preselection_supersession_retains_the_incident_for_exact_reobservation () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let first = candidate ();
    coordinator . set_pending_valid (first . clone ()) . unwrap ();
    let active = coordinator . begin (
      MaintenanceOrigin::PendingReconciliation, Some (first)) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    coordinator . record_server_evidence (
      &active . incident_id, active . epoch, ServerEvidenceRecord {
        path: "first-evidence" . into (), bundle_sha256: "first" . into (),
        artifact_count: 1, total_file_bytes: 1,
      }) . unwrap ();
    coordinator . transition (
      &active . incident_id, active . epoch,
      MaintenancePhase::SelectingPartial) . unwrap ();
    coordinator . selection_superseded (
      &active . incident_id, active . epoch, "disk advanced" . into ())
      . unwrap ();
    let CoordinatorState::Active (retrying) = &coordinator . state else {
      panic! ("superseded incident stopped being active"); };
    assert_eq! (retrying . phase, MaintenancePhase::FinalObservation);
    assert! (retrying . candidate . is_none ());
    assert! (retrying . server_evidence . is_none ());
    assert! (retrying . successor_observation_required);

    let sequence = coordinator . next_observation_sequence ();
    let replacement = CandidateSummary {
      id: CandidateId::new (), covered_sequence: sequence,
      ..candidate ()
    };
    assert! (coordinator . record_observed_candidate (
      &active . incident_id, active . epoch, replacement . clone ())
      . unwrap ());
    let CoordinatorState::Active (ready) = &coordinator . state else {
      panic! ("replacement candidate stopped being active"); };
    assert_eq! (ready . candidate, Some (replacement));
    assert_eq! (ready . phase, MaintenancePhase::ArchiveReady);
    assert! (!ready . successor_observation_required);
  }

  #[test]
  fn pull_journals_authorization_and_every_exit_before_final_observation () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin (MaintenanceOrigin::Pull, None) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    assert! (coordinator . authorize_external_mutation (
      &active . incident_id, active . epoch) . unwrap ());
    assert! (!coordinator . authorize_external_mutation (
      &active . incident_id, active . epoch) . unwrap ());
    let record = ExternalMutationRecord {
      outcome: ExternalMutationOutcome::Failed,
      details: vec!["repo-a exited 1" . into ()],
    };
    assert! (coordinator . external_mutation_finished (
      &active . incident_id, active . epoch, record . clone ()) . unwrap ());
    assert! (!coordinator . external_mutation_finished (
      &active . incident_id, active . epoch, record . clone ()) . unwrap ());
    let changed = ExternalMutationRecord {
      outcome: ExternalMutationOutcome::Completed,
      details: Vec::new (),
    };
    assert! (coordinator . external_mutation_finished (
      &active . incident_id, active . epoch, changed) . is_err ());
    let CoordinatorState::Active (retained) = &coordinator . state else {
      panic! ("pull incident stopped being active"); };
    assert_eq! (retained . phase, MaintenancePhase::FinalObservation);
    assert_eq! (retained . external_mutation, Some (record));
  }

  #[test]
  fn invalid_disk_cannot_overwrite_a_later_store_health_failure () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, Vec::new (), MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "manifest" . into ()) . unwrap ();
    coordinator . block_store_health (
      &active . incident_id, active . epoch, "index failed" . into ())
      . unwrap ();
    assert! (coordinator . block_invalid_disk (
      &active . incident_id, active . epoch, "generic worker error" . into ())
      . is_err ());
    let CoordinatorState::Active (recorded) = &coordinator . state else {
      panic! ("incident stopped being active"); };
    assert_eq! (recorded . phase, MaintenancePhase::BlockedStoreHealth);
    assert_eq! (recorded . blocking_reason . as_deref (), Some ("index failed"));
  }

  fn settlement (id : &str) -> ViewSettlementRecord {
    ViewSettlementRecord {
      buffer_id: id . into (),
      buffer_key: Some (format! ("{}_key", id)),
      kind: BufferKind::ContentView,
      view_uri: Some (format! ("uri-{}", id)),
      origin_buffer_id: None,
      origin_view_uri: None,
      origin_application_token: None,
      origin_location: None,
      dirty: true,
      impacted: true,
      parse_uncertain: false,
      uncertainty_reason: None,
      observed_ids: vec!["node" . into ()],
      resolved_primary_ids: vec!["node" . into ()],
      base_graph_generation: 1,
      base_presentation_generation: 0,
      base_server_revision: 4,
      base_application_token: 9,
      planned_disposition: ViewDisposition::Interrupted,
      requirement: ViewSettlementRequirement::RetirementAck,
      application: None,
      resolution: ViewSettlementResolution::Pending,
      acknowledged: false,
    }
  }

  #[test]
  fn exact_view_settlement_inventory_and_acks_gate_finalization () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let frozen = |id : &str| FrozenBufferRecord {
      buffer_id: id . into (), kind: BufferKind::ContentView,
      lifecycle: "live-view" . into (), disposable: false,
      continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
      origin_buffer_id: None, origin_view_uri: None,
      origin_application_token: None, origin_location: None,
      source_set: "all" . into (),
      view_uri: Some (format! ("uri-{}", id)), graph_generation: 1,
      presentation_generation: 0, server_revision: 4,
      application_token: 9, dirty: true, logical_dirty: false,
      undo_required: false, maintenance_epoch: None,
      presentation_stale: false, search_stale: false, herald_bearing: false,
      last_fetched_sha256: "a" . repeat (64),
      current_sha256: "b" . repeat (64),
    };
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![frozen ("one"), frozen ("two")],
      MaintenanceTargets { paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default () })
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
      ViewSettlementRequirement::RetirementAck, Some ("wrong"), 1, 0,
      4, 9, None)
      . is_err ());
    assert! (!coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("uri-one"), 1, 0,
      4, 9, None)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "two",
      ViewSettlementRequirement::RetirementAck, Some ("uri-two"), 1, 0,
      4, 9, None)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("uri-one"), 1, 0,
      4, 9, None)
      . unwrap ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "one",
      ViewSettlementRequirement::RetirementAck, Some ("wrong"), 1, 0,
      4, 9, None)
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

  #[test]
  fn application_settlement_acknowledges_only_the_exact_staged_offer () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![FrozenBufferRecord {
        buffer_id: "view" . into (), kind: BufferKind::ContentView,
        lifecycle: "live-view" . into (), disposable: false,
        continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
        origin_buffer_id: None, origin_view_uri: None,
        origin_application_token: None, origin_location: None,
        source_set: "all" . into (),
        view_uri: Some ("uri-view" . into ()), graph_generation: 1,
        presentation_generation: 0, server_revision: 4,
        application_token: 9, dirty: false, logical_dirty: false,
        undo_required: false, maintenance_epoch: None,
        presentation_stale: false, search_stale: false, herald_bearing: false,
        last_fetched_sha256: "a" . repeat (64),
        current_sha256: "a" . repeat (64),
      }], MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    let CoordinatorState::Active (state) = &mut coordinator . state else {
      unreachable! () };
    state . phase = MaintenancePhase::Presenting;
    state . selected_store = Some (SelectedStoreRecord {
      graph_generation: GraphGeneration::INITIAL . successor (),
      manifest_revision: ManifestRevision::INITIAL . successor (),
      tantivy_generation: 1,
      tantivy_outcome: "committed" . into (),
    });
    let mut record = settlement ("view");
    record . dirty = false;
    record . planned_disposition = ViewDisposition::Refreshed;
    record . requirement = ViewSettlementRequirement::ApplicationAck;
    record . application = Some (ViewApplicationRecord {
      content: "* rendered\n" . into (),
      content_sha256: "b" . repeat (64),
      resulting_graph_generation: 2,
      resulting_presentation_generation: 3,
      resulting_server_revision: 5,
      resulting_application_token: 10,
      warnings: Vec::new (),
    });
    coordinator . record_view_settlements (
      &active . incident_id, active . epoch, vec![record]) . unwrap ();
    let exact = ViewApplicationAcknowledgement {
      content_sha256: "b" . repeat (64),
      resulting_graph_generation: 2,
      resulting_presentation_generation: 3,
      resulting_server_revision: 5,
      resulting_application_token: 10,
    };
    let mut changed = exact . clone ();
    changed . resulting_application_token = 11;
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "view",
      ViewSettlementRequirement::ApplicationAck, Some ("uri-view"), 1, 0,
      4, 9,
      Some (&changed)) . is_err ());
    let mut census_coordinator = coordinator . clone ();
    assert! (census_coordinator . acknowledge_view_application_from_census (
      "view", &exact) . unwrap ());
    let CoordinatorState::Active (census_active) = &census_coordinator . state
      else { unreachable! () };
    assert_eq! (census_active . view_settlements["view"] . resolution,
      ViewSettlementResolution::CensusApplied);
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "view",
      ViewSettlementRequirement::ApplicationAck, Some ("uri-view"), 1, 0,
      4, 9,
      Some (&exact)) . unwrap ());
    assert! (coordinator . acknowledge_view_application_from_census (
      "view", &exact) . is_err ());
    assert! (coordinator . acknowledge_view_settlement (
      &active . incident_id, active . epoch, "view",
      ViewSettlementRequirement::ApplicationAck, Some ("uri-view"), 1, 0,
      4, 9,
      Some (&exact)) . unwrap ());
  }

  #[test]
  fn complete_census_explicitly_resolves_absent_application_debt () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_with_archive_contract_and_targets (
      MaintenanceOrigin::ExplicitPartialReload, None, "session" . into (),
      "emacs" . into (), "all" . into (), GraphGeneration::INITIAL,
      ManifestRevision::INITIAL, vec![FrozenBufferRecord {
        buffer_id: "view" . into (), kind: BufferKind::ContentView,
        lifecycle: "live-view" . into (), disposable: false,
        continuation_id: None, recipe: "()" . into (), root_ids: Vec::new (),
        origin_buffer_id: None, origin_view_uri: None,
        origin_application_token: None, origin_location: None,
        source_set: "all" . into (), view_uri: Some ("uri-view" . into ()),
        graph_generation: 1, presentation_generation: 0, server_revision: 4,
        application_token: 9, dirty: false, logical_dirty: false,
        undo_required: false, maintenance_epoch: None,
        presentation_stale: false, search_stale: false, herald_bearing: false,
        last_fetched_sha256: "a" . repeat (64),
        current_sha256: "a" . repeat (64),
      }], MaintenanceTargets {
        paths: Vec::new (), ids: vec!["node" . into ()],
        ..MaintenanceTargets::default ()
      }) . unwrap ();
    let CoordinatorState::Active (state) = &mut coordinator . state else {
      unreachable! () };
    state . phase = MaintenancePhase::Presenting;
    let mut record = settlement ("view");
    record . dirty = false;
    record . planned_disposition = ViewDisposition::Refreshed;
    record . requirement = ViewSettlementRequirement::ApplicationAck;
    record . application = Some (ViewApplicationRecord {
      content: "* rendered\n" . into (),
      content_sha256: "b" . repeat (64),
      resulting_graph_generation: 2,
      resulting_presentation_generation: 3,
      resulting_server_revision: 5,
      resulting_application_token: 10,
      warnings: Vec::new (),
    });
    coordinator . record_view_settlements (
      &active . incident_id, active . epoch, vec![record]) . unwrap ();
    assert_eq! (coordinator . reconcile_absent_view_settlements (
      &["view" . into ()] . into_iter () . collect ()) . unwrap (),
      Vec::<String>::new ());
    assert_eq! (coordinator . reconcile_absent_view_settlements (
      &BTreeSet::new ()) . unwrap (), vec!["view"]);
    let CoordinatorState::Active (state) = &coordinator . state else {
      unreachable! () };
    let resolved = &state . view_settlements["view"];
    assert_eq! (resolved . resolution,
      ViewSettlementResolution::CensusAbsent);
    assert! (resolved . acknowledged && resolved . application . is_some ());
    assert_eq! (state . phase, MaintenancePhase::FinalizingArchive);
    assert_eq! (coordinator . reconcile_absent_view_settlements (
      &BTreeSet::new ()) . unwrap (), Vec::<String>::new ());
  }

  #[test]
  fn replacement_controller_never_rewrites_archive_provenance () {
    let mut coordinator = MaintenanceCoordinator::new ();
    let active = coordinator . begin_for_client (
      MaintenanceOrigin::ExplicitPartialReload, None, "archive-writer" . into ())
      . unwrap ();
    assert! (coordinator . adopt_attached_session ("replacement") . is_err ());
    coordinator . archive_ready (
      &active . incident_id, active . epoch, "initial-sha" . into ()) . unwrap ();
    assert! (coordinator . adopt_attached_session ("replacement") . unwrap ());
    assert! (!coordinator . adopt_attached_session ("replacement") . unwrap ());
    let CoordinatorState::Active (active) = &coordinator . state else {
      unreachable! () };
    assert_eq! (active . archive_owner_session_id, "archive-writer");
    assert_eq! (active . controlling_session_id (), "replacement");
    assert_eq! (active . initial_archive_manifest_sha256 . as_deref (),
      Some ("initial-sha"));
  }
}
