//! Runtime integration owned by partial rebuild and durable maintenance.
//!
//! Keeping these methods beside the feature leaves the general runtime module
//! focused on query leases, store publication and interactive connection
//! ownership.  Callers still use 'ServerRuntime'; this is only an ownership
//! boundary for readers of the implementation.

use super::{ServerRuntime, SelectedRuntimeSnapshot};

use crate::maintenance::archive::VerifiedInitialArchive;
use crate::maintenance::candidate::ObservedDiskCandidate;
use crate::maintenance::observation::ObservationService;
use crate::maintenance::{
  CandidateId,
  IncidentId,
  SelectedStoreRecord,
  CoordinatorState,
  MaintenanceCoordinator,
  QueuedObservationReason,
};
use crate::runtime::interactive_session::{
  InteractiveSession,
  QueuedServerEvent,
};
use crate::serve::protocol::TcpToClient;
use crate::types::env::SkgEnv;
use crate::types::views_state::{ViewSaveBase, ViewState};

use std::collections::{BTreeMap, BTreeSet};
use std::path::PathBuf;
use std::sync::Arc;

impl ServerRuntime {
  pub fn start_background_services (self : &Arc<Self>) -> Result<(), String> {
    let snapshot = self . selected_snapshot ();
    let service = ObservationService::start (
      Arc::downgrade (self), &snapshot . env . config)?;
    *self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())? =
      Some (service);
    // Install the service before its first signal.  The initial scan may
    // discover that its base generation was superseded and enqueue a
    // successor scan; that reschedule must never race an uninstalled sender.
    self . schedule_full_observation (QueuedObservationReason::Startup)?;
    Ok (( ))
  }

  pub fn schedule_full_observation (
    &self,
    reason : QueuedObservationReason,
  ) -> Result<(), String> {
    self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())?
      . as_ref ()
      . ok_or_else (|| "observation service is not running" . to_string ())?
      . full_sweep (reason)
  }

  pub fn schedule_path_observation (
    &self,
    paths  : Vec<PathBuf>,
    reason : QueuedObservationReason,
  ) -> Result<(), String> {
    self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())?
      . as_ref ()
      . ok_or_else (|| "observation service is not running" . to_string ())?
      . observe_paths (paths, reason)
  }

  pub fn schedule_maintenance_target_observation (
    &self,
    incident : crate::maintenance::IncidentId,
    epoch    : crate::maintenance::MaintenanceEpoch,
  ) -> Result<(), String> {
    self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())?
      . as_ref ()
      . ok_or_else (|| "observation service is not running" . to_string ())?
      . observe_maintenance_targets (incident, epoch)
  }

  pub fn schedule_maintenance_final_observation (
    &self,
    incident : crate::maintenance::IncidentId,
    epoch    : crate::maintenance::MaintenanceEpoch,
  ) -> Result<(), String> {
    self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())?
      . as_ref ()
      . ok_or_else (|| "observation service is not running" . to_string ())?
      . observe_maintenance_final_disk (incident, epoch)
  }

  pub(crate) fn replace_observation_config (
    &self,
    config : &crate::types::misc::SkgConfig,
  ) -> Result<(), String> {
    self . observation . lock ()
      . map_err (|_| "observation service lock poisoned" . to_string ())?
      . as_mut ()
      . ok_or_else (|| "observation service is not running" . to_string ())?
      . replace_config (config)
  }

  pub fn observe_git_presentation (&self) -> Result<(bool, bool), String> {
    let snapshot = self . selected_snapshot ();
    let mut interactive = self . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let diff_mode_enabled = interactive . views . diff_mode_enabled;
    let InteractiveSession {
      views, active_source_set, collateral_scheduler, ..
    } = &mut *interactive;
    let (changed, queued) = collateral_scheduler . observe_presentation (
      views, &snapshot . env, active_source_set)?;
    let event = queued . map (|refresh| QueuedServerEvent {
      frame_kind: TcpToClient::RefreshQueued . repr_in_client () . into (),
      operation_id: refresh . operation_id (),
      payload: refresh . payload (),
    });
    drop (interactive);
    if let Some (event) = event { self . queue_server_event (event); }
    Ok ((changed, diff_mode_enabled))
  }

  /// Perform an exact Git observation and return the process-retained
  /// generation/signature pair established by it.  Maintenance journals this
  /// beside the candidate observation sequence at the G1 presentation fence.
  pub fn exact_git_presentation_identity (&self) -> Result<(u64, String), String> {
    self . observe_git_presentation ()?;
    let interactive = self . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let generation = interactive . collateral_scheduler
      . presentation_generation ();
    let signature = interactive . collateral_scheduler
      . presentation_signature_hex ()
      . ok_or_else (|| "Git presentation signature is not initialized"
        . to_string ())?;
    Ok ((generation, signature))
  }

  pub fn retain_candidate (&self, candidate : Arc<ObservedDiskCandidate>) {
    let coordinator : MaintenanceCoordinator = self . maintenance_snapshot ();
    let mut protected : BTreeSet<CandidateId> = journal_candidate_id (
      &coordinator . state) . cloned () . into_iter () . collect ();
    for incident in coordinator . incidents () {
      if let Ok (active) = coordinator . incident (&incident . incident_id, incident . epoch) {
        if let Some (candidate) = &active . candidate {
          protected . insert (candidate . id . clone ()); } } }
    let mut candidates = self . candidates . lock () . unwrap ();
    retain_candidate_entry (&mut candidates, &protected,
      candidate . summary . id . clone (), candidate);
  }

  pub fn candidate (&self, id : &CandidateId)
    -> Option<Arc<ObservedDiskCandidate>>
  {
    self . candidates . lock () . unwrap () . get (id) . cloned ()
  }

  pub fn retain_verified_archive (
    &self,
    incident : crate::maintenance::IncidentId,
    archive  : VerifiedInitialArchive,
  ) {
    let mut archives = self . verified_archives . lock () . unwrap ();
    archives . insert (incident, Arc::new (archive));
  }

  pub fn verified_archive (
    &self,
    incident : &crate::maintenance::IncidentId,
  ) -> Option<Arc<VerifiedInitialArchive>> {
    self . verified_archives . lock () . unwrap ()
      . get (incident) . cloned ()
  }

  /// Retain the exact published pair before the graph barrier is lifted.
  /// Report workers can then finish G1 after a newer save publishes G2.
  pub(crate) fn retain_incident_snapshot (
    &self,
    incident : &IncidentId,
    record : &SelectedStoreRecord,
  ) -> Result<(), String> {
    let snapshot : Arc<SelectedRuntimeSnapshot> = self . selected_snapshot ();
    if snapshot . selected . graph_generation != record . graph_generation
    || snapshot . selected . manifest_revision != record . manifest_revision {
      return Err ("incident retention does not match the published pair" . into ()); }
    let mut snapshots = self . incident_snapshots . lock ()
      . map_err (|_| "incident snapshot retention poisoned" . to_string ())?;
    if let Some (retained) = snapshots . get (incident) {
      if !Arc::ptr_eq (&retained . selected, &snapshot . selected) {
        return Err ("incident already retained a different selected pair" . into ()); }
    } else { snapshots . insert (incident . clone (), snapshot); }
    Ok (( ))
  }

  pub(crate) fn incident_snapshot (
    &self,
    incident : &IncidentId,
  ) -> Result<Arc<SelectedRuntimeSnapshot>, String> {
    self . incident_snapshots . lock ()
      . map_err (|_| "incident snapshot retention poisoned" . to_string ())?
      . get (incident) . cloned () . ok_or_else (||
        "incident pair requires evidence reconstruction after restart" . into ())
  }

  pub fn queue_server_event (&self, event : QueuedServerEvent) {
    self . interactive . lock () . unwrap ()
      . queued_server_events . push_back (event);
  }

  /// Constructors running after closure produce results outside the fixed
  /// census. Reopening admission never upgrades those retained view records.
  pub fn admit_view_response (
    &self,
    request : &str,
    env : &SkgEnv,
    state : &mut ViewState,
  ) -> Result<(), String> {
    let requested : String = crate::serve::util::value_from_request_sexp (
      "requested-view-write-authority", request)
      . unwrap_or_else (|_| "editable" . into ());
    if requested != "editable" && requested != "read-only" {
      return Err ("unknown requested view write authority" . into ()); }
    state . writes_admitted = requested == "editable"
      && self . maintenance_snapshot () . state . policy () . skg_saves_allowed
      && self . authority_failure () . is_none ();
    state . retain_save_base (ViewSaveBase::from_env (env, &state . source_set))?;
    Ok (( ))
  }

  /// Propose a pure coordinator transition to the process owner. Its ordered
  /// publisher must complete before this request receives durable success.
  pub fn transition_maintenance<T> (
    &self,
    transition : impl FnMut (&mut MaintenanceCoordinator) -> Result<T, String>,
  ) -> Result<T, String> {
    self . owner . transition (transition)
  }

  pub fn publish_semantically_equal_manifest (
    &self,
    expected_graph_generation : crate::types::store_state::GraphGeneration,
    expected_manifest_revision : crate::types::store_state::ManifestRevision,
    manifest : crate::types::store_state::SelectedPathManifest,
  ) -> Result<(), String> {
    let control = self . reserve_mutation (
      format! ("manifest/{}", uuid::Uuid::new_v4 ()),
      expected_graph_generation, expected_manifest_revision)?;
    let env = match self . writer_env . lock () {
      Ok (env) => env,
      Err (_) => { control . finish ()?;
        return Err ("writer environment poisoned" . into ()); }
    };
    let current = env . in_rust_graph . load_full ();
    if current . graph_generation != expected_graph_generation
    || current . manifest_revision != expected_manifest_revision {
      control . finish ()?;
      return Err ("semantic no-op candidate was superseded" . into ()); }
    control . authorize ()?;
    env . in_rust_graph . store (Arc::new (
      current . with_semantically_equal_manifest (manifest)));
    self . publish_selected_from_env (&control, &env)?;
    control . finish ()?;

    Ok (( ))
  }
}

/// A background observation is allowed to discover successor work while an
/// incident is active.  Its process-local candidate must never evict the exact
/// candidate named by the durable coordinator before that incident has used
/// it.  Unreferenced older observations remain bounded to the newest entry.
fn retain_candidate_entry<T> (
  candidates  : &mut BTreeMap<CandidateId, T>,
  protected   : &BTreeSet<CandidateId>,
  incoming_id : CandidateId,
  incoming    : T,
) {
  candidates . retain (|id, _| protected . contains (id));
  candidates . insert (incoming_id, incoming);
}

fn journal_candidate_id (state : &CoordinatorState) -> Option<&CandidateId> {
  match state {
    CoordinatorState::Pending (pending) =>
      pending . candidate . as_ref () . map (|candidate| &candidate . id),
    CoordinatorState::Active (active) =>
      active . candidate . as_ref () . map (|candidate| &candidate . id),
    _ => None,
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn successor_candidate_does_not_evict_journaled_candidate () {
    let protected = CandidateId::new ();
    let stale = CandidateId::new ();
    let incoming = CandidateId::new ();
    let mut candidates = BTreeMap::from ([
      (protected . clone (), "active"),
      (stale . clone (), "stale"),
    ]);
    retain_candidate_entry (
      &mut candidates, &BTreeSet::from ([protected . clone ()]), incoming . clone (), "successor");
    assert_eq! (candidates . len (), 2);
    assert_eq! (candidates . get (&protected), Some (&"active"));
    assert_eq! (candidates . get (&incoming), Some (&"successor"));
    assert! (!candidates . contains_key (&stale));
  }
}
