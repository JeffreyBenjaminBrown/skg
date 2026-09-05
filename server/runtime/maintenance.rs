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
  CoordinatorState,
  MaintenanceCoordinator,
  QueuedObservationReason,
};
use crate::runtime::interactive_session::{
  InteractiveSession,
  QueuedServerEvent,
};
use crate::serve::protocol::TcpToClient;

use std::collections::BTreeMap;
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
    let protected = {
      let coordinator = self . maintenance . lock () . unwrap ();
      journal_candidate_id (&coordinator . state) . cloned ()
    };
    let mut candidates = self . candidates . lock () . unwrap ();
    retain_candidate_entry (&mut candidates, protected . as_ref (),
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
    archives . retain (|id, _| id == &incident);
    archives . insert (incident, Arc::new (archive));
  }

  pub fn verified_archive (
    &self,
    incident : &crate::maintenance::IncidentId,
  ) -> Option<Arc<VerifiedInitialArchive>> {
    self . verified_archives . lock () . unwrap ()
      . get (incident) . cloned ()
  }

  pub fn queue_server_event (&self, event : QueuedServerEvent) {
    self . interactive . lock () . unwrap ()
      . queued_server_events . push_back (event);
  }

  pub fn persist_maintenance_state (&self) {
    let coordinator = self . maintenance . lock () . unwrap () . clone ();
    if let Err (error) = self . maintenance_journal . persist (&coordinator) {
      tracing::error! (%error, "could not persist maintenance state"); }
  }

  /// Persist the server-known half of a view born during maintenance before
  /// its successful query response is put on the wire.
  pub fn enroll_maintenance_view (
    &self,
    uri   : &crate::types::views_state::ViewUri,
    state : &crate::types::views_state::ViewState,
  ) -> Result<bool, String> {
    let enrollment = crate::maintenance::PendingViewEnrollment {
      view_uri: uri . repr_in_client (),
      graph_generation: state . graph_generation,
      presentation_generation: state . presentation_generation,
      server_revision: state . revision,
      application_token: state . client_application_token,
    };
    self . transition_maintenance (|coordinator|
      coordinator . enroll_pending_view (enrollment . clone ()))
  }

  /// Apply one coordinator transition and durably publish it as one critical
  /// section.  A journal failure restores the previous in-memory state, so a
  /// successful protocol response can never describe an unjournaled boundary.
  pub fn transition_maintenance<T> (
    &self,
    transition : impl FnOnce (&mut MaintenanceCoordinator) -> Result<T, String>,
  ) -> Result<T, String> {
    let mut coordinator = self . maintenance . lock ()
      . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
    let before = coordinator . clone ();
    let result = match transition (&mut coordinator) {
      Ok (result) => result,
      Err (error) => {
        *coordinator = before;
        return Err (error); }
    };
    if let Err (error) = self . maintenance_journal . persist (&coordinator) {
      *coordinator = before;
      return Err (format! (
        "maintenance transition could not be journaled: {}", error)); }
    Ok (result)
  }

  pub fn publish_semantically_equal_manifest (
    &self,
    expected_graph_generation : crate::types::store_state::GraphGeneration,
    expected_manifest_revision : crate::types::store_state::ManifestRevision,
    manifest : crate::types::store_state::SelectedPathManifest,
  ) -> Result<(), String> {
    let selection = self . generation_gate . begin_selection (
      expected_graph_generation, false)?;
    let env = self . writer_env . lock ()
      . map_err (|_| "writer environment poisoned" . to_string ())?;
    let current = env . in_rust_graph . load_full ();
    if current . graph_generation != expected_graph_generation
    || current . manifest_revision != expected_manifest_revision
    {
      return Err ("semantic no-op candidate was superseded" . into ()); }
    env . in_rust_graph . store (Arc::new (
      current . with_semantically_equal_manifest (manifest)));
    self . selected . store (Arc::new (
      SelectedRuntimeSnapshot::from_env (&env)));
    drop (env);
    selection . retain_generation ();
    Ok (( ))
  }
}

/// A background observation is allowed to discover successor work while an
/// incident is active.  Its process-local candidate must never evict the exact
/// candidate named by the durable coordinator before that incident has used
/// it.  Unreferenced older observations remain bounded to the newest entry.
fn retain_candidate_entry<T> (
  candidates  : &mut BTreeMap<CandidateId, T>,
  protected   : Option<&CandidateId>,
  incoming_id : CandidateId,
  incoming    : T,
) {
  candidates . retain (|id, _| Some (id) == protected);
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
      &mut candidates, Some (&protected), incoming . clone (), "successor");
    assert_eq! (candidates . len (), 2);
    assert_eq! (candidates . get (&protected), Some (&"active"));
    assert_eq! (candidates . get (&incoming), Some (&"successor"));
    assert! (!candidates . contains_key (&stale));
  }
}
