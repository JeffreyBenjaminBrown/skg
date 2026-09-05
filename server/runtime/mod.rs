pub mod generation_gate;
pub mod interactive_session;

use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::evidence::MaintenanceEvidenceStore;
use crate::maintenance::{
  CandidateId,
  CoordinatorState,
  MaintenanceCoordinator,
  QueuedObservationReason,
};
use crate::maintenance::candidate::ObservedDiskCandidate;
use crate::maintenance::archive::VerifiedInitialArchive;
use crate::maintenance::observation::ObservationService;
use crate::runtime::generation_gate::{GenerationGate, QueryLease};
use crate::runtime::interactive_session::InteractiveSession;
use crate::types::env::SkgEnv;
use crate::types::store_state::SelectedStoreState;

use arc_swap::ArcSwap;
use std::collections::BTreeMap;
use std::path::PathBuf;
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Clone)]
pub struct SelectedRuntimeSnapshot {
  pub env      : SkgEnv,
  pub selected : Arc<SelectedStoreState>,
}

impl SelectedRuntimeSnapshot {
  fn from_env (env : &SkgEnv) -> Self {
    let selected = env . in_rust_graph . load_full ();
    let mut pinned_env = env . clone ();
    pinned_env . in_rust_graph = Arc::new (
      ArcSwap::from (selected . clone ()));
    Self { env: pinned_env, selected }
  }
}

pub struct RuntimeQueryLease {
  pub snapshot : Arc<SelectedRuntimeSnapshot>,
  _gate_lease  : QueryLease,
}

#[derive(Debug)]
struct InteractiveConnectionState {
  active_connection : Option<String>,
}

#[derive(Clone, Debug)]
pub struct InteractiveConnectionSlot {
  state : Arc<Mutex<InteractiveConnectionState>>,
}

#[derive(Debug)]
pub struct InteractiveConnectionGuard {
  slot          : InteractiveConnectionSlot,
  connection_id : String,
}

pub struct ServerRuntime {
  selected              : ArcSwap<SelectedRuntimeSnapshot>,
  writer_env            : Mutex<SkgEnv>,
  pub generation_gate   : GenerationGate,
  pub interactive       : Mutex<InteractiveSession>,
  pub maintenance       : Mutex<MaintenanceCoordinator>,
  pub maintenance_journal : MaintenanceJournalStore,
  pub maintenance_evidence : MaintenanceEvidenceStore,
  pub interactive_slot  : InteractiveConnectionSlot,
  candidates            : Mutex<BTreeMap<CandidateId, Arc<ObservedDiskCandidate>>>,
  verified_archives     : Mutex<BTreeMap<crate::maintenance::IncidentId,
                                         Arc<VerifiedInitialArchive>>>,
  observation           : Mutex<Option<ObservationService>>,
}

impl ServerRuntime {
  pub fn new (env : SkgEnv) -> Result<Self, String> {
    let snapshot = SelectedRuntimeSnapshot::from_env (&env);
    let graph_generation = snapshot . selected . graph_generation;
    let mut interactive = InteractiveSession::new (&env . config)?;
    if let Err (error) = interactive . collateral_scheduler . seed_presentation (&env) {
      tracing::warn! (%error, "could not seed Git presentation signature"); }
    let maintenance_journal =
      MaintenanceJournalStore::for_config (&env . config . config_path);
    let maintenance = maintenance_journal . load () . active
      . map (|loaded| loaded . coordinator)
      . unwrap_or_else (MaintenanceCoordinator::new);
    let maintenance_evidence = MaintenanceEvidenceStore::alongside (
      &maintenance_journal);
    Ok (Self {
      selected: ArcSwap::from_pointee (snapshot),
      writer_env: Mutex::new (env),
      generation_gate: GenerationGate::new (graph_generation),
      interactive: Mutex::new (interactive),
      maintenance: Mutex::new (maintenance),
      maintenance_journal,
      maintenance_evidence,
      interactive_slot: InteractiveConnectionSlot::new (),
      candidates: Mutex::new (BTreeMap::new ()),
      verified_archives: Mutex::new (BTreeMap::new ()),
      observation: Mutex::new (None),
    })
  }

  pub fn selected_snapshot (&self) -> Arc<SelectedRuntimeSnapshot> {
    self . selected . load_full () }

  pub fn query_lease (&self) -> Result<RuntimeQueryLease, String> {
    loop {
      let snapshot = self . selected_snapshot ();
      if let Some (gate_lease) = self . generation_gate . acquire_query (
          snapshot . selected . graph_generation)?
      {
        return Ok (RuntimeQueryLease {
          snapshot,
          _gate_lease: gate_lease,
        }); }
    }
  }

  pub fn with_writer_env<T> (
    &self,
    function : impl FnOnce (&mut SkgEnv) -> T,
  ) -> T {
    let mut env = self . writer_env . lock () . unwrap ();
    function (&mut env)
  }

  pub(crate) fn lock_writer_env (&self) -> Result<MutexGuard<'_, SkgEnv>, String> {
    self . writer_env . lock ()
      . map_err (|_| "writer environment poisoned" . to_string ())
  }

  pub(crate) fn publish_selected_from_env (&self, env : &SkgEnv) {
    self . selected . store (Arc::new (
      SelectedRuntimeSnapshot::from_env (env)));
  }

  /// Run one selected-store mutation behind the generation boundary and
  /// publish the resulting environment atomically. Existing handlers still
  /// receive their familiar mutable arguments while ownership lives here.
  pub fn with_store_transition<T> (
    &self,
    exclusive : bool,
    function  : impl FnOnce (&mut SkgEnv, &mut InteractiveSession) -> T,
  ) -> Result<T, String> {
    let before = self . selected_snapshot ();
    let selection = self . generation_gate . begin_selection (
      before . selected . graph_generation, exclusive)?;
    let mut env = self . writer_env . lock ()
      . map_err (|_| "writer environment poisoned" . to_string ())?;
    let mut interactive = self . interactive . lock ()
      . map_err (|_| "interactive session poisoned" . to_string ())?;
    let result = function (&mut env, &mut interactive);
    let after = Arc::new (SelectedRuntimeSnapshot::from_env (&env));
    let after_generation = after . selected . graph_generation;
    self . selected . store (after);
    drop (interactive);
    drop (env);
    if after_generation == before . selected . graph_generation {
      selection . retain_generation ();
    } else {
      selection . publish (after_generation)?; }
    Ok (result)
  }

  /// Publish the environment and exact SelectedStoreState together after a
  /// writer has completed all store work.
  pub fn refresh_selected_snapshot (&self) {
    let env = self . writer_env . lock () . unwrap ();
    self . selected . store (Arc::new (
      SelectedRuntimeSnapshot::from_env (&env)));
  }

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
    let event = queued . map (|refresh|
      crate::runtime::interactive_session::QueuedServerEvent {
        frame_kind: crate::serve::protocol::TcpToClient::RefreshQueued
          . repr_in_client () . into (),
        operation_id: refresh . operation_id (),
        payload: refresh . payload (),
      });
    drop (interactive);
    if let Some (event) = event { self . queue_server_event (event); }
    Ok ((changed, diff_mode_enabled))
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

  pub fn queue_server_event (
    &self,
    event : crate::runtime::interactive_session::QueuedServerEvent,
  ) {
    self . interactive . lock () . unwrap ()
      . queued_server_events . push_back (event);
  }

  pub fn persist_maintenance_state (&self) {
    let coordinator = self . maintenance . lock () . unwrap () . clone ();
    if let Err (error) = self . maintenance_journal . persist (&coordinator) {
      tracing::error! (%error, "could not persist maintenance state"); }
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
  candidates : &mut BTreeMap<CandidateId, T>,
  protected  : Option<&CandidateId>,
  incoming_id : CandidateId,
  incoming   : T,
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

impl InteractiveConnectionSlot {
  fn new () -> Self {
    Self { state: Arc::new (Mutex::new (InteractiveConnectionState {
      active_connection: None,
    })) }
  }

  pub fn try_attach (&self) -> Result<InteractiveConnectionGuard, String> {
    let mut state = self . state . lock ()
      . map_err (|_| "interactive connection guard poisoned" . to_string ())?;
    if state . active_connection . is_some () {
      return Err ("one interactive client is already connected" . into ()); }
    let connection_id = uuid::Uuid::new_v4 () . to_string ();
    state . active_connection = Some (connection_id . clone ());
    Ok (InteractiveConnectionGuard {
      slot: self . clone (), connection_id,
    })
  }

  pub fn attached (&self) -> bool {
    self . state . lock () . unwrap () . active_connection . is_some () }
}

impl Drop for InteractiveConnectionGuard {
  fn drop (&mut self) {
    if let Ok (mut state) = self . slot . state . lock () {
      if state . active_connection . as_deref ()
         == Some (&self . connection_id)
      {
        state . active_connection = None; }}
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

  #[test]
  fn second_interactive_attachment_is_refused_until_drop () {
    let slot = InteractiveConnectionSlot::new ();
    let first = slot . try_attach () . unwrap ();
    assert! (slot . try_attach () . is_err ());
    drop (first);
    assert! (slot . try_attach () . is_ok ());
  }
}
