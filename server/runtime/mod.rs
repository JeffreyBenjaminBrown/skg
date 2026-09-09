pub mod generation_gate;
pub mod interactive_session;
mod maintenance;
mod incident_recovery;
mod owner;
pub(crate) mod query_waits;
pub(crate) mod save_operations;
pub use save_operations::{recover_source_effects_before_startup, commit_recovered_source_effects};

use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::evidence::MaintenanceEvidenceStore;
use crate::maintenance::{
  CandidateId,
  MaintenanceCoordinator,
};
use crate::maintenance::candidate::ObservedDiskCandidate;
use crate::maintenance::archive::VerifiedInitialArchive;
use crate::maintenance::observation::ObservationService;
use crate::runtime::interactive_session::InteractiveSession;
use crate::runtime::owner::{CoordinatorOwner, MutationStage};
pub(crate) use owner::MutationControl;
use crate::types::env::{GraphReadSnapshot, SkgEnv};
use crate::types::store_state::{SelectedStoreState, GraphGeneration, ManifestRevision};

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex, MutexGuard};

#[derive(Clone)]
pub struct SelectedRuntimeSnapshot {
  pub env      : SkgEnv,
  pub selected : Arc<SelectedStoreState>,
}

impl SelectedRuntimeSnapshot {
  fn from_env (env : &SkgEnv) -> Self {
    let pinned : SkgEnv = env . pinned ();
    let selected : Arc<SelectedStoreState> = pinned . in_rust_graph . load_full ();
    Self { env: pinned, selected }
  }
}

pub struct RuntimeQueryLease {
  pub snapshot : Arc<SelectedRuntimeSnapshot>,
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
  server_session_id     : String,
  writer_env            : Mutex<SkgEnv>,
  pub interactive       : Mutex<InteractiveSession>,
  owner                 : CoordinatorOwner,
  pub maintenance_evidence : MaintenanceEvidenceStore,
  pub interactive_slot  : InteractiveConnectionSlot,
  candidates            : Mutex<BTreeMap<CandidateId, Arc<ObservedDiskCandidate>>>,
  verified_archives     : Mutex<BTreeMap<crate::maintenance::IncidentId,
                                         Arc<VerifiedInitialArchive>>>,
  incident_snapshots    : Mutex<BTreeMap<crate::maintenance::IncidentId,
                                         Arc<GraphReadSnapshot>>>,
  observation           : Mutex<Option<ObservationService>>,
}

impl ServerRuntime {
  pub fn new (mut env : SkgEnv) -> Result<Self, String> {
    save_operations::require_resolved_startup_saves (&env . config)?;
    env . searcher = env . tantivy_index . reader . searcher ();
    let initial = env . in_rust_graph . load_full ();
    if initial . searcher . is_none () {
      env . in_rust_graph . store (Arc::new (
        (*initial) . clone () . with_searcher (env . searcher . clone ()))); }
    let snapshot = SelectedRuntimeSnapshot::from_env (&env);
    let mut interactive = InteractiveSession::new (&env . config)?;
    if let Err (error) = interactive . collateral_scheduler . seed_presentation (&env) {
      tracing::warn! (%error, "could not seed Git presentation signature"); }
    let maintenance_journal =
      MaintenanceJournalStore::for_config (&env . config . config_path);
    let maintenance = maintenance_journal . load () . require_authority ()?
      . map (|loaded| loaded . coordinator)
      . unwrap_or_else (MaintenanceCoordinator::new);
    let maintenance_evidence = MaintenanceEvidenceStore::alongside (
      &maintenance_journal);
    Ok (Self {
      server_session_id: uuid::Uuid::new_v4 () . to_string (),
      writer_env: Mutex::new (env),
      interactive: Mutex::new (interactive),
      owner: CoordinatorOwner::start_with_snapshot (
        maintenance, maintenance_journal . clone (), Arc::new (snapshot)),
      maintenance_evidence,
      interactive_slot: InteractiveConnectionSlot::new (),
      candidates: Mutex::new (BTreeMap::new ()),
      verified_archives: Mutex::new (BTreeMap::new ()),
      incident_snapshots: Mutex::new (BTreeMap::new ()),
      observation: Mutex::new (None),
    })
  }

  pub fn selected_snapshot (&self) -> Arc<SelectedRuntimeSnapshot> {
    self . owner . selected_snapshot ()
      . expect ("live runtime starts with a selected graph/search pair") }

  pub fn server_session_id (&self) -> &str { &self . server_session_id }

  pub(crate) fn validate_session_authority (
    &self,
    request : &str,
  ) -> Result<(), String> {
    let claimed : String = crate::serve::util::value_from_request_sexp (
      "server-session-id", request)?;
    if claimed != self . server_session_id {
      return Err ("this buffer belongs to an earlier server session; preserve its text and open a fresh live view" . into ()); }
    Ok (( ))
  }

  pub fn maintenance_snapshot (&self) -> MaintenanceCoordinator {
    self . owner . snapshot () }

  pub(crate) fn publication (
    &self,
  ) -> (u64, Arc<SelectedRuntimeSnapshot>, MaintenanceCoordinator, Option<String>) {
    self . owner . publication ()
  }

  pub fn authority_failure (&self) -> Option<String> {
    self . owner . failure () . or_else (|| self . owner . mutation_status ()
      . and_then (|status| status . blocked_reason)) }

  pub fn query_lease (&self) -> Result<RuntimeQueryLease, String> {
    Ok (RuntimeQueryLease { snapshot: self . selected_snapshot () }) }

  pub(crate) fn reserve_mutation (
    &self,
    operation_id : impl Into<String>,
    graph_generation : GraphGeneration,
    manifest_revision : ManifestRevision,
  ) -> Result<MutationControl, String> {
    let token = self . owner . reserve_mutation (
      operation_id, graph_generation, manifest_revision)?;
    Ok (self . owner . mutation_control (&token)) }

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

  pub(crate) fn publish_selected_from_env (
    &self,
    control : &MutationControl,
    env : &SkgEnv,
  ) -> Result<(), String> {
    control . publish (Arc::new (SelectedRuntimeSnapshot::from_env (env))) }

  /// One reservation spans preparation, authorized effects and publication.
  /// The decision owner remains responsive while this adapter runs its worker.
  pub(crate) fn with_store_transition<T> (
    &self,
    operation_id : String,
    function : impl FnOnce (&mut SkgEnv, &mut InteractiveSession, &MutationControl) -> T,
  ) -> Result<T, String> {
    let before = self . selected_snapshot ();
    let control = self . reserve_mutation (operation_id,
      before . selected . graph_generation, before . selected . manifest_revision)?;
    let mut env = match self . writer_env . lock () {
      Ok (env) => env,
      Err (_) => {
        control . finish ()?;
        return Err ("writer environment poisoned" . into ()); }
    };
    let mut interactive = match self . interactive . lock () {
      Ok (interactive) => interactive,
      Err (_) => {
        control . finish ()?;
        return Err ("interactive session poisoned" . into ()); }
    };
    let result = function (&mut env, &mut interactive, &control);
    if let Some (searcher) = &env . in_rust_graph . load_full () . searcher {
      env . searcher = searcher . clone (); }
    let status = self . owner . mutation_status ()
      . ok_or ("mutation lost its owner reservation")?;
    if let Some (reason) = status . blocked_reason { return Err (reason); }
    if status . stage == MutationStage::Authorized {
      self . publish_selected_from_env (&control, &env)?;
    } else if status . stage == MutationStage::Prepared
        && !Arc::ptr_eq (&before . selected, &env . in_rust_graph . load_full ())
    {
      let reason = "worker changed selected stores without owner authorization";
      control . block (reason)?;
      return Err (reason . into ());
    }
    control . finish ()?;
    Ok (result)
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
  fn second_interactive_attachment_is_refused_until_drop () {
    let slot = InteractiveConnectionSlot::new ();
    let first = slot . try_attach () . unwrap ();
    assert! (slot . try_attach () . is_err ());
    drop (first);
    assert! (slot . try_attach () . is_ok ());
  }
}
