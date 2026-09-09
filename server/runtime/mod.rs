pub mod generation_gate;
pub mod interactive_session;
mod maintenance;
mod owner;

use crate::maintenance::journal::MaintenanceJournalStore;
use crate::maintenance::evidence::MaintenanceEvidenceStore;
use crate::maintenance::{
  CandidateId,
  MaintenanceCoordinator,
};
use crate::maintenance::candidate::ObservedDiskCandidate;
use crate::maintenance::archive::VerifiedInitialArchive;
use crate::maintenance::observation::ObservationService;
use crate::runtime::generation_gate::{GenerationGate, QueryLease};
use crate::runtime::interactive_session::InteractiveSession;
use crate::runtime::owner::CoordinatorOwner;
use crate::types::env::SkgEnv;
use crate::types::store_state::SelectedStoreState;

use arc_swap::ArcSwap;
use std::collections::BTreeMap;
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
    pinned_env . searcher = selected . searcher . clone ()
      . expect ("a live publication has a matching Searcher");
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
  owner                 : CoordinatorOwner,
  pub maintenance_evidence : MaintenanceEvidenceStore,
  pub interactive_slot  : InteractiveConnectionSlot,
  candidates            : Mutex<BTreeMap<CandidateId, Arc<ObservedDiskCandidate>>>,
  verified_archives     : Mutex<BTreeMap<crate::maintenance::IncidentId,
                                         Arc<VerifiedInitialArchive>>>,
  observation           : Mutex<Option<ObservationService>>,
}

impl ServerRuntime {
  pub fn new (mut env : SkgEnv) -> Result<Self, String> {
    env . searcher = env . tantivy_index . reader . searcher ();
    let initial = env . in_rust_graph . load_full ();
    if initial . searcher . is_none () {
      env . in_rust_graph . store (Arc::new (
        (*initial) . clone () . with_searcher (env . searcher . clone ()))); }
    let snapshot = SelectedRuntimeSnapshot::from_env (&env);
    let graph_generation = snapshot . selected . graph_generation;
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
      selected: ArcSwap::from_pointee (snapshot),
      writer_env: Mutex::new (env),
      generation_gate: GenerationGate::new (graph_generation),
      interactive: Mutex::new (interactive),
      owner: CoordinatorOwner::start (maintenance, maintenance_journal . clone ()),
      maintenance_evidence,
      interactive_slot: InteractiveConnectionSlot::new (),
      candidates: Mutex::new (BTreeMap::new ()),
      verified_archives: Mutex::new (BTreeMap::new ()),
      observation: Mutex::new (None),
    })
  }

  pub fn selected_snapshot (&self) -> Arc<SelectedRuntimeSnapshot> {
    self . selected . load_full () }

  pub fn maintenance_snapshot (&self) -> MaintenanceCoordinator {
    self . owner . snapshot () }

  pub fn authority_failure (&self) -> Option<String> {
    self . owner . failure () }

  pub(crate) fn maintenance_admission_guard (&self)
    -> Result<MutexGuard<'_, ()>, String>
  {
    self . owner . admission_guard () }

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
