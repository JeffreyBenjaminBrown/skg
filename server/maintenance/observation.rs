//! Process-owned, low-priority filesystem observation.

use crate::maintenance::candidate::{
  DiskObservation,
  observe_complete_disk,
  observe_targeted_disk,
};
use crate::maintenance::{
  CoordinatorState,
  IncidentId,
  MaintenanceEpoch,
  MaintenanceIdOutcome,
  MaintenanceOrigin,
  MaintenancePhase,
  PendingReason,
  QueuedObservationReason,
};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::QueuedServerEvent;
use crate::serve::protocol::TcpToClient;

use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use sexp::{Atom, Sexp};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::{Weak, mpsc};
use std::thread;
use std::time::Duration;

#[derive(Clone, Debug)]
enum ObservationSignal {
  Paths (Vec<PathBuf>, QueuedObservationReason),
  FullSweep (QueuedObservationReason),
  MaintenanceTargets (IncidentId, MaintenanceEpoch),
  WatcherFailure (String),
}

pub struct ObservationService {
  sender   : mpsc::Sender<ObservationSignal>,
  _watcher : RecommendedWatcher,
}

impl ObservationService {
  pub fn start (
    runtime : Weak<ServerRuntime>,
    sources : Vec<PathBuf>,
  ) -> Result<Self, String> {
    let (sender, receiver) = mpsc::channel ();
    let callback_sender = sender . clone ();
    let mut watcher = RecommendedWatcher::new (
      move |result : notify::Result<Event>| match result {
        Ok (event) => {
          let _ = callback_sender . send (ObservationSignal::Paths (
            event . paths, QueuedObservationReason::FilesystemEvent)); }
        Err (error) => {
          let _ = callback_sender . send (
            ObservationSignal::WatcherFailure (error . to_string ())); }
      },
      Config::default ()) . map_err (|error| error . to_string ())?;
    for source in sources {
      watcher . watch (&source, RecursiveMode::NonRecursive)
        . map_err (|error| format! (
          "could not watch {}: {}", source . display (), error))?;
    }
    thread::Builder::new () . name ("skg-observer" . into ())
      . spawn (move || observation_worker (runtime, receiver))
      . map_err (|error| error . to_string ())?;
    Ok (Self { sender, _watcher: watcher })
  }

  pub fn observe_paths (
    &self,
    paths  : Vec<PathBuf>,
    reason : QueuedObservationReason,
  ) -> Result<(), String> {
    self . sender . send (ObservationSignal::Paths (paths, reason))
      . map_err (|_| "observation worker stopped" . to_string ())
  }

  pub fn full_sweep (
    &self,
    reason : QueuedObservationReason,
  ) -> Result<(), String> {
    self . sender . send (ObservationSignal::FullSweep (reason))
      . map_err (|_| "observation worker stopped" . to_string ())
  }

  pub fn observe_maintenance_targets (
    &self,
    incident : IncidentId,
    epoch    : MaintenanceEpoch,
  ) -> Result<(), String> {
    self . sender . send (ObservationSignal::MaintenanceTargets (
      incident, epoch))
      . map_err (|_| "observation worker stopped" . to_string ())
  }
}

fn observation_worker (
  runtime  : Weak<ServerRuntime>,
  receiver : mpsc::Receiver<ObservationSignal>,
) {
  while let Ok (first) = receiver . recv () {
    let mut paths = Vec::new ();
    let mut reasons = Vec::new ();
    let mut maintenance_jobs = Vec::new ();
    absorb_signal (
      first, &mut paths, &mut reasons, &mut maintenance_jobs);
    while let Ok (signal) = receiver . recv_timeout (
        Duration::from_millis (175))
    {
      absorb_signal (
        signal, &mut paths, &mut reasons, &mut maintenance_jobs); }
    let Some (runtime) = runtime . upgrade () else { return; };
    if !reasons . is_empty () || !paths . is_empty () {
      run_observation (&runtime, paths, reasons); }
    for (incident, epoch) in maintenance_jobs {
      run_target_observation (&runtime, incident, epoch); }
    thread::yield_now ();
  }
}

fn absorb_signal (
  signal  : ObservationSignal,
  paths   : &mut Vec<PathBuf>,
  reasons : &mut Vec<String>,
  maintenance_jobs : &mut Vec<(IncidentId, MaintenanceEpoch)>,
) {
  match signal {
    ObservationSignal::Paths (new_paths, reason) => {
      paths . extend (new_paths);
      reasons . push (reason . label () . into ()); }
    ObservationSignal::FullSweep (reason) =>
      reasons . push (reason . label () . into ()),
    ObservationSignal::MaintenanceTargets (incident, epoch) =>
      maintenance_jobs . push ((incident, epoch)),
    ObservationSignal::WatcherFailure (error) => {
      reasons . push (format! ("watcher failure: {}", error)); }
  }
}

fn run_target_observation (
  runtime  : &ServerRuntime,
  incident : IncidentId,
  epoch    : MaintenanceEpoch,
) {
  enum Failure {
    InvalidDisk (String),
    Operational (String),
  }
  impl From<String> for Failure {
    fn from (error : String) -> Self { Self::Operational (error) }
  }
  impl From<&str> for Failure {
    fn from (error : &str) -> Self { Self::Operational (error . into ()) }
  }

  let result = (|| -> Result<Option<String>, Failure> {
    let active = {
      let coordinator = runtime . maintenance . lock ()
        . map_err (|_| "maintenance coordinator poisoned" . to_string ())?;
      let CoordinatorState::Active (active) = &coordinator . state else {
        return Ok (None); };
      if active . incident_id != incident || active . epoch != epoch
      || active . origin != MaintenanceOrigin::ExplicitPartialReload
      || active . phase != MaintenancePhase::FinalObservation
      {
        return Ok (None); }
      active . clone ()
    };
    let sequence = runtime . transition_maintenance (|coordinator|
      Ok (coordinator . next_observation_sequence ()))?;
    let snapshot = runtime . selected_snapshot ();
    if snapshot . selected . graph_generation != active . g0_graph_generation
    || snapshot . selected . manifest_revision != active . g0_manifest_revision
    {
      return Err ("target observation G0 was superseded" . into ()); }
    let (targets, requested_id_outcomes) = resolve_target_pids (
      &snapshot . env . config, &snapshot . selected . graph,
      &active . targets . paths, &active . targets . ids)?;
    runtime . transition_maintenance (|coordinator|
      coordinator . record_requested_id_outcomes (
        &incident, epoch, requested_id_outcomes . clone ()))?;
    let candidate = match observe_targeted_disk (
      &snapshot . env . config, &snapshot . selected, sequence, &targets)
    {
      DiskObservation::Valid (candidate) => candidate,
      DiskObservation::Invalid { details } => return Err (
        Failure::InvalidDisk (format! (
          "targeted disk is invalid: {}", details . join ("; ")))),
      DiskObservation::Unstable { details } => return Err (
        Failure::InvalidDisk (format! (
          "targeted disk was unstable: {}", details . join ("; ")))),
      DiskObservation::ByteEquivalent
      | DiskObservation::SemanticallyEqual { .. } => return Err (
        Failure::Operational (
          "targeted observation returned a non-candidate result" . into ())),
    };
    runtime . retain_candidate (candidate . clone ());
    runtime . transition_maintenance (|coordinator|
      coordinator . record_observed_candidate (
        &incident, epoch, candidate . summary . clone ()))?;
    let verified = runtime . verified_archive (&incident)
      . ok_or_else (|| "verified initial archive was not retained"
        . to_string ())?;
    crate::serve::handlers::maintenance_protocol::select_and_stage_candidate (
      runtime, &incident, epoch, &verified)
      . map (Some) . map_err (Failure::Operational)
  })();

  let payload = match result {
    Ok (Some (payload)) => payload,
    Ok (None) => return,
    Err (failure) => {
      let error = match failure {
        Failure::InvalidDisk (error) => {
          let _ = runtime . transition_maintenance (|coordinator|
            coordinator . block_invalid_disk (
              &incident, epoch, error . clone ()));
          error
        }
        Failure::Operational (error) => error,
      };
      let phase = match &runtime . maintenance . lock () . unwrap () . state {
        CoordinatorState::Active (active) => active . phase . label (),
        state => state . label (),
      };
      Sexp::List (vec![
        field ("status", "origin-operation-failed"),
        field ("incident-id", incident . as_str ()),
        field ("maintenance-epoch", &epoch . get () . to_string ()),
        field ("phase", phase),
        field ("error", &error),
      ]) . to_string ()
    }
  };
  runtime . queue_server_event (QueuedServerEvent {
    frame_kind: TcpToClient::MaintenanceStatus . repr_in_client () . into (),
    operation_id: format! ("maintenance-origin-{}", incident),
    payload,
  });
}

fn resolve_target_pids (
  config : &crate::types::misc::SkgConfig,
  graph  : &crate::dbs::in_rust_graph::InRustGraph,
  paths  : &[String],
  ids    : &[String],
) -> Result<(BTreeSet<crate::types::misc::ID>, Vec<MaintenanceIdOutcome>), String> {
  let mut result = BTreeSet::new ();
  let mut outcomes = Vec::new ();
  for value in ids {
    let id = crate::types::misc::ID::from (value . as_str ());
    match graph . pid_of (&id) {
      Some (pid) => {
        result . insert (pid . clone ());
        outcomes . push (MaintenanceIdOutcome {
          requested_id: value . clone (),
          pid: Some (pid . to_string ()),
          reason: None,
          paths: possible_target_paths (config, &pid),
        });
      }
      None => outcomes . push (MaintenanceIdOutcome {
        requested_id: value . clone (),
        pid: None,
        reason: Some ("ID is not present in the selected graph" . into ()),
        paths: Vec::new (),
      }),
    }
  }
  for value in paths {
    let path = Path::new (value);
    let absolute = if path . is_absolute () {
      path . to_path_buf ()
    } else {
      config . data_root . join (path)
    };
    let Some ((_, pid)) = config . sources
      . source_and_pid_for_direct_path (&absolute)
    else {
      return Err (format! (
        "partial reload path is not a direct configured .skg file: {}",
        value));
    };
    result . insert (pid);
  }
  Ok ((result, outcomes))
}

fn possible_target_paths (
  config : &crate::types::misc::SkgConfig,
  pid    : &crate::types::misc::ID,
) -> Vec<String> {
  config . ordered_sources () . into_iter () . map (|source| {
    config . sources . get (&source)
      . expect ("ordered source exists") . path
      . join (format! ("{}.skg", pid)) . to_string_lossy () . into_owned ()
  }) . collect ()
}

fn run_observation (
  runtime : &ServerRuntime,
  mut paths : Vec<PathBuf>,
  reasons : Vec<String>,
) {
  paths . sort ();
  paths . dedup ();
  let sequence = {
    let mut coordinator = runtime . maintenance . lock () . unwrap ();
    let sequence = coordinator . next_observation_sequence ();
    let _ = coordinator . observation_started ();
    sequence
  };
  runtime . persist_maintenance_state ();
  let snapshot = runtime . selected_snapshot ();
  let result = observe_complete_disk (
    &snapshot . env . config, &snapshot . selected, sequence);
  let latest = runtime . selected_snapshot ();
  if latest . selected . graph_generation
     != snapshot . selected . graph_generation
  || latest . selected . manifest_revision
     != snapshot . selected . manifest_revision
  {
    let _ = runtime . schedule_full_observation (
      QueuedObservationReason::SelectedGenerationAdvanced);
    return;
  }
  match result {
    DiskObservation::ByteEquivalent => {
      let _ = runtime . maintenance . lock () . unwrap ()
        . observation_equal ();
      runtime . persist_maintenance_state ();
    }
    DiskObservation::SemanticallyEqual { manifest, .. } => {
      match runtime . publish_semantically_equal_manifest (
          snapshot . selected . graph_generation,
          snapshot . selected . manifest_revision,
          manifest)
      {
        Ok (( )) => {
          let _ = runtime . maintenance . lock () . unwrap ()
            . observation_equal ();
          runtime . persist_maintenance_state ();
        }
        Err (error) => {
          let _ = runtime . schedule_full_observation (
            QueuedObservationReason::SelectedGenerationAdvanced);
          tracing::debug! (%error, "semantic no-op observation was superseded");
        }
      }
    }
    DiskObservation::Valid (candidate) => {
      runtime . retain_candidate (candidate . clone ());
      let summary = candidate . summary . clone ();
      if runtime . maintenance . lock () . unwrap ()
        . set_pending_valid (summary . clone ()) . is_ok ()
      {
        runtime . persist_maintenance_state ();
        runtime . queue_server_event (QueuedServerEvent {
          frame_kind: TcpToClient::MaintenanceOffer . repr_in_client () . into (),
          operation_id: format! ("candidate-{}", summary . id),
          payload: candidate_offer_payload (&summary, &reasons, &paths),
        });
      }
    }
    DiskObservation::Invalid { mut details } => {
      details . extend (reasons);
      publish_pending_problem (
        runtime, PendingReason::InvalidDisk, details);
    }
    DiskObservation::Unstable { mut details } => {
      details . extend (reasons);
      publish_pending_problem (
        runtime, PendingReason::UnstableDisk, details);
    }
  }
}

fn publish_pending_problem (
  runtime : &ServerRuntime,
  reason  : PendingReason,
  details : Vec<String>,
) {
  if runtime . maintenance . lock () . unwrap ()
    . set_pending_invalid (reason . clone (), details . clone ()) . is_ok ()
  {
    runtime . persist_maintenance_state ();
    runtime . queue_server_event (QueuedServerEvent {
      frame_kind: TcpToClient::MaintenanceStatus . repr_in_client () . into (),
      operation_id: format! ("observation-{}",
        runtime . maintenance . lock () . unwrap ()
          . observation_sequence . get ()),
      payload: pending_problem_payload (&reason, &details),
    });
  }
}

fn candidate_offer_payload (
  summary : &crate::maintenance::CandidateSummary,
  reasons : &[String],
  paths   : &[PathBuf],
) -> String {
  Sexp::List (vec![
    field ("candidate-id", summary . id . as_str ()),
    field ("base-graph-generation",
      &summary . base_graph_generation . get () . to_string ()),
    field ("base-manifest-revision",
      &summary . base_manifest_revision . get () . to_string ()),
    list_field ("changed-primary-ids", &summary . changed_primary_ids),
    list_field ("reasons", reasons),
    list_field ("observed-paths", &paths . iter ()
      . map (|path| path . to_string_lossy () . into_owned ()) . collect::<Vec<_>> ()),
  ]) . to_string ()
}

fn pending_problem_payload (
  reason  : &PendingReason,
  details : &[String],
) -> String {
  Sexp::List (vec![
    field ("pending-reason", reason . label ()),
    list_field ("details", details),
  ]) . to_string ()
}

fn field (key : &str, value : &str) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::Atom (Atom::S (value . into ())),
  ])
}

fn list_field (key : &str, values : &[String]) -> Sexp {
  Sexp::List (vec![
    Sexp::Atom (Atom::S (key . into ())),
    Sexp::List (values . iter () . map (|value|
      Sexp::Atom (Atom::S (value . clone ()))) . collect ()),
  ])
}
