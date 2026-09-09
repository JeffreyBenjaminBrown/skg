//! Process-owned, low-priority filesystem observation.

use crate::maintenance::candidate::{
  DiskObservation,
  observe_complete_disk,
  observe_complete_maintenance_disk,
  observe_targeted_disk,
};
use crate::dbs::filesystem::not_nodes::{
  load_config,
  reject_archive_source_overlap,
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
use crate::maintenance::view_impact::plan_invalid_preselection_retirements;
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::QueuedServerEvent;
use crate::serve::protocol::TcpToClient;
use crate::serve::handlers::reload_batch::reload_batch_active;
use crate::types::misc::SkgConfig;

use git2::Repository;
use notify::event::{AccessKind, AccessMode};
use notify::{
  Config, Event, EventKind, RecommendedWatcher, RecursiveMode, Watcher,
};
use sexp::{Atom, Sexp};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::{Weak, mpsc};
use std::thread;
use std::time::{Duration, Instant};

const OBSERVATION_QUIET_INTERVAL : Duration = Duration::from_millis (175);
const OBSERVATION_MAX_BATCH_AGE : Duration = Duration::from_secs (1);

#[derive(Clone, Debug)]
enum ObservationSignal {
  Paths (Vec<PathBuf>, QueuedObservationReason),
  FullSweep (QueuedObservationReason),
  MaintenanceTargets (IncidentId, MaintenanceEpoch),
  MaintenanceFinalDisk (IncidentId, MaintenanceEpoch),
  GitPresentation,
  WatcherFailure (String),
}

pub struct ObservationService {
  sender                : mpsc::Sender<ObservationSignal>,
  _source_watcher       : RecommendedWatcher,
  _presentation_watcher : RecommendedWatcher,
}

impl ObservationService {
  pub fn start (
    runtime : Weak<ServerRuntime>,
    config  : &SkgConfig,
  ) -> Result<Self, String> {
    let (sender, receiver) = mpsc::channel ();
    let source_watcher = configured_source_watcher (&sender, config)?;
    let presentation_watcher = configured_presentation_watcher (
      &sender, config)?;
    thread::Builder::new () . name ("skg-observer" . into ())
      . spawn (move || observation_worker (runtime, receiver))
      . map_err (|error| error . to_string ())?;
    Ok (Self {
      sender,
      _source_watcher: source_watcher,
      _presentation_watcher: presentation_watcher,
    })
  }

  /// Prepare every new watch before dropping the old watcher.  Events from
  /// the brief overlap share this worker queue and are harmlessly coalesced;
  /// a setup failure leaves the complete old watch set installed.
  pub fn replace_config (&mut self, config : &SkgConfig)
    -> Result<(), String>
  {
    let source_watcher = configured_source_watcher (&self . sender, config)?;
    let presentation_watcher = configured_presentation_watcher (
      &self . sender, config)?;
    self . _source_watcher = source_watcher;
    self . _presentation_watcher = presentation_watcher;
    Ok (( ))
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

  pub fn observe_maintenance_final_disk (
    &self,
    incident : IncidentId,
    epoch    : MaintenanceEpoch,
  ) -> Result<(), String> {
    self . sender . send (ObservationSignal::MaintenanceFinalDisk (
      incident, epoch))
      . map_err (|_| "observation worker stopped" . to_string ())
  }
}

fn configured_source_watcher (
  sender  : &mpsc::Sender<ObservationSignal>,
  config  : &SkgConfig,
) -> Result<RecommendedWatcher, String> {
  let callback_sender = sender . clone ();
  let mut watcher = RecommendedWatcher::new (
    move |result : notify::Result<Event>| match result {
      Ok (event) if event_may_describe_mutation (&event . kind) => {
        let _ = callback_sender . send (ObservationSignal::Paths (
          event . paths, QueuedObservationReason::FilesystemEvent)); }
      Ok (_) => { }
      Err (error) => {
        let _ = callback_sender . send (
          ObservationSignal::WatcherFailure (error . to_string ())); }
    },
    Config::default ()) . map_err (|error| error . to_string ())?;
  for source in config . sources . values () {
    watcher . watch (&source . path, RecursiveMode::NonRecursive)
      . map_err (|error| format! (
        "could not watch {}: {}", source . path . display (), error))?;
  }
  Ok (watcher)
}

#[derive(Clone, Debug, Eq, Ord, PartialEq, PartialOrd)]
struct PresentationWatchTarget {
  path      : PathBuf,
  recursive : bool,
}

fn configured_presentation_watcher (
  sender : &mpsc::Sender<ObservationSignal>,
  config : &SkgConfig,
) -> Result<RecommendedWatcher, String> {
  let callback_sender = sender . clone ();
  let mut watcher = RecommendedWatcher::new (
    move |result : notify::Result<Event>| match result {
      Ok (event) if event_may_describe_mutation (&event . kind) => {
        let _ = callback_sender . send (ObservationSignal::GitPresentation); }
      Ok (_) => { }
      Err (error) => {
        let _ = callback_sender . send (
          ObservationSignal::WatcherFailure (error . to_string ())); }
    },
    Config::default ()) . map_err (|error| error . to_string ())?;
  for target in presentation_watch_targets (config)? {
    let mode = if target . recursive {
      RecursiveMode::Recursive
    } else { RecursiveMode::NonRecursive };
    watcher . watch (&target . path, mode) . map_err (|error| format! (
      "could not watch Git metadata {}: {}",
      target . path . display (), error))?;
  }
  Ok (watcher)
}

/// `notify` 8's Linux watcher subscribes to `IN_OPEN`.  Exact source and Git
/// presentation observations necessarily open the files they inspect, so
/// treating an open/read notification as a change makes either observer feed
/// itself forever.  A close-after-write remains relevant (and conservative
/// unknown access kinds remain relevant), because some backends may use it as
/// their only durable indication of a write.
fn event_may_describe_mutation (kind : &EventKind) -> bool {
  !matches! (kind,
    EventKind::Access (AccessKind::Open (_))
    | EventKind::Access (AccessKind::Read)
    | EventKind::Access (AccessKind::Close (AccessMode::Read)))
}

/// Watch replaceable Git metadata through its containing directories.  A
/// linked worktree owns HEAD and index in its per-worktree Git directory,
/// while refs and packed-refs remain in the common directory.
fn presentation_watch_targets (
  config : &SkgConfig,
) -> Result<Vec<PresentationWatchTarget>, String> {
  let mut targets = BTreeSet::new ();
  for source in config . sources . values () {
    let Ok (repo) = Repository::discover (&source . path) else { continue; };
    if repo . workdir () . is_none () { continue; }
    let repo_path = canonical_or_original (repo . path ());
    let common_path = resolve_common_git_dir (&repo_path)?;
    targets . insert (PresentationWatchTarget {
      path: repo_path,
      recursive: false,
    });
    targets . insert (PresentationWatchTarget {
      path: common_path . clone (),
      recursive: false,
    });
    let refs = common_path . join ("refs");
    if refs . is_dir () {
      targets . insert (PresentationWatchTarget {
        path: canonical_or_original (&refs),
        recursive: true,
      });
    }
  }
  Ok (targets . into_iter () . collect ())
}

fn resolve_common_git_dir (repo_path : &Path) -> Result<PathBuf, String> {
  let marker = repo_path . join ("commondir");
  let Ok (value) = fs::read_to_string (&marker) else {
    return Ok (repo_path . to_path_buf ()); };
  let value = value . trim ();
  if value . is_empty () {
    return Err (format! ("Git commondir is empty: {}", marker . display ())); }
  let path = Path::new (value);
  Ok (canonical_or_original (&if path . is_absolute () {
    path . to_path_buf ()
  } else { repo_path . join (path) }))
}

fn canonical_or_original (path : &Path) -> PathBuf {
  path . canonicalize () . unwrap_or_else (|_| path . to_path_buf ())
}

#[derive(Default)]
struct ObservationBatch {
  paths                  : Vec<PathBuf>,
  reasons                : Vec<String>,
  maintenance_jobs       : Vec<(IncidentId, MaintenanceEpoch)>,
  maintenance_final_jobs : Vec<(IncidentId, MaintenanceEpoch)>,
  observe_presentation    : bool,
}

fn collect_observation_batch (
  receiver : &mpsc::Receiver<ObservationSignal>,
  first    : ObservationSignal,
) -> ObservationBatch {
  collect_observation_batch_with_limits (
    receiver, first, OBSERVATION_QUIET_INTERVAL,
    OBSERVATION_MAX_BATCH_AGE)
}

fn collect_observation_batch_with_limits (
  receiver       : &mpsc::Receiver<ObservationSignal>,
  first          : ObservationSignal,
  quiet_interval : Duration,
  max_batch_age  : Duration,
) -> ObservationBatch {
  let started : Instant = Instant::now ();
  let max_deadline : Instant = started + max_batch_age;
  let mut quiet_deadline : Instant = started + quiet_interval;
  let mut batch : ObservationBatch = ObservationBatch::default ();
  absorb_signal (
    first, &mut batch . paths, &mut batch . reasons,
    &mut batch . maintenance_jobs, &mut batch . maintenance_final_jobs,
    &mut batch . observe_presentation);
  loop {
    let now : Instant = Instant::now ();
    let quiet_remaining : Duration = quiet_deadline
      . saturating_duration_since (now);
    let age_remaining : Duration = max_deadline
      . saturating_duration_since (now);
    let timeout : Duration = quiet_remaining . min (age_remaining);
    if timeout . is_zero () { break; }
    let signal = match receiver . recv_timeout (timeout) {
      Ok (signal) => signal,
      Err (mpsc::RecvTimeoutError::Timeout)
      | Err (mpsc::RecvTimeoutError::Disconnected) => break,
    };
    absorb_signal (
      signal, &mut batch . paths, &mut batch . reasons,
      &mut batch . maintenance_jobs, &mut batch . maintenance_final_jobs,
      &mut batch . observe_presentation);
    quiet_deadline = Instant::now () + quiet_interval;
  }
  batch
}

fn observation_worker (
  runtime  : Weak<ServerRuntime>,
  receiver : mpsc::Receiver<ObservationSignal>,
) {
  while let Ok (first) = receiver . recv () {
    let batch : ObservationBatch = collect_observation_batch (
      &receiver, first);
    let ObservationBatch {
      paths, reasons, maintenance_jobs, maintenance_final_jobs,
      observe_presentation,
    } = batch;
    let Some (runtime) = runtime . upgrade () else { return; };
    // A process-owned exact sweep is queued when the final bracket closes.
    // Events consumed inside the bracket therefore need no client-side
    // retention and, critically, may not publish a mid-batch candidate.
    if (!reasons . is_empty () || !paths . is_empty ())
       && !reload_batch_active ()
    {
      run_observation (&runtime, paths, reasons); }
    for (incident, epoch) in maintenance_jobs {
      run_target_observation (&runtime, incident, epoch); }
    for (incident, epoch) in maintenance_final_jobs {
      run_final_observation (&runtime, incident, epoch); }
    if observe_presentation {
      if let Err (error) = runtime . observe_git_presentation () {
        tracing::warn! (%error, "Git presentation observation failed"); }}
    thread::yield_now ();
  }
}

fn absorb_signal (
  signal  : ObservationSignal,
  paths   : &mut Vec<PathBuf>,
  reasons : &mut Vec<String>,
  maintenance_jobs : &mut Vec<(IncidentId, MaintenanceEpoch)>,
  maintenance_final_jobs : &mut Vec<(IncidentId, MaintenanceEpoch)>,
  observe_presentation : &mut bool,
) {
  match signal {
    ObservationSignal::Paths (new_paths, reason) => {
      paths . extend (new_paths);
      reasons . push (reason . label () . into ()); }
    ObservationSignal::FullSweep (reason) =>
      reasons . push (reason . label () . into ()),
    ObservationSignal::MaintenanceTargets (incident, epoch) =>
      maintenance_jobs . push ((incident, epoch)),
    ObservationSignal::MaintenanceFinalDisk (incident, epoch) =>
      maintenance_final_jobs . push ((incident, epoch)),
    ObservationSignal::GitPresentation => *observe_presentation = true,
    ObservationSignal::WatcherFailure (error) => {
      reasons . push (format! ("watcher failure: {}", error));
      *observe_presentation = true; }
  }
}

enum OriginObservationFailure {
  InvalidDisk (String),
  Operational (String),
}

fn validate_live_config_replacement (
  old : &crate::types::misc::SkgConfig,
  new : &crate::types::misc::SkgConfig,
) -> Result<(), String> {
  let mut startup_only = Vec::new ();
  if old . port != new . port { startup_only . push ("port"); }
  if old . timing_log != new . timing_log {
    startup_only . push ("timing_log"); }
  if old . beep_when_server_becomes_available
      != new . beep_when_server_becomes_available
  {
    startup_only . push ("beep_when_server_becomes_available"); }
  if startup_only . is_empty () {
    Ok (( ))
  } else {
    Err (format! (
      "startup-only setting(s) changed: {}; restart the server to apply them",
      startup_only . join (", ")))
  }
}

impl From<String> for OriginObservationFailure {
  fn from (error : String) -> Self { Self::Operational (error) }
}

impl From<&str> for OriginObservationFailure {
  fn from (error : &str) -> Self { Self::Operational (error . into ()) }
}

fn run_final_observation (
  runtime  : &ServerRuntime,
  incident : IncidentId,
  epoch    : MaintenanceEpoch,
) {
  let result = (|| -> Result<Option<String>, OriginObservationFailure> {
    let active = {
      let coordinator = runtime . maintenance_snapshot ();
      let CoordinatorState::Active (active) = &coordinator . state else {
        return Ok (None); };
      if active . incident_id != incident || active . epoch != epoch
      || (!active . force_full_rebuild_recovery
          && !matches! (active . origin,
            MaintenanceOrigin::PendingReconciliation
            | MaintenanceOrigin::Pull
            | MaintenanceOrigin::FullRebuild))
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
      return Err ("final observation G0 was superseded" . into ()); }
    let proposed_config = if active . origin == MaintenanceOrigin::FullRebuild {
      let path = snapshot . env . config . config_path . to_string_lossy ();
      let config = load_config (&path) . map_err (|error|
        OriginObservationFailure::InvalidDisk (format! (
          "replacement config is invalid: {}", error)))?;
      validate_live_config_replacement (&snapshot . env . config, &config)
        . map_err (|error| OriginObservationFailure::InvalidDisk (format! (
          "replacement config requires a server restart: {}", error)))?;
      reject_archive_source_overlap (
        &config . maintenance_archive_identity, &snapshot . env . config)
        . map_err (|error| OriginObservationFailure::InvalidDisk (format! (
          "replacement archive root is unsafe against the selected source catalog: {}",
          error)))?;
      config
    } else {
      snapshot . env . config . clone ()
    };
    let candidate = match observe_complete_maintenance_disk (
        &proposed_config, &snapshot . selected, sequence)
    {
      DiskObservation::Valid (candidate) => candidate,
      DiskObservation::Invalid { details } => return Err (
        OriginObservationFailure::InvalidDisk (format! (
          "final disk is invalid: {}", details . join ("; ")))),
      DiskObservation::Unstable { details } => return Err (
        OriginObservationFailure::InvalidDisk (format! (
          "final disk was unstable: {}", details . join ("; ")))),
      DiskObservation::ByteEquivalent
      | DiskObservation::SemanticallyEqual { .. } => return Err (
        OriginObservationFailure::Operational (
          "maintenance observation returned no candidate" . into ())),
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
      . map (Some) . map_err (OriginObservationFailure::Operational)
  })();

  queue_origin_result (runtime, incident, epoch, result);
}

fn run_target_observation (
  runtime  : &ServerRuntime,
  incident : IncidentId,
  epoch    : MaintenanceEpoch,
) {
  let result = (|| -> Result<Option<String>, OriginObservationFailure> {
    let active = {
      let coordinator = runtime . maintenance_snapshot ();
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
        OriginObservationFailure::InvalidDisk (format! (
          "targeted disk is invalid: {}", details . join ("; ")))),
      DiskObservation::Unstable { details } => return Err (
        OriginObservationFailure::InvalidDisk (format! (
          "targeted disk was unstable: {}", details . join ("; ")))),
      DiskObservation::ByteEquivalent
      | DiskObservation::SemanticallyEqual { .. } => return Err (
        OriginObservationFailure::Operational (
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
      . map (Some) . map_err (OriginObservationFailure::Operational)
  })();

  queue_origin_result (runtime, incident, epoch, result);
}

fn queue_origin_result (
  runtime  : &ServerRuntime,
  incident : IncidentId,
  epoch    : MaintenanceEpoch,
  result   : Result<Option<String>, OriginObservationFailure>,
) {
  let payload = match result {
    Ok (Some (payload)) => payload,
    Ok (None) => return,
    Err (failure) => {
      let error = match failure {
        OriginObservationFailure::InvalidDisk (error) => {
          let staged = runtime . transition_maintenance (|coordinator|
            coordinator . block_invalid_disk (
              &incident, epoch, error . clone ())) . and_then (|_| {
            let active = match &runtime . maintenance_snapshot ()
              . state
            {
              CoordinatorState::Active (active)
                if active . incident_id == incident
                && active . epoch == epoch => active . clone (),
              _ => return Err (
                "invalid-disk incident authority changed" . into ()),
            };
            if active . external_mutation . is_none () {
              return Ok (( )); }
            let verified = runtime . verified_archive (&incident)
              . ok_or_else (||
                "verified initial archive was not retained" . to_string ())?;
            let retirements = plan_invalid_preselection_retirements (
              &active, &verified)?;
            runtime . transition_maintenance (|coordinator|
              coordinator . record_preselection_retirements (
                &incident, epoch, retirements . clone ()))
          });
          match staged {
            Ok (( )) => error,
            Err (retirement_error) => format! (
              "{}; dirty-view retirement could not be staged: {}",
              error, retirement_error),
          }
        }
        OriginObservationFailure::Operational (error) => error,
      };
      let phase = match &runtime . maintenance_snapshot () . state {
        CoordinatorState::Active (active) => active . phase . label (),
        state => state . label (),
      };
      let mut fields = vec![
        field ("status", "origin-operation-failed"),
        field ("incident-id", incident . as_str ()),
        field ("maintenance-epoch", &epoch . get () . to_string ()),
        field ("phase", phase),
        field ("error", &error),
      ];
      if let CoordinatorState::Active (active) =
        &runtime . maintenance_snapshot () . state
      {
        if !active . preselection_retirements . is_empty () {
          fields . push (
            crate::serve::handlers::maintenance_protocol::
              preselection_retirements_field (active)); }
      }
      Sexp::List (fields) . to_string ()
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
  let (sequence, deferred) = match runtime . transition_maintenance (|coordinator| {
    if let Some (sequence) = coordinator . defer_ordinary_observation () {
      Ok ((sequence, true))
    } else {
      let sequence = coordinator . next_observation_sequence ();
      let _ = coordinator . observation_started ();
      Ok ((sequence, false))
    }
  }) {
    Ok (observation) => observation,
    Err (error) => {
      tracing::error! (%error, "observation could not record its authority");
      retry_unpublished_observation (runtime);
      return; }
  };
  if deferred {
    tracing::debug! (
      observation_sequence = sequence . get (),
      "deferred ordinary observation until serialized maintenance completes");
    return;
  }
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
      if runtime . transition_maintenance (|coordinator|
        coordinator . observation_equal ()) . is_err () {
        retry_unpublished_observation (runtime); }
    }
    DiskObservation::SemanticallyEqual { manifest, .. } => {
      match runtime . publish_semantically_equal_manifest (
          snapshot . selected . graph_generation,
          snapshot . selected . manifest_revision,
          manifest)
      {
        Ok (( )) => {
          if runtime . transition_maintenance (|coordinator|
            coordinator . observation_equal ()) . is_err () {
            retry_unpublished_observation (runtime); }
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
      if runtime . transition_maintenance (|coordinator|
        coordinator . set_pending_valid (summary . clone ())) . is_ok ()
      {
        runtime . queue_server_event (QueuedServerEvent {
          frame_kind: TcpToClient::MaintenanceOffer . repr_in_client () . into (),
          operation_id: format! ("candidate-{}", summary . id),
          payload: candidate_offer_payload (&summary, &reasons, &paths),
        });
      } else {
        retry_unpublished_observation (runtime);
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
  if runtime . transition_maintenance (|coordinator|
    coordinator . set_pending_invalid (reason . clone (), details . clone ())) . is_ok ()
  {
    runtime . queue_server_event (QueuedServerEvent {
      frame_kind: TcpToClient::MaintenanceStatus . repr_in_client () . into (),
      operation_id: format! ("observation-{}",
        runtime . maintenance_snapshot ()
          . observation_sequence . get ()),
      payload: pending_problem_payload (&reason, &details),
    });
  } else {
    retry_unpublished_observation (runtime);
  }
}

/// A competing save or journal proposal can temporarily prevent publishing
/// an observation. Retain the hint by observing again after the bounded batch
/// delay; a refused proposal is never evidence that disk is unchanged.
fn retry_unpublished_observation (runtime : &ServerRuntime) {
  if runtime . authority_failure () . is_none () {
    let _ = runtime . schedule_full_observation (
      QueuedObservationReason::SelectedGenerationAdvanced); }
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

#[cfg(test)]
mod tests {
  use super::{
    ObservationSignal,
    PresentationWatchTarget,
    configured_source_watcher,
    collect_observation_batch_with_limits,
    event_may_describe_mutation,
    presentation_watch_targets,
    validate_live_config_replacement,
  };
  use crate::types::misc::{SkgConfig, SkgfileSource, SourceName};
  use git2::{Repository, Signature};
  use notify::event::{AccessKind, AccessMode, ModifyKind};
  use notify::EventKind;
  use std::collections::HashMap;
  use std::fs;
  use std::path::{Path, PathBuf};
  use std::sync::mpsc;
  use std::thread;
  use std::time::{Duration, Instant};
  use tempfile::TempDir;

  fn config () -> SkgConfig {
    SkgConfig::dummyFromSources (Default::default ())
  }

  fn source_config (source : &Path) -> SkgConfig {
    let name = SourceName::from ("source");
    SkgConfig::dummyFromSources (HashMap::from ([(name . clone (),
      SkgfileSource {
        name,
        abbreviation: None,
        path: source . to_path_buf (),
        user_owns_it: true,
      })]))
  }

  fn create_initial_commit (repo : &Repository) {
    let mut index = repo . index () . unwrap ();
    let tree_id = index . write_tree () . unwrap ();
    let tree = repo . find_tree (tree_id) . unwrap ();
    let signature = Signature::now ("test", "test@example.com") . unwrap ();
    repo . commit (Some ("HEAD"), &signature, &signature, "initial", &tree,
                   &[]) . unwrap ();
  }

  #[test]
  fn live_replacement_rejects_process_startup_settings () {
    let old = config ();
    let mut new = old . clone ();
    new . port += 1;
    new . timing_log = !old . timing_log;
    let error = validate_live_config_replacement (&old, &new) . unwrap_err ();
    assert! (error . contains ("port"), "{}", error);
    assert! (error . contains ("timing_log"), "{}", error);
    assert! (error . contains ("restart the server"), "{}", error);
  }

  #[test]
  fn live_replacement_accepts_runtime_paths_and_limits () {
    let old = config ();
    let mut new = old . clone ();
    new . tantivy_folder = "replacement-index" . into ();
    new . maintenance_archive_folder = "replacement-archives" . into ();
    new . initial_node_limit += 1;
    new . max_ancestry_depth += 1;
    assert! (validate_live_config_replacement (&old, &new) . is_ok ());
  }

  #[test]
  fn watcher_events_ignore_reads_but_retain_possible_writes () {
    assert! (!event_may_describe_mutation (&EventKind::Access (
      AccessKind::Open (AccessMode::Read))));
    assert! (!event_may_describe_mutation (&EventKind::Access (
      AccessKind::Open (AccessMode::Any))));
    assert! (!event_may_describe_mutation (&EventKind::Access (
      AccessKind::Read)));
    assert! (!event_may_describe_mutation (&EventKind::Access (
      AccessKind::Close (AccessMode::Read))));
    assert! (event_may_describe_mutation (&EventKind::Access (
      AccessKind::Close (AccessMode::Write))));
    assert! (event_may_describe_mutation (&EventKind::Modify (
      ModifyKind::Any)));
  }

  #[test]
  fn sustained_notifications_reach_the_maximum_batch_age () {
    let (sender, receiver) = mpsc::channel ();
    let producer = thread::spawn (move || {
      let started : Instant = Instant::now ();
      while started . elapsed () < Duration::from_millis (300) {
        if sender . send (ObservationSignal::Paths (
          vec![PathBuf::from ("changed.skg")],
          crate::maintenance::QueuedObservationReason::FilesystemEvent))
          . is_err ()
        {
          break;
        }
        thread::sleep (Duration::from_millis (2));
      }
    });
    let first : ObservationSignal = receiver . recv () . unwrap ();
    let started : Instant = Instant::now ();
    let batch = collect_observation_batch_with_limits (
      &receiver, first, Duration::from_millis (20),
      Duration::from_millis (80));
    let elapsed : Duration = started . elapsed ();
    drop (receiver);
    producer . join () . unwrap ();
    assert! (batch . paths . len () > 1);
    assert! (elapsed < Duration::from_millis (250),
      "batch waited {:?} despite its maximum age", elapsed);
  }

  #[cfg(target_os = "linux")]
  #[test]
  fn reading_watched_source_does_not_queue_an_observation () {
    let temporary = TempDir::new () . unwrap ();
    let path = temporary . path () . join ("node.skg");
    fs::write (&path, "- id: node\n") . unwrap ();
    let (sender, receiver) = mpsc::channel ();
    let _watcher = configured_source_watcher (
      &sender, &source_config (temporary . path ())) . unwrap ();

    fs::read (&path) . unwrap ();
    assert! (matches! (
      receiver . recv_timeout (Duration::from_millis (250)),
      Err (mpsc::RecvTimeoutError::Timeout)));

    fs::write (&path, "- id: changed\n") . unwrap ();
    let signal = receiver . recv_timeout (Duration::from_secs (2)) . unwrap ();
    assert! (matches! (signal,
      ObservationSignal::Paths (paths, _)
        if paths . iter () . any (|event_path| event_path == &path)));
  }

  #[test]
  fn presentation_watches_index_head_packed_refs_and_loose_refs () {
    let temporary = TempDir::new () . unwrap ();
    let repo = Repository::init (temporary . path ()) . unwrap ();
    create_initial_commit (&repo);
    let git_dir = repo . path () . canonicalize () . unwrap ();
    let targets = presentation_watch_targets (
      &source_config (temporary . path ())) . unwrap ();
    assert! (targets . contains (&PresentationWatchTarget {
      path: git_dir . clone (), recursive: false }));
    assert! (targets . contains (&PresentationWatchTarget {
      path: git_dir . join ("refs") . canonicalize () . unwrap (),
      recursive: true,
    }));
  }

  #[test]
  fn linked_worktree_watches_per_worktree_and_common_git_dirs () {
    let temporary = TempDir::new () . unwrap ();
    let main_path = temporary . path () . join ("main");
    let linked_path = temporary . path () . join ("linked");
    let repo = Repository::init (&main_path) . unwrap ();
    create_initial_commit (&repo);
    repo . worktree ("linked", &linked_path, None) . unwrap ();
    let linked = Repository::open (&linked_path) . unwrap ();
    let linked_git_dir = linked . path () . canonicalize () . unwrap ();
    let common_git_dir = repo . path () . canonicalize () . unwrap ();
    let targets = presentation_watch_targets (
      &source_config (&linked_path)) . unwrap ();
    assert! (targets . contains (&PresentationWatchTarget {
      path: linked_git_dir, recursive: false }));
    assert! (targets . contains (&PresentationWatchTarget {
      path: common_git_dir . clone (), recursive: false }));
    assert! (targets . contains (&PresentationWatchTarget {
      path: common_git_dir . join ("refs") . canonicalize () . unwrap (),
      recursive: true,
    }));
  }
}
