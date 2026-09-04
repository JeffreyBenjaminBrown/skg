//! Process-owned, low-priority filesystem observation.

use crate::maintenance::candidate::{
  DiskObservation,
  observe_complete_disk,
};
use crate::maintenance::{PendingReason, QueuedObservationReason};
use crate::runtime::ServerRuntime;
use crate::runtime::interactive_session::QueuedServerEvent;
use crate::serve::protocol::TcpToClient;

use notify::{Config, Event, RecommendedWatcher, RecursiveMode, Watcher};
use sexp::{Atom, Sexp};
use std::path::PathBuf;
use std::sync::{Weak, mpsc};
use std::thread;
use std::time::Duration;

#[derive(Clone, Debug)]
enum ObservationSignal {
  Paths (Vec<PathBuf>, QueuedObservationReason),
  FullSweep (QueuedObservationReason),
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
}

fn observation_worker (
  runtime  : Weak<ServerRuntime>,
  receiver : mpsc::Receiver<ObservationSignal>,
) {
  while let Ok (first) = receiver . recv () {
    let mut paths = Vec::new ();
    let mut reasons = Vec::new ();
    absorb_signal (first, &mut paths, &mut reasons);
    while let Ok (signal) = receiver . recv_timeout (
        Duration::from_millis (175))
    {
      absorb_signal (signal, &mut paths, &mut reasons); }
    let Some (runtime) = runtime . upgrade () else { return; };
    run_observation (&runtime, paths, reasons);
    thread::yield_now ();
  }
}

fn absorb_signal (
  signal  : ObservationSignal,
  paths   : &mut Vec<PathBuf>,
  reasons : &mut Vec<String>,
) {
  match signal {
    ObservationSignal::Paths (new_paths, reason) => {
      paths . extend (new_paths);
      reasons . push (reason . label () . into ()); }
    ObservationSignal::FullSweep (reason) =>
      reasons . push (reason . label () . into ()),
    ObservationSignal::WatcherFailure (error) => {
      reasons . push (format! ("watcher failure: {}", error)); }
  }
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
