use super::{MutationControl, SelectedRuntimeSnapshot, ServerRuntime};
use crate::maintenance::coordinator::MaintenanceCoordinator;
use crate::runtime::query_waits::query_test_runtime;
use crate::types::misc::ID;
use crate::types::store_state::SelectedStoreState;
use std::collections::BTreeSet;

use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::sync::Arc;
use std::thread::{self, JoinHandle};

#[test]
fn writer_transition_keeps_readers_live_and_releases_reservation () {
  let (_temp, runtime, fixture_operation) :
    (tempfile::TempDir, Arc<ServerRuntime>, String) = query_test_runtime ();
  let before : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  // The query fixture publishes G2 directly; seed its writer adapter from G2.
  runtime . with_writer_env (|env| *env = before . env . clone ());
  let (entered_sender, entered_receiver) : (SyncSender<()>, Receiver<()>) = sync_channel (0);
  let (release_sender, release_receiver) : (SyncSender<()>, Receiver<()>) = sync_channel (0);
  let worker_runtime : Arc<ServerRuntime> = Arc::clone (&runtime);
  let worker_operation : String = "writer-transition-paused" . into ();
  let worker : JoinHandle<Result<(), String>> = thread::spawn (move || {
    worker_runtime . with_writer_transition (worker_operation, move |env, control| {
      entered_sender . send (()) . unwrap ();
      release_receiver . recv () . unwrap ();
      control . authorize () . unwrap ();
      let selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
      env . in_rust_graph . store (Arc::new (selected . with_cyclic_roots (
        BTreeSet::from ([ID::from ("writer-publication-marker")]))));
    })
  });
  entered_receiver . recv () . unwrap ();

  let observed : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  assert_eq! (observed . selected . graph_generation, before . selected . graph_generation);
  let maintenance : MaintenanceCoordinator = runtime . maintenance_snapshot ();
  assert! (maintenance . query_waits . waits . contains_key (&fixture_operation));
  assert! (runtime . interactive . try_lock () . is_ok ());

  let competing : Result<MutationControl, String> = runtime . reserve_mutation (
    "writer-transition-competing", before . selected . graph_generation,
    before . selected . manifest_revision);
  assert! (competing . is_err (), "second mutation reservation unexpectedly succeeded");

  release_sender . send (()) . unwrap ();
  worker . join () . unwrap () . unwrap ();

  let after : Arc<SelectedRuntimeSnapshot> = runtime . selected_snapshot ();
  assert_eq! (after . selected . graph_generation, before . selected . graph_generation);
  assert_eq! (after . selected . manifest_revision, before . selected . manifest_revision);
  assert_eq! (after . selected . cyclic_roots,
    BTreeSet::from ([ID::from ("writer-publication-marker")]));
  assert_eq! (after . env . searcher . num_docs (), before . env . searcher . num_docs ());
  let released : MutationControl = runtime . reserve_mutation (
    "writer-transition-after", after . selected . graph_generation,
    after . selected . manifest_revision) . unwrap ();
  released . finish () . unwrap ();
}
