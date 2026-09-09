//! One ordered worker prepares Tantivy updates from a named candidate graph.
//! It returns the terminal result for the exact batch; only the owner may
//! publish that graph with its completed Searcher. Existing readers keep
//! their previous immutable search snapshot while this worker runs.
//!
//! The completion ledger is process-local. Durable operation outcomes and
//! source-effect recovery belong to the owner's transaction journal.

use crate::context::context_origin_types_for_graph;
use crate::save::{nodecompletes_from_graph, update_tantivy_from_saveinstructions};
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::tantivy::write::reconstruct_index_from_nodes;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::save::DefineNode;

use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::sync::mpsc::{Sender, channel};
use std::sync::{Arc, Condvar, Mutex, MutexGuard, OnceLock};

/// Held by every Tantivy writer for the duration of its writer's life,
/// so two never coexist (Tantivy allows one IndexWriter per directory).
/// Recovers a poisoned mutex: a panic mid-write must not wedge all
/// future writes, since the index is a rebuildable cache.
static TANTIVY_WRITE_LOCK : Mutex<()> = Mutex::new (());

pub fn lock_tantivy_writes () -> MutexGuard<'static, ()> {
  TANTIVY_WRITE_LOCK . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () ) }

/// One queued index update: the saved instructions, the per-pid context
/// origin types to stamp on each doc, and a handle to the index. Owned,
/// so it can move to the worker thread.
pub struct TantivyWriteTask {
  pub tantivy_index : TantivyIndex,
  pub instructions  : Vec<DefineNode>,
  pub context_types : HashMap<ID, String>,
  /// Exact selected graph for complete in-place reconstruction if the
  /// incremental write fails.  An Arc makes the common path cheap.
  pub recovery_graph : Arc<InRustGraph>,
  pub cyclic_roots   : BTreeSet<ID>,
}


#[derive(Clone, Copy, Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
pub struct TantivyGeneration (u64);

impl TantivyGeneration {
  pub fn get (self) -> u64 { self . 0 }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TantivyGenerationStatus {
  Pending,
  Committed,
  Reconstructed (String),
  Failed (String), }

struct GenerationLedger {
  next_generation : u64,
  statuses        : BTreeMap<TantivyGeneration, TantivyGenerationStatus>, }

impl GenerationLedger {
  fn new () -> Self {
    Self { next_generation: 1, statuses: BTreeMap::new () } }

  fn begin (&mut self) -> TantivyGeneration {
    let generation = TantivyGeneration (self . next_generation);
    self . next_generation = self . next_generation
      . checked_add (1)
      . expect ("Tantivy generation exhausted u64");
    assert_eq! (
      self . statuses . insert (
        generation, TantivyGenerationStatus::Pending),
      None,
      "a Tantivy generation is assigned exactly once" );
    generation }

  fn latest (&self) -> Option<TantivyGeneration> {
    self . statuses . last_key_value () . map ( |(g, _)| *g ) }

  fn finish (
    &mut self,
    generation : TantivyGeneration,
    status     : TantivyGenerationStatus,
  ) {
    assert! (! matches! (status, TantivyGenerationStatus::Pending));
    let old = self . statuses . insert (generation, status);
    assert_eq! (
      old, Some (TantivyGenerationStatus::Pending),
      "a known pending Tantivy generation reaches one terminal state" ); }
}

/// Shared between the worker and enqueue/wait APIs.  Completed entries are
/// retained while callers may ask about their exact in-process index batch.
struct CompletionState {
  ledger  : Mutex<GenerationLedger>,
  changed : Condvar, }

struct QueuedTantivyWrite {
  generation : TantivyGeneration,
  task       : TantivyWriteTask, }

struct Worker {
  sender     : Mutex<Sender<QueuedTantivyWrite>>,
  completion : Arc<CompletionState>, }

static WORKER : OnceLock<Worker> = OnceLock::new ();

fn worker () -> &'static Worker {
  WORKER . get_or_init ( || {
    let (sender, receiver) = channel::<QueuedTantivyWrite> ();
    let completion : Arc<CompletionState> = Arc::new ( CompletionState {
      ledger  : Mutex::new (GenerationLedger::new ()),
      changed : Condvar::new (), } );
    let worker_completion : Arc<CompletionState> = completion . clone ();
    std::thread::spawn ( move || {
      while let Ok (queued) = receiver . recv () {
        // update_tantivy_from_saveinstructions takes the write lock itself.
        let incremental = std::panic::catch_unwind (
          std::panic::AssertUnwindSafe ( ||
            update_tantivy_from_saveinstructions (
              &queued . task . instructions,
              &queued . task . tantivy_index,
              &queued . task . context_types )));
        let terminal = match incremental {
          Ok (Ok (_)) => TantivyGenerationStatus::Committed,
          Ok (Err (e)) => recover_generation (
            queued . generation, &queued . task, e . to_string ()),
          Err (_) => recover_generation (
            queued . generation, &queued . task,
            "Tantivy writer panicked while applying the batch" . into ()), };
        finish_generation (
          &worker_completion, queued . generation, terminal); } });
    Worker { sender: Mutex::new (sender), completion } } ) }

fn lock_ledger (
  completion : &CompletionState,
) -> MutexGuard<'_, GenerationLedger> {
  completion . ledger . lock ()
    . unwrap_or_else ( |poisoned| poisoned . into_inner () )
}

fn finish_generation (
  completion : &CompletionState,
  generation : TantivyGeneration,
  status     : TantivyGenerationStatus,
) {
  lock_ledger (completion) . finish (generation, status);
  completion . changed . notify_all (); }

fn failed_generation_status (
  generation : TantivyGeneration,
  reason     : String,
) -> TantivyGenerationStatus {
  tracing::error! (
    generation = generation . get (),
    "Background Tantivy write failed: {}. The filesystem is correct; \
     run 'rebuild dbs' to resync the search index.",
    reason );
  TantivyGenerationStatus::Failed (reason)
}

fn recover_generation (
  generation         : TantivyGeneration,
  task               : &TantivyWriteTask,
  incremental_reason : String,
) -> TantivyGenerationStatus {
  tracing::error! (
    generation = generation . get (),
    "Background Tantivy write failed: {}. Reconstructing from the exact selected graph...",
    incremental_reason);
  let nodes = nodecompletes_from_graph (&task . recovery_graph);
  let labels = context_origin_types_for_graph (
    &task . recovery_graph, &task . cyclic_roots);
  match std::panic::catch_unwind (std::panic::AssertUnwindSafe (||
    reconstruct_index_from_nodes (&nodes, &task . tantivy_index, &labels))) {
    Ok (Ok (_)) => TantivyGenerationStatus::Reconstructed (
      incremental_reason),
    Ok (Err (recovery_error)) => failed_generation_status (
      generation, format! (
        "incremental update failed ({}); complete reconstruction failed ({})",
        incremental_reason, recovery_error)),
    Err (_) => failed_generation_status (
      generation, format! (
        "incremental update failed ({}); complete reconstruction panicked",
        incremental_reason)), }
}

/// Enqueue a Tantivy index update to commit in the background, in FIFO
/// order.  Returns the generation immediately; callers decide when they
/// need to wait for or present its terminal result.
pub fn enqueue_tantivy_write (
  task : TantivyWriteTask,
) -> TantivyGeneration {
  let worker : &Worker = worker ();
  let generation = lock_ledger (&worker . completion) . begin ();
  let queued = QueuedTantivyWrite { generation, task };
  if let Err (e) = worker . sender . lock ()
    . unwrap_or_else ( |p| p . into_inner () ) . send (queued)
  {
    let reason = format! ("Tantivy background worker unavailable: {}", e);
    tracing::error! (generation = generation . get (), "{}", reason);
    finish_generation (
      &worker . completion,
      generation,
      TantivyGenerationStatus::Failed (reason)); }
  generation }

/// The newest generation accepted by the process, if there has been one.
/// Capture this before a read barrier so later saves cannot extend the wait.
pub fn latest_tantivy_generation () -> Option<TantivyGeneration> {
  lock_ledger (&worker () . completion) . latest () }

/// Block until one exact generation commits or fails.
pub fn wait_for_tantivy_generation (
  generation : TantivyGeneration,
) -> TantivyGenerationStatus {
  wait_for_generation_in (&worker () . completion, generation)
}

fn wait_for_generation_in (
  completion : &CompletionState,
  generation : TantivyGeneration,
) -> TantivyGenerationStatus {
  let mut ledger = lock_ledger (completion);
  loop {
    match ledger . statuses . get (&generation) {
      Some (TantivyGenerationStatus::Pending) => {
        ledger = completion . changed . wait (ledger)
          . unwrap_or_else ( |p| p . into_inner () ); }
      Some (terminal) => return terminal . clone (),
      None => return TantivyGenerationStatus::Failed (format! (
        "unknown Tantivy generation {}", generation . get ())), } } }

/// Block until every generation at or before the captured bound is terminal.
/// FIFO execution makes the bound's terminal transition the usual wakeup,
/// but checking all entries makes the contract explicit and testable.
pub fn wait_for_tantivy_writes_through (
  through : TantivyGeneration,
) -> Vec<(TantivyGeneration, TantivyGenerationStatus)> {
  wait_for_writes_through_in (&worker () . completion, through)
}

fn wait_for_writes_through_in (
  completion : &CompletionState,
  through    : TantivyGeneration,
) -> Vec<(TantivyGeneration, TantivyGenerationStatus)> {
  let mut ledger = lock_ledger (completion);
  loop {
    let known_through = ledger . statuses . contains_key (&through);
    let pending_through = ledger . statuses . range (..=through)
      . any ( |(_, status)| matches! (
        status, TantivyGenerationStatus::Pending) );
    if known_through && ! pending_through {
      return ledger . statuses . range (..=through)
        . map ( |(generation, status)| (*generation, status . clone ()) )
        . collect (); }
    if ! known_through {
      return vec! [(through, TantivyGenerationStatus::Failed (format! (
        "unknown Tantivy generation {}", through . get ())))]; }
    ledger = completion . changed . wait (ledger)
      . unwrap_or_else ( |p| p . into_inner () ); } }

/// Block through the newest generation visible when called.  Work enqueued
/// later belongs to a later read barrier and does not prolong this one.
pub fn wait_for_tantivy_writes_idle () {
  if let Some (through) = latest_tantivy_generation () {
    let _ = wait_for_tantivy_writes_through (through); } }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::init::empty_in_ram_tantivy_index;
  use crate::dbs::tantivy::title_and_source_by_id;
  use crate::types::nodes::complete::empty_node_complete;
  use std::sync::mpsc;

  #[test]
  fn ledger_assigns_monotonic_generations_and_one_terminal_each () {
    let mut ledger = GenerationLedger::new ();
    let first = ledger . begin ();
    let second = ledger . begin ();
    assert_eq! (first . get (), 1);
    assert_eq! (second . get (), 2);
    assert_eq! (ledger . latest (), Some (second));
    ledger . finish (first, TantivyGenerationStatus::Committed);
    ledger . finish (
      second, TantivyGenerationStatus::Failed ("broken" . into ())) ;
    assert_eq! (
      ledger . statuses . get (&first),
      Some (&TantivyGenerationStatus::Committed));
    assert_eq! (
      ledger . statuses . get (&second),
      Some (&TantivyGenerationStatus::Failed ("broken" . into ()))); }

  #[test]
  #[should_panic (expected = "one terminal state")]
  fn ledger_rejects_a_second_terminal_outcome () {
    let mut ledger = GenerationLedger::new ();
    let generation = ledger . begin ();
    ledger . finish (generation, TantivyGenerationStatus::Committed);
    ledger . finish (generation, TantivyGenerationStatus::Committed); }

  #[test]
  fn exact_and_through_waits_wake_on_terminal_status () {
    let completion = Arc::new (CompletionState {
      ledger: Mutex::new (GenerationLedger::new ()),
      changed: Condvar::new (), });
    let (first, second) = {
      let mut ledger = lock_ledger (&completion);
      (ledger . begin (), ledger . begin ()) };
    let waiter_completion = completion . clone ();
    let (sent, received) = mpsc::channel ();
    let waiter = std::thread::spawn (move || {
      sent . send (wait_for_writes_through_in (
        &waiter_completion, second)) . unwrap (); });
    finish_generation (
      &completion, first, TantivyGenerationStatus::Committed);
    finish_generation (
      &completion, second,
      TantivyGenerationStatus::Failed ("no segment" . into ()));
    let statuses = received . recv () . unwrap ();
    waiter . join () . unwrap ();
    assert_eq! (statuses, vec! [
      (first, TantivyGenerationStatus::Committed),
      (second, TantivyGenerationStatus::Failed ("no segment" . into ())),
    ]);
    assert_eq! (
      wait_for_generation_in (&completion, second),
      TantivyGenerationStatus::Failed ("no segment" . into ())); }

  #[test]
  fn failed_incremental_generation_reconstructs_its_exact_graph () {
    let index = empty_in_ram_tantivy_index () . unwrap ();
    let mut node = empty_node_complete ();
    node . pid = ID::new ("recovered-generation");
    node . title = "recovered generation title" . into ();
    let graph = Arc::new (InRustGraph::from_nodecompletes (&[node . clone ()]));
    let task = TantivyWriteTask {
      tantivy_index: index . clone (),
      instructions: Vec::new (),
      context_types: HashMap::new (),
      recovery_graph: graph,
      cyclic_roots: BTreeSet::new (),
    };
    assert_eq! (
      recover_generation (
        TantivyGeneration (700), &task, "injected failure" . into ()),
      TantivyGenerationStatus::Reconstructed ("injected failure" . into ()));
    assert_eq! (
      title_and_source_by_id (&index, &index . reader . searcher (), &node . pid)
        . map (|(title, _)| title),
      Some (node . title));
  }
}
