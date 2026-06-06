//! Background Tantivy index writer.
//!
//! Each save's Tantivy update — the ~60% of the save spent in
//! 'writer.commit()' (see profiling-save.org) — used to block the
//! response, even though the save's re-rendered result is built from
//! the in-Rust graph and never reads the search index. The save now
//! ENQUEUES its index update here and returns; the commit happens off
//! the critical path.
//!
//! A single worker thread applies the queued updates in FIFO order, so
//! two rapid saves of the same node can never commit out of order. A
//! search blocks on 'wait_for_tantivy_writes_idle' until the queue has
//! drained, so it always sees an index reflecting every save issued so
//! far (read-your-writes), at the cost of waiting through any in-flight
//! commit.
//!
//! 'lock_tantivy_writes' serializes EVERY Tantivy writer (this worker,
//! search-make-link's 'update_index_with_nodes', the init/rebuild
//! context pass), since backgrounding the worker means it can now run
//! concurrently with those. On a background-write failure the worker
//! logs and moves on: the filesystem already holds the truth, so the
//! index stays recoverable via a 'rebuild dbs'.

use crate::save::update_tantivy_from_saveinstructions;
use crate::types::misc::{ID, TantivyIndex};
use crate::types::save::DefineNode;

use std::collections::HashMap;
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
  pub context_types : HashMap<ID, String>, }

/// Shared between the worker thread and the enqueue/wait API: the count
/// of writes not yet committed, and a condvar signalled when it reaches
/// zero.
struct Inflight {
  count  : Mutex<usize>,
  idle   : Condvar, }

struct Worker {
  sender   : Mutex<Sender<TantivyWriteTask>>,
  inflight : Arc<Inflight>, }

static WORKER : OnceLock<Worker> = OnceLock::new ();

fn worker () -> &'static Worker {
  WORKER . get_or_init ( || {
    let (sender, receiver) = channel::<TantivyWriteTask> ();
    let inflight : Arc<Inflight> = Arc::new ( Inflight {
      count : Mutex::new (0),
      idle  : Condvar::new (), } );
    let worker_inflight : Arc<Inflight> = inflight . clone ();
    std::thread::spawn ( move || {
      while let Ok (task) = receiver . recv () {
        // update_tantivy_from_saveinstructions takes the write lock itself.
        if let Err (e) = update_tantivy_from_saveinstructions (
          &task . instructions, &task . tantivy_index, &task . context_types )
        { tracing::error! (
            "Background Tantivy write failed: {}. The filesystem is correct; \
             run 'rebuild dbs' to resync the search index.", e ); }
        decrement_and_maybe_notify (&worker_inflight); } });
    Worker { sender : Mutex::new (sender), inflight } } ) }

fn decrement_and_maybe_notify (inflight : &Inflight) {
  let mut count : MutexGuard<usize> =
    inflight . count . lock () . unwrap_or_else ( |p| p . into_inner () );
  *count = count . saturating_sub (1);
  if *count == 0 { inflight . idle . notify_all (); } }

/// Enqueue a Tantivy index update to commit in the background, in FIFO
/// order. Returns immediately — the save does not wait for the commit.
pub fn enqueue_tantivy_write (
  task : TantivyWriteTask,
) {
  let worker : &Worker = worker ();
  { let mut count : MutexGuard<usize> =
      worker . inflight . count . lock () . unwrap_or_else ( |p| p . into_inner () );
    *count += 1; }
  if let Err (e) = worker . sender . lock ()
    . unwrap_or_else ( |p| p . into_inner () ) . send (task)
  { // worker thread is gone; undo the count so 'wait_for_idle' can't hang
    tracing::error! ("Tantivy background worker unavailable: {}", e);
    decrement_and_maybe_notify (&worker . inflight); } }

/// Block until every enqueued Tantivy write has committed, so the
/// caller (a search) sees an index reflecting all saves issued so far.
pub fn wait_for_tantivy_writes_idle () {
  let worker : &Worker = worker ();
  let mut count : MutexGuard<usize> =
    worker . inflight . count . lock () . unwrap_or_else ( |p| p . into_inner () );
  while *count > 0 {
    count = worker . inflight . idle . wait (count)
      . unwrap_or_else ( |p| p . into_inner () ); } }
