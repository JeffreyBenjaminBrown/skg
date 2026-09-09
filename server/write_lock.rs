//! Process-global serialization of graph read-modify-write.
//!
//! The in-Rust graph is published via `ArcSwap` (RCU): a writer does
//! load -> clone-mutate -> store. Two concurrent writers can lose an
//! update (last `store` wins). Saves, reloads, and rebuilds each mutate
//! the three stores together; this lock ensures at most one such writer
//! runs at a time. Reads (renders) never take it -- they stay lock-free
//! on the `ArcSwap`.
//!
//! It is a tokio async mutex, which is runtime-agnostic: it works under
//! the `futures::executor::block_on` that drives each connection thread,
//! so the critical section can span the asynchronous store update.

use std::sync::OnceLock;
use tokio::sync::{Mutex, MutexGuard};

static GRAPH_WRITE_LOCK : OnceLock<Mutex<()>> = OnceLock::new ();

fn cell () -> &'static Mutex<()> {
  GRAPH_WRITE_LOCK . get_or_init ( || Mutex::new (( )) ) }

/// Acquire the global store-write lock, to be held for the duration of
/// one save / reload / rebuild's mutation of the derived stores. Drop
/// the returned guard to release.
pub async fn acquire_graph_write_lock (
) -> MutexGuard<'static, ()> {
  cell () . lock () . await }
