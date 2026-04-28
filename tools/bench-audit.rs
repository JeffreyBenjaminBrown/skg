/* bench-audit
 * Benchmark the paced-audit's per-pid cost.
 * Usage: cargo run --bin bench-audit [config-path] [n_pids]
 *   default config: data/skgconfig.toml
 *   default n_pids: 64
 *
 * Exercises the same code path as the scheduled audit daemon:
 * initializes the in-Rust graph from .skg files, opens one TypeDB
 * read transaction, then calls 'audit_one_node' on N pids and
 * reports per-pid wall time.
 *
 * Expects TypeDB to be running on 127.0.0.1:1729 with the configured
 * database already populated. The server binary should NOT be
 * running (or auto_audit_daily should be false) to avoid contention.
 */

use skg::dbs::filesystem::multiple_nodes::check_for_duplicate_ids_across_sources;
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::memory::{
  audit::audit_one_node,
  InRustGraph, InRustGraphHandle,
  init_global_handle, new_handle,
};
use skg::types::misc::{ID, SkgConfig};
use skg::types::nodes::complete::NodeComplete;

use futures::stream::{StreamExt, FuturesUnordered};
use std::env;
use std::sync::Arc;
use std::time::Instant;
use typedb_driver::{
  Credentials, DriverOptions, Transaction, TransactionType, TypeDBDriver,
};

#[tokio::main]
async fn main () -> Result<(), Box<dyn std::error::Error>> {
  let args : Vec<String> = env::args () . collect ();
  let config_path : &str =
    if args . len () > 1 { &args[1] } else { "data/skgconfig.toml" };
  let n_pids : usize =
    if args . len () > 2 { args[2] . parse () ? } else { 64 };

  println! ("=== Audit per-pid benchmark ===");
  println! ("Config:  {}", config_path);
  println! ("N pids:  {}", n_pids);

  let config : SkgConfig = load_config (config_path) ?;

  // Build the in-Rust graph from disk.
  let t0 : Instant = Instant::now ();
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) ?;
  check_for_duplicate_ids_across_sources (
    &nodes, &config . data_root) ?;
  let graph : InRustGraph = InRustGraph::from_nodecompletes (&nodes);
  let handle : InRustGraphHandle = new_handle (graph);
  init_global_handle (Arc::clone (&handle));
  let snap : Arc<InRustGraph> = handle . load_full ();
  println! ("Loaded graph ({} nodes) in {:.3}s",
            snap . nodes . len (),
            t0 . elapsed () . as_secs_f64 ());

  // Connect to TypeDB.
  let driver : TypeDBDriver = TypeDBDriver::new (
    "127.0.0.1:1729",
    Credentials::new ("admin", "password"),
    DriverOptions::new (false, None) ?
  ) . await ?;

  // Pick a deterministic sample of pids (first N by sort order).
  let mut pids : Vec<ID> =
    snap . nodes . keys () . cloned () . collect ();
  pids . sort ();
  pids . truncate (n_pids);

  // Warm-up the connection + tx so the first-call overhead is not
  // attributed to the timed batch.
  let tx_warm : Transaction = driver . transaction (
    &config . db_name, TransactionType::Read ) . await ?;
  let _ = audit_one_node (&snap, &tx_warm, &pids[0]) . await ?;
  drop (tx_warm);

  // Sequential baseline + collect mismatch counts for correctness.
  let (seq_batch_ms, mismatch_count) = {
    let tx : Transaction = driver . transaction (
      &config . db_name, TransactionType::Read ) . await ?;
    let t : Instant = Instant::now ();
    let mut mm_total : usize = 0;
    for pid in &pids {
      let mm = audit_one_node (&snap, &tx, pid) . await ?;
      mm_total += mm . len (); }
    (t . elapsed () . as_secs_f64 () * 1000.0, mm_total) };
  println! ("\nSequential (1 pid at a time):    {:.2} ms  ({:.2} ms/pid)  mismatches={}",
            seq_batch_ms, seq_batch_ms / n_pids as f64, mismatch_count);

  // Pid-level concurrency sweep.
  for pid_conc in [2usize, 4, 8, 16, 32] {
    let tx : Transaction = driver . transaction (
      &config . db_name, TransactionType::Read ) . await ?;
    let t : Instant = Instant::now ();
    let mut stream = FuturesUnordered::new ();
    let mut iter = pids . iter ();
    for _ in 0..pid_conc {
      if let Some (pid) = iter . next () {
        stream . push (audit_one_node (&snap, &tx, pid)); } }
    let mut completed : usize = 0;
    while let Some (res) = stream . next () . await {
      let _ = res ?;
      completed += 1;
      if let Some (pid) = iter . next () {
        stream . push (audit_one_node (&snap, &tx, pid)); } }
    let ms : f64 = t . elapsed () . as_secs_f64 () * 1000.0;
    println! ("Pid concurrency = {:>2}:           {:.2} ms  ({:.2} ms/pid, completed {})",
              pid_conc, ms, ms / n_pids as f64, completed); }
  Ok (( )) }
