//! Opt-in scheduled consistency audit — paced daemon.
//!
//! When 'SkgConfig.auto_audit_daily' is true, the server runs the
//! in-Rust-graph-vs-TypeDB audit in a background thread at nice(19),
//! processing pids in batches of 128 with a 15-second sleep between
//! batches. A full 29k-node corpus takes ~1 hour, during which
//! TypeDB gets regular breathing room.
//!
//! Per-pid progress is persisted in '<data_root>/.audit.csv' (see
//! [[audit_store.rs]]), so a crash or restart mid-pass resumes
//! where it left off: pids stamped with today's date are skipped.
//!
//! Daemon lifecycle: runs for the whole server process. When the
//! current pass completes, the thread sleeps ~60s, checks for date
//! rollover, and on a new day rebuilds 'to_audit' from
//! (corpus - audited-today) and starts again. If skg is restarted
//! daily, resume works too — on startup the thread picks up today's
//! unfinished pids.
//!
//! Mismatch reporting:
//! - '<data_root>/audits.org' gets one headline per day with all the
//!   day's mismatches as sub-headlines, appended as they're found.
//! - The pending-warning slot ("N audit errors — see audits.org") is
//!   updated after each batch with the count of pids newly flagged
//!   since the last client-facing notification. Drained by the next
//!   outbound buffer (view render or save response).
//!
//! Batch tuning constants are at the top of this file so a reviewer
//! can see them without hunting through code.

use futures::executor::block_on;
use rand::seq::SliceRandom;
use std::collections::HashSet;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{Duration, SystemTime, UNIX_EPOCH};
use typedb_driver::{Transaction, TransactionType, TypeDBDriver};

use crate::dbs::in_rust_graph::{InRustGraph, InRustGraphHandle};
use crate::dbs::in_rust_graph::audit::{
  Mismatch,
  audit_one_node,
};
use crate::dbs::in_rust_graph::audit_store::{
  AuditRecord, RecordedMismatch,
  load, pids_needing_audit_today,
  prune_to_corpus, record_from_mismatch, save};
use crate::types::misc::{ID, SkgConfig};

// ---- Tunable constants ------------------------------------------

/// Pids audited per batch before sleeping. At 10 TypeDB queries per
/// pid, one batch fires ~1280 queries in rapid succession (bounded
/// by TYPEDB_CONCURRENT_TRANSACTIONS) before we pause.
const BATCH_SIZE : usize = 128;

/// Minimum seconds to sleep between batches. The actual sleep is
/// 'max (BATCH_SLEEP_SECS, batch_duration)', which caps TypeDB
/// utilization at ~50% regardless of how long a batch takes. A
/// fixed 15s between 60s batches would leave TypeDB pegged at ~80%;
/// scaling the sleep to match the batch gives real breathing room.
const BATCH_SLEEP_SECS : u64 = 15;

/// Seconds to sleep when the current pass is complete, before
/// checking whether a new day has started. Controls how quickly the
/// daemon notices midnight rollover on a long-running server.
const IDLE_POLL_SECS : u64 = 60;

// ---- Pending-warning slot ---------------------------------------

/// Pending audit warning for the next outbound buffer. Set by the
/// background audit thread after each batch that turned up new
/// mismatches; drained by whichever outbound-buffer path fires next
/// (view render or save response), which prepends the text to its
/// buffer.
static PENDING_AUDIT_WARNING : Mutex<Option<String>> = Mutex::new (None);

/// Pids that the audit flagged today but whose mismatches have not
/// yet been reported to the client (because no outbound buffer has
/// drained the warning slot since). Cleared when the slot is drained.
static UNREPORTED_FLAGGED_PIDS : Mutex<Option<HashSet<ID>>> =
  Mutex::new (None);

fn set_pending_audit_warning (msg : String) {
  *PENDING_AUDIT_WARNING . lock () . unwrap () = Some (msg); }

/// Called by outbound-buffer paths. Returns any pending warning and
/// clears BOTH the warning slot and the unreported-pids set, so
/// subsequent batches accumulate a fresh batch of 'new since
/// client-notified' pids.
pub fn take_pending_audit_warning () -> Option<String> {
  let msg : Option<String> =
    PENDING_AUDIT_WARNING . lock () . unwrap () . take ();
  if let Some (set) = UNREPORTED_FLAGGED_PIDS . lock () . unwrap () . as_mut () {
    set . clear (); }
  msg }

/// Add a pid newly flagged by this batch to the unreported set and
/// refresh the warning slot to reflect the cumulative count.
fn note_new_flagged_pid (pid : ID) {
  let mut guard = UNREPORTED_FLAGGED_PIDS . lock () . unwrap ();
  let set : &mut HashSet<ID> = guard . get_or_insert_with (HashSet::new);
  set . insert (pid);
  let n : usize = set . len ();
  drop (guard);
  set_pending_audit_warning ( format! (
    "{} audit error{} detected today — see 'audits.org'.",
    n, if n == 1 { "" } else { "s" } )); }

// ---- Startup entry point ----------------------------------------

/// If 'auto_audit_daily' is enabled, spawn the background audit
/// daemon. Returns immediately.
pub fn schedule_daemon (
  config : &SkgConfig,
  driver : Arc<TypeDBDriver>,
  graph  : InRustGraphHandle,
) {
  if ! config . auto_audit_daily {
    return; }
  tracing::info! ("auto_audit_daily: starting paced audit daemon");
  let data_root : PathBuf = config . data_root . clone ();
  let db_name   : String  = config . db_name . clone ();
  thread::spawn ( move || {
    set_lowest_priority ();
    daemon_loop (data_root, db_name, driver, graph); } ); }

// ---- Daemon ----------------------------------------------------

fn daemon_loop (
  data_root : PathBuf,
  db_name   : String,
  driver    : Arc<TypeDBDriver>,
  graph     : InRustGraphHandle,
) {
  let mut today : String = today_yyyy_mm_dd ();
  let mut store : Vec<AuditRecord> =
    initial_store_for_today (&data_root, &graph, &today);
  let mut to_audit : Vec<ID> =
    build_todo (&graph, &store, &today);
  tracing::info! (
    initial_todo = to_audit . len (), date = %today,
    "auto_audit_daily: daemon loop starting");
  loop {
    if to_audit . is_empty () {
      thread::sleep (Duration::from_secs (IDLE_POLL_SECS));
      let now : String = today_yyyy_mm_dd ();
      if now != today {
        today = now;
        store = initial_store_for_today (&data_root, &graph, &today);
        to_audit = build_todo (&graph, &store, &today);
        tracing::info! (
          new_todo = to_audit . len (), date = %today,
          "auto_audit_daily: date rolled over; new pass starting"); }
      continue; }
    // Grab up to BATCH_SIZE pids from the end of the to_audit list.
    let take : usize = BATCH_SIZE . min (to_audit . len ());
    let batch_start : usize = to_audit . len () - take;
    let batch : Vec<ID> = to_audit . split_off (batch_start);
    let batch_start_time : std::time::Instant =
      std::time::Instant::now ();
    process_batch (
      &batch, &graph, &db_name, &driver,
      &data_root, &today, &mut store);
    // Adaptive sleep: at least BATCH_SLEEP_SECS, but no less than
    // the batch's wall-clock duration. Caps TypeDB utilization at
    // 50% regardless of query speed.
    let work_dur : Duration = batch_start_time . elapsed ();
    let sleep_dur : Duration =
      work_dur . max (Duration::from_secs (BATCH_SLEEP_SECS));
    thread::sleep (sleep_dur); } }

/// On pass start (initial startup or date rollover): load the
/// existing store, prune entries for pids no longer in the corpus,
/// save the pruned store back. Returns the pruned store.
fn initial_store_for_today (
  data_root : &Path,
  graph     : &InRustGraphHandle,
  _today    : &str,
) -> Vec<AuditRecord> {
  let snap : Arc<InRustGraph> = graph . load_full ();
  let corpus : HashSet<ID> =
    snap . nodes . keys () . cloned () . collect ();
  let raw : Vec<AuditRecord> = load (data_root);
  let pruned : Vec<AuditRecord> = prune_to_corpus (raw, &corpus);
  if let Err (e) = save (data_root, &pruned) {
    tracing::warn! (error = %e,
      "auto_audit_daily: failed to persist pruned store"); }
  pruned }

/// Build the per-pass to-audit list: corpus minus pids already
/// audited today. Randomized so repeat passes don't always hit the
/// same nodes first.
fn build_todo (
  graph : &InRustGraphHandle,
  store : &[AuditRecord],
  today : &str,
) -> Vec<ID> {
  let snap : Arc<InRustGraph> = graph . load_full ();
  let corpus : HashSet<ID> =
    snap . nodes . keys () . cloned () . collect ();
  let mut list : Vec<ID> =
    pids_needing_audit_today (store, &corpus, today);
  list . shuffle (&mut rand::thread_rng ());
  list }

/// Audit all 'pids' concurrently (up to PID_CONCURRENCY at once)
/// on the given tx, preserving input order in the output vec.
async fn audit_batch_concurrently<'a> (
  snap  : &'a Arc<InRustGraph>,
  tx    : &'a Transaction,
  pids  : &'a [ID],
) -> Vec<(ID, Result<Vec<Mismatch>, String>)> {
  use futures::stream::{FuturesUnordered, StreamExt};
  type Fut<'a> = std::pin::Pin<
    Box<dyn std::future::Future<
      Output = (ID, Result<Vec<Mismatch>, String>) > + 'a>>;
  let mut stream : FuturesUnordered<Fut<'a>> = FuturesUnordered::new ();
  let mut iter = pids . iter ();
  for _ in 0 .. PID_CONCURRENCY {
    if let Some (pid) = iter . next () {
      let pid_owned : ID = pid . clone ();
      stream . push ( Box::pin ( async move {
        ( pid_owned . clone (),
          audit_one_node (snap, tx, &pid_owned) . await
            . map_err ( |e| e . to_string () ) ) } )); } }
  let mut out : Vec<(ID, Result<Vec<Mismatch>, String>)> =
    Vec::with_capacity (pids . len ());
  while let Some (done) = stream . next () . await {
    out . push (done);
    if let Some (pid) = iter . next () {
      let pid_owned : ID = pid . clone ();
      stream . push ( Box::pin ( async move {
        ( pid_owned . clone (),
          audit_one_node (snap, tx, &pid_owned) . await
            . map_err ( |e| e . to_string () ) ) } )); } }
  out }

/// How many pids to audit concurrently within a batch. Bench with
/// the real 29k-node corpus (see tools/bench-audit.rs) showed:
///   - sequential (1):  1.86 ms/pid
///   - concurrency 8:   0.73 ms/pid (~2.5x faster)
///   - concurrency 16: 0.86 ms/pid (starts regressing)
///   - concurrency 32: 1.13 ms/pid (worse)
/// Sweet spot is around 8 — above that, TypeDB's planner/executor
/// starts thrashing.
const PID_CONCURRENCY : usize = 8;

/// Process one batch: audit each pid, update the store in in_rust_graph,
/// persist the whole store, append discovered mismatches to
/// audits.org, and refresh the client-facing warning slot.
///
/// Opens one read transaction per batch, reuses it across all pids,
/// and audits up to PID_CONCURRENCY pids concurrently.
fn process_batch (
  batch     : &[ID],
  graph     : &InRustGraphHandle,
  db_name   : &str,
  driver    : &TypeDBDriver,
  data_root : &Path,
  today     : &str,
  store     : &mut Vec<AuditRecord>,
) {
  let t_batch : std::time::Instant = std::time::Instant::now ();
  let snap : Arc<InRustGraph> = graph . load_full ();
  let tx : Transaction = match block_on (
    driver . transaction (db_name, TransactionType::Read) )
  { Ok (t) => t,
    Err (e) => {
      tracing::warn! (error = %e,
        "auto_audit_daily: could not open read tx; skipping batch");
      return; } };
  let t_first_pid : std::time::Instant = std::time::Instant::now ();
  let results : Vec<(ID, Result<Vec<Mismatch>, String>)> = block_on (
    audit_batch_concurrently (&snap, &tx, batch));
  let mut new_mismatches : Vec<Mismatch> = Vec::new ();
  for (pid, result) in results {
    let mismatches : Vec<Mismatch> = match result {
      Ok (v)  => v,
      Err (e) => {
        tracing::warn! (pid = %pid, error = %e,
          "auto_audit_daily: audit of node failed; skipping");
        continue; } };
    let recorded : Vec<RecordedMismatch> =
      mismatches . iter () . map (record_from_mismatch) . collect ();
    if ! mismatches . is_empty () {
      note_new_flagged_pid (pid . clone ());
      new_mismatches . extend (mismatches); }
    upsert_record (store, AuditRecord {
      pid : pid . clone (),
      audited_on : today . to_string (),
      mismatches : recorded, }); }
  let audit_elapsed : f64 =
    t_first_pid . elapsed () . as_secs_f64 ();
  drop (tx);
  if let Err (e) = save (data_root, store) {
    tracing::warn! (error = %e,
      "auto_audit_daily: failed to persist store after batch"); }
  if ! new_mismatches . is_empty () {
    append_mismatches_to_audits_org (
      data_root, today, &new_mismatches); }
  tracing::info! (
    pids = batch . len (),
    audit_s = format! ("{:.2}", audit_elapsed),
    batch_s = format! ("{:.2}", t_batch . elapsed () . as_secs_f64 ()),
    "auto_audit_daily: batch done"); }

/// Replace or insert an AuditRecord in 'store' by pid.
fn upsert_record (store : &mut Vec<AuditRecord>, rec : AuditRecord) {
  if let Some (existing) = store . iter_mut ()
    . find ( |r| r . pid == rec . pid )
  { *existing = rec; }
  else { store . push (rec); } }

// ---- audits.org writer ------------------------------------------

/// Append mismatches to '<data_root>/audits.org'. If this is the
/// first append of the day, write today's section heading first.
///
/// "First append of the day" is detected by scanning the file's
/// current contents for '* <today>'. If it's there, skip the
/// heading; otherwise write it.
fn append_mismatches_to_audits_org (
  data_root  : &Path,
  date       : &str,
  mismatches : &[Mismatch],
) {
  let path : PathBuf = data_root . join ("audits.org");
  let need_heading : bool = ! today_heading_present (&path, date);
  let mut file = match OpenOptions::new ()
    . create (true) . append (true) . open (&path)
  { Ok (f)  => f,
    Err (e) => { tracing::warn! (
      path = %path . display (), error = %e,
      "auto_audit_daily: failed to open audits.org");
      return; } };
  let mut text : String = String::new ();
  if need_heading {
    text . push_str ( & format! ("\n* {}\n", date)); }
  for m in mismatches {
    match m {
      Mismatch::Relationship (m) => {
        text . push_str ( & format! (
          "** Node '{}': {} {} mismatch\n   InRustGraph: {:?}\n   TypeDB: {:?}\n",
          m . pid, m . relation, m . role,
          m . in_rust_graph, m . typedb )); }
      Mismatch::Source (m) => {
        text . push_str ( & format! (
          "** Node '{}': source mismatch\n   InRustGraph: {}\n   TypeDB: {}\n",
          m . pid,
          m . in_rust_graph,
          m . typedb )); }
      Mismatch::MissingTypedbNode { pid } => {
        text . push_str ( & format! (
          "** Node '{}': missing TypeDB node\n",
          pid )); }}}
  if let Err (e) = file . write_all (text . as_bytes ()) {
    tracing::warn! (path = %path . display (), error = %e,
      "auto_audit_daily: failed to append to audits.org"); } }

fn today_heading_present (path : &Path, date : &str) -> bool {
  let needle : String = format! ("* {}", date);
  match fs::read_to_string (path) {
    Ok (text) => text . lines () . any ( |l| l . starts_with (&needle) ),
    Err (_)   => false, } }

// ---- Priority / date helpers ------------------------------------

/// Drop this thread's scheduling priority to the OS minimum so the
/// audit never contends with user-facing work. On Linux this is
/// 'nice(19)'.
///
/// PITFALL: nice only affects CPU on this thread (the client side of
/// audit queries). TypeDB processes queries on its own threads at
/// normal priority, so niceness alone wouldn't prevent TypeDB
/// saturation — which is why the audit is ALSO paced (BATCH_SLEEP_SECS)
/// to give TypeDB breathing room between bursts.
#[cfg(unix)]
fn set_lowest_priority () {
  // Safety: nice(2) is a pure syscall; 19 is the maximum niceness.
  unsafe { libc::nice (19); } }

#[cfg(not(unix))]
fn set_lowest_priority () { /* No-op on non-Unix */ }

/// Format the current local date as 'YYYY-MM-DD'. No chrono
/// dependency: we convert UTC seconds to a calendar date by hand.
fn today_yyyy_mm_dd () -> String {
  let secs : u64 = SystemTime::now ()
    . duration_since (UNIX_EPOCH) . map ( |d| d . as_secs () )
    . unwrap_or (0);
  days_since_epoch_to_date_string (secs / 86_400) }

/// Convert days-since-1970-01-01 to 'YYYY-MM-DD'.
fn days_since_epoch_to_date_string (days : u64) -> String {
  // Shift the reference to 2000-03-01 so leap-year arithmetic is simple:
  // 2000-03-01 is 11_017 days after 1970-01-01.
  let days_from_2000_03_01 : i64 = days as i64 - 11_017;
  // 400-year cycle has 146_097 days; 100-year cycle 36_524; 4-year 1_461.
  let (mut y, mut d) : (i64, i64) = (2000, days_from_2000_03_01);
  let q400 : i64 = d . div_euclid (146_097); y += 400 * q400; d -= 146_097 * q400;
  let q100 : i64 = (d / 36_524) . min (3); y += 100 * q100; d -= 36_524 * q100;
  let q4   : i64 = d / 1_461;               y +=   4 * q4;  d -=  1_461 * q4;
  let q1   : i64 = (d / 365) . min (3);     y +=       q1;  d -=    365 * q1;
  // 'd' is now days into a March-started year. Convert to month/day.
  // Month lengths starting Mar: 31,30,31,30,31,31,30,31,30,31,31,29
  let month_lengths : [i64; 12] = [31,30,31,30,31,31,30,31,30,31,31,29];
  let mut m : i64 = 0;
  while m < 11 && d >= month_lengths[m as usize] {
    d -= month_lengths[m as usize]; m += 1; }
  let day   : i64 = d + 1;
  let month : i64 = ((m + 2) % 12) + 1;   // Mar=3..Feb=2
  let year  : i64 = if month <= 2 { y + 1 } else { y };
  format! ("{:04}-{:02}-{:02}", year, month, day) }

#[cfg(test)]
mod tests {
  use super::*;
  use tempfile::TempDir;

  #[test]
  fn date_conversion_known_values () {
    assert_eq! (days_since_epoch_to_date_string (0), "1970-01-01");
    assert_eq! (days_since_epoch_to_date_string (10_957), "2000-01-01");
    assert_eq! (days_since_epoch_to_date_string (11_016), "2000-02-29");
    assert_eq! (days_since_epoch_to_date_string (11_017), "2000-03-01");
    assert_eq! (days_since_epoch_to_date_string (20_453), "2025-12-31");
    assert_eq! (days_since_epoch_to_date_string (20_454), "2026-01-01"); }

  #[test]
  fn today_heading_detection () {
    let tmp : TempDir = TempDir::new () . unwrap ();
    let path : PathBuf = tmp . path () . join ("audits.org");
    // Missing file: no heading.
    assert! (! today_heading_present (&path, "2026-04-21"));
    fs::write (
      &path,
      "\n* 2026-04-20\n** prior stuff\n\n* 2026-04-21\n** today\n"
    ) . unwrap ();
    assert! (today_heading_present (&path, "2026-04-21"));
    assert! (today_heading_present (&path, "2026-04-20"));
    assert! (! today_heading_present (&path, "2026-04-22")); }

  #[test]
  fn upsert_replaces_existing () {
    let mut store : Vec<AuditRecord> = vec! [
      AuditRecord { pid : ID::new ("a"),
                    audited_on : "2026-04-20" . to_string (),
                    mismatches : Vec::new () },
    ];
    upsert_record (&mut store, AuditRecord {
      pid : ID::new ("a"),
      audited_on : "2026-04-21" . to_string (),
      mismatches : Vec::new () });
    assert_eq! (store . len (), 1);
    assert_eq! (store[0] . audited_on, "2026-04-21"); }

  #[test]
  fn upsert_inserts_new () {
    let mut store : Vec<AuditRecord> = Vec::new ();
    upsert_record (&mut store, AuditRecord {
      pid : ID::new ("a"),
      audited_on : "2026-04-21" . to_string (),
      mismatches : Vec::new () });
    assert_eq! (store . len (), 1);
    assert_eq! (store[0] . pid . as_str (), "a"); }

}
