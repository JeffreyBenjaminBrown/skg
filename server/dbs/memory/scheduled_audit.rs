//! Opt-in scheduled consistency audit.
//!
//! When 'SkgConfig.auto_audit_daily' is true, the server runs the
//! memory-vs-TypeDB audit at most once per day, backgrounded at the
//! lowest OS scheduling priority so the user never feels it.
//!
//! Staleness is tracked by the date stored in '{data_root}/last-audit.time'.
//! On server startup, if today's date (local) is later than the date in
//! that file — or the file doesn't exist — an audit thread spawns.
//! On completion, the thread writes today's date back, and on mismatch
//! appends to '{data_root}/audits.org'.

use futures::executor::block_on;
use std::fs::{self, OpenOptions};
use std::io::Write;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;
use std::time::{SystemTime, UNIX_EPOCH};
use typedb_driver::TypeDBDriver;

use crate::dbs::memory::{InRustGraph, InRustGraphHandle};
use crate::dbs::memory::audit::{audit_memory_against_typedb, Mismatch};
use crate::types::misc::SkgConfig;

/// Pending audit warning for the next outbound buffer. Set by the
/// background audit thread on mismatch; drained by whichever
/// outbound-buffer path fires next (view render or save response),
/// which prepends the text to its buffer.
static PENDING_AUDIT_WARNING : Mutex<Option<String>> = Mutex::new (None);

/// Set the pending-warning slot (overwriting any prior value).
pub fn set_pending_audit_warning (msg : String) {
  *PENDING_AUDIT_WARNING . lock () . unwrap () = Some (msg); }

/// Take the pending-warning slot if present. Called by outbound-
/// buffer paths; they prepend the returned text to their buffer.
pub fn take_pending_audit_warning () -> Option<String> {
  PENDING_AUDIT_WARNING . lock () . unwrap () . take () }

/// Convenience: if a warning is pending, prepend it (plus a blank
/// line) to 'buf'. Used by handlers that emit buffer text.
pub fn prepend_audit_warning (buf : String) -> String {
  match take_pending_audit_warning () {
    None      => buf,
    Some (w)  => format! ("{}\n\n{}", w, buf), } }

/// If 'auto_audit_daily' is enabled and no audit has run today, spawn
/// a backgrounded audit thread. Returns immediately; the thread does
/// its own bookkeeping.
pub fn schedule_if_stale (
  config : &SkgConfig,
  driver : Arc<TypeDBDriver>,
  graph  : InRustGraphHandle,
) {
  if ! config . auto_audit_daily {
    return; }
  let today : String = today_yyyy_mm_dd ();
  let stamp_path : PathBuf = last_audit_path (config);
  let last : Option<String> = fs::read_to_string (&stamp_path)
    . ok ()
    . map ( |s| s . trim () . to_string () );
  let stale : bool = match &last {
    None            => true,
    Some (date)     => date . as_str () < today . as_str (), };
  if ! stale {
    tracing::debug! (last = ?last, "auto_audit_daily: last audit is recent; skipping");
    return; }
  tracing::info! ("auto_audit_daily: scheduling background audit");
  let config_clone   : SkgConfig = config . clone ();
  let data_root      : PathBuf  = config . data_root . clone ();
  let db_name        : String   = config . db_name . clone ();
  thread::spawn ( move || {
    set_lowest_priority ();
    let snap : Arc<InRustGraph> = graph . load_full ();
    let result : Result <Vec<Mismatch>, Box<dyn std::error::Error>> =
      block_on ( audit_memory_against_typedb (
        &snap, &db_name, &driver ) );
    match result {
      Ok (mismatches) => {
        if ! mismatches . is_empty () {
          append_mismatches_to_audits_org (
            &data_root, &today, &mismatches );
          set_pending_audit_warning ( format! (
            "{} audit error{} detected during the daily audit — see 'audits.org'.",
            mismatches . len (),
            if mismatches . len () == 1 { "" } else { "s" } )); }
        write_last_audit_stamp (&config_clone, &today);
        tracing::info! (
          mismatches = mismatches . len (),
          "auto_audit_daily: audit complete"); }
      Err (e) => {
        // Don't update last-audit.time on error — try again next launch.
        tracing::error! (error = %e,
          "auto_audit_daily: audit failed"); } } } ); }

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

fn last_audit_path (config : &SkgConfig) -> PathBuf {
  config . data_root . join ("last-audit.time") }

fn write_last_audit_stamp (config : &SkgConfig, date : &str) {
  let path : PathBuf = last_audit_path (config);
  if let Err (e) = fs::write (&path, format! ("{}\n", date)) {
    tracing::warn! (path = %path . display (), error = %e,
      "auto_audit_daily: failed to write last-audit.time"); } }

fn append_mismatches_to_audits_org (
  data_root  : &std::path::Path,
  date       : &str,
  mismatches : &[Mismatch],
) {
  let path : PathBuf = data_root . join ("audits.org");
  let mut file = match OpenOptions::new ()
    . create (true) . append (true) . open (&path)
  { Ok (f)  => f,
    Err (e) => { tracing::warn! (
      path = %path . display (), error = %e,
      "auto_audit_daily: failed to open audits.org");
      return; } };
  let mut text : String = format! (
    "\n* {} ({} error{})\n",
    date, mismatches . len (),
    if mismatches . len () == 1 { "" } else { "s" } );
  for m in mismatches {
    text . push_str ( & format! (
      "** Node '{}': {} {} mismatch\n   Memory: {:?}\n   TypeDB: {:?}\n",
      m . pid, m . relation, m . role,
      m . memory, m . typedb )); }
  if let Err (e) = file . write_all (text . as_bytes ()) {
    tracing::warn! (path = %path . display (), error = %e,
      "auto_audit_daily: failed to append to audits.org"); } }

/// Drop this thread's scheduling priority to the OS minimum so the
/// audit never contends with user-facing work. On Linux this is
/// 'nice(19)'.
#[cfg(unix)]
fn set_lowest_priority () {
  // Safety: nice(2) is a pure syscall; 19 is the maximum niceness.
  unsafe { libc::nice (19); } }

#[cfg(not(unix))]
fn set_lowest_priority () { /* No-op on non-Unix */ }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn date_conversion_known_values () {
    // UNIX epoch.
    assert_eq! (days_since_epoch_to_date_string (0), "1970-01-01");
    // First day of 2000.
    assert_eq! (days_since_epoch_to_date_string (10_957), "2000-01-01");
    // Leap-day transition.
    assert_eq! (days_since_epoch_to_date_string (11_016), "2000-02-29");
    assert_eq! (days_since_epoch_to_date_string (11_017), "2000-03-01");
    // A post-2000 date we can cross-check against a calendar.
    assert_eq! (days_since_epoch_to_date_string (20_453), "2025-12-31");
    assert_eq! (days_since_epoch_to_date_string (20_454), "2026-01-01"); } }
