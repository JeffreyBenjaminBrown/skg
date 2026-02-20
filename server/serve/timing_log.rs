use crate::types::misc::SkgConfig;

use std::cell::{Cell, RefCell};
use std::fs::OpenOptions;
use std::future::Future;
use std::io::Write;
use std::time::{Duration, Instant};

struct TimingEntry {
  start    : Instant, // for sorting: earlier start => appears first in log
  depth    : usize,   // nesting level (0 = outermost). A caller has a lower level than its children. (Callees are rendered as org-mode children in the log.)
  label    : String,  // describes what action was logged
  duration : Duration,
}

/// Entries are buffered (not written immediately)
/// so that the log reads in pre-order (parent before children)
/// even though entries complete in post-order.
/// The outermost timed block flushes the buffer,
/// sorted by start time, which recovers pre-order.
thread_local! {
  static TIMING_DEPTH  : Cell<usize> =
    Cell::new (0);
  static TIMING_BUFFER : RefCell<Vec<TimingEntry>> =
    RefCell::new (Vec::new() ); }

/// Run a sync closure, log its duration, return its result.
pub fn timed<T> (
  config : &SkgConfig,
  label  : &str,
  f      : impl FnOnce() -> T,
) -> T {
  if ! config.timing_log { return f(); }
  let depth : usize =
    TIMING_DEPTH.with ( |d| {
      let current : usize = d.get();
      d.set ( current + 1 );
      current } );
  let t0 : Instant = Instant::now();
  let result : T = f();
  let duration : Duration = t0.elapsed();
  push_buffer_timing_entry ( t0, depth, label, duration );
  TIMING_DEPTH.with ( |d| d.set ( d.get() - 1 ));
  if depth == 0 { flush_timing_buffer ( config ); }
  result }

/// Like 'timed' but for async code.
/// Run a future, log its duration, return its result.
pub async fn timed_async<T, F : Future<Output = T>> (
  config : &SkgConfig,
  label  : &str,
  f      : F,
) -> T {
  if ! config.timing_log { return f.await; }
  let depth : usize =
    TIMING_DEPTH.with ( |d| {
      let current : usize = d.get();
      d.set ( current + 1 );
      current } );
  let t0 : Instant = Instant::now();
  let result : T = f.await;
  let duration : Duration = t0.elapsed();
  push_buffer_timing_entry ( t0, depth, label, duration );
  TIMING_DEPTH.with ( |d| d.set ( d.get() - 1 ));
  if depth == 0 {
    flush_timing_buffer ( config ); }
  result }

fn push_buffer_timing_entry (
  start    : Instant,
  depth    : usize,
  label    : &str,
  duration : Duration,
) {
  TIMING_BUFFER.with ( |buf|
    buf.borrow_mut().push ( TimingEntry {
      start,
      depth,
      label    : label.to_string(),
      duration } )) }

fn flush_timing_buffer (
  config : &SkgConfig,
) {
  let mut entries : Vec<TimingEntry> =
    TIMING_BUFFER.with ( |buf|
      buf.replace ( Vec::new() ));
  entries.sort_by_key ( |e| e.start );
  let path : std::path::PathBuf =
    config.config_dir.join ( "timing.log" );
  if let Ok ( mut file ) =
    OpenOptions::new()
    .append ( true )
    .create ( true )
    .open ( &path )
  { for entry in entries.iter() {
      let stars : String =
        "*".repeat ( entry.depth + 1 );
      let _ : Result<(), std::io::Error> =
        writeln! ( file, "{} {}: {:.3}s",
                   stars, entry.label,
                   entry.duration.as_secs_f64 () ); }} }

pub fn clear_timing_log (
  config : &SkgConfig
) {
  if ! config.timing_log { return; }
  let path : std::path::PathBuf =
    config.config_dir.join ( "timing.log" );
  let _ : Result<std::fs::File, std::io::Error> =
    OpenOptions::new()
    .write ( true )
    .truncate ( true )
    .create ( true )
    .open ( &path ); }
