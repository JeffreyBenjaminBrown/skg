use skg::dbs::filesystem::not_nodes::load_config;
use skg::diff_analysis::diff_analysis_report;
use skg::diff_analysis::diff::diff_snapshots;
use skg::diff_analysis::render::render_report;
use skg::diff_analysis::snapshot::read_snapshot_pair;
use skg::diff_analysis::types::{DiffReport, DiffSelection, SnapshotPair};
use skg::types::misc::SkgConfig;

use std::env;
use std::error::Error;
use std::time::{Duration, Instant};

fn main (
) -> Result<(), Box<dyn Error>> {
  let args : Vec<String> =
    env::args () . collect ();
  let config_path : &str =
    args . get (1)
      . map ( |s| s . as_str () )
      . unwrap_or ("data/skgconfig.toml");
  let selection : DiffSelection =
    selection_from_args (&args) ?;
  let config : SkgConfig =
    load_config (config_path) ?;
  if args . get (3) . map ( |s| s . as_str () ) != Some ("full") {
    let rendered : String =
      timed ("diff_analysis_report", || {
        diff_analysis_report (&config, selection) }) ?;
    println! ("rendered bytes: {}", rendered . len ());
    return Ok (( )); }
  let total_start : Instant =
    Instant::now ();
  let pair : SnapshotPair =
    timed ("read snapshots", || read_snapshot_pair (&config, selection)) ?;
  println! (
    "before nodes: {}, before ids: {}",
    pair . before . nodes . len (),
    pair . before . id_sources . len ());
  println! (
    "after nodes: {}, after ids: {}",
    pair . after . nodes . len (),
    pair . after . id_sources . len ());
  let report : DiffReport =
    timed ("diff snapshots", || Ok::<DiffReport, String> (
      diff_snapshots (&pair))) ?;
  println! (
    "duplicate ids: {}, buckets: {}, affected nodes: {}",
    report . duplicate_ids . len (),
    report . buckets . len (),
    report . buckets . iter ()
      . map ( |bucket| bucket . nodes . len () )
      . sum::<usize> ());
  let rendered : String =
    timed ("render report", || Ok::<String, String> (
      render_report (&report))) ?;
  println! ("rendered bytes: {}", rendered . len ());
  println! ("total: {}", format_duration (total_start . elapsed ()));
  Ok (( )) }

fn selection_from_args (
  args : &[String],
) -> Result<DiffSelection, String> {
  let mode : &str =
    args . get (2)
      . map ( |s| s . as_str () )
      . unwrap_or ("all");
  match mode {
    "all"      => Ok ( DiffSelection {
      include_staged: true, include_unstaged: true }),
    "staged"   => Ok ( DiffSelection {
      include_staged: true, include_unstaged: false }),
    "unstaged" => Ok ( DiffSelection {
      include_staged: false, include_unstaged: true }),
    _          => Err ( format! (
      "Usage: bench-diff-analysis [CONFIG] [all|staged|unstaged], got {:?}",
      mode )), } }

fn timed<T, E, F> (
  label : &str,
  f     : F,
) -> Result<T, E>
where
  F : FnOnce () -> Result<T, E>,
{
  let start : Instant =
    Instant::now ();
  let result : Result<T, E> =
    f ();
  println! ("{}: {}", label, format_duration (start . elapsed ()));
  result
}

fn format_duration (
  duration : Duration,
) -> String {
  format! (
    "{}.{:03}s",
    duration . as_secs (),
    duration . subsec_millis ()) }
