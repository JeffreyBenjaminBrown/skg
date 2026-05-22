pub mod diff;
pub mod render;
pub mod snapshot;
pub mod types;

use crate::diff_analysis::diff::{diff_snapshots, diff_snapshots_for_pids};
use crate::diff_analysis::render::render_report;
use crate::diff_analysis::snapshot::{read_changed_snapshot_pair, read_snapshot_pair};
use crate::diff_analysis::types::{
  ChangedSnapshotPair, DiffReport, DiffSelection, SnapshotPair};
use crate::types::misc::SkgConfig;

pub fn diff_analysis_report (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<String, String> {
  let report : DiffReport =
    match read_changed_snapshot_pair (config, selection) ? {
      Some (changed) =>
        report_from_changed_snapshot_pair (&changed),
      None => {
        let pair : SnapshotPair =
          read_snapshot_pair (config, selection) ?;
        diff_snapshots (&pair) }, };
  Ok ( render_report (&report) )
}

fn report_from_changed_snapshot_pair (
  changed : &ChangedSnapshotPair,
) -> DiffReport {
  if changed . affected_pids . is_empty () {
    DiffReport::default ()
  } else {
    diff_snapshots_for_pids (&changed . pair, &changed . affected_pids) }
}
