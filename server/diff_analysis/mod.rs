pub mod diff;
pub mod render;
pub mod snapshot;
pub mod types;
pub mod vanished;

use crate::diff_analysis::diff::{diff_snapshots, diff_snapshots_for_pids};
use crate::diff_analysis::render::render_report;
use crate::diff_analysis::snapshot::{read_changed_snapshot_pair, read_snapshot_pair};
use crate::diff_analysis::types::{
  ChangedSnapshotPair, DiffReport, DiffSelection, GraphSnapshot, SnapshotPair};
use crate::diff_analysis::vanished::{
  dangling_ids_in_snapshot, investigate_vanished_ids};
use crate::types::misc::SkgConfig;

pub fn diff_analysis_report (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<String, String> {
  diff_analysis_report_with_overPrivateText_pids (config, selection)
    . map ( |(report, _)| report )
}

/// Build the report and retain the telescope-coarse overPrivateText PIDs from both
/// compared snapshots so the transport can attach a release warning.
pub fn diff_analysis_report_with_overPrivateText_pids (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<(String, Vec<crate::types::misc::ID>), String> {
  let (mut report, after, mut overPrivateText_pids)
    : (DiffReport, GraphSnapshot, Vec<crate::types::misc::ID>) =
    match read_changed_snapshot_pair (config, selection) ? {
      Some (changed) => {
        let overPrivateText = overPrivateText_pids_in_pair (&changed . pair);
        ( report_from_changed_snapshot_pair (&changed),
          changed . pair . after,
          overPrivateText ) },
      None => {
        let pair : SnapshotPair =
          read_snapshot_pair (config, selection) ?;
        let report : DiffReport = diff_snapshots (&pair);
        let overPrivateText = overPrivateText_pids_in_pair (&pair);
        (report, pair . after, overPrivateText) }, };
  overPrivateText_pids . sort ();
  overPrivateText_pids . dedup ();
  report . vanished =
    investigate_vanished_ids (
      config, & dangling_ids_in_snapshot (&after) );
  Ok (( render_report (&report), overPrivateText_pids ))
}

fn overPrivateText_pids_in_pair (
  pair : &SnapshotPair,
) -> Vec<crate::types::misc::ID> {
  pair . before . nodes . values ()
    . chain (pair . after . nodes . values ())
    . filter ( |node| node . overPrivateText_telescope )
    . map ( |node| node . pid . clone () )
    . collect ()
}

fn report_from_changed_snapshot_pair (
  changed : &ChangedSnapshotPair,
) -> DiffReport {
  if changed . affected_pids . is_empty () {
    DiffReport::default ()
  } else {
    diff_snapshots_for_pids (&changed . pair, &changed . affected_pids) }
}
