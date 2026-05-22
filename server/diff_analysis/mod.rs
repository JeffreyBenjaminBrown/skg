pub mod diff;
pub mod render;
pub mod snapshot;
pub mod types;

use crate::diff_analysis::diff::diff_snapshots;
use crate::diff_analysis::render::render_report;
use crate::diff_analysis::snapshot::read_snapshot_pair;
use crate::diff_analysis::types::{DiffReport, DiffSelection, SnapshotPair};
use crate::types::misc::SkgConfig;

pub fn diff_analysis_report (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<String, String> {
  let pair : SnapshotPair =
    read_snapshot_pair (config, selection) ?;
  let report : DiffReport =
    diff_snapshots (&pair);
  Ok ( render_report (&report) )
}
