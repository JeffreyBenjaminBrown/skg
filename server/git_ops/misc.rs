use crate::types::git::{DiffStatus, PathDiffStatus};

use git2::{DiffDelta, Delta, Repository};
use std::path::{Path, PathBuf};

/// Convert a git2 DiffDelta to our PathDiffStatus type.
pub(super) fn diff_delta_to_entry (
  delta : &DiffDelta
) -> Option<PathDiffStatus> {
  let status : DiffStatus =
    match delta . status() {
      Delta::Added | Delta::Untracked => DiffStatus::Added,
      Delta::Modified => DiffStatus::Modified,
      Delta::Deleted => DiffStatus::Deleted,
      Delta::Renamed | Delta::Copied => DiffStatus::Modified, // For renamed/copied, the new_file path is what we care about
      _ => return None, }; // Ignore other status types (Ignored, Typechange, etc.)
  let path : Option<&Path> = // Get the path from the appropriate file based on status
    match status {
      DiffStatus::Deleted => delta . old_file() . path(),
      _ => delta . new_file() . path(), };
  path . map ( |p| PathDiffStatus {
    path   : p . to_path_buf(),
    status }) }

/// Convert an absolute path to a path relative to the repository root.
/// Returns None if the path is not within the repository.
pub fn path_relative_to_repo (
  repo     : &Repository,
  abs_path : &Path
) -> Option<PathBuf> {
  let workdir : &Path =
    repo . workdir() ?;
  abs_path . strip_prefix (workdir) . ok () . map (
    |p| p . to_path_buf() ) }
