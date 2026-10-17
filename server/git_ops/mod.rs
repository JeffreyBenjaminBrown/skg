/// Git operations module for diff view functionality.
/// Uses the git2 crate for programmatic access to gitrepos.

pub mod misc;
pub mod read_gitrepo;
pub mod diff;
pub mod find_and_stage_moves;
pub mod stage_and_commit;
pub mod git_snapshot;
pub mod git_snapshot_types;
