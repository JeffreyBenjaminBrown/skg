/// Shared setup for the git-diff round-trip tests.

pub use super::super::common::*;

/// A parent whose contains is REORDERED between HEAD and worktree: 'mover'
/// goes from [a, mover, b] to [a, b, mover]. All four .skg files exist
/// unchanged in both states; only parent.skg's ordering differs.
pub fn setup_git_repo_with_reorder_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/roundtrip/fixtures/reorder/head",
    "tests/git_diff_view/roundtrip/fixtures/reorder/worktree",
  )
}

/// A parent that references 'ghost' at HEAD though ghost.skg exists in NO
/// state (it has no file in head/ or worktree/ -- standing in for a node
/// deleted by an earlier commit). The worktree drops the reference, so the
/// diff treats ghost as a removed member whose source resolves to nothing.
pub fn setup_git_repo_with_dangling_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/roundtrip/fixtures/dangling/head",
    "tests/git_diff_view/roundtrip/fixtures/dangling/worktree",
  )
}
