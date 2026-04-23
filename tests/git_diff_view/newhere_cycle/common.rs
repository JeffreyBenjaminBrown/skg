/// Shared definitions for git diff view newhere-cycle tests.

pub use super::super::common::*;

/// The expected git diff view output.
/// Node 1 contains itself – the child occurrence should show (diff new-here).
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 1) (unstaged newM))) 1
";

/// Create a git repo with head->worktree transition from newhere_cycle fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/newhere_cycle/fixtures/head",
    "tests/git_diff_view/newhere_cycle/fixtures/worktree",
  )
}

/// Same transition, staged.
pub fn setup_git_repo_with_fixtures_staged(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures_staged(
    repo_path,
    "tests/git_diff_view/newhere_cycle/fixtures/head",
    "tests/git_diff_view/newhere_cycle/fixtures/worktree",
  )
}

/// Expected output when the transition is staged.
pub const GIT_DIFF_VIEW_STAGED: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 1) (staged newM))) 1
";
