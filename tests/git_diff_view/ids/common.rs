/// Shared definitions for git diff view ids change tests.

pub use super::super::common::*;

/// The expected git diff view output for id changes.
/// When an id is added or removed, an idCol scaffold appears showing the diff.
/// Individual ids appear as children with diff markers for added/removed.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg idCol) its IDs
*** (skg id) 1
*** (skg id (diff removed)) 2
*** (skg id (diff new)) 2'
*** (skg id) 3
** (skg (node (id child) (source main))) child
";

/// Create a git repo with head->worktree transition from ids fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/ids/fixtures/head",
    "tests/git_diff_view/ids/fixtures/worktree",
  )
}
