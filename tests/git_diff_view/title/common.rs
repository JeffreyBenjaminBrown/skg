/// Shared definitions for git diff view title/body change tests.

pub use super::super::common::*;

/// The expected git diff view output for title/body changes.
/// TextChanged scaffolds appear as children of nodes whose title or body changed.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1 has a new title.
** (skg textChanged) Text changed. See git diff.
** (skg (node (id 11) (source main))) 11
11 has a new body.
*** (skg textChanged) Text changed. See git diff.
** (skg (node (id 12) (source main))) 12
";

/// Create a git repo with head->worktree transition from title fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/title/fixtures/head",
    "tests/git_diff_view/title/fixtures/worktree",
  )
}
