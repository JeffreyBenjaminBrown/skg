/// Shared definitions for git diff view content tests.

pub use super::super::common::*;

/// The expected git diff view output.
/// This is what multi_root_view should produce with diff_mode_enabled=true.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 11) (source main))) 11
*** (skg (node (id gets-removed) (source main) indefinitive (diff removed))) gets-removed
*** (skg (node (id moves) (diff new-here))) moves
** (skg (node (id 12) (source main))) 12
*** (skg (node (id moves) (source main) indefinitive (diff removed-here))) moves
* (skg (node (id new) (source main))) new
";

/// Create a git repo with head->worktree transition from content fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/content/fixtures/head",
    "tests/git_diff_view/content/fixtures/worktree",
  )
}
