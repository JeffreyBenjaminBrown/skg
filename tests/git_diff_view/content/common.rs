/// Shared definitions for git diff view content tests.

pub use super::super::common::*;

/// The expected git diff view output.
/// This is what multi_root_view should produce with diff_mode_enabled=true.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 11) (source main))) 11
*** (skg (node (id gets-removed) (source main) indef (unstaged removedX removedM))) gets-removed
*** (skg (node (id moves) (unstaged newM))) moves
** (skg (node (id 12) (source main))) 12
*** (skg (node (id moves) (source main) indef (unstaged removedM))) moves
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

/// Same transition, staged.
pub fn setup_git_repo_with_fixtures_staged(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures_staged(
    repo_path,
    "tests/git_diff_view/content/fixtures/head",
    "tests/git_diff_view/content/fixtures/worktree",
  )
}

pub fn setup_git_repo_with_subscribee_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/content/fixtures-subscribee/head",
    "tests/git_diff_view/content/fixtures-subscribee/worktree",
  )
}

/// #1 fix coverage: a subscriber whose subscribes_to dropped node 22 between
/// HEAD and worktree (22's .skg file still present, so the removal is
/// membership-only). The removed subscribee 22 must render as a phantom with
/// (unstaged removedM) -- its relation is subscribes_to, not contains, so the
/// marker comes from build_child_data's net-removal fallback, not phantom_axes.
pub fn setup_git_repo_with_removed_subscribee_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/content/fixtures-removed-subscribee/head",
    "tests/git_diff_view/content/fixtures-removed-subscribee/worktree",
  )
}

/// §C: the same removed-subscribee transition, but STAGED -- so the phantom's
/// membership axis must report (staged removedM), proving per-stage works for a
/// sharing relation (subscribes_to), not just the net unstaged fallback.
pub fn setup_git_repo_with_removed_subscribee_fixtures_staged(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures_staged(
    repo_path,
    "tests/git_diff_view/content/fixtures-removed-subscribee/head",
    "tests/git_diff_view/content/fixtures-removed-subscribee/worktree",
  )
}

/// Expected output when the transition is staged rather than unstaged.
pub const GIT_DIFF_VIEW_STAGED: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg (node (id 11) (source main))) 11
*** (skg (node (id gets-removed) (source main) indef (staged removedX removedM))) gets-removed
*** (skg (node (id moves) (staged newM))) moves
** (skg (node (id 12) (source main))) 12
*** (skg (node (id moves) (source main) indef (staged removedM))) moves
* (skg (node (id new) (source main))) new
";
