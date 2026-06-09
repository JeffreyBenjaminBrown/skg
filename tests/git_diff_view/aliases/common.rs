/// Shared definitions for git diff view aliases-change tests.

pub use super::super::common::*;

/// Expected git diff view output when a node's aliases list changes.
/// Mirrors the IDCol diff (ids/common.rs): an AliasCol scaffold appears, with
/// each alias as a child carrying a per-stage diff marker for added/removed.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (source main))) 1
** (skg aliasCol)
*** (skg alias (unstaged removedM)) old-alias
*** (skg alias (unstaged newM)) new-alias
*** (skg alias) keep
** (skg (node (id child) (source main))) child
";

/// Create a git repo with head->worktree transition from aliases fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/aliases/fixtures/head",
    "tests/git_diff_view/aliases/fixtures/worktree",
  )
}
