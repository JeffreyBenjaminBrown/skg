/// Shared definitions for git diff view text (title/body) change tests.

pub use super::super::common::*;

/// The expected git diff view output for title/body changes.
/// TextChanged scaffolds appear as children of nodes whose title or body changed.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (repo main))) 1 has a new title.
** (skg (textChanged unstaged))
** (skg (node (id 11) (repo main))) 11
11 has a new body.
*** (skg (textChanged unstaged))
** (skg (node (id 12) (repo main))) 12
";

/// Create a git repo with head->worktree transition from text fixtures.
pub fn setup_gitrepo_with_fixtures(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures(
    gitrepo_path,
    "tests/git_diff_view/text/fixtures/head",
    "tests/git_diff_view/text/fixtures/worktree",
  )
}

/// Same transition, staged (index == worktree != HEAD).
pub fn setup_gitrepo_with_fixtures_staged(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures_staged(
    gitrepo_path,
    "tests/git_diff_view/text/fixtures/head",
    "tests/git_diff_view/text/fixtures/worktree",
  )
}

/// Expected diff view when the text changes are staged.
pub const GIT_DIFF_VIEW_STAGED: &str = "\
* (skg (node (id 1) (repo main))) 1 has a new title.
** (skg (textChanged staged))
** (skg (node (id 11) (repo main))) 11
11 has a new body.
*** (skg (textChanged staged))
** (skg (node (id 12) (repo main))) 12
";
