/// Shared definitions for collateral-view diff tests.

pub use super::super::common::*;

pub use ego_tree::Tree;

pub use skg::serve::handlers::save_buffer::SaveResponse;
pub use skg::types::views_state::ViewUri;
pub use skg::types::viewnode::ViewNode;

/// The expected initial diff view for a containing b (text changed).
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id a) (source main))) a
** (skg (node (id b) (source main))) b
new body
*** (skg (textChanged unstaged))
";

/// The initial diff view when b's body change is staged rather
/// than unstaged.
pub const GIT_DIFF_VIEW_STAGED: &str = "\
* (skg (node (id a) (source main))) a
** (skg (node (id b) (source main))) b
new body
*** (skg (textChanged staged))
";

/// Create a git repo with head->worktree transition from collateral fixtures.
pub fn setup_git_repo_with_fixtures(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures(
    repo_path,
    "tests/git_diff_view/collateral/fixtures/head",
    "tests/git_diff_view/collateral/fixtures/worktree",
  )
}

/// Same transition, but staged (index == worktree, both differ from HEAD).
pub fn setup_git_repo_with_fixtures_staged(
  repo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_git_repo_with_fixtures_staged(
    repo_path,
    "tests/git_diff_view/collateral/fixtures/head",
    "tests/git_diff_view/collateral/fixtures/worktree",
  )
}
