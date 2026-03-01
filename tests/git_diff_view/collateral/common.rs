/// Shared definitions for collateral-view diff tests.

pub use super::super::common::*;

pub use ego_tree::Tree;

pub use skg::serve::ConnectionState;
pub use skg::serve::handlers::save_buffer::rerender_collateral_views;
pub use skg::serve::handlers::save_buffer::SaveResult;
pub use skg::types::skg_memory::SkgnodesInMemory;
pub use skg::types::viewnode::{ViewUri, ViewNode};

/// The expected initial diff view for a containing b (text changed).
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id a) (source main))) a
** (skg (node (id b) (source main))) b
new body
*** (skg textChanged)
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
