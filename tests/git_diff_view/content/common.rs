/// Shared definitions for git diff view content tests.

pub use super::super::common::*;

/// The expected git diff view output.
/// This is what multi_root_view should produce with diff_mode_enabled=true.
pub const GIT_DIFF_VIEW: &str = "\
* (skg (node (id 1) (repo main))) 1
** (skg (node (id 11) (repo main))) 11
*** (skg (node (id gets-removed) (repo main) writeProtected (unstaged deletedN removedR))) gets-removed
*** (skg (node (id moves) (unstaged addedR))) moves
** (skg (node (id 12) (repo main))) 12
*** (skg (node (id moves) (repo main) writeProtected (unstaged removedR))) moves
* (skg (node (id new) (repo main))) new
";

/// Create a gitrepo with head->worktree transition from content fixtures.
pub fn setup_gitrepo_with_fixtures(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures/head",
    "tests/git_diff_view/content/fixtures/worktree",
  )
}

/// Same transition, staged.
pub fn setup_gitrepo_with_fixtures_staged(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures_staged(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures/head",
    "tests/git_diff_view/content/fixtures/worktree",
  )
}

pub fn setup_gitrepo_with_subscribee_fixtures(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures-subscribee/head",
    "tests/git_diff_view/content/fixtures-subscribee/worktree",
  )
}

/// #1 fix coverage: a subscriber whose subscribesTo dropped node 22 between
/// HEAD and worktree (22's .skg file still present, so the removal is
/// membership-only). The removed subscribee 22 must render as a phantom with
/// (unstaged removedR) -- its relation is subscribesTo, not contains, so the
/// membership sign comes from build_child_data's net-removal fallback, not phantom_axes.
pub fn setup_gitrepo_with_removed_subscribee_fixtures(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures-removed-subscribee/head",
    "tests/git_diff_view/content/fixtures-removed-subscribee/worktree",
  )
}

/// §C: the same removed-subscribee transition, but STAGED -- so the phantom's
/// relationship axis must report (staged removedR), proving per-stage works for a
/// sharing relation (subscribesTo), not just the net unstaged fallback.
pub fn setup_gitrepo_with_removed_subscribee_fixtures_staged(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures_staged(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures-removed-subscribee/head",
    "tests/git_diff_view/content/fixtures-removed-subscribee/worktree",
  )
}

/// The added direction: a subscriber whose subscribesTo GAINED node 22
/// between HEAD and worktree, so the present member 22 must carry
/// (unstaged addedR) (TODO/DONE/full-schema/DONE/12-2_diff-mode-policy_discussion.org,
/// outbound folder completeness).
pub fn setup_gitrepo_with_added_subscribee_fixtures(
  gitrepo_path: &Path,
) -> Result<Repository, Box<dyn Error>> {
  super::super::common::setup_gitrepo_with_fixtures(
    gitrepo_path,
    "tests/git_diff_view/content/fixtures-added-subscribee/head",
    "tests/git_diff_view/content/fixtures-added-subscribee/worktree",
  )
}

/// Expected when 11 is also a view root (TODO/DONE/fork-fixes.org, no git
/// ghosts under write-protected nodes): the copy of 11 under 1 draws
/// write-protected and so gets NO removed-member phantoms; the
/// editable root copy of 11 carries them.
pub const GIT_DIFF_VIEW_WRITE_PROTECTED_NO_GHOSTS: &str = "\
* (skg (node (id 1) (repo main))) 1
** (skg (node (id 11) (repo main) writeProtected)) 11
** (skg (node (id 12) (repo main))) 12
*** (skg (node (id moves) (repo main) writeProtected (unstaged removedR))) moves
* (skg (node (id 11) (repo main))) 11
** (skg (node (id gets-removed) (repo main) writeProtected (unstaged deletedN removedR))) gets-removed
** (skg (node (id moves) (unstaged addedR))) moves
";

/// Expected output when the transition is staged rather than unstaged.
pub const GIT_DIFF_VIEW_STAGED: &str = "\
* (skg (node (id 1) (repo main))) 1
** (skg (node (id 11) (repo main))) 11
*** (skg (node (id gets-removed) (repo main) writeProtected (staged deletedN removedR))) gets-removed
*** (skg (node (id moves) (staged addedR))) moves
** (skg (node (id 12) (repo main))) 12
*** (skg (node (id moves) (repo main) writeProtected (staged removedR))) moves
* (skg (node (id new) (repo main))) new
";
