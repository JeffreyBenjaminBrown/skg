/// Stages every '.skg' change of each owned skgrepo, deletions
/// included, and git-commits it with one message
/// ('TODO/total-diff-view/2_plan.org', "the summary and the commit").
/// A skgrepo whose staged tree equals its HEAD's has no changes and
/// gets no commit. The author and committer are the gitrepo's
/// configured user.

use crate::types::misc::{SkgConfig, Skgrepo, SkgrepoName};

use super::git_snapshot::repo_prefix_in_gitrepo;
use super::read_gitrepo::open_gitrepo;

use git2::{Commit, Index, Oid, Repository, Signature, Tree};
use std::path::{Path, PathBuf};

/// What staging and committing did in one owned skgrepo: the
/// git-commit made, None when it had no changes, or why it failed.
pub type SkgrepoCommitOutcome = Result<Option<Oid>, String>;

/// Every owned skgrepo, in config order, with its outcome. A failure
/// in one skgrepo does not stop the others.
pub fn stage_and_commit_owned_skgrepos (
  config  : &SkgConfig,
  message : &str,
) -> Vec<(SkgrepoName, SkgrepoCommitOutcome)> {
  config . owned_skgrepos_in_config_order () . into_iter ()
    . map ( |skgrepo_name| {
      let outcome : SkgrepoCommitOutcome =
        match config . skgrepos . get (&skgrepo_name) {
          Some (skgrepo) => stage_and_commit_skgrepo (skgrepo, message),
          None => Err ( format! (
            "Repo '{}' not found in config", skgrepo_name )), };
      ( skgrepo_name, outcome ) } )
    . collect () }

fn stage_and_commit_skgrepo (
  skgrepo : &Skgrepo,
  message : &str,
) -> SkgrepoCommitOutcome {
  let skgrepo_path : &Path = Path::new (&skgrepo . path);
  let gitrepo : Repository = open_gitrepo (skgrepo_path)
    . ok_or_else ( || "it is not in a git repository" . to_string () ) ?;
  let prefix : PathBuf = repo_prefix_in_gitrepo (&gitrepo, skgrepo_path) ?;
  let tree_id : Oid = stage_skg_files (&gitrepo, &skg_pathspec (&prefix))
    . map_err ( |e| format! ( "staging failed: {}", e . message () )) ?;
  let head_commit : Commit = gitrepo . head ()
    . and_then ( |head| head . peel_to_commit () )
    . map_err ( |e| format! ( "it has no HEAD commit: {}", e . message () )) ?;
  if head_commit . tree_id () == tree_id { return Ok (None); }
  let commit_id : Oid = {
    let tree : Tree = gitrepo . find_tree (tree_id)
      . map_err ( |e| e . message () . to_string () ) ?;
    let signature : Signature = gitrepo . signature ()
      . map_err ( |e| format! (
        "no git author is configured: {}", e . message () )) ?;
    gitrepo . commit ( Some ("HEAD"), &signature, &signature, message,
                       &tree, &[ &head_commit ] )
      . map_err ( |e| format! ( "git-commit failed: {}", e . message () )) ? };
  Ok ( Some (commit_id) ) }

/// The '.skg' files under 'prefix', the skgrepo's folder relative to
/// its gitrepo's root.
fn skg_pathspec (
  prefix : &Path,
) -> String {
  if prefix . as_os_str () . is_empty () { "*.skg" . to_string () }
  else { format! ( "{}/*.skg", prefix . display () ) }}

/// Stage every file 'pathspec' matches, as 'git add --all' does, and
/// return the git index's tree.
fn stage_skg_files (
  gitrepo  : &Repository,
  pathspec : &str,
) -> Result<Oid, git2::Error> {
  let mut index : Index = gitrepo . index () ?;
  index . add_all ( [pathspec], git2::IndexAddOption::DEFAULT, None ) ?;
  // 'add_all' does not stage the deletion of a file gone from the worktree.
  index . update_all ( [pathspec], None ) ?;
  index . write () ?;
  index . write_tree () }
