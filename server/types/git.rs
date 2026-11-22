//! Git-related types.

use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;
use std::path::PathBuf;

//
// Types
//

/// The nodes of a skgrepo's '.skg' files that git sees added or
/// deleted, staged or not ('compute_diff_for_skgrepo'). They title
/// nodes the search index lacks.
#[derive(Debug, Clone, Default)]
pub struct SkgrepoDiff {
  /// Nodes that exist in the index or worktree but not in the
  /// stage baseline. Used to title new/untracked files before the
  /// search index has seen them.
  pub added_nodes: HashMap<ID, Graphnode>,
  /// Nodes that existed in HEAD but not in worktree (deleted files).
  /// Loaded from git HEAD or from the git index. Used to title them.
  pub deleted_nodes: HashMap<ID, Graphnode>,
}

/// A single entry representing a changed file.
#[derive(Debug, Clone)]
pub struct PathDiffStatus {
  pub path   : PathBuf,
  pub status : GitDiffStatus,
}

/// Status of a file in the git diff.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GitDiffStatus {
  Added,    // File was added (not in HEAD)
  Modified, // File was modified
  Deleted,  // File was deleted (in HEAD but not on disk)
}

//
// Implementations
//

impl SkgrepoDiff {
  /// The diff of a skgrepo that is not in a gitrepo: empty.
  pub fn new_not_gitrepo () -> Self {
    SkgrepoDiff::default () } }
