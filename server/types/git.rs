/// Git-related types.

use crate::types::misc::ID;
use crate::types::nodes::complete::Graphnode;

use std::collections::HashMap;
use std::path::PathBuf;

//
// Types
//

/// One step of change between the two graphs of a straddle.
/// '+' (Plus) means a transition from absent to present
///   (node file added, or relationship added).
/// '-' (Minus) means present to absent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign { Plus, Minus }

/// The sign of a node, as the diff view shows it: a 'Sign' of its
/// existence, or '~' (Modified), "modified": the node exists in both
/// graphs of the straddle and records at least one change-forest leaf
/// ('GraphStraddle::node_sign'). Relationships and folder entries take
/// a 'Sign', never '~'.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum NodeSign { Plus, Minus, Modified }

/// Represents the per-stage diff for an entire skgrepo directory.
/// 'staged'   maps each changed '.skg' file to its HEAD-vs-index diff.
/// 'unstaged' maps each changed '.skg' file to its index-vs-worktree diff.
/// A file may appear in either, both, or neither.
#[derive(Debug, Clone)]
pub struct SkgrepoDiff {
  pub is_gitrepo: bool,
  pub staged   : HashMap<PathBuf, GraphnodeDiff>,
  pub unstaged : HashMap<PathBuf, GraphnodeDiff>,
  /// Nodes that exist in the index or worktree but not in the
  /// stage baseline. Used to title new/untracked files before the
  /// search index has seen them.
  pub added_nodes: HashMap<ID, Graphnode>,
  /// Nodes that existed in HEAD but not in worktree (deleted files).
  /// Loaded from git HEAD or from the git index. Used to title them.
  pub deleted_nodes: HashMap<ID, Graphnode>,
}

/// All the diff info for a single .skg file.
#[derive(Debug, Clone)]
pub struct GraphnodeDiff {
  pub status: GitDiffStatus,
  /// The "before" state of this stage: HEAD for staged, index for
  /// unstaged. Populated only for Deleted status; read by
  /// 'collect_deleted_nodes_for_both' to resolve phantom titles.
  /// Name reflects the stage's baseline rather than always-HEAD.
  pub before_node: Option<Graphnode>,
  /// The "after" state of this stage: index for staged, worktree for
  /// unstaged. Populated only for Added status; used for unindexed
  /// titles of new files.
  pub after_node: Option<Graphnode>,
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

impl From<Sign> for NodeSign {
  fn from (
    sign : Sign,
  ) -> NodeSign {
    match sign {
      Sign::Plus  => NodeSign::Plus,
      Sign::Minus => NodeSign::Minus, } } }

impl SkgrepoDiff {
  pub fn new_not_gitrepo () -> Self {
    SkgrepoDiff {
      is_gitrepo  : false,
      staged       : HashMap::new(),
      unstaged     : HashMap::new(),
      added_nodes  : HashMap::new(),
      deleted_nodes: HashMap::new() } } }
