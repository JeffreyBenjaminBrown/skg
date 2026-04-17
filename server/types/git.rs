/// Git-related types for diff view functionality.

use crate::types::list::Diff_Item;
use crate::types::misc::{ID, SourceName};
use crate::types::skgnode::SkgNode;

use std::collections::HashMap;
use std::path::PathBuf;

//
// Types
//

/// One step of change along a diff axis.
/// '+' (Plus) means a transition from absent to present
///   (file added, or membership added).
/// '-' (Minus) means present to absent.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Sign { Plus, Minus }

/// Per-stage existence diff for a node's '.skg' file.
/// 'staged'   compares HEAD  vs index.
/// 'unstaged' compares index vs worktree.
/// Both signs being identical within a single ExistenceAxes is impossible
/// (it would require the file to be both present and absent in the index)
/// but the type does not enforce that; consumers must.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct ExistenceAxes {
  pub staged   : Option<Sign>,
  pub unstaged : Option<Sign>,
}

/// Per-stage membership diff for a node's appearance at a particular position
/// in its parent's contains list. Same shape as ExistenceAxes but tracks a
/// different fact (membership at this position rather than file existence).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub struct MembershipAxes {
  pub staged   : Option<Sign>,
  pub unstaged : Option<Sign>,
}

impl ExistenceAxes {
  pub fn is_empty (&self) -> bool {
    self . staged . is_none () && self . unstaged . is_none () }

  /// Atoms for this stage's existence change, if any:
  /// '+' -> "newX", '-' -> "removedX".
  fn atom_for_stage (sign: Option<Sign>) -> Option<&'static str> {
    match sign {
      Some (Sign::Plus)  => Some ("newX"),
      Some (Sign::Minus) => Some ("removedX"),
      None               => None, } }

  /// The atom-list for the staged side, e.g. "newX" or "" if empty.
  pub fn staged_atom (&self) -> Option<&'static str> {
    Self::atom_for_stage (self . staged) }
  pub fn unstaged_atom (&self) -> Option<&'static str> {
    Self::atom_for_stage (self . unstaged) }
}

impl MembershipAxes {
  pub fn is_empty (&self) -> bool {
    self . staged . is_none () && self . unstaged . is_none () }

  fn atom_for_stage (sign: Option<Sign>) -> Option<&'static str> {
    match sign {
      Some (Sign::Plus)  => Some ("newM"),
      Some (Sign::Minus) => Some ("removedM"),
      None               => None, } }

  pub fn staged_atom (&self) -> Option<&'static str> {
    Self::atom_for_stage (self . staged) }
  pub fn unstaged_atom (&self) -> Option<&'static str> {
    Self::atom_for_stage (self . unstaged) }
}

impl Sign {
  /// Parse an axis atom like "newX", "removedM" into (axis-letter, sign).
  /// Returns Some ((axis, sign)) where axis is 'X' or 'M'.
  pub fn parse_axis_atom (s: &str) -> Option<(char, Sign)> {
    match s {
      "newX"     => Some (('X', Sign::Plus)),
      "removedX" => Some (('X', Sign::Minus)),
      "newM"     => Some (('M', Sign::Plus)),
      "removedM" => Some (('M', Sign::Minus)),
      _          => None, } }
}

impl GitDiffStatus {
  /// Map a file-level git status to an existence-axis sign.
  /// Modified files have no existence change.
  pub fn to_existence_sign (&self) -> Option<Sign> {
    match self {
      GitDiffStatus::Added    => Some (Sign::Plus),
      GitDiffStatus::Deleted  => Some (Sign::Minus),
      GitDiffStatus::Modified => None, } } }

/// Represents changes for an entire source directory.
/// (I assume each source is a separate git repo,
/// but the code might work even if some are part of the same repo.)
/// Represents the per-stage diff for an entire source directory.
/// 'staged'   maps each changed '.skg' file to its HEAD-vs-index diff.
/// 'unstaged' maps each changed '.skg' file to its index-vs-worktree diff.
/// A file may appear in either, both, or neither.
#[derive(Debug, Clone)]
pub struct SourceDiff {
  pub is_git_repo: bool,
  pub staged   : HashMap<PathBuf, SkgnodeDiff>,
  pub unstaged : HashMap<PathBuf, SkgnodeDiff>,
  /// Nodes that existed in HEAD but not in worktree (deleted files).
  /// Loaded from git HEAD or from the index. Used for phantom titles.
  pub deleted_nodes: HashMap<ID, SkgNode>,
}

/// All the diff info for a single .skg file.
#[derive(Debug, Clone)]
pub struct SkgnodeDiff {
  // TODO ? Since we keep the skgnode around for deleted nodes already, why not just do that for everything, and dispense with the node_changes field?
  pub status: GitDiffStatus,
  pub node_changes: Option<NodeChanges>,
  pub head_node: Option<SkgNode>, // only for deleted files
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

/// Represents changes to a single SkgNode.
#[derive(Debug, Clone, Default)]
pub struct NodeChanges {
  pub text_changed  : bool,
  pub aliases_diff  : Vec<Diff_Item<String>>,
  pub ids_diff      : Vec<Diff_Item<ID>>,
  pub contains_diff : Vec<Diff_Item<ID>>,
}

//
// Implementations
//

impl SourceDiff {
  pub fn new_not_git_repo () -> Self {
    SourceDiff {
      is_git_repo  : false,
      staged       : HashMap::new(),
      unstaged     : HashMap::new(),
      deleted_nodes: HashMap::new() } } }

//
// Functions
//

/// Look up the NodeChanges for a TrueNode from source_diffs.
/// Returns None if not in diff view, the source is not a git repo,
/// or there are no recorded changes for this node's .skg file in
/// either stage. If both stages have changes, the unstaged side is
/// returned (transitional; see `apply_diff_to_forest` rewrite).
pub fn node_changes_for_truenode<'a> (
  source_diffs : &'a Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> Option<&'a NodeChanges> {
  source_diffs . as_ref() . and_then(
    |diffs| {
      let sourcediff : &SourceDiff = diffs . get (source) ?;
      if !sourcediff . is_git_repo { return None; }
      let file_path : PathBuf =
        PathBuf::from( format!( "{}.skg", pid . 0 ) );
      sourcediff . unstaged . get (&file_path)
        . or_else( || sourcediff . staged . get (&file_path) ) ?
        . node_changes . as_ref() } ) }
