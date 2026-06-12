/// Git-related types for diff view functionality.

use crate::types::list::{Diff_Item, compute_interleaved_diff};
use crate::types::misc::{ID, SourceName};
use crate::types::nodes::complete::NodeComplete;

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

/// Represents the per-stage diff for an entire source directory.
/// 'staged'   maps each changed '.skg' file to its HEAD-vs-index diff.
/// 'unstaged' maps each changed '.skg' file to its index-vs-worktree diff.
/// A file may appear in either, both, or neither.
#[derive(Debug, Clone)]
pub struct SourceDiff {
  pub is_git_repo: bool,
  pub staged   : HashMap<PathBuf, NodeCompleteDiff>,
  pub unstaged : HashMap<PathBuf, NodeCompleteDiff>,
  /// Nodes that exist in the index or worktree but not in the
  /// stage baseline. Used to title new/untracked files before the
  /// search index has seen them.
  pub added_nodes: HashMap<ID, NodeComplete>,
  /// Nodes that existed in HEAD but not in worktree (deleted files).
  /// Loaded from git HEAD or from the index. Used for phantom titles.
  pub deleted_nodes: HashMap<ID, NodeComplete>,
}

/// All the diff info for a single .skg file.
#[derive(Debug, Clone)]
pub struct NodeCompleteDiff {
  // TODO ? Since we keep the nodecomplete around for deleted nodes already, why not just do that for everything, and dispense with the node_changes field?
  pub status: GitDiffStatus,
  pub node_changes: Option<NodeChanges>,
  /// The "before" state of this stage: HEAD for staged, INDEX for
  /// unstaged. Populated only for Deleted status; read by
  /// 'collect_deleted_nodes_for_both' to resolve phantom titles.
  /// Name reflects the stage's baseline rather than always-HEAD.
  pub before_node: Option<NodeComplete>,
  /// The "after" state of this stage: INDEX for staged, worktree for
  /// unstaged. Populated only for Added status; used for unindexed
  /// titles of new files.
  pub after_node: Option<NodeComplete>,
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

/// Represents changes to a single NodeComplete.
#[derive(Debug, Clone, Default)]
pub struct NodeChanges {
  pub text_changed  : bool,
  pub aliases_diff  : Vec<Diff_Item<String>>,
  pub ids_diff      : Vec<Diff_Item<ID>>,
  pub contains_diff : Vec<Diff_Item<ID>>,
  /// Per-stage diffs of the node's sharing relations, so a PartnerCol
  /// member added to or removed from one of them carries a PER-STAGE
  /// membership axis (staged vs unstaged) instead of only the
  /// net-removal fallback.  Each membership-sign consumer names which
  /// relation its col represents and reads exactly that diff
  /// ('NodeRelation::diff_in_nodechanges'): one owner can bear the
  /// same ID in two relations, changed in different stages, and a
  /// phantom's stage label must come from its own col's relation.
  pub subscribes_to_diff      : Vec<Diff_Item<ID>>,
  pub hides_diff              : Vec<Diff_Item<ID>>,
  pub overrides_view_of_diff  : Vec<Diff_Item<ID>>,
}

//
// Implementations
//

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

  /// Whether the member is present after both stages: the later
  /// stage with a sign wins (unstaged follows staged); no signs at
  /// all means unchanged, hence present.
  pub fn net_is_present (&self) -> bool {
    match (self . staged, self . unstaged) {
      (_, Some (Sign::Minus))    => false,
      (_, Some (Sign::Plus))     => true,
      (Some (Sign::Minus), None) => false,
      _                          => true, } }
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

  /// Whether the member is present after both stages: the later
  /// stage with a sign wins (unstaged follows staged); no signs at
  /// all means unchanged, hence present.
  pub fn net_is_present (&self) -> bool {
    match (self . staged, self . unstaged) {
      (_, Some (Sign::Minus))    => false,
      (_, Some (Sign::Plus))     => true,
      (Some (Sign::Minus), None) => false,
      _                          => true, } }
}

impl SourceDiff {
  pub fn new_not_git_repo () -> Self {
    SourceDiff {
      is_git_repo  : false,
      staged       : HashMap::new(),
      unstaged     : HashMap::new(),
      added_nodes  : HashMap::new(),
      deleted_nodes: HashMap::new() } } }

impl GitDiffStatus {
  /// Map a file-level git status to an existence-axis sign.
  /// Modified files have no existence change.
  pub fn to_existence_sign (&self) -> Option<Sign> {
    match self {
      GitDiffStatus::Added    => Some (Sign::Plus),
      GitDiffStatus::Deleted  => Some (Sign::Minus),
      GitDiffStatus::Modified => None, } } }

//
// Functions
//

/// Returns the (staged, unstaged) pair of NodeChanges for a TrueNode.
/// Either element may be None (that stage has no diff entry for this
/// file, or the entry has 'node_changes: None').
///
/// Consumers that need a flat HEAD→worktree view can compose both
/// stages via 'net_diff_from_per_stage'. Consumers that need
/// per-stage signs use 'axes_from_per_stage_diffs'.
pub fn per_stage_node_changes_for_truenode<'a> (
  source_diffs : &'a Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> (Option<&'a NodeChanges>, Option<&'a NodeChanges>) {
  let sd : Option<&SourceDiff> =
    source_diffs . as_ref () . and_then ( |d| d . get (source) );
  let sd : Option<&SourceDiff> =
    sd . filter ( |sd| sd . is_git_repo );
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let staged : Option<&NodeChanges> =
    sd . and_then ( |sd| sd . staged   . get (&file_path) )
       . and_then ( |d| d . node_changes . as_ref () );
  let unstaged : Option<&NodeChanges> =
    sd . and_then ( |sd| sd . unstaged . get (&file_path) )
       . and_then ( |d| d . node_changes . as_ref () );
  (staged, unstaged) }

/// Union the per-stage signs for a list-field diff into a single
/// (item, MembershipAxes) list. Order comes from whichever stage is
/// present (prefers unstaged if both -- the worktree-relative baseline).
/// Items present in only one stage's diff are
/// included with their stage's sign and the other stage unset.
///
/// 'Unchanged' in a single stage contributes an unset axis for that
/// stage: the item is still listed (so goal-list building can see it)
/// but no sign is placed there.
pub fn axes_from_per_stage_diffs<T: Clone + Eq + std::hash::Hash> (
  staged_diff   : Option<&[Diff_Item<T>]>,
  unstaged_diff : Option<&[Diff_Item<T>]>,
) -> Vec<(T, MembershipAxes)> {
  let mut result : Vec<(T, MembershipAxes)> = Vec::new ();
  let mut index : HashMap<T, usize> = HashMap::new ();
  let baseline : &[Diff_Item<T>] = unstaged_diff
    . or (staged_diff)
    . unwrap_or (&[]);
  for item in baseline {
    let value : T = match item {
      Diff_Item::Unchanged (v) | Diff_Item::New (v) | Diff_Item::Removed (v)
        => v . clone (), };
    if ! index . contains_key (&value) {
      index . insert ( value . clone (), result . len () );
      result . push ( ( value, MembershipAxes::default () )); } }
  let apply = | result : &mut Vec<(T, MembershipAxes)>,
                index  : &mut HashMap<T, usize>,
                diff   : Option<&[Diff_Item<T>]>,
                set_axis : fn(&mut MembershipAxes, Option<Sign>) | {
    if let Some (slice) = diff {
      for item in slice {
        let (value, sign) : (T, Option<Sign>) = match item {
          Diff_Item::New      (v) => ( v . clone (), Some (Sign::Plus)),
          Diff_Item::Removed  (v) => ( v . clone (), Some (Sign::Minus)),
          Diff_Item::Unchanged (_) => continue, };
        let i : usize = *index . entry (value . clone ())
          . or_insert_with ( || {
            result . push ( ( value . clone (), MembershipAxes::default () ));
            result . len () - 1 });
        set_axis (&mut result[i] . 1, sign); } } };
  apply (&mut result, &mut index, staged_diff,
         |m, s| m . staged   = s);
  apply (&mut result, &mut index, unstaged_diff,
         |m, s| m . unstaged = s);
  result }

/// Per-stage file-level ExistenceAxes for a node, derived from
/// 'SourceDiff's staged / unstaged maps. Each stage's sign comes
/// from the file's git status in that stage (Added → Plus,
/// Deleted → Minus, Modified / absent → None).
///
/// Used by the definitive-expand path (extendDefinitiveSubtree_fromGit, for the
/// TrueNode's own existence axes and for phantoms of a removed parent's
/// children).
pub fn file_existence_axes_from_source_diff (
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> ExistenceAxes {
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  existence_axes_in_source_diff (
    source_diffs . as_ref () . and_then ( |d| d . get (source) ),
    &file ) }

/// The staged (HEAD->index) and unstaged (index->worktree) EXISTENCE signs for a
/// `.skg` file within a single source's diff. The one place that reads a file's
/// per-stage status; the callers differ only in how they pick the SourceDiff to
/// read (by the node's own source, or with a fallback).
pub fn existence_axes_in_source_diff (
  source_diff : Option<&SourceDiff>,
  file        : &PathBuf,
) -> ExistenceAxes {
  let staged : Option<Sign> = source_diff
    . and_then ( |sd| sd . staged . get (file) )
    . and_then ( |d| d . status . to_existence_sign () );
  let unstaged : Option<Sign> = source_diff
    . and_then ( |sd| sd . unstaged . get (file) )
    . and_then ( |d| d . status . to_existence_sign () );
  ExistenceAxes { staged, unstaged } }

/// Compose two stage diffs into a single HEAD→worktree diff, with each item at
/// its correct position.
///
/// Reconstructs the HEAD and worktree item-lists from the per-stage diffs --
/// HEAD = the items a stage marks Unchanged or Removed, worktree = those it
/// marks Unchanged or New -- then LCS-diffs the two lists directly. This keeps a
/// cross-stage change at its real position. (Composing the two stages' signs
/// classifies each item correctly but can mis-ORDER an item that changed in one
/// stage relative to siblings that changed in the other -- it lands at the tail
/// rather than interleaved.)
///
/// Items absent from both HEAD and worktree (added staged, then removed
/// unstaged) are dropped — they're not part of the net diff.
///
/// Used by consumers that need a flat "what changed HEAD vs worktree" view. For
/// the per-stage signs separately, use 'axes_from_per_stage_diffs'.
pub fn net_diff_from_per_stage<T: Clone + Eq + std::hash::Hash + Ord> (
  staged_diff   : Option<&[Diff_Item<T>]>,
  unstaged_diff : Option<&[Diff_Item<T>]>,
) -> Vec<Diff_Item<T>> {
  // A present-but-EMPTY stage diff carries no list (it lists items only when
  // that stage actually changed something), so treat empty as absent and read
  // the list from the other stage.
  let staged   : Option<&[Diff_Item<T>]> =
    staged_diff   . filter ( |s| ! s . is_empty () );
  let unstaged : Option<&[Diff_Item<T>]> =
    unstaged_diff . filter ( |s| ! s . is_empty () );
  let head : Vec<T> =
    // HEAD = items present before the change: a stage marks them Unchanged or
    // Removed. Prefer the staged stage (HEAD->index); if it is empty/absent then
    // HEAD == index, so the unstaged stage's before-state (index) serves.
    staged . or (unstaged) . unwrap_or (&[]) . iter ()
      . filter_map ( |item| match item {
          Diff_Item::Unchanged (v) | Diff_Item::Removed (v) => Some (v . clone ()),
          Diff_Item::New (_) => None } )
      . collect ();
  let worktree : Vec<T> =
    // worktree = items present after the change: a stage marks them Unchanged or
    // New. Prefer the unstaged stage (index->worktree); if it is empty/absent
    // then worktree == index, so the staged stage's after-state (index) serves.
    unstaged . or (staged) . unwrap_or (&[]) . iter ()
      . filter_map ( |item| match item {
          Diff_Item::Unchanged (v) | Diff_Item::New (v) => Some (v . clone ()),
          Diff_Item::Removed (_) => None } )
      . collect ();
  compute_interleaved_diff (&head, &worktree) }

#[cfg(test)]
#[path = "../../tests/unit/types_git.rs"]
mod net_diff_tests;
