/// Git-related types for diff view functionality.

use crate::types::list::Diff_Item;
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

impl SourceDiff {
  pub fn new_not_git_repo () -> Self {
    SourceDiff {
      is_git_repo  : false,
      staged       : HashMap::new(),
      unstaged     : HashMap::new(),
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
/// present (prefers unstaged if both, matching apply_diff_to_viewforest's
/// baseline choice). Items present in only one stage's diff are
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
/// Used by both the de-novo diff-application path (for the TrueNode
/// itself) and the save-rerender expand path (for phantoms of a
/// removed parent's children). Previously each caller rolled its own
/// lookup; extract lets them share + test it.
pub fn file_existence_axes_from_source_diff (
  source_diffs : &Option<HashMap<SourceName, SourceDiff>>,
  pid          : &ID,
  source       : &SourceName,
) -> ExistenceAxes {
  let sd : Option<&SourceDiff> =
    source_diffs . as_ref () . and_then ( |d| d . get (source) );
  let file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ) );
  let staged : Option<Sign> = sd
    . and_then ( |sd| sd . staged . get (&file) )
    . and_then ( |d| d . status . to_existence_sign () );
  let unstaged : Option<Sign> = sd
    . and_then ( |sd| sd . unstaged . get (&file) )
    . and_then ( |d| d . status . to_existence_sign () );
  ExistenceAxes { staged, unstaged } }

/// Compose two stage diffs into a single HEAD→worktree diff.
///
/// Classifies each item by presence at HEAD and at worktree:
///   present in HEAD    iff staged   is None OR Minus
///   present in worktree iff unstaged is None OR Plus
/// and emits Unchanged / New / Removed accordingly. Items absent
/// from both HEAD and worktree (Plus then Minus: added staged, then
/// removed unstaged) are dropped — they're not part of the net diff.
///
/// Used by consumers that need a flat "what changed HEAD vs
/// worktree" view across both stages. For consumers that need the
/// per-stage signs separately, use 'axes_from_per_stage_diffs'.
pub fn net_diff_from_per_stage<T: Clone + Eq + std::hash::Hash> (
  staged_diff   : Option<&[Diff_Item<T>]>,
  unstaged_diff : Option<&[Diff_Item<T>]>,
) -> Vec<Diff_Item<T>> {
  // Determine membership at each snapshot -- HEAD, index, worktree
  // -- by treating each stage as a transition:
  //   staged:   HEAD   -> index    (None = same)
  //   unstaged: index  -> worktree (None = same)
  // A Plus arrow means "not present before, present after"; Minus
  // is the reverse; None means "present before iff present after".
  // When a stage's sign is None we need to decide whether "present
  // before = present after" resolves to "both present" or "both
  // absent" -- and that's determined by the OTHER stage, or (if
  // both stages are None) by the fact that the item appears in
  // axes at all, which happens via 'Unchanged' in the baseline
  // diff and therefore means present throughout.
  let axes : Vec<(T, MembershipAxes)> =
    axes_from_per_stage_diffs (staged_diff, unstaged_diff);
  let mut out : Vec<Diff_Item<T>> = Vec::with_capacity (axes . len ());
  for (id, m) in axes {
    let in_head : bool = match m . staged {
      Some (Sign::Plus)  => false,
      Some (Sign::Minus) => true,
      None               => match m . unstaged {
        Some (Sign::Plus)  => false,
        Some (Sign::Minus) => true,
        None               => true, }, }; // present throughout
    let in_wt : bool = match m . unstaged {
      Some (Sign::Plus)  => true,
      Some (Sign::Minus) => false,
      None               => match m . staged {
        Some (Sign::Plus)  => true,
        Some (Sign::Minus) => false,
        None               => true, }, }; // present throughout
    let item : Option<Diff_Item<T>> = match (in_head, in_wt) {
      (true,  true)  => Some (Diff_Item::Unchanged (id)),
      (false, true)  => Some (Diff_Item::New       (id)),
      (true,  false) => Some (Diff_Item::Removed   (id)),
      (false, false) => None, };
    if let Some (d) = item { out . push (d); } }
  out }

#[cfg(test)]
mod net_diff_tests {
  use super::*;

  fn removed<T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::Removed (v) }
  fn new_   <T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::New      (v) }
  fn same   <T: Clone> (v: T) -> Diff_Item<T> { Diff_Item::Unchanged (v) }

  // The case that motivated lifting this bug: an item removed on
  // the STAGED side only, with no entry on the unstaged side,
  // should produce a net Removed -- NOT Unchanged. The earlier
  // code mis-classified (staged=Minus, unstaged=None) as
  // Unchanged, causing the rerender pipeline to try reading the
  // now-missing .skg file.
  #[test]
  fn staged_only_remove_is_net_removed () {
    let staged   : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
    let unstaged : Vec<Diff_Item<&str>> = vec! [];
    let net : Vec<Diff_Item<&str>> =
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) );
    assert_eq! ( net, vec! [ Diff_Item::Removed ("x") ] ); }

  #[test]
  fn staged_only_add_is_net_new () {
    let staged   : Vec<Diff_Item<&str>> = vec! [ new_ ("x") ];
    let unstaged : Vec<Diff_Item<&str>> = vec! [];
    assert_eq! (
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) ),
      vec! [ Diff_Item::New ("x") ] ); }

  #[test]
  fn unstaged_only_remove_is_net_removed () {
    let staged   : Vec<Diff_Item<&str>> = vec! [];
    let unstaged : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
    assert_eq! (
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) ),
      vec! [ Diff_Item::Removed ("x") ] ); }

  #[test]
  fn unstaged_only_add_is_net_new () {
    let staged   : Vec<Diff_Item<&str>> = vec! [];
    let unstaged : Vec<Diff_Item<&str>> = vec! [ new_ ("x") ];
    assert_eq! (
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) ),
      vec! [ Diff_Item::New ("x") ] ); }

  // Unchanged items in the baseline must survive as net Unchanged
  // (not dropped). Callers rely on goal-list construction seeing
  // them to preserve order.
  #[test]
  fn baseline_unchanged_is_net_unchanged () {
    let unstaged : Vec<Diff_Item<&str>> = vec! [ same ("x") ];
    assert_eq! (
      net_diff_from_per_stage (
        None, Some (&unstaged) ),
      vec! [ Diff_Item::Unchanged ("x") ] ); }

  // Added staged then removed unstaged: net is no change (drop).
  #[test]
  fn added_staged_then_removed_unstaged_drops () {
    let staged   : Vec<Diff_Item<&str>> = vec! [ new_    ("x") ];
    let unstaged : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
    assert_eq! (
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) ),
      Vec::<Diff_Item<&str>>::new () ); }

  // Removed staged then re-added unstaged: present throughout -> Unchanged.
  #[test]
  fn removed_staged_then_readded_unstaged_is_unchanged () {
    let staged   : Vec<Diff_Item<&str>> = vec! [ removed ("x") ];
    let unstaged : Vec<Diff_Item<&str>> = vec! [ new_    ("x") ];
    assert_eq! (
      net_diff_from_per_stage (
        Some (&staged), Some (&unstaged) ),
      vec! [ Diff_Item::Unchanged ("x") ] ); }
}
