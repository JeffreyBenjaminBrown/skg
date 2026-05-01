/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only placeholder for a removed node.

use super::views_state::nodecomplete_from_inrustgraph_or_disk;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{ExistenceAxes, MembershipAxes, NodeCompleteDiff, Sign, SourceDiff};
use super::list::Diff_Item;
use super::misc::{ID, SkgConfig, SourceName};

/// Unified title lookup for phantom nodes.
/// Lookup order: source_diffs deleted_nodes → in-Rust graph/disk → fallback.
pub fn title_for_phantom (
  id           : &ID,
  source       : &SourceName,
  source_diffs : Option<&HashMap<SourceName, SourceDiff>>,
  config       : &SkgConfig,
) -> String {
  source_diffs
    . and_then( |diffs| diffs . get (source) )
    . and_then( |sd| sd . deleted_nodes . get (id) )
    . map( |n| n . title . clone() )
    . or_else( || nodecomplete_from_inrustgraph_or_disk (
                    config, id, source )
                  . ok() . map( |n| n . title ) )
    . unwrap_or_else( || format!( "TITLE NOT FOUND for ID {}", id . 0 )) }

/// Diff axes for a phantom node, for use by the save / rerender pipeline.
///
/// A phantom is a child that appears in the view because it was in
/// the parent's contains list at some point (HEAD or index) but is
/// not in the worktree's contains list.
///
/// Axes are computed per-stage from the git diff data, so that
/// already-staged contains-list changes are attributed to the staged
/// side rather than the unstaged side:
///   staged   = HEAD vs index
///   unstaged = index vs worktree
/// Both being Some together is possible (e.g. child added staged,
/// then removed unstaged).
pub fn phantom_axes (
  child_id      : &ID,
  child_source  : &SourceName,
  parent_id     : &ID,
  parent_source : &SourceName,
  source_diffs  : Option<&HashMap<SourceName, SourceDiff>>,
) -> (ExistenceAxes, MembershipAxes) {
  // Existence: the child's own file-level status in each stage.
  let child_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", child_id . 0 ) );
  let child_sd : Option<&SourceDiff> =
    source_diffs . and_then ( |d| d . get (child_source) );
  let ex_staged : Option<Sign> =
    child_sd . and_then ( |sd| sd . staged . get (&child_file) )
      . and_then ( |d| d . status . to_existence_sign () );
  let ex_unstaged : Option<Sign> =
    child_sd . and_then ( |sd| sd . unstaged . get (&child_file) )
      . and_then ( |d| d . status . to_existence_sign () );
  let existence : ExistenceAxes =
    ExistenceAxes { staged: ex_staged, unstaged: ex_unstaged };

  // Membership: the child's presence in the parent's contains list
  // in each stage. New(id) -> Plus; Removed(id) -> Minus.
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_id . 0 ) );
  let parent_sd : Option<&SourceDiff> =
    source_diffs . and_then ( |d| d . get (parent_source) );
  let sign_from_parent_stage =
    | stage_map : &HashMap<PathBuf, NodeCompleteDiff> | -> Option<Sign> {
      stage_map . get (&parent_file)
        . and_then ( |d| d . node_changes . as_ref () )
        . and_then ( |nc| nc . contains_diff . iter ()
                            . find_map ( |d| match d {
          Diff_Item::New     (id) if id == child_id => Some (Sign::Plus),
          Diff_Item::Removed (id) if id == child_id => Some (Sign::Minus),
          _ => None, } ) ) };
  let mem_staged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . staged) );
  let mem_unstaged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . unstaged) );
  let membership : MembershipAxes =
    MembershipAxes { staged: mem_staged, unstaged: mem_unstaged };

  // Fall back to the old "unstaged Minus" assumption when we have
  // zero per-stage signal for membership. Subscription phantoms go
  // through this function too, and there's no contains_diff to read
  // for them — preserve existing behavior in that case.
  let membership : MembershipAxes =
    if membership . is_empty ()
      { MembershipAxes { staged: None, unstaged: Some (Sign::Minus) } }
    else { membership };

  (existence, membership) }

/// Find the source for a node by checking which source directory
/// contains its .skg file on disk. Returns None if not found in any source.
pub fn source_from_disk (
  id     : &ID,
  config : &SkgConfig,
) -> Option<SourceName> {
  let filename : String = format!( "{}.skg", id . 0 );
  for (source_name, source_config) in &config . sources {
    let path : PathBuf =
      PathBuf::from( &source_config . path ) . join (&filename);
    if path . exists() {
      return Some( source_name . clone() ); }}
  None }

#[cfg(test)]
mod tests {
  use super::*;
  use super::super::git::{GitDiffStatus, NodeChanges};

  fn source_name (s: &str) -> SourceName { SourceName ( s . to_string () ) }
  fn id        (s: &str) -> ID         { ID ( s . to_string () ) }

  fn make_parent_diff (
    contains_diff : Vec<Diff_Item<ID>>,
  ) -> NodeCompleteDiff {
    NodeCompleteDiff {
      status: GitDiffStatus::Modified,
      node_changes: Some ( NodeChanges {
        text_changed:  false,
        aliases_diff:  Vec::new (),
        ids_diff:      Vec::new (),
        contains_diff, } ),
      before_node: None, } }

  fn source_diff_with_parent_contains (
    parent_pid : &ID,
    staged_ops   : Vec<Diff_Item<ID>>,
    unstaged_ops : Vec<Diff_Item<ID>>,
  ) -> SourceDiff {
    let parent_file : PathBuf =
      PathBuf::from ( format! ( "{}.skg", parent_pid . 0 ) );
    let mut staged   : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
    let mut unstaged : HashMap<PathBuf, NodeCompleteDiff> = HashMap::new ();
    if !staged_ops . is_empty () {
      staged . insert ( parent_file . clone (),
                        make_parent_diff (staged_ops) ); }
    if !unstaged_ops . is_empty () {
      unstaged . insert ( parent_file,
                          make_parent_diff (unstaged_ops) ); }
    SourceDiff {
      is_git_repo: true,
      staged, unstaged,
      deleted_nodes: HashMap::new (), } }

  #[test]
  fn staged_removal_is_attributed_to_staged_side () {
    let parent : ID = id ("parent");
    let child  : ID = id ("child");
    let src    : SourceName = source_name ("public");
    let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
    diffs . insert ( src . clone (),
                     source_diff_with_parent_contains (
                       &parent,
                       vec! [ Diff_Item::Removed (child . clone ()) ],
                       vec! [] ) );
    let (ex, mem) = phantom_axes (
      &child, &src, &parent, &src, Some (&diffs) );
    assert_eq! ( mem, MembershipAxes {
      staged: Some (Sign::Minus), unstaged: None } );
    assert_eq! ( ex, ExistenceAxes::default () );
  }

  #[test]
  fn unstaged_removal_is_attributed_to_unstaged_side () {
    let parent : ID = id ("parent");
    let child  : ID = id ("child");
    let src    : SourceName = source_name ("public");
    let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
    diffs . insert ( src . clone (),
                     source_diff_with_parent_contains (
                       &parent,
                       vec! [],
                       vec! [ Diff_Item::Removed (child . clone ()) ] ) );
    let (_, mem) = phantom_axes (
      &child, &src, &parent, &src, Some (&diffs) );
    assert_eq! ( mem, MembershipAxes {
      staged: None, unstaged: Some (Sign::Minus) } );
  }

  #[test]
  fn staged_add_then_unstaged_remove () {
    let parent : ID = id ("parent");
    let child  : ID = id ("child");
    let src    : SourceName = source_name ("public");
    let mut diffs : HashMap<SourceName, SourceDiff> = HashMap::new ();
    diffs . insert ( src . clone (),
                     source_diff_with_parent_contains (
                       &parent,
                       vec! [ Diff_Item::New     (child . clone ()) ],
                       vec! [ Diff_Item::Removed (child . clone ()) ] ) );
    let (_, mem) = phantom_axes (
      &child, &src, &parent, &src, Some (&diffs) );
    assert_eq! ( mem, MembershipAxes {
      staged: Some (Sign::Plus), unstaged: Some (Sign::Minus) } );
  }

  #[test]
  fn no_parent_contains_diff_falls_back_to_unstaged_minus () {
    // Fallback preserves legacy behavior for callers (e.g. subscription
    // phantoms) where there's no contains_diff entry for this child.
    let parent : ID = id ("parent");
    let child  : ID = id ("child");
    let src    : SourceName = source_name ("public");
    let diffs  : HashMap<SourceName, SourceDiff> = HashMap::new ();
    let (_, mem) = phantom_axes (
      &child, &src, &parent, &src, Some (&diffs) );
    assert_eq! ( mem, MembershipAxes {
      staged: None, unstaged: Some (Sign::Minus) } );
  }
}
