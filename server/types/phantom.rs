/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only placeholder for a removed node.

use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_source;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{ExistenceAxes, MembershipAxes, NodeCompleteDiff, Sign, SourceDiff, existence_axes_in_source_diff};
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
    . or_else( || nodecomplete_rustFirst_by_pid_and_source (
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
  let existence : ExistenceAxes =
    existence_axes_in_source_diff (
      source_diffs . and_then ( |d| d . get (child_source) ),
      &child_file );

  // Membership: the child's presence in the parent's contains list
  // in each stage. New(id) -> Plus; Removed(id) -> Minus.
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_id . 0 ) );
  let parent_sd : Option<&SourceDiff> =
    source_diffs . and_then ( |d| d . get (parent_source) );
  // §C: the child appears in exactly one of the parent's relations -- contains
  // (content / hidden-in member), subscribes_to (subscribee member), or hides
  // (hidden-outside member) -- so check the per-stage diff of all three and use
  // whichever carries this child. This gives a PER-STAGE membership axis for
  // PartnerCol members too, not only content children.
  let sign_from_parent_stage =
    | stage_map : &HashMap<PathBuf, NodeCompleteDiff> | -> Option<Sign> {
      let nc = stage_map . get (&parent_file)
        . and_then ( |d| d . node_changes . as_ref () ) ?;
      for diff_list in [ & nc . contains_diff,
                         & nc . subscribes_to_diff,
                         & nc . hides_diff ] {
        if let Some (sign) = diff_list . iter () . find_map ( |d| match d {
          Diff_Item::New     (id) if id == child_id => Some (Sign::Plus),
          Diff_Item::Removed (id) if id == child_id => Some (Sign::Minus),
          _ => None, } )
        { return Some (sign); } }
      None };
  let mem_staged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . staged) );
  let mem_unstaged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . unstaged) );
  let membership : MembershipAxes =
    MembershipAxes { staged: mem_staged, unstaged: mem_unstaged };

  // Fall back to a net "unstaged Minus" only when NO per-stage signal exists in
  // any of the parent's three relation diffs -- a defensive net for a removed
  // phantom whose removal the parent's per-stage NodeChanges can't express (e.g.
  // the parent's source_diff is absent, or a net-only removal). The sharing
  // cols that DO diff membership -- Subscribee (subscribes_to), HiddenIn
  // (contains), HiddenOutside (hides) -- are all now covered per-stage above;
  // the PartnerCols (Subscriber/Overrider/...) never produce removed members
  // (their reconcile passes empty removed_ids), so they never reach this path.
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
#[path = "../../tests/unit/types_phantom.rs"]
mod tests;
