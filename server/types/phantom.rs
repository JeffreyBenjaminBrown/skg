/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only placeholder for a removed node.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::node_lookup::nodecomplete_rustFirst_by_pid_and_repo;
use crate::dbs::in_rust_graph::InRustGraph;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{ExistenceAxes, MembershipAxes, NodeCompleteDiff, Sign, RepoDiff, existence_axes_in_repo_diff};
use super::list::Diff_Item;
use super::misc::{ID, SkgConfig, SkgfileRepo, RepoName};

/// Unified title lookup for phantom nodes.
/// Lookup order: repo_diffs deleted_nodes → in-Rust graph/disk → fallback.
pub fn title_for_phantom (
  graph        : &InRustGraph,
  id           : &ID,
  repo       : &RepoName,
  repo_diffs : Option<&HashMap<RepoName, RepoDiff>>,
  config       : &SkgConfig,
) -> String {
  repo_diffs
    . and_then( |diffs| diffs . get (repo) )
    . and_then( |sd| sd . deleted_nodes . get (id) )
    . map( |n| n . title . clone() )
    . or_else( || nodecomplete_rustFirst_by_pid_and_repo (
                    graph, config, id, repo )
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
  child_repo  : &RepoName,
  parent_id     : &ID,
  parent_repo : &RepoName,
  relation      : NodeRelation, // the relation the caller's folder represents
  repo_diffs  : Option<&HashMap<RepoName, RepoDiff>>,
) -> (ExistenceAxes, MembershipAxes) {
  // Existence: the child's own file-level status in each stage.
  let child_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", child_id . 0 ) );
  let existence : ExistenceAxes =
    existence_axes_in_repo_diff (
      repo_diffs . and_then ( |d| d . get (child_repo) ),
      &child_file );

  // Membership: the child's presence in the parent's list for the
  // NAMED relation, in each stage. New(id) -> Plus; Removed(id) ->
  // Minus. Exactly one relation diff is read -- the folder's own -- so
  // a phantom's stage label can never come from a DIFFERENT relation
  // that happens to involve the same ID (one owner can bear the same
  // ID in two relations, changed in different stages).
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_id . 0 ) );
  let parent_sd : Option<&RepoDiff> =
    repo_diffs . and_then ( |d| d . get (parent_repo) );
  let sign_from_parent_stage =
    | stage_map : &HashMap<PathBuf, NodeCompleteDiff> | -> Option<Sign> {
      let nc = stage_map . get (&parent_file)
        . and_then ( |d| d . node_changes . as_ref () ) ?;
      let diff_list : &[Diff_Item<ID>] =
        relation . diff_in_nodechanges (nc) ?;
      diff_list . iter () . find_map ( |d| match d {
        Diff_Item::New     (id) if id == child_id => Some (Sign::Plus),
        Diff_Item::Removed (id) if id == child_id => Some (Sign::Minus),
        _ => None, } ) };
  let mem_staged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . staged) );
  let mem_unstaged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . unstaged) );
  let membership : MembershipAxes =
    MembershipAxes { staged: mem_staged, unstaged: mem_unstaged };

  // Fall back to a net "unstaged Minus" only when the named relation's
  // per-stage diff carries no signal for this child. A per-stage
  // signal is genuinely unavailable in two cases: (a) the parent's
  // file is not listed as Modified in either stage map (no
  // NodeChanges exists -- e.g. its repo_diff is absent entirely),
  // yet the caller's goal-list computation still found a
  // HEAD-side-only member; (b) a filter folder, whose DERIVED membership
  // can change while no single input relation's diff names the child
  // (its three-snapshot comparison supplies exact labels instead, and
  // bypasses this function).
  let membership : MembershipAxes =
    if membership . is_empty ()
      { MembershipAxes { staged: None, unstaged: Some (Sign::Minus) } }
    else { membership };

  (existence, membership) }

/// A node's HOME read from disk. When owned and non-owned files use
/// the same pid, the owned telescope wins; otherwise the home is
/// the most public repo holding a section. Returns None if no
/// repo holds one.
///
/// Walks 'ordered_repos' (the privacy order, most public first),
/// never 'config.repos' -- that is a HashMap, whose iteration
/// order Rust randomizes per process, so returning its first hit
/// answered arbitrarily for any node with more than one section.
/// Since the home is DEFINITIONALLY the most public section
/// (docs/telescopes.org), the first hit in privacy order is the
/// answer; a home whose section carries no title is a violation the
/// fold reports, not a reason to keep looking.
pub fn home_from_disk (
  id     : &ID,
  config : &SkgConfig,
) -> Option<RepoName> {
  let filename : String = format!( "{}.skg", id . 0 );
  let ordered_repos : Vec<RepoName> = config . ordered_repos ();
  for owned_only in [true, false] {
    for repo_name in &ordered_repos {
      if config . user_owns_repo (repo_name) != owned_only {
        continue; }
      let Some (repo_config) : Option<&SkgfileRepo> =
        config . repos . get (repo_name) else { continue; };
      let path : PathBuf =
        PathBuf::from( &repo_config . path ) . join (&filename);
      if path . exists() {
        return Some( repo_name . clone () ); }} }
  None }

#[cfg(test)]
#[path = "../../tests/unit/types_phantom.rs"]
mod tests;
