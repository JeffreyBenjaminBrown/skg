/// Utilities for phantom node lookup in git diff view.
/// A phantom is a display-only vognode for a removed node.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::dbs::node_lookup::graphnode_graphFirst_by_pid_and_skgrepo;
use crate::dbs::in_rust_graph::InRustGraph;

use std::collections::HashMap;
use std::path::PathBuf;

use super::git::{NodeAxes, RelationshipAxes, GraphnodeDiff, Sign, SkgRepoDiff, node_axes_in_skgrepo_diff};
use super::list::Diff_Item;
use super::misc::{ID, SkgConfig, SkgRepo, SkgRepoName};

/// Unified title lookup for phantom nodes.
/// Lookup order: repo_diffs deleted_nodes → in-Rust graph/disk → fallback.
pub fn title_for_phantom (
  graph         : &InRustGraph,
  skgid         : &ID,
  skgrepo       : &SkgRepoName,
  skgrepo_diffs : Option<&HashMap<SkgRepoName, SkgRepoDiff>>,
  config        : &SkgConfig,
) -> String {
  skgrepo_diffs
    . and_then( |diffs| diffs . get (skgrepo) )
    . and_then( |sd| sd . deleted_nodes . get (skgid) )
    . map( |n| n . title . clone() )
    . or_else( || graphnode_graphFirst_by_pid_and_skgrepo (
                    graph, config, skgid, skgrepo )
                  . ok() . map( |n| n . title ) )
    . unwrap_or_else( || format!( "TITLE NOT FOUND for ID {}", skgid . 0 )) }

/// Diff axes for a phantom node, for use by the save / rerender pipeline.
///
/// A phantom is a child that appears in the view because it was in
/// the parent's contains list at some point (HEAD or the git index) but is
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
  child_skgid    : &ID,
  child_skgrepo  : &SkgRepoName,
  parent_skgid   : &ID,
  parent_skgrepo : &SkgRepoName,
  relation       : NodeRelation, // the relation the caller's folder represents
  skgrepo_diffs  : Option<&HashMap<SkgRepoName, SkgRepoDiff>>,
) -> (NodeAxes, RelationshipAxes) {
  // Node axes: the child's own file-level status in each stage.
  let child_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", child_skgid . 0 ) );
  let node_axes : NodeAxes =
    node_axes_in_skgrepo_diff (
      skgrepo_diffs . and_then ( |d| d . get (child_skgrepo) ),
      &child_file );

  // Relationship axes: the child's presence in the parent's list for the
  // NAMED relation, in each stage. New(id) -> Plus; Removed(id) ->
  // Minus. Exactly one relation diff is read -- the folder's own -- so
  // a phantom's stage label can never come from a DIFFERENT relation
  // that happens to involve the same ID (one recorder can bear the same
  // ID in two relations, changed in different stages).
  let parent_file : PathBuf =
    PathBuf::from ( format! ( "{}.skg", parent_skgid . 0 ) );
  let parent_sd : Option<&SkgRepoDiff> =
    skgrepo_diffs . and_then ( |d| d . get (parent_skgrepo) );
  let sign_from_parent_stage =
    | stage_map : &HashMap<PathBuf, GraphnodeDiff> | -> Option<Sign> {
      let nc = stage_map . get (&parent_file)
        . and_then ( |d| d . node_changes . as_ref () ) ?;
      let diff_list : &[Diff_Item<ID>] =
        relation . diff_in_nodechanges (nc) ?;
      diff_list . iter () . find_map ( |d| match d {
        Diff_Item::New     (skgid) if skgid == child_skgid => Some (Sign::Plus),
        Diff_Item::Removed (skgid) if skgid == child_skgid => Some (Sign::Minus),
        _ => None, } ) };
  let mem_staged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . staged) );
  let mem_unstaged : Option<Sign> =
    parent_sd . and_then ( |sd| sign_from_parent_stage (&sd . unstaged) );
  let relationship_axes : RelationshipAxes =
    RelationshipAxes { staged: mem_staged, unstaged: mem_unstaged };

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
  let relationship_axes : RelationshipAxes =
    if relationship_axes . is_empty ()
      { RelationshipAxes { staged: None, unstaged: Some (Sign::Minus) } }
    else { relationship_axes };

  (node_axes, relationship_axes) }

/// A node's HOME read from disk. When owned and non-owned files use
/// the same pid, the owned telescope wins; otherwise the home is
/// the most public skgrepo holding a section. Returns None if no
/// skgrepo holds one.
///
/// Walks 'ordered_repos' (the privacy order, most public first),
/// never 'config.repos' -- that is a HashMap, whose iteration
/// order Rust randomizes per process, so returning its first hit
/// answered arbitrarily for any node with more than one section.
/// Since the home is DEFINITIONALLY the most public section
/// (docs/telescopes.org), the first hit in privacy order is the
/// answer; a home whose section carries no title is a violation the
/// composition reports, not a reason to keep looking.
pub fn home_from_disk (
  skgid     : &ID,
  config : &SkgConfig,
) -> Option<SkgRepoName> {
  let filename : String = format!( "{}.skg", skgid . 0 );
  let ordered_skgrepos : Vec<SkgRepoName> = config . ordered_skgrepos ();
  for owned_only in [true, false] {
    for skgrepo_name in &ordered_skgrepos {
      if config . skgrepo_is_owned (skgrepo_name) != owned_only {
        continue; }
      let Some (skgrepo_config) : Option<&SkgRepo> =
        config . skgrepos . get (skgrepo_name) else { continue; };
      let path : PathBuf =
        PathBuf::from( &skgrepo_config . path ) . join (&filename);
      if path . exists() {
        return Some( skgrepo_name . clone () ); }} }
  None }

#[cfg(test)]
#[path = "../../tests/unit/types_phantom.rs"]
mod tests;
