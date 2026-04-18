use crate::types::git::{ GitDiffStatus, PathDiffStatus, SourceDiff, SkgnodeDiff, NodeChanges };
use crate::types::list::{compute_interleaved_diff, Diff_Item};
use crate::types::misc::{ID, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::skgnode::SkgNode;

use super::misc::path_relative_to_repo;
use super::read_repo::{ get_file_content_at_head, get_file_content_at_index, get_staged_changed_skg_files, get_unstaged_changed_skg_files, open_repo, };

use std::collections::HashMap;
use std::error::Error as StdError;
use std::path::{Path, PathBuf};
use std::fs;

/// Compute the per-stage diff for a source.
/// 'staged'   compares HEAD  to the index.
/// 'unstaged' compares the index to the worktree.
pub fn compute_diff_for_source (
  source_path : &Path
) -> Result<SourceDiff, Box<dyn StdError>> {
  let repo : git2::Repository =
    match open_repo (source_path) {
      Some (r) => r,
      None => return Ok ( SourceDiff::new_not_git_repo() ) };
  let staged : HashMap<PathBuf, SkgnodeDiff> =
    build_stage_diffs (
      source_path, &repo,
      &get_staged_changed_skg_files (&repo) ?,
      Stage::Staged ) ?;
  let unstaged : HashMap<PathBuf, SkgnodeDiff> =
    build_stage_diffs (
      source_path, &repo,
      &get_unstaged_changed_skg_files (&repo) ?,
      Stage::Unstaged ) ?;
  let deleted_nodes : HashMap<ID, SkgNode> =
    collect_deleted_nodes_for_both (&staged, &unstaged);
  Ok ( SourceDiff { is_git_repo: true,
                    staged,
                    unstaged,
                    deleted_nodes }) }

/// Tag for which pair of git states a stage compares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage { Staged, Unstaged }

fn build_stage_diffs (
  source_path : &Path,
  repo        : &git2::Repository,
  changed     : &[PathDiffStatus],
  stage       : Stage,
) -> Result<HashMap<PathBuf, SkgnodeDiff>, Box<dyn StdError>> {
  let mut result : HashMap<PathBuf, SkgnodeDiff> =
    HashMap::new();
  for entry in changed {
    let skgnode_diff : SkgnodeDiff =
      compute_skgnode_diff_for_stage (
        source_path, repo, entry, stage ) ?;
    result . insert ( entry . path . clone(), skgnode_diff ); }
  Ok (result) }

fn compute_skgnode_diff_for_stage (
  source_path : &Path,
  repo        : &git2::Repository,
  entry       : &PathDiffStatus,
  stage       : Stage,
) -> Result<SkgnodeDiff, Box<dyn StdError>> {
  let abs_path : PathBuf =
    source_path . join ( &entry . path );
  let rel_path : PathBuf =
    path_relative_to_repo ( repo, &abs_path )
      . unwrap_or_else ( || entry . path . clone() );
  // "before" is HEAD for staged, index for unstaged.
  // "after"  is index for staged, worktree for unstaged.
  let before_node : Option<SkgNode> = match (&entry . status, stage) {
    (GitDiffStatus::Added, _)    => None,
    (_, Stage::Staged)           => load_from_head  (repo, &rel_path),
    (_, Stage::Unstaged)         => load_from_index (repo, &rel_path), };
  let after_node : Option<SkgNode> = match (&entry . status, stage) {
    (GitDiffStatus::Deleted, _)  => None,
    (_, Stage::Staged)           => load_from_index (repo, &rel_path),
    (_, Stage::Unstaged)         => load_from_disk  (&abs_path), };
  let node_changes : Option<NodeChanges> =
    match (&before_node, &after_node) {
      (Some (old), Some (new)) => Some ( compare_skgnodes (old, new) ),
      _                        => None };
  Ok ( SkgnodeDiff {
    status: entry . status . clone(),
    node_changes,
    head_node: before_node }) }

/// Parses a NodeFS from a YAML blob, then attaches a default
/// (empty) source to produce a SkgNode. This preserves today's
/// behavior: diff.rs doesn't know the real source of its blobs,
/// so nodes built here have source at its default. Downstream
/// consumers that care about source do not use diff-derived nodes.
fn nodefs_as_skgnode_with_default_source (
  yaml : &str,
) -> Option<SkgNode> {
  let node_fs : NodeFS = serde_yaml::from_str (yaml) . ok () ?;
  Some ( node_fs . into_complete ( SourceName::default ())) }

fn load_from_head (
  repo     : &git2::Repository,
  rel_path : &Path,
) -> Option<SkgNode> {
  get_file_content_at_head (repo, rel_path) . ok () . flatten ()
    . and_then ( |s| nodefs_as_skgnode_with_default_source (&s) ) }

fn load_from_index (
  repo     : &git2::Repository,
  rel_path : &Path,
) -> Option<SkgNode> {
  get_file_content_at_index (repo, rel_path) . ok () . flatten ()
    . and_then ( |s| nodefs_as_skgnode_with_default_source (&s) ) }

fn load_from_disk (
  abs_path : &Path,
) -> Option<SkgNode> {
  fs::read_to_string (abs_path) . ok ()
    . and_then ( |s| nodefs_as_skgnode_with_default_source (&s) ) }

/// Collect SkgNodes for files that were deleted in either stage.
/// Used to look up titles and bodies for phantom nodes.
fn collect_deleted_nodes_for_both (
  staged   : &HashMap<PathBuf, SkgnodeDiff>,
  unstaged : &HashMap<PathBuf, SkgnodeDiff>,
) -> HashMap<ID, SkgNode> {
  let mut result : HashMap<ID, SkgNode> =
    HashMap::new();
  for diffs in [staged, unstaged] {
    for skgnode_diff in diffs . values () {
      if skgnode_diff . status == GitDiffStatus::Deleted {
        if let Some ( ref head_node ) = skgnode_diff . head_node {
          let pid : &ID = &head_node . pid;
          result . insert ( pid . clone(), head_node . clone() ); }} } }
  result }

/// Compare two SkgNodes and return the differences.
fn compare_skgnodes (
  old : &SkgNode,
  new : &SkgNode,
) -> NodeChanges {
  let text_changed : bool =
    old . title != new . title ||
    old . body  != new . body;
  let aliases_diff : Vec<Diff_Item<String>> =
    compute_interleaved_diff (
      old . aliases . or_default(),
      new . aliases . or_default() );
  let ids_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      & old . all_ids() . cloned() . collect::<Vec<ID>>(),
      & new . all_ids() . cloned() . collect::<Vec<ID>>() );
  let contains_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      &old . contains,
      &new . contains );
  NodeChanges {
    text_changed,
    aliases_diff,
    ids_diff,
    contains_diff }}
