use crate::types::git::{ GitDiffStatus, PathDiffStatus, SourceDiff, SkgnodeDiff, NodeChanges };
use crate::types::list::{diff_lists, compute_interleaved_diff, Diff_as_TwoLists_Lossy, Diff_Item};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;

use super::misc::path_relative_to_repo;
use super::read_repo::{open_repo, get_changed_skg_files, get_file_content_at_head};

use std::collections::HashMap;
use std::error::Error as StdError;
use std::path::{Path, PathBuf};
use std::fs;

/// Compares the current state of .skg files with HEAD.
pub fn compute_diff_for_source (
  source_path : &Path
) -> Result<SourceDiff, Box<dyn StdError>> {
  let repo : git2::Repository =
    match open_repo ( source_path ) {
      Some ( r ) => r,
      None => return Ok ( SourceDiff::new_not_git_repo() ) };
  let changed_files : Vec<PathDiffStatus> =
    get_changed_skg_files ( &repo ) ?;
  let mut skgnode_diffs : HashMap<PathBuf, SkgnodeDiff> =
    HashMap::new();
  for entry in changed_files {
    let skgnode_diff : SkgnodeDiff =
      compute_skgnode_diff ( source_path, &repo, &entry ) ?;
    skgnode_diffs . insert ( entry . path . clone(), skgnode_diff ); }
  let deleted_nodes : HashMap<ID, SkgNode> =
    collect_deleted_nodes ( &skgnode_diffs );
  Ok ( SourceDiff { is_git_repo: true,
                    skgnode_diffs,
                    deleted_nodes }) }

fn compute_skgnode_diff (
  source_path : &Path,
  repo        : &git2::Repository,
  entry       : &PathDiffStatus,
) -> Result<SkgnodeDiff, Box<dyn StdError>> {
  let abs_path : PathBuf =
    source_path . join ( &entry . path );
  let rel_path : PathBuf =
    path_relative_to_repo ( repo, &abs_path )
      . unwrap_or_else ( || entry . path . clone() );
  let head_node : Option<SkgNode> =
    match entry . status {
      GitDiffStatus::Added => None,
      _ => get_file_content_at_head ( repo, &rel_path ) ?
             . and_then ( |s| serde_yaml::from_str ( &s ) . ok() ) };
  let worktree_node : Option<SkgNode> =
    match entry . status {
      GitDiffStatus::Deleted => None,
      _ => fs::read_to_string ( &abs_path ) . ok()
             . and_then ( |s| serde_yaml::from_str ( &s ) . ok() ) };
  let node_changes : Option<NodeChanges> =
    match (&head_node, &worktree_node) {
      (Some(old), Some(new)) => {
        Some ( compare_skgnodes ( old, new )) },
      _ => None };
  Ok ( SkgnodeDiff {
    status: entry . status . clone(),
    node_changes,
    head_node }) }

/// Collect SkgNodes for deleted files (exist in HEAD but not worktree).
fn collect_deleted_nodes (
  skgnode_diffs : &HashMap<PathBuf, SkgnodeDiff>,
) -> HashMap<ID, SkgNode> {
  let mut result : HashMap<ID, SkgNode> =
    HashMap::new();
  for skgnode_diff in skgnode_diffs . values() {
    if skgnode_diff . status == GitDiffStatus::Deleted {
      if let Some ( ref head_node ) = skgnode_diff . head_node {
        if let Some ( pid ) = head_node . ids . first() {
          result . insert ( pid . clone(), head_node . clone() ); }} }}
  result }

/// Compare two SkgNodes and return the differences.
fn compare_skgnodes (
  old : &SkgNode,
  new : &SkgNode,
) -> NodeChanges {
  let text_changed : bool =
    old . title != new . title ||
    old . body  != new . body;
  let aliases_diff : Diff_as_TwoLists_Lossy<String> =
    diff_lists (
      & old . aliases . clone() . unwrap_or_default(),
      & new . aliases . clone() . unwrap_or_default() );
  let ids_diff : Diff_as_TwoLists_Lossy<ID> =
    diff_lists ( & old . ids, & new . ids );
  let ids_interleaved : Vec<Diff_Item<ID>> =
    compute_interleaved_diff ( & old . ids, & new . ids );
  let contains_diff : Diff_as_TwoLists_Lossy<ID> =
    diff_lists (
      & old . contains . clone() . unwrap_or_default(),
      & new . contains . clone() . unwrap_or_default() );
  NodeChanges {
    text_changed,
    aliases_diff,
    ids_diff,
    ids_interleaved,
    contains_diff }}
