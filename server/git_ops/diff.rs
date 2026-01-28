use crate::types::git::{ GitDiffStatus, PathDiffStatus, SourceDiff, FileDiff, NodeChanges };
use crate::types::list::{diff_lists, ListDiff};
use crate::types::misc::ID;
use crate::types::skgnode::SkgNode;

use super::misc::path_relative_to_repo;
use super::read_repo::{open_repo, get_changed_skg_files, get_file_content_at_head};

use std::collections::HashMap;
use std::error::Error as StdError;
use std::path::{Path, PathBuf};

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
  let mut file_diffs : HashMap<PathBuf, FileDiff> =
    HashMap::new();
  for entry in changed_files {
    let file_diff : FileDiff =
      compute_file_diff ( source_path, &repo, &entry ) ?;
    file_diffs . insert ( entry . path . clone(), file_diff ); }
  Ok ( SourceDiff { is_git_repo: true,
                    file_diffs }) }

fn compute_file_diff (
  source_path : &Path,
  repo        : &git2::Repository,
  entry       : &PathDiffStatus,
) -> Result<FileDiff, Box<dyn StdError>> {
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
      _ => std::fs::read_to_string ( &abs_path ) . ok()
             . and_then ( |s| serde_yaml::from_str ( &s ) . ok() ) };
  let node_changes : Option<NodeChanges> =
    match (&head_node, &worktree_node) {
      (Some(old), Some(new)) => {
        Some ( compare_skgnodes ( old, new )) },
      _ => None };
  Ok ( FileDiff {
    status: entry . status . clone(),
    node_changes }) }

/// Compare two SkgNodes and return the differences.
fn compare_skgnodes (
  old : &SkgNode,
  new : &SkgNode,
) -> NodeChanges {
  let text_changed : bool =
    old . title != new . title ||
    old . body  != new . body;
  let aliases_diff : ListDiff<String> =
    diff_lists (
      & old . aliases . clone() . unwrap_or_default(),
      & new . aliases . clone() . unwrap_or_default() );
  let ids_diff : ListDiff<ID> =
    diff_lists ( & old . ids, & new . ids );
  NodeChanges {
    text_changed,
    aliases_diff,
    ids_diff }}
