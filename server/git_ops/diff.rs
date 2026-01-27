use crate::types::git::{
  DiffStatus, PathDiffStatus, SourceDiff, FileDiff, NodeChanges };
use crate::types::list::{diff_lists, ListDiff};
use crate::types::misc::{ID, SkgConfig};
use crate::types::skgnode::SkgNode;

use std::collections::HashMap;
use std::error::Error as StdError;
use std::path::{Path, PathBuf};

use super::misc::path_relative_to_repo;
use super::read_repo::{open_repo, get_changed_skg_files, get_file_content_at_head};

pub fn compute_diff_for_every_source (
  config : &SkgConfig
) -> HashMap<String, SourceDiff> {
  let mut source_diffs : HashMap<String, SourceDiff> =
    HashMap::new();
  for (source_name, source_config) in &config . sources {
    let source_path : &Path =
      Path::new ( &source_config . path );
    match compute_diff_for_source ( source_path ) {
      Ok ( diff ) => {
        source_diffs . insert ( source_name . clone(), diff ); },
      Err ( e ) => { // Log error but continue with other sources
        eprintln! ( "Warning: Failed to compute diff for source '{}': {}",
                    source_name, e ); }}}
  source_diffs }

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
  Ok ( SourceDiff {
    is_git_repo: true,
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
  let head_content : Option<String> =
    match entry . status {
      DiffStatus::Added => None,
      _ => get_file_content_at_head ( repo, &rel_path ) ? };
  let worktree_content : Option<String> =
    match entry . status {
      DiffStatus::Deleted => None,
      _ => std::fs::read_to_string ( &abs_path ) . ok() };
  let node_changes : Option<NodeChanges> = // Parse both versions and compute changes
    match (&head_content, &worktree_content) {
      (Some(old), Some(new)) => {
        Some ( compute_node_changes ( old, new )) },
      _ => None };
  Ok ( FileDiff {
    status: entry . status . clone(),
    node_changes }) }

/// Compute changes between two versions of a node's content.
/// Parses both as SkgNode and compares fields.
fn compute_node_changes (
  old_content : &str,
  new_content : &str,
) -> NodeChanges {
  let old_opt : Option<SkgNode> =
    serde_yaml::from_str ( old_content ) . ok();
  let new_opt : Option<SkgNode> =
    serde_yaml::from_str ( new_content ) . ok();
  match (old_opt, new_opt) {
    (Some(old), Some(new)) => {
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
        ids_diff }},
    _ => NodeChanges::default() }}
