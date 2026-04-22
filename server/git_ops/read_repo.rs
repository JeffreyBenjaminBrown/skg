use crate::types::git::PathDiffStatus;
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;

use git2::{Repository, Diff, DiffOptions, Error, ErrorCode, ObjectType};
use std::error::Error as StdError;
use std::path::{Path, PathBuf};
use std::str::from_utf8;

use super::misc::{diff_delta_to_entry, path_relative_to_repo};

/// Load a NodeComplete from git HEAD given its ID and source.
pub fn nodecomplete_from_git_head (
  pid    : &ID,
  src    : &SourceName,
  config : &SkgConfig,
) -> Result<NodeComplete, Box<dyn StdError>> {
  let source_config : &SkgfileSource =
    config . sources . get (src)
    . ok_or_else ( || format! ( "Source '{}' not found in config",
                                src )) ?;
  let source_path : &Path =
    Path::new ( &source_config . path );
  let repo : git2::Repository =
    open_repo (source_path) . ok_or_else ( || format! (
      "Could not open git repo at {:?}", source_path )) ?;
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ));
  let rel_path : PathBuf =
    path_relative_to_repo ( &repo, &source_path . join (&file_path) )
    . unwrap_or (file_path);
  let content : Option<String> =
    get_file_content_at_head ( &repo, &rel_path ) ?;
  let content_str : String =
    content . ok_or_else ( || format! ( "File {:?} not found at HEAD",
                                        rel_path )) ?;
  let node_fs : NodeFS =
    serde_yaml::from_str (&content_str) . map_err (
      |e| format! ( "Failed to parse NodeComplete for {}: {}",
                    pid . 0, e )) ?;
  Ok ( node_fs . into_complete ( src . clone ())) }

/// Load a NodeComplete for a node whose worktree file is gone, preferring
/// the index version over HEAD when both exist (since the index is
/// staged and therefore more recent).
/// Returns Err if neither location has the file.
pub fn nodecomplete_from_index_or_head (
  pid    : &ID,
  src    : &SourceName,
  config : &SkgConfig,
) -> Result<NodeComplete, Box<dyn StdError>> {
  let source_config : &SkgfileSource =
    config . sources . get (src)
    . ok_or_else ( || format! ( "Source '{}' not found in config",
                                src )) ?;
  let source_path : &Path =
    Path::new ( &source_config . path );
  let repo : git2::Repository =
    open_repo (source_path) . ok_or_else ( || format! (
      "Could not open git repo at {:?}", source_path )) ?;
  let file_path : PathBuf =
    PathBuf::from ( format! ( "{}.skg", pid . 0 ));
  let rel_path : PathBuf =
    path_relative_to_repo ( &repo, &source_path . join (&file_path) )
    . unwrap_or (file_path . clone ());
  // Index first.
  if let Some (content) = get_file_content_at_index (&repo, &rel_path) ? {
    let node_fs : NodeFS = serde_yaml::from_str (&content) . map_err (
      |e| format! ( "Failed to parse NodeComplete for {} from index: {}",
                    pid . 0, e )) ?;
    return Ok ( node_fs . into_complete ( src . clone ())); }
  // HEAD fallback.
  let content : String = get_file_content_at_head (&repo, &rel_path) ?
    . ok_or_else ( || format! (
      "File {:?} not found in index or HEAD", rel_path )) ?;
  let node_fs : NodeFS = serde_yaml::from_str (&content) . map_err (
    |e| format! ( "Failed to parse NodeComplete for {} from HEAD: {}",
                  pid . 0, e )) ?;
  Ok ( node_fs . into_complete ( src . clone ())) }

/// Get the list of staged changes: files whose contents in the index
/// differ from HEAD.
pub fn get_staged_changed_skg_files (
  repo : &Repository
) -> Result<Vec<PathDiffStatus>, Error> {
  let head_tree : git2::Tree =
    repo . head() ? . peel_to_tree() ?;
  let mut opts : DiffOptions =
    DiffOptions::new();
  opts . pathspec ("*.skg");
  let diff : Diff =
    repo . diff_tree_to_index (
      Some (&head_tree),
      None,
      Some (&mut opts)) ?;
  diff_to_entries (&diff) }

/// Get the list of unstaged changes: files whose worktree contents
/// differ from the index. Includes untracked files.
pub fn get_unstaged_changed_skg_files (
  repo : &Repository
) -> Result<Vec<PathDiffStatus>, Error> {
  let mut opts : DiffOptions =
    DiffOptions::new();
  opts . pathspec ("*.skg");
  opts . include_untracked (true);
  let diff : Diff =
    repo . diff_index_to_workdir (
      None,
      Some (&mut opts)) ?;
  diff_to_entries (&diff) }

fn diff_to_entries (
  diff : &Diff
) -> Result<Vec<PathDiffStatus>, Error> {
  let mut entries : Vec<PathDiffStatus> =
    Vec::new();
  for delta in diff . deltas() {
    let entry : Option<PathDiffStatus> =
      diff_delta_to_entry (&delta);
    if let Some (e) = entry {
      entries . push (e); }}
  Ok (entries) }

/// Open the repository containing the given path.
/// Returns None if the path is not in a git repository.
pub fn open_repo (
  source_path : &Path
) -> Option<Repository> {
  Repository::discover (source_path) . ok () }

/// Get the content of a file at HEAD.
/// Returns None if the file doesn't exist at HEAD.
/// The path should be relative to the repository root.
pub fn get_file_content_at_head (
  repo     : &Repository,
  rel_path : &Path
) -> Result<Option<String>, Error> {
  let head_commit : git2::Commit =
    repo . head() ? . peel_to_commit() ?;
  let tree : git2::Tree =
    head_commit . tree() ?;
  match tree . get_path (rel_path) { // Try to find the file in the tree
    Ok (entry) => {
      if entry . kind() != Some (ObjectType::Blob) {
        return Ok (None); }
      let blob : git2::Blob =
        repo . find_blob ( entry . id() ) ?;
      let content : Option<String> =
        from_utf8 ( blob . content() )
          . map ( |s| s . to_string() )
          . ok();
      Ok (content) },
    Err (e) if e . code() == ErrorCode::NotFound => {
      Ok (None) },
    Err (e) => Err (e) }}

/// Get the content of a file at the index (the staging area).
/// Returns None if the file is not in the index.
/// The path should be relative to the repository root.
pub fn get_file_content_at_index (
  repo     : &Repository,
  rel_path : &Path
) -> Result<Option<String>, Error> {
  let index : git2::Index =
    repo . index () ?;
  match index . get_path ( rel_path, 0 ) {
    Some (entry) => {
      let blob : git2::Blob =
        repo . find_blob ( entry . id ) ?;
      let content : Option<String> =
        from_utf8 ( blob . content () )
          . map ( |s| s . to_string () )
          . ok ();
      Ok (content) },
    None => Ok (None) }}

/// Check if HEAD is a merge commit (has multiple parents).
/// This is used to abort saves when the comparison baseline is ambiguous.
pub fn head_is_merge_commit (
  repo : &Repository
) -> Result<bool, Error> {
  let head_commit : git2::Commit =
    repo . head() ? . peel_to_commit() ?;
  Ok ( head_commit . parent_count() > 1 ) }
