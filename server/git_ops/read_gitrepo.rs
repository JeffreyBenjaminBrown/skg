use crate::types::git::PathDiffStatus;

use git2::{Repository, Diff, DiffOptions, Error, ErrorCode, ObjectType};
use std::path::Path;
use std::str::from_utf8;

use super::misc::diff_delta_to_entry;


/// Get the list of staged changes: files whose contents in the git index
/// differ from HEAD.
pub fn get_staged_changed_skg_files (
  gitrepo : &Repository
) -> Result<Vec<PathDiffStatus>, Error> {
  let head_tree : git2::Tree =
    gitrepo . head() ? . peel_to_tree() ?;
  let mut opts : DiffOptions =
    DiffOptions::new();
  opts . pathspec ("*.skg");
  let diff : Diff =
    gitrepo . diff_tree_to_index (
      Some (&head_tree),
      None,
      Some (&mut opts)) ?;
  diff_to_entries (&diff) }

/// Get the list of unstaged changes: files whose worktree contents
/// differ from the git index. Includes untracked files.
pub fn get_unstaged_changed_skg_files (
  gitrepo : &Repository
) -> Result<Vec<PathDiffStatus>, Error> {
  let mut opts : DiffOptions =
    DiffOptions::new();
  opts . pathspec ("*.skg");
  opts . include_untracked (true);
  let diff : Diff =
    gitrepo . diff_index_to_workdir (
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
/// Returns None if the path is not in a gitrepo.
pub fn open_gitrepo (
  skgrepo_path : &Path
) -> Option<Repository> {
  Repository::discover (skgrepo_path) . ok () }

/// The git remote a receiver would fetch this skgrepo from, as
/// `(remote_name, fetch_url)`: the remote named `origin` when one
/// exists, else the first remote listed (whose name is reported
/// alongside, precisely because it is not the conventional
/// `origin`). The URL is the fetch URL -- the string `git remote -v`
/// prints between the remote name and `(fetch)`.
///
/// `None` when the skgrepo directory is not itself a gitrepo,
/// has no remotes, or the chosen remote carries no (utf-8) URL. The
/// path is opened EXACTLY, without searching parent directories, so a
/// bare skgrepo directory that merely sits inside some unrelated
/// enclosing gitrepo is never mistaken for a gitrepo of its own (and
/// its enclosing gitrepo's remote never misreported as the skgrepo's).
pub fn skgrepo_git_remote (
  skgrepo_path : &Path
) -> Option<(String, String)> {
  let gitrepo : Repository =
    Repository::open (skgrepo_path) . ok () ?;
  let remotes : git2::string_array::StringArray =
    gitrepo . remotes () . ok () ?;
  let names : Vec<&str> =
    remotes . iter ()
    . filter_map ( |name| name . ok () . flatten () )
    . collect ();
  let chosen_name : &str =
    if names . iter () . any ( |n| *n == "origin" ) {
      "origin"
    } else {
      names . first () . copied () ? };
  let remote : git2::Remote =
    gitrepo . find_remote (chosen_name) . ok () ?;
  let url : String =
    remote . url () . ok () . map ( str::to_string ) ?;
  Some (( chosen_name . to_string (), url )) }

/// Get the content of a file at HEAD.
/// Returns None if the file doesn't exist at HEAD.
/// The path should be relative to the repository root.
pub fn get_file_content_at_head (
  gitrepo     : &Repository,
  rel_path : &Path
) -> Result<Option<String>, Error> {
  let head_commit : git2::Commit =
    gitrepo . head() ? . peel_to_commit() ?;
  let tree : git2::Tree =
    head_commit . tree() ?;
  match tree . get_path (rel_path) { // Try to find the file in the tree
    Ok (entry) => {
      if entry . kind() != Some (ObjectType::Blob) {
        return Ok (None); }
      let blob : git2::Blob =
        gitrepo . find_blob ( entry . id() ) ?;
      let content : Option<String> =
        from_utf8 ( blob . content() )
          . map ( |s| s . to_string() )
          . ok();
      Ok (content) },
    Err (e) if e . code() == ErrorCode::NotFound => {
      Ok (None) },
    Err (e) => Err (e) }}

/// Get the content of a file at the index (the staging area).
/// Returns None if the file is not in the git index.
/// The path should be relative to the repository root.
pub fn get_file_content_at_index (
  gitrepo     : &Repository,
  rel_path : &Path
) -> Result<Option<String>, Error> {
  let index : git2::Index =
    gitrepo . index () ?;
  match index . get_path ( rel_path, 0 ) {
    Some (entry) => {
      let blob : git2::Blob =
        gitrepo . find_blob ( entry . id ) ?;
      let content : Option<String> =
        from_utf8 ( blob . content () )
          . map ( |s| s . to_string () )
          . ok ();
      Ok (content) },
    None => Ok (None) }}
