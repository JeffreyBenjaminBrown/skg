use crate::types::git::{ GitDiffStatus, PathDiffStatus, SourceDiff, NodeCompleteDiff, NodeChanges };
use crate::types::list::{compute_interleaved_diff, Diff_Item};
use crate::types::misc::{ID, MSV, SourceName, members_msv, members_of};
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::complete::NodeComplete;

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
  source_path : &Path,
  source_name : &SourceName,
) -> Result<SourceDiff, Box<dyn StdError>> {
  let repo : git2::Repository =
    match open_repo (source_path) {
      Some (r) => r,
      None => return Ok ( SourceDiff::new_not_git_repo() ) };
  let staged : HashMap<PathBuf, NodeCompleteDiff> =
    build_stage_diffs (
      source_path, &repo,
      &get_staged_changed_skg_files (&repo) ?,
      Stage::Staged, source_name ) ?;
  let unstaged : HashMap<PathBuf, NodeCompleteDiff> =
    build_stage_diffs (
      source_path, &repo,
      &get_unstaged_changed_skg_files (&repo) ?,
      Stage::Unstaged, source_name ) ?;
  let deleted_nodes : HashMap<ID, NodeComplete> =
    collect_deleted_nodes_for_both (&staged, &unstaged);
  let added_nodes : HashMap<ID, NodeComplete> =
    collect_added_nodes_for_both (&staged, &unstaged);
  Ok ( SourceDiff { is_git_repo: true,
                    staged,
                    unstaged,
                    added_nodes,
                    deleted_nodes }) }

/// Tag for which pair of git states a stage compares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage { Staged, Unstaged }

fn build_stage_diffs (
  source_path : &Path,
  repo        : &git2::Repository,
  changed     : &[PathDiffStatus],
  stage       : Stage,
  source_name : &SourceName,
) -> Result<HashMap<PathBuf, NodeCompleteDiff>, Box<dyn StdError>> {
  let mut result : HashMap<PathBuf, NodeCompleteDiff> =
    HashMap::new();
  for entry in changed {
    let source_relative_path : Option<PathBuf> =
      source_relative_changed_path (repo, source_path, &entry . path);
    let Some (source_relative_path) : Option<PathBuf> = source_relative_path else {
      continue; };
    let nodecomplete_diff : NodeCompleteDiff =
      compute_nodecomplete_diff_for_stage (
        source_path, repo, entry, &source_relative_path,
        stage, source_name ) ?;
    result . insert ( source_relative_path, nodecomplete_diff ); }
  Ok (result) }

/// Return a changed path only when it is a direct `.skg` child of this
/// configured source. Git diffs are repository-wide; source enumeration is
/// direct-child-only, so sibling source directories must never be parsed
/// under the current source's identity.
fn source_relative_changed_path (
  repo        : &git2::Repository,
  source_path : &Path,
  repo_path   : &Path,
) -> Option<PathBuf> {
  let source_relative_path : PathBuf =
    path_relative_to_repo (repo, source_path) ?;
  let file_relative_path : &Path =
    if source_relative_path . as_os_str () . is_empty () {
      repo_path
    } else {
      repo_path . strip_prefix (&source_relative_path) . ok () ? };
  if file_relative_path . parent () . is_some_and (
    |parent| ! parent . as_os_str () . is_empty () ) {
    return None; }
  let extension : Option<&str> = file_relative_path . extension ()
    . and_then ( |extension| extension . to_str () );
  if extension != Some ("skg") {
    return None; }
  Some (file_relative_path . to_path_buf ()) }

fn compute_nodecomplete_diff_for_stage (
  source_path : &Path,
  repo        : &git2::Repository,
  entry       : &PathDiffStatus,
  source_relative_path : &Path,
  stage       : Stage,
  source_name : &SourceName,
) -> Result<NodeCompleteDiff, Box<dyn StdError>> {
  let abs_path : PathBuf =
    source_path . join ( source_relative_path );
  let rel_path : PathBuf =
    path_relative_to_repo ( repo, &abs_path )
      . unwrap_or_else ( || entry . path . clone() );
  // "before" is HEAD for staged, index for unstaged.
  // "after"  is index for staged, worktree for unstaged.
  let before_node : Option<NodeComplete> = match (&entry . status, stage) {
    (GitDiffStatus::Added, _)    => None,
    (_, Stage::Staged)           =>
      load_from_head  (repo, &rel_path, source_name),
    (_, Stage::Unstaged)         =>
      load_from_index (repo, &rel_path, source_name), };
  let after_node : Option<NodeComplete> = match (&entry . status, stage) {
    (GitDiffStatus::Deleted, _)  => None,
    (_, Stage::Staged)           =>
      load_from_index (repo, &rel_path, source_name),
    (_, Stage::Unstaged)         =>
      load_from_disk  (&abs_path, source_name), };
  let node_changes : Option<NodeChanges> =
    match (&before_node, &after_node) {
      (Some (old), Some (new)) => Some ( compare_nodecompletes (old, new) ),
      _                        => None };
  Ok ( NodeCompleteDiff {
    status: entry . status . clone(),
    node_changes,
    before_node,
    after_node }) }

/// Parses a NodeFS from a YAML blob, then attaches the configured source
/// whose historical section supplied the blob.
///
/// Parse failures are logged at WARN level (with path + origin for
/// context) and returned as None — distinct from "file absent",
/// which also returns None but without logging. Callers can't
/// currently distinguish the two cases, but a corrupt file will at
/// least show up in the server logs.
fn nodefs_as_nodecomplete_with_source (
  yaml    : &str,
  origin  : &str,
  context : &Path,
  source  : &SourceName,
) -> Option<NodeComplete> {
  match serde_yaml::from_str::<NodeFS> (yaml) {
    Ok (node_fs) =>
      Some ( node_fs . into_complete_as_single_section ( source . clone ())),
    Err (e) => {
      tracing::warn! (
        origin, path = %context . display (), error = %e,
        "compute_nodecomplete_diff: YAML parse failed; \
         treating as if file were absent");
      None } } }

fn load_from_head (
  repo     : &git2::Repository,
  rel_path : &Path,
  source   : &SourceName,
) -> Option<NodeComplete> {
  get_file_content_at_head (repo, rel_path) . ok () . flatten ()
    . and_then ( |s| nodefs_as_nodecomplete_with_source (
                       &s, "HEAD", rel_path, source) ) }

fn load_from_index (
  repo     : &git2::Repository,
  rel_path : &Path,
  source   : &SourceName,
) -> Option<NodeComplete> {
  get_file_content_at_index (repo, rel_path) . ok () . flatten ()
    . and_then ( |s| nodefs_as_nodecomplete_with_source (
                       &s, "index", rel_path, source) ) }

fn load_from_disk (
  abs_path : &Path,
  source   : &SourceName,
) -> Option<NodeComplete> {
  fs::read_to_string (abs_path) . ok ()
    . and_then ( |s| nodefs_as_nodecomplete_with_source (
                       &s, "worktree", abs_path, source) ) }

/// Collect NodeCompletes for files that were deleted in either stage.
/// Used to look up titles and bodies for phantom nodes.
fn collect_deleted_nodes_for_both (
  staged   : &HashMap<PathBuf, NodeCompleteDiff>,
  unstaged : &HashMap<PathBuf, NodeCompleteDiff>,
) -> HashMap<ID, NodeComplete> {
  let mut result : HashMap<ID, NodeComplete> =
    HashMap::new();
  for diffs in [staged, unstaged] {
    for nodecomplete_diff in diffs . values () {
      if nodecomplete_diff . status == GitDiffStatus::Deleted {
        if let Some ( ref before_node ) = nodecomplete_diff . before_node {
          let pid : &ID = &before_node . pid;
          result . insert ( pid . clone(), before_node . clone() ); }} } }
  result }

/// Collect NodeCompletes for files added in either stage.
/// Used to look up titles for new/untracked nodes before Tantivy has
/// indexed them.
fn collect_added_nodes_for_both (
  staged   : &HashMap<PathBuf, NodeCompleteDiff>,
  unstaged : &HashMap<PathBuf, NodeCompleteDiff>,
) -> HashMap<ID, NodeComplete> {
  let mut result : HashMap<ID, NodeComplete> =
    HashMap::new();
  for diffs in [staged, unstaged] {
    for nodecomplete_diff in diffs . values () {
      if nodecomplete_diff . status == GitDiffStatus::Added {
        if let Some ( ref after_node ) = nodecomplete_diff . after_node {
          let pid : &ID = &after_node . pid;
          result . insert ( pid . clone(), after_node . clone() ); }} } }
  result }

/// Compare two NodeCompletes and return the differences.
fn compare_nodecompletes (
  old : &NodeComplete,
  new : &NodeComplete,
) -> NodeChanges {
  let text_changed : bool =
    old . title != new . title ||
    old . body  != new . body;
  let old_aliases : MSV<String> = members_msv (& old . aliases);
  let new_aliases : MSV<String> = members_msv (& new . aliases);
  let aliases_diff : Vec<Diff_Item<String>> =
    compute_interleaved_diff (
      old_aliases . or_default(),
      new_aliases . or_default() );
  let ids_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      & old . all_ids() . cloned() . collect::<Vec<ID>>(),
      & new . all_ids() . cloned() . collect::<Vec<ID>>() );
  let old_contains : Vec<ID> = members_of (& old . contains);
  let new_contains : Vec<ID> = members_of (& new . contains);
  let contains_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      &old_contains,
      &new_contains );
  // §C: per-stage diffs of the sharing relations, so a removed
  // subscribee / hidden-outside member gets a per-stage membership axis.
  let old_subscribes_to : MSV<ID> = members_msv (& old . subscribes_to);
  let new_subscribes_to : MSV<ID> = members_msv (& new . subscribes_to);
  let subscribes_to_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      old_subscribes_to . or_default (),
      new_subscribes_to . or_default () );
  let old_hides : MSV<ID> = members_msv (
    & old . hides_from_its_subscriptions);
  let new_hides : MSV<ID> = members_msv (
    & new . hides_from_its_subscriptions);
  let hides_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      old_hides . or_default (),
      new_hides . or_default () );
  let old_overrides_view_of : MSV<ID> = members_msv (& old . overrides_view_of);
  let new_overrides_view_of : MSV<ID> = members_msv (& new . overrides_view_of);
  let overrides_view_of_diff : Vec<Diff_Item<ID>> =
    compute_interleaved_diff (
      old_overrides_view_of . or_default (),
      new_overrides_view_of . or_default () );
  NodeChanges {
    text_changed,
    aliases_diff,
    ids_diff,
    contains_diff,
    subscribes_to_diff,
    hides_diff,
    overrides_view_of_diff }}
