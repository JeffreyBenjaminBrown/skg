use crate::types::git::{ GitDiffStatus, PathDiffStatus, RepoDiff, GraphnodeDiff, NodeChanges };
use crate::types::list::{compute_interleaved_diff, Diff_Item};
use crate::types::misc::{ID, MSV, RepoName, members_msv, members_of};
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::nodes::complete::Graphnode;

use super::misc::path_relative_to_gitrepo;
use super::read_gitrepo::{ get_file_content_at_head, get_file_content_at_index, get_staged_changed_skg_files, get_unstaged_changed_skg_files, open_gitrepo, };

use std::collections::HashMap;
use std::error::Error as StdError;
use std::path::{Path, PathBuf};
use std::fs;

/// Compute the per-stage diff for a Skg repo.
/// 'staged'   compares HEAD  to the index.
/// 'unstaged' compares the index to the worktree.
pub fn compute_diff_for_repo (
  repo_path : &Path
) -> Result<RepoDiff, Box<dyn StdError>> {
  let gitrepo : git2::Repository =
    match open_gitrepo (repo_path) {
      Some (r) => r,
      None => return Ok ( RepoDiff::new_not_gitrepo() ) };
  let staged : HashMap<PathBuf, GraphnodeDiff> =
    build_stage_diffs (
      repo_path, &gitrepo,
      &get_staged_changed_skg_files (&gitrepo) ?,
      Stage::Staged ) ?;
  let unstaged : HashMap<PathBuf, GraphnodeDiff> =
    build_stage_diffs (
      repo_path, &gitrepo,
      &get_unstaged_changed_skg_files (&gitrepo) ?,
      Stage::Unstaged ) ?;
  let deleted_nodes : HashMap<ID, Graphnode> =
    collect_deleted_nodes_for_both (&staged, &unstaged);
  let added_nodes : HashMap<ID, Graphnode> =
    collect_added_nodes_for_both (&staged, &unstaged);
  Ok ( RepoDiff { is_gitrepo: true,
                    staged,
                    unstaged,
                    added_nodes,
                    deleted_nodes }) }

/// Tag for which pair of git states a stage compares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage { Staged, Unstaged }

fn build_stage_diffs (
  repo_path : &Path,
  gitrepo        : &git2::Repository,
  changed     : &[PathDiffStatus],
  stage       : Stage,
) -> Result<HashMap<PathBuf, GraphnodeDiff>, Box<dyn StdError>> {
  let mut result : HashMap<PathBuf, GraphnodeDiff> =
    HashMap::new();
  for entry in changed {
    let graphnode_diff : GraphnodeDiff =
      compute_graphnode_diff_for_stage (
        repo_path, gitrepo, entry, stage ) ?;
    result . insert ( entry . path . clone(), graphnode_diff ); }
  Ok (result) }

fn compute_graphnode_diff_for_stage (
  repo_path : &Path,
  gitrepo        : &git2::Repository,
  entry       : &PathDiffStatus,
  stage       : Stage,
) -> Result<GraphnodeDiff, Box<dyn StdError>> {
  let abs_path : PathBuf =
    repo_path . join ( &entry . path );
  let rel_path : PathBuf =
    path_relative_to_gitrepo ( gitrepo, &abs_path )
      . unwrap_or_else ( || entry . path . clone() );
  // "before" is HEAD for staged, index for unstaged.
  // "after"  is index for staged, worktree for unstaged.
  let before_node : Option<Graphnode> = match (&entry . status, stage) {
    (GitDiffStatus::Added, _)    => None,
    (_, Stage::Staged)           => load_from_head  (gitrepo, &rel_path),
    (_, Stage::Unstaged)         => load_from_index (gitrepo, &rel_path), };
  let after_node : Option<Graphnode> = match (&entry . status, stage) {
    (GitDiffStatus::Deleted, _)  => None,
    (_, Stage::Staged)           => load_from_index (gitrepo, &rel_path),
    (_, Stage::Unstaged)         => load_from_disk  (&abs_path), };
  let node_changes : Option<NodeChanges> =
    match (&before_node, &after_node) {
      (Some (old), Some (new)) => Some ( compare_graphnodes (old, new) ),
      _                        => None };
  Ok ( GraphnodeDiff {
    status: entry . status . clone(),
    node_changes,
    before_node: before_node . filter (
      |_| entry . status == GitDiffStatus::Deleted),
    after_node: after_node . filter (
      |_| entry . status == GitDiffStatus::Added) }) }

/// Parses a GraphnodeOnDisk from a YAML blob, then attaches a default
/// (empty) Skg repo to produce a Graphnode. This preserves today's
/// behavior: diff.rs doesn't know the real Skg repo of its blobs,
/// so nodes built here have Skg repo at its default. Downstream
/// consumers that care about Skg repo do not use diff-derived nodes.
///
/// Parse failures are logged at WARN level (with path + origin for
/// context) and returned as None — distinct from "file absent",
/// which also returns None but without logging. Callers can't
/// currently distinguish the two cases, but a corrupt file will at
/// least show up in the server logs.
fn graphnode_on_disk_as_graphnode_with_default_repo (
  yaml    : &str,
  origin  : &str,
  context : &Path,
) -> Option<Graphnode> {
  match serde_yaml::from_str::<GraphnodeOnDisk> (yaml) {
    Ok (node_fs) =>
      Some ( node_fs . into_complete_as_single_section ( RepoName::default ())),
    Err (e) => {
      tracing::warn! (
        origin, path = %context . display (), error = %e,
        "compute_graphnode_diff: YAML parse failed; \
         treating as if file were absent");
      None } } }

fn load_from_head (
  gitrepo     : &git2::Repository,
  rel_path : &Path,
) -> Option<Graphnode> {
  get_file_content_at_head (gitrepo, rel_path) . ok () . flatten ()
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_repo (
                       &s, "HEAD", rel_path) ) }

fn load_from_index (
  gitrepo     : &git2::Repository,
  rel_path : &Path,
) -> Option<Graphnode> {
  get_file_content_at_index (gitrepo, rel_path) . ok () . flatten ()
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_repo (
                       &s, "index", rel_path) ) }

fn load_from_disk (
  abs_path : &Path,
) -> Option<Graphnode> {
  fs::read_to_string (abs_path) . ok ()
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_repo (
                       &s, "worktree", abs_path) ) }

/// Collect Graphnodes for files that were deleted in either stage.
/// Used to look up titles and bodies for phantom nodes.
fn collect_deleted_nodes_for_both (
  staged   : &HashMap<PathBuf, GraphnodeDiff>,
  unstaged : &HashMap<PathBuf, GraphnodeDiff>,
) -> HashMap<ID, Graphnode> {
  let mut result : HashMap<ID, Graphnode> =
    HashMap::new();
  for diffs in [staged, unstaged] {
    for graphnode_diff in diffs . values () {
      if graphnode_diff . status == GitDiffStatus::Deleted {
        if let Some ( ref before_node ) = graphnode_diff . before_node {
          let pid : &ID = &before_node . pid;
          result . insert ( pid . clone(), before_node . clone() ); }} } }
  result }

/// Collect Graphnodes for files added in either stage.
/// Used to look up titles for new/untracked nodes before Tantivy has
/// indexed them.
fn collect_added_nodes_for_both (
  staged   : &HashMap<PathBuf, GraphnodeDiff>,
  unstaged : &HashMap<PathBuf, GraphnodeDiff>,
) -> HashMap<ID, Graphnode> {
  let mut result : HashMap<ID, Graphnode> =
    HashMap::new();
  for diffs in [staged, unstaged] {
    for graphnode_diff in diffs . values () {
      if graphnode_diff . status == GitDiffStatus::Added {
        if let Some ( ref after_node ) = graphnode_diff . after_node {
          let pid : &ID = &after_node . pid;
          result . insert ( pid . clone(), after_node . clone() ); }} } }
  result }

/// Compare two Graphnodes and return the differences.
fn compare_graphnodes (
  old : &Graphnode,
  new : &Graphnode,
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
  // subscribee / hidden-outside member gets a per-stage relationship axis.
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
