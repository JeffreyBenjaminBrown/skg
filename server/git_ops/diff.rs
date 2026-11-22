use crate::dbs::filesystem::retired_keys::{quoted_keys, retired_keys_held};
use crate::types::git::{ GitDiffStatus, PathDiffStatus, SkgrepoDiff };
use crate::types::misc::SkgrepoName;
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::nodes::complete::Graphnode;

use super::misc::path_relative_to_gitrepo;
use super::read_gitrepo::{ get_file_content_at_head, get_file_content_at_index, get_staged_changed_skg_files, get_unstaged_changed_skg_files, open_gitrepo, };

use std::error::Error as StdError;
use std::path::{Path, PathBuf};
use std::fs;

/// The nodes of the skgrepo's '.skg' files that git sees added or
/// deleted: in the git index since HEAD (staged), then in the worktree
/// since the git index (unstaged, untracked files among them). Where
/// both stages add, or both delete, one pid, the unstaged node is kept.
pub fn compute_diff_for_skgrepo (
  skgrepo_path : &Path
) -> Result<SkgrepoDiff, Box<dyn StdError>> {
  let gitrepo : git2::Repository =
    match open_gitrepo (skgrepo_path) {
      Some (r) => r,
      None => return Ok ( SkgrepoDiff::new_not_gitrepo() ) };
  let mut skgrepo_diff : SkgrepoDiff =
    SkgrepoDiff::default ();
  add_nodes_added_and_deleted_by_stage (
    skgrepo_path, &gitrepo,
    &get_staged_changed_skg_files (&gitrepo) ?,
    Stage::Staged, &mut skgrepo_diff );
  add_nodes_added_and_deleted_by_stage (
    skgrepo_path, &gitrepo,
    &get_unstaged_changed_skg_files (&gitrepo) ?,
    Stage::Unstaged, &mut skgrepo_diff );
  Ok (skgrepo_diff) }

/// Tag for which pair of git states a stage compares.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Stage { Staged, Unstaged }

/// Into 'skgrepo_diff', the node of each file of 'changed' that
/// 'stage' adds, as the stage leaves it (the index for staged, the
/// worktree for unstaged), and of each it deletes, as the stage found
/// it (HEAD for staged, the index for unstaged). A modified file adds
/// and deletes no node, so it is not read.
fn add_nodes_added_and_deleted_by_stage (
  skgrepo_path : &Path,
  gitrepo      : &git2::Repository,
  changed      : &[PathDiffStatus],
  stage        : Stage,
  skgrepo_diff : &mut SkgrepoDiff,
) {
  for entry in changed {
    let abs_path : PathBuf =
      skgrepo_path . join ( &entry . path );
    let rel_path : PathBuf =
      path_relative_to_gitrepo ( gitrepo, &abs_path )
        . unwrap_or_else ( || entry . path . clone() );
    match entry . status {
      GitDiffStatus::Added => {
        let added : Option<Graphnode> = match stage {
          Stage::Staged   => load_from_index (gitrepo, &rel_path),
          Stage::Unstaged => load_from_disk  (&abs_path), };
        if let Some (node) = added {
          skgrepo_diff . added_nodes . insert ( node . pid . clone(), node ); }},
      GitDiffStatus::Deleted => {
        let deleted : Option<Graphnode> = match stage {
          Stage::Staged   => load_from_head  (gitrepo, &rel_path),
          Stage::Unstaged => load_from_index (gitrepo, &rel_path), };
        if let Some (node) = deleted {
          skgrepo_diff . deleted_nodes . insert ( node . pid . clone(), node ); }},
      GitDiffStatus::Modified => {}, }} }

/// Parses a GraphnodeOnDisk from a YAML blob, then attaches a default
/// (empty) skgrepo to produce a Graphnode. This preserves today's
/// behavior: diff.rs doesn't know the real skgrepo of its blobs,
/// so nodes built here have skgrepo at its default. Downstream
/// consumers that care about skgrepo do not use diff-derived nodes.
///
/// Parse failures are logged at WARN level (with path + origin for
/// context) and returned as None — distinct from "file absent",
/// which also returns None but without logging. Callers can't
/// currently distinguish the two cases, but a corrupt file will at
/// least show up in the server logs. A file holding a retired key
/// ('retired_keys.rs') is treated likewise: read, it would seem a node
/// with no forks.
fn graphnode_on_disk_as_graphnode_with_default_skgrepo (
  yaml    : &str,
  origin  : &str,
  context : &Path,
) -> Option<Graphnode> {
  let retired : Vec<&'static str> = retired_keys_held (yaml);
  if ! retired . is_empty () {
    tracing::warn! (
      origin, path = %context . display (), keys = %quoted_keys (&retired),
      "compute_graphnode_diff: the file holds keys retired in November 2026; \
       treating as if file were absent");
    return None; }
  match yaml_serde::from_str::<GraphnodeOnDisk> (yaml) {
    Ok (node_fs) =>
      Some ( node_fs . into_complete_as_single_section ( SkgrepoName::default ())),
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
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_skgrepo (
                       &s, "HEAD", rel_path) ) }

fn load_from_index (
  gitrepo     : &git2::Repository,
  rel_path : &Path,
) -> Option<Graphnode> {
  get_file_content_at_index (gitrepo, rel_path) . ok () . flatten ()
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_skgrepo (
                       &s, "index", rel_path) ) }

fn load_from_disk (
  abs_path : &Path,
) -> Option<Graphnode> {
  fs::read_to_string (abs_path) . ok ()
    . and_then ( |s| graphnode_on_disk_as_graphnode_with_default_skgrepo (
                       &s, "worktree", abs_path) ) }
