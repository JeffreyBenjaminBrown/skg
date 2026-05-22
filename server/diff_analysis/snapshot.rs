use crate::dbs::filesystem::multiple_nodes::read_skg_files_from_folder;
use crate::diff_analysis::types::{DiffSelection, GraphSnapshot, SnapshotKind, SnapshotPair};
use crate::git_ops::misc::path_relative_to_repo;
use crate::git_ops::read_repo::{head_is_merge_commit, open_repo};
use crate::types::misc::{ID, SkgConfig, SkgfileSource, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::fs::NodeFS;

use git2::{ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::from_utf8;

pub fn read_snapshot_pair (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<SnapshotPair, String> {
  let (before_kind, after_kind) : (SnapshotKind, SnapshotKind) =
    endpoint_kinds (selection) ?;
  validate_sources_for_selection (config, before_kind, after_kind) ?;
  let before : GraphSnapshot =
    read_graph_snapshot (config, before_kind) ?;
  let after : GraphSnapshot =
    read_graph_snapshot (config, after_kind) ?;
  Ok ( SnapshotPair { before, after } )
}

fn endpoint_kinds (
  selection : DiffSelection,
) -> Result<(SnapshotKind, SnapshotKind), String> {
  match (selection . include_staged, selection . include_unstaged) {
    (true,  true)  => Ok ((SnapshotKind::Head,  SnapshotKind::Worktree)),
    (true,  false) => Ok ((SnapshotKind::Head,  SnapshotKind::Index)),
    (false, true)  => Ok ((SnapshotKind::Index, SnapshotKind::Worktree)),
    (false, false) => Err (
      "Diff analysis must include staged changes, unstaged changes, or both."
        . to_string () ), } }

fn validate_sources_for_selection (
  config      : &SkgConfig,
  before_kind : SnapshotKind,
  after_kind  : SnapshotKind,
) -> Result<(), String> {
  let needs_head : bool =
    before_kind == SnapshotKind::Head ||
    after_kind  == SnapshotKind::Head;
  for (source_name, source) in &config . sources {
    let source_path : &Path =
      Path::new ( &source . path );
    let repo : Repository =
      open_repo (source_path) . ok_or_else ( || format! (
        "Cannot compute diff analysis: source '{}' is not in a git repository.",
        source_name )) ?;
    repo . head () . map_err ( |e| format! (
      "Cannot compute diff analysis: source '{}' has no HEAD commit: {}",
      source_name, e )) ?;
    if needs_head && head_is_merge_commit (&repo) . map_err ( |e| format! (
      "Cannot compute diff analysis: could not inspect HEAD for source '{}': {}",
      source_name, e )) ? {
      return Err ( format! (
        "Cannot compute diff analysis: HEAD is a merge commit in source '{}'.",
        source_name )); }} 
  Ok (( )) }

fn read_graph_snapshot (
  config : &SkgConfig,
  kind   : SnapshotKind,
) -> Result<GraphSnapshot, String> {
  let mut nodes : Vec<NodeComplete> = Vec::new ();
  for source_name in config . sources . keys () {
    let mut source_nodes : Vec<NodeComplete> =
      match kind {
        SnapshotKind::Head =>
          read_source_from_head (config, source_name) ?,
        SnapshotKind::Index =>
          read_source_from_index (config, source_name) ?,
        SnapshotKind::Worktree =>
          read_skg_files_from_folder (source_name, config)
            . map_err ( |e| format! (
              "Reading worktree source '{}': {}", source_name, e )) ?, };
    nodes . append (&mut source_nodes); }
  Ok (snapshot_from_nodes (nodes))
}

fn snapshot_from_nodes (
  nodes : Vec<NodeComplete>,
) -> GraphSnapshot {
  let mut by_pid : HashMap<ID, NodeComplete> =
    HashMap::new ();
  let mut id_sources : HashMap<ID, BTreeSet<SourceName>> =
    HashMap::new ();
  for node in nodes {
    for id in node . all_ids () {
      id_sources . entry (id . clone ())
        . or_insert_with (BTreeSet::new)
        . insert (node . source . clone ()); }
    by_pid . insert (node . pid . clone (), node); }
  GraphSnapshot { nodes: by_pid, id_sources }
}

fn read_source_from_head (
  config      : &SkgConfig,
  source_name : &SourceName,
) -> Result<Vec<NodeComplete>, String> {
  let source : &SkgfileSource =
    config . sources . get (source_name) . ok_or_else ( || format! (
      "Source '{}' not found in config", source_name )) ?;
  let source_path : &Path =
    Path::new ( &source . path );
  let repo : Repository =
    open_repo (source_path) . ok_or_else ( || format! (
      "Could not open git repo for source '{}'", source_name )) ?;
  let prefix : PathBuf =
    source_prefix_in_repo (&repo, source_path) ?;
  let tree : git2::Tree =
    repo . head ()
      . and_then ( |h| h . peel_to_tree () )
      . map_err ( |e| format! (
        "Reading HEAD tree for source '{}': {}", source_name, e )) ?;
  let mut nodes : Vec<NodeComplete> = Vec::new ();
  let mut parse_error : Option<String> = None;
  let walk_result : Result<(), git2::Error> =
    tree . walk (TreeWalkMode::PreOrder, |root, entry| {
    if parse_error . is_some () {
      return TreeWalkResult::Abort; }
    if entry . kind () != Some (ObjectType::Blob) {
      return TreeWalkResult::Ok; }
    let rel_path : PathBuf =
      PathBuf::from (root) . join (entry . name () . unwrap_or (""));
    if ! path_is_source_skg (&rel_path, &prefix) {
      return TreeWalkResult::Ok; }
    let oid : git2::Oid = entry . id ();
    match repo . find_blob (oid)
      . map_err ( |e| e . to_string () )
      . and_then ( |blob| parse_blob_node (
        blob . content (), source_name, &rel_path ) ) {
      Ok (node) => nodes . push (node),
      Err (e) => {
        parse_error = Some (e);
        return TreeWalkResult::Abort; } }
    TreeWalkResult::Ok
  });
  if let Some (error) = parse_error {
    return Err (error); }
  walk_result . map_err ( |e| format! (
    "Walking HEAD tree for source '{}': {}", source_name, e )) ?;
  Ok (nodes)
}

fn read_source_from_index (
  config      : &SkgConfig,
  source_name : &SourceName,
) -> Result<Vec<NodeComplete>, String> {
  let source : &SkgfileSource =
    config . sources . get (source_name) . ok_or_else ( || format! (
      "Source '{}' not found in config", source_name )) ?;
  let source_path : &Path =
    Path::new ( &source . path );
  let repo : Repository =
    open_repo (source_path) . ok_or_else ( || format! (
      "Could not open git repo for source '{}'", source_name )) ?;
  let prefix : PathBuf =
    source_prefix_in_repo (&repo, source_path) ?;
  let index : git2::Index =
    repo . index () . map_err ( |e| format! (
      "Reading index for source '{}': {}", source_name, e )) ?;
  let mut nodes : Vec<NodeComplete> =
    Vec::new ();
  for entry in index . iter () {
    let rel_path : PathBuf =
      PathBuf::from (String::from_utf8_lossy (&entry . path) . to_string ());
    if ! path_is_source_skg (&rel_path, &prefix) {
      continue; }
    let blob : git2::Blob =
      repo . find_blob (entry . id) . map_err ( |e| format! (
        "Reading index blob {:?} for source '{}': {}",
        rel_path, source_name, e )) ?;
    let node : NodeComplete =
      parse_blob_node (blob . content (), source_name, &rel_path) ?;
    nodes . push (node); }
  Ok (nodes)
}

fn source_prefix_in_repo (
  repo        : &Repository,
  source_path : &Path,
) -> Result<PathBuf, String> {
  let canonical_source_path : PathBuf =
    fs::canonicalize (source_path)
      . unwrap_or_else ( |_| source_path . to_path_buf () );
  path_relative_to_repo (repo, &canonical_source_path)
    . ok_or_else ( || format! (
      "Source path {:?} is not inside its git repository", source_path ))
}

fn path_is_source_skg (
  rel_path : &Path,
  prefix   : &Path,
) -> bool {
  rel_path . starts_with (prefix) &&
    rel_path . extension () . map_or (
      false, |ext| ext == "skg" )
}

fn parse_blob_node (
  bytes       : &[u8],
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<NodeComplete, String> {
  let yaml : &str =
    from_utf8 (bytes) . map_err ( |e| format! (
      "Blob {:?} is not UTF-8: {}", rel_path, e )) ?;
  let node_fs : NodeFS =
    serde_yaml::from_str (yaml) . map_err ( |e| format! (
      "Parsing {:?}: {}", rel_path, e )) ?;
  let stem : String =
    rel_path . file_stem ()
      . ok_or_else ( || format! (
        "Path {:?} has no file stem", rel_path )) ?
      . to_string_lossy () . to_string ();
  if node_fs . pid . 0 != stem {
    return Err ( format! (
      "Path {:?} has pid {}, expected {}", rel_path, node_fs . pid, stem )); }
  Ok ( node_fs . into_complete (source_name . clone ()) )
}
