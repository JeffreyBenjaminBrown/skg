//! The before-graph of the straddle: owned skgrepos read at HEAD,
//! cached by HEAD identity. Also the graph a commit would leave, read
//! from trees no commit holds yet ('read_candidate_graph_snapshot'),
//! uncached.

use crate::dbs::filesystem::multiple_nodes::{
  read_skg_sections_from_folder};
use crate::dbs::filesystem::retired_keys::{
  migration_tools_for, quoted_keys, retired_keys_held};
use crate::telescope::compose::compose_telescope;
use crate::telescope::types::{
  Telescope, separate_illegal_sections,
};
use crate::git_ops::git_snapshot_types::{GraphSnapshot, GitSnapshotKind};
use crate::git_ops::misc::path_relative_to_gitrepo;
use crate::git_ops::stage_and_commit::skg_paths_differing_from_head;
use crate::git_ops::read_gitrepo::open_gitrepo;
use crate::types::misc::{
  ID, SkgConfig, Skgrepo, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;

use git2::{ObjectType, Oid, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

static SNAPSHOT_CACHE : OnceLock<Mutex<GitSnapshotCache>> =
  OnceLock::new ();

/// The snapshot of the current owned HEADs, under the key it was read
/// for, or nothing. A snapshot read for a new key replaces it, since
/// once a HEAD moves the snapshot of the old one is not read again.
#[derive(Default)]
struct GitSnapshotCache {
  key_and_snapshot : Option<(String, GraphSnapshot)>,
}

impl GitSnapshotCache {
  fn get (
    &self,
    key : &str,
  ) -> Option<&GraphSnapshot> {
    self . key_and_snapshot . as_ref ()
      . filter ( |(cached_key, _)| cached_key == key )
      . map ( |(_, git_snapshot)| git_snapshot ) }

  fn keep_only (
    &mut self,
    key          : String,
    git_snapshot : GraphSnapshot,
  ) {
    self . key_and_snapshot = Some ((key, git_snapshot)); }

  fn clear (
    &mut self,
  ) {
    self . key_and_snapshot = None; }
}

/// The before endpoint of the graph straddle: every owned skgrepo at
/// HEAD, every foreign skgrepo at its worktree. The live graph reads
/// foreign skgrepos from their worktrees too, so their sections are
/// the same at both endpoints and contribute no change.
/// 'owned_head_identity' is that of 'owned_head_identity (config)';
/// it keys the cache.
pub fn read_owned_head_graph_snapshot_maybe_cached (
  config              : &SkgConfig,
  owned_head_identity : &str,
) -> Result<GraphSnapshot, String> {
  git_snapshot_from_cache_or_read (
    format! ("OwnedHead|{}", owned_head_identity),
    || read_graph_snapshot_per_skgrepo (
      config,
      |skgrepo_name| if config . skgrepo_is_owned (skgrepo_name) {
        GitSnapshotKind::Head } else { GitSnapshotKind::Worktree } ))
}

/// The HEAD commit of every owned skgrepo, as one string: equal
/// strings mean no owned HEAD moved. Refuses when an owned skgrepo is
/// not in a gitrepo or has no HEAD commit.
pub fn owned_head_identity (
  config : &SkgConfig,
) -> Result<String, String> {
  let mut parts : Vec<String> =
    Vec::new ();
  for skgrepo_name in config . owned_skgrepos_in_declaration_order () {
    let skgrepo : &Skgrepo =
      config . skgrepos . get (&skgrepo_name) . ok_or_else ( || format! (
        "Repo '{}' not found in config", skgrepo_name )) ?;
    let gitrepo : Repository =
      open_gitrepo (Path::new (&skgrepo . path)) . ok_or_else ( || format! (
        "Cannot read HEAD of skgrepo '{}': it is not in a git repository.",
        skgrepo_name )) ?;
    let identity : String =
      gitrepo . head ()
        . and_then ( |head| head . peel_to_commit () )
        . map ( |commit| format! ("head:{}", commit . id ()) )
        . map_err ( |e| format! (
          "Cannot read HEAD of skgrepo '{}': it has no HEAD commit: {}",
          skgrepo_name, e )) ?;
    parts . push (format! (
      "{}:{}:{}", skgrepo_name, skgrepo . path . display (), identity )); }
  Ok (parts . join ("|"))
}

/// How many nodes of the foreign skgrepos changed between HEAD and
/// the worktree, found as the owned ones are
/// ('skg_paths_differing_from_head'): the stems of their changed '.skg' files, each
/// counted once, so that a node with sections in two foreign
/// skgrepos counts once. The graph straddle reads foreign skgrepos at
/// their worktrees, so their changes never reach it; the processed
/// summary reports this count instead. A foreign skgrepo that is not
/// in a gitrepo, or has no HEAD commit, counts nothing.
pub fn foreign_changed_node_count (
  config : &SkgConfig,
) -> usize {
  config . ordered_skgrepos () . into_iter ()
    . filter ( |skgrepo_name| ! config . skgrepo_is_owned (skgrepo_name) )
    . filter_map ( |skgrepo_name| {
      let skgrepo : &Skgrepo = config . skgrepos . get (&skgrepo_name) ?;
      let skgrepo_path : &Path = Path::new (&skgrepo . path);
      let gitrepo : Repository = open_gitrepo (skgrepo_path) ?;
      let prefix : PathBuf =
        repo_prefix_in_gitrepo (&gitrepo, skgrepo_path) . ok () ?;
      skg_paths_differing_from_head (&gitrepo, &prefix) . ok () } )
    . flatten ()
    . filter_map ( |(path, _)| path . file_stem ()
                   . map ( |stem| stem . to_string_lossy () . to_string () ))
    . collect::<BTreeSet<String>> ()
    . len () }

/// The graph a commit would leave: each owned skgrepo read from the
/// tree 'trees' gives it (one not there from HEAD), each foreign
/// skgrepo from its worktree, as the before-graph of the straddle is
/// read. Not cached: a tree no commit holds yet is read once.
pub fn read_candidate_graph_snapshot (
  config : &SkgConfig,
  trees  : &HashMap<SkgrepoName, Oid>,
) -> Result<GraphSnapshot, String> {
  read_graph_snapshot_per_skgrepo (
    config,
    |skgrepo_name|
    if ! config . skgrepo_is_owned (skgrepo_name) { GitSnapshotKind::Worktree }
    else { match trees . get (skgrepo_name) {
      Some (tree) => GitSnapshotKind::Tree (*tree),
      None        => GitSnapshotKind::Head, }} ) }

/// Sections arrive in fold order ('ordered_skgrepos') so each
/// telescope composes with its home section first. Every skgrepo
/// whose tree holds retired keys is named in one refusal, one line
/// each, so that the user commits each migration before trying again.
fn read_graph_snapshot_per_skgrepo (
  config            : &SkgConfig,
  kind_for_skgrepo  : impl Fn (&SkgrepoName) -> GitSnapshotKind,
) -> Result<GraphSnapshot, String> {
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  let mut retired_keys_refusals : Vec<String> = Vec::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let kind : GitSnapshotKind =
      kind_for_skgrepo (&skgrepo_name);
    let label : String =
      format! ("read repo '{}' from {:?}", skgrepo_name, kind);
    let read : Result<Vec<(SkgrepoName, GraphnodeOnDisk)>, TreeReadError> =
      profile_step_result (&label, || match kind {
        GitSnapshotKind::Head =>
          read_skgrepo_from_tree (config, &skgrepo_name, None),
        GitSnapshotKind::Tree (tree) =>
          read_skgrepo_from_tree (config, &skgrepo_name, Some (tree)),
        GitSnapshotKind::Worktree =>
          read_skg_sections_from_folder (&skgrepo_name, config)
            . map_err ( |e| TreeReadError::Other ( format! (
              "Reading worktree repo '{}': {}", skgrepo_name, e ))), });
    match read {
      Ok (mut skgrepo_sections) => sections . append (&mut skgrepo_sections),
      Err (TreeReadError::HoldsRetiredKeys (refusal)) =>
        retired_keys_refusals . push (refusal),
      Err (TreeReadError::Other (error)) => return Err (error), }}
  if ! retired_keys_refusals . is_empty () {
    return Err ( retired_keys_refusals . join ("\n") ); }
  profile_step ("snapshot_from_sections", || {
    git_snapshot_from_sections (config, sections) })
}

fn git_snapshot_from_cache_or_read (
  key  : String,
  read : impl FnOnce () -> Result<GraphSnapshot, String>,
) -> Result<GraphSnapshot, String> {
  if let Some (git_snapshot) =
    git_snapshot_cache ()
      . lock ()
      . map_err ( |e| format! (
        "Git snapshot cache lock failed: {}", e )) ?
      . get (&key)
      . cloned () {
    profile_log ("snapshot cache hit", Duration::from_millis (0));
    return Ok (git_snapshot); }
  profile_log ("snapshot cache miss", Duration::from_millis (0));
  let git_snapshot : GraphSnapshot =
    read () ?;
  git_snapshot_cache ()
    . lock ()
    . map_err ( |e| format! (
      "Git snapshot cache lock failed: {}", e )) ?
    . keep_only (key, git_snapshot . clone ());
  Ok (git_snapshot)
}

/// Empty the snapshot cache, so the next read of a snapshot reads
/// disk. A key names HEAD commits only, so an entry that read a
/// worktree (the foreign skgrepos of 'read_owned_head_graph_snapshot_maybe_cached')
/// can be stale under its key; a rebuild of the session-lifetime
/// stores, which rereads every skgrepo, calls this.
pub fn forget_cached_git_snapshots (
) -> Result<(), String> {
  git_snapshot_cache ()
    . lock ()
    . map_err ( |e| format! (
      "Git snapshot cache lock failed: {}", e )) ?
    . clear ();
  Ok (( )) }

fn git_snapshot_cache (
) -> &'static Mutex<GitSnapshotCache> {
  SNAPSHOT_CACHE . get_or_init ( || Mutex::new (GitSnapshotCache::default ()) )
}

fn profile_step<T, F> (
  label : &str,
  f     : F,
) -> T
where
  F : FnOnce () -> T,
{
  let start : Instant =
    Instant::now ();
  let result : T =
    f ();
  profile_log (label, start . elapsed ());
  result
}

fn profile_step_result<T, E, F> (
  label : &str,
  f     : F,
) -> Result<T, E>
where
  F : FnOnce () -> Result<T, E>,
{
  let start : Instant =
    Instant::now ();
  let result : Result<T, E> =
    f ();
  profile_log (label, start . elapsed ());
  result
}

fn profile_log (
  label    : &str,
  duration : Duration,
) {
  if std::env::var_os ("SKG_PROFILE_GIT_SNAPSHOT") . is_none () {
    return; }
  eprintln! (
    "git-snapshot profile: {}: {}.{:03}s",
    label,
    duration . as_secs (),
    duration . subsec_millis ()); }

/// Group sections by pid (sections must arrive in fold order),
/// normalize owned/non-owned pid collisions, and compose each telescope.
fn git_snapshot_from_sections (
  config   : &SkgConfig,
  sections : Vec<(SkgrepoName, GraphnodeOnDisk)>,
) -> Result<GraphSnapshot, String> {
  let mut sections_by_pid : HashMap<ID, Vec<(SkgrepoName, GraphnodeOnDisk)>> =
    HashMap::new ();
  for (skgrepo_name, node_fs) in sections {
    sections_by_pid . entry (node_fs . pid . clone ())
      . or_insert_with (Vec::new)
      . push (( skgrepo_name, node_fs )); }
  for (pid, telescope_sections) in &mut sections_by_pid {
    let sections : Vec<(SkgrepoName, GraphnodeOnDisk)> =
      std::mem::take (telescope_sections);
    let (legitimate, illegal) =
      separate_illegal_sections (sections, config);
    *telescope_sections = legitimate;
    if illegal . is_some () {
      tracing::warn! (
        pid = %pid,
        "git snapshot ignored the sections of an illegal edit to a foreign node" ); }}
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |gnid : &ID| -> ID {
    pid_of . get (gnid) . cloned ()
      . unwrap_or_else ( || gnid . clone () ) };
  let mut by_pid : HashMap<ID, Graphnode> =
    HashMap::new ();
  for (pid, telescope_sections) in sections_by_pid {
    let node : Graphnode =
      fold_telescope_tolerating_homelessness (
        config, &pid, telescope_sections, &resolve ) ?;
    by_pid . insert (pid, node); }
  Ok ( GraphSnapshot { nodes: by_pid } )
}

/// Compose one telescope, but where init would hard-error on a
/// titleless telescope (no home), a git snapshot must not: a commit
/// can legitimately hold an ill-formed state (e.g. a home section
/// deleted in one commit and recreated in the next). Retry with a
/// placeholder title on the most public section, so the before-graph
/// of the graph straddle can still hold the telescope.
fn fold_telescope_tolerating_homelessness (
  config   : &SkgConfig,
  pid      : &ID,
  sections : Vec<(SkgrepoName, GraphnodeOnDisk)>,
  resolve  : &dyn Fn (&ID) -> ID,
) -> Result<Graphnode, String> {
  let retry : Vec<(SkgrepoName, GraphnodeOnDisk)> =
    sections . clone ();
  let telescope : Telescope =
    Telescope::try_new ( pid . clone (), sections, config )
    . map_err ( |e| e . to_string () ) ?;
  match compose_telescope ( telescope, resolve )
  { Ok (node) => Ok (node),
    Err (_) => {
      let mut retry : Vec<(SkgrepoName, GraphnodeOnDisk)> = retry;
      match retry . first_mut () {
        Some ((_, node_fs)) =>
          node_fs . title =
            Some ("(no titled section)" . to_string ()),
        None => return Err ( format! (
          "Telescope '{}' has no sections to fold.", pid )), }
      let retry_telescope : Telescope =
        Telescope::try_new ( pid . clone (), retry, config )
        . map_err ( |e| e . to_string () ) ?;
      compose_telescope ( retry_telescope, resolve )
        . map_err ( |e| e . to_string () ) }}
}

/// Why a skgrepo's tree was not read.
enum TreeReadError {
  /// It holds retired keys; the refusal names its files
  /// ('tree_holds_retired_keys').
  HoldsRetiredKeys (String),
  Other (String),
}

impl From<String> for TreeReadError {
  fn from (
    error : String,
  ) -> TreeReadError {
    TreeReadError::Other (error) }
}

/// The skgrepo's sections as the tree 'tree' of its gitrepo holds
/// them, or as HEAD's tree does when 'tree' is None. A tree holding a
/// retired key ('retired_keys.rs') is refused, naming its files: the
/// owner has migrated the worktree (else the server would not have
/// started) but not yet committed the migration.
fn read_skgrepo_from_tree (
  config       : &SkgConfig,
  skgrepo_name : &SkgrepoName,
  tree         : Option<Oid>,
) -> Result<Vec<(SkgrepoName, GraphnodeOnDisk)>, TreeReadError> {
  let skgrepo : &Skgrepo =
    config . skgrepos . get (skgrepo_name) . ok_or_else ( || format! (
      "Repo '{}' not found in config", skgrepo_name )) ?;
  let skgrepo_path : &Path =
    Path::new ( &skgrepo . path );
  let gitrepo : Repository =
    open_gitrepo (skgrepo_path) . ok_or_else ( || format! (
      "Could not open Git repo for Skg repo '{}'", skgrepo_name )) ?;
  let prefix : PathBuf =
    repo_prefix_in_gitrepo (&gitrepo, skgrepo_path) ?;
  let tree_label : &str =
    if tree . is_some () { "tree to commit" } else { "HEAD" };
  let tree : git2::Tree = match tree {
    Some (tree) => gitrepo . find_tree (tree) . map_err ( |e| format! (
      "Reading tree {} for repo '{}': {}", tree, skgrepo_name, e )) ?,
    None => gitrepo . head ()
      . and_then ( |h| h . peel_to_tree () )
      . map_err ( |e| format! (
        "Reading HEAD tree for repo '{}': {}", skgrepo_name, e )) ?, };
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  let mut parse_error : Option<String> = None;
  let mut retired : Vec<(PathBuf, Vec<&'static str>)> = Vec::new ();
  let walk_result : Result<(), git2::Error> =
    tree . walk (TreeWalkMode::PreOrder, |root, entry| {
    if parse_error . is_some () {
      return TreeWalkResult::Abort; }
    if entry . kind () != Some (ObjectType::Blob) {
      return TreeWalkResult::Ok; }
    let rel_path : PathBuf =
      PathBuf::from (root) . join (entry . name () . unwrap_or (""));
    if ! path_is_skgrepo_skg (&rel_path, &prefix) {
      return TreeWalkResult::Ok; }
    let oid : git2::Oid = entry . id ();
    let blob : git2::Blob = match gitrepo . find_blob (oid) {
      Ok (blob) => blob,
      Err (e) => {
        parse_error = Some (e . to_string ());
        return TreeWalkResult::Abort; } };
    let retired_here : Vec<&'static str> =
      from_utf8 ( blob . content () )
      . map ( retired_keys_held ) . unwrap_or_default ();
    if ! retired_here . is_empty () {
      retired . push (( rel_path, retired_here ));
      return TreeWalkResult::Ok; }
    match parse_blob_section ( blob . content (), &rel_path ) {
      Ok (node_fs) =>
        sections . push (( skgrepo_name . clone (), node_fs )),
      Err (e) => {
        parse_error = Some (e);
        return TreeWalkResult::Abort; } }
    TreeWalkResult::Ok
  });
  if let Some (error) = parse_error {
    return Err ( TreeReadError::Other (error) ); }
  walk_result . map_err ( |e| format! (
    "Walking HEAD tree for repo '{}': {}", skgrepo_name, e )) ?;
  if ! retired . is_empty () {
    return Err ( TreeReadError::HoldsRetiredKeys ( tree_holds_retired_keys (
      skgrepo_name, skgrepo_path, tree_label, &retired ))); }
  Ok (sections)
}

/// The refusal of a git tree holding retired keys: the files (the
/// first few, and how many), and the commit that ends it.
fn tree_holds_retired_keys (
  skgrepo_name : &SkgrepoName,
  skgrepo_path : &Path,
  tree_label   : &str,
  retired      : &[(PathBuf, Vec<&'static str>)],
) -> String {
  const FILES_NAMED : usize = 5;
  let named : Vec<String> = retired . iter () . take (FILES_NAMED)
    . map ( |(path, keys)| format! (
      "{} (holds {})", path . display (), quoted_keys (keys) ))
    . collect ();
  let more : String =
    if retired . len () > FILES_NAMED {
      format! ("; and {} more", retired . len () - FILES_NAMED) }
    else { String::new () };
  let keys : Vec<&str> = retired . iter ()
    . flat_map ( |(_, keys)| keys . iter () . copied () )
    . collect ();
  format! (
    "The {} of skgrepo '{}' holds keys retired in November 2026, in {} file(s): {}{}. If you have run {} over the worktree, commit the migration first: git -C {} commit -am 'Migrate to forks'",
    tree_label, skgrepo_name, retired . len (), named . join ("; "), more,
    migration_tools_for (&keys), skgrepo_path . display () ) }

pub(crate) fn repo_prefix_in_gitrepo (
  gitrepo        : &Repository,
  skgrepo_path   : &Path,
) -> Result<PathBuf, String> {
  let canonical_skgrepo_path : PathBuf =
    fs::canonicalize (skgrepo_path)
      . unwrap_or_else ( |_| skgrepo_path . to_path_buf () );
  path_relative_to_gitrepo (gitrepo, &canonical_skgrepo_path)
    . ok_or_else ( || format! (
      "Skg repo path {:?} is not inside its Git repository", skgrepo_path ))
}

pub(crate) fn path_is_skgrepo_skg (
  rel_path : &Path,
  prefix   : &Path,
) -> bool {
  rel_path . starts_with (prefix) &&
    rel_path . extension () . map_or (
      false, |ext| ext == "skg" )
}

/// One file's contents as a section (pid-checked against the file
/// stem). The snapshot composes same-pid sections into one telescope.
pub(crate) fn parse_blob_section (
  bytes    : &[u8],
  rel_path : &Path,
) -> Result<GraphnodeOnDisk, String> {
  let yaml : &str =
    from_utf8 (bytes) . map_err ( |e| format! (
      "Blob {:?} is not UTF-8: {}", rel_path, e )) ?;
  let node_fs : GraphnodeOnDisk =
    yaml_serde::from_str (yaml) . map_err ( |e| format! (
      "Parsing {:?}: {}", rel_path, e )) ?;
  let stem : String =
    rel_path . file_stem ()
      . ok_or_else ( || format! (
        "Path {:?} has no file stem", rel_path )) ?
      . to_string_lossy () . to_string ();
  if node_fs . pid . 0 != stem {
    return Err ( format! (
      "Path {:?} has pid {}, expected {}", rel_path, node_fs . pid, stem )); }
  Ok (node_fs)
}

/// One file as a whole node -- the per-blob view the vanished-node
/// history search uses (it inspects one historical blob at a time,
/// so there is no telescope to compose).
pub(crate) fn parse_blob_node (
  bytes        : &[u8],
  skgrepo_name : &SkgrepoName,
  rel_path     : &Path,
) -> Result<Graphnode, String> {
  parse_blob_section (bytes, rel_path)
    . map ( |node_fs|
            node_fs . into_complete_as_single_section (
              skgrepo_name . clone () ) )
}

#[cfg(test)]
#[path = "../../tests/unit/git_snapshot_cache.rs"]
mod tests;
