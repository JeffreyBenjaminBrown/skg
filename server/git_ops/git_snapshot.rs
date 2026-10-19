use crate::dbs::filesystem::multiple_nodes::{
  read_skg_sections_from_folder};
use crate::telescope::compose::compose_telescope;
use crate::telescope::types::{
  Telescope, retain_owned_sections_when_pid_folderlides,
};
use crate::git_ops::git_snapshot_types::{GraphSnapshot, GitSnapshotKind};
use crate::git_ops::misc::path_relative_to_gitrepo;
use crate::git_ops::read_gitrepo::{
  get_staged_changed_skg_files, get_unstaged_changed_skg_files,
  open_gitrepo};
use crate::types::misc::{
  ID, SkgConfig, Skgrepo, SkgrepoName};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;

use git2::{ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use std::sync::{Mutex, OnceLock};
use std::time::{Duration, Instant};

static SNAPSHOT_CACHE : OnceLock<Mutex<HashMap<String, GraphSnapshot>>> =
  OnceLock::new ();

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
  for skgrepo_name in config . owned_skgrepos_in_config_order () {
    let skgrepo : &Skgrepo =
      config . skgrepos . get (&skgrepo_name) . ok_or_else ( || format! (
        "Repo '{}' not found in config", skgrepo_name )) ?;
    let gitrepo : Repository =
      open_gitrepo (Path::new (&skgrepo . path)) . ok_or_else ( || format! (
        "Cannot open the diff view: Skg repo '{}' is not in a git repository.",
        skgrepo_name )) ?;
    let identity : String =
      gitrepo . head ()
        . and_then ( |head| head . peel_to_commit () )
        . map ( |commit| format! ("head:{}", commit . id ()) )
        . map_err ( |e| format! (
          "Cannot open the diff view: Skg repo '{}' has no HEAD commit: {}",
          skgrepo_name, e )) ?;
    parts . push (format! (
      "{}:{}:{}", skgrepo_name, skgrepo . path . display (), identity )); }
  Ok (parts . join ("|"))
}

/// How many '.skg' files of the foreign skgrepos differ between HEAD
/// and the worktree. The graph straddle reads foreign skgrepos at their
/// worktrees, so their changes never reach it; the readiness summary
/// reports this count instead. A foreign skgrepo that is not in a
/// gitrepo, or has no HEAD commit, counts nothing.
pub fn foreign_changed_skg_file_count (
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
      changed_paths_for_skgrepo (
        &gitrepo, &prefix, GitSnapshotKind::Head, GitSnapshotKind::Worktree )
        . ok ()
        . map ( |paths| paths . len () ) } )
    . sum () }

/// Sections arrive in privacy order (ordered_repos) so each
/// telescope composes with its most public section first.
fn read_graph_snapshot_per_skgrepo (
  config            : &SkgConfig,
  kind_for_skgrepo  : impl Fn (&SkgrepoName) -> GitSnapshotKind,
) -> Result<GraphSnapshot, String> {
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let kind : GitSnapshotKind =
      kind_for_skgrepo (&skgrepo_name);
    let label : String =
      format! ("read repo '{}' from {:?}", skgrepo_name, kind);
    let mut skgrepo_sections : Vec<(SkgrepoName, GraphnodeOnDisk)> =
      profile_step_result (&label, || match kind {
        GitSnapshotKind::Head =>
          read_skgrepo_from_head (config, &skgrepo_name),
        GitSnapshotKind::Index =>
          read_skgrepo_from_index (config, &skgrepo_name),
        GitSnapshotKind::Worktree =>
          read_skg_sections_from_folder (&skgrepo_name, config)
            . map_err ( |e| format! (
              "Reading worktree repo '{}': {}", skgrepo_name, e )), }) ?;
    sections . append (&mut skgrepo_sections); }
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
    . insert (key, git_snapshot . clone ());
  Ok (git_snapshot)
}

/// Empty the snapshot cache, so the next read of each snapshot reads
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
) -> &'static Mutex<HashMap<String, GraphSnapshot>> {
  SNAPSHOT_CACHE . get_or_init ( || Mutex::new (HashMap::new ()) )
}

fn changed_paths_for_skgrepo (
  gitrepo        : &Repository,
  prefix      : &Path,
  before_kind : GitSnapshotKind,
  after_kind  : GitSnapshotKind,
) -> Result<BTreeSet<PathBuf>, String> {
  let mut paths : BTreeSet<PathBuf> =
    BTreeSet::new ();
  match (before_kind, after_kind) {
    (GitSnapshotKind::Head, GitSnapshotKind::Index) => {
      for entry in get_staged_changed_skg_files (gitrepo)
        . map_err ( |e| format! (
          "Reading staged changed .skg files: {}", e )) ? {
        if path_is_skgrepo_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    (GitSnapshotKind::Index, GitSnapshotKind::Worktree) => {
      for entry in get_unstaged_changed_skg_files (gitrepo)
        . map_err ( |e| format! (
          "Reading unstaged changed .skg files: {}", e )) ? {
        if path_is_skgrepo_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    (GitSnapshotKind::Head, GitSnapshotKind::Worktree) => {
      for entry in get_staged_changed_skg_files (gitrepo)
        . map_err ( |e| format! (
          "Reading staged changed .skg files: {}", e )) ?
        . into_iter ()
        . chain (get_unstaged_changed_skg_files (gitrepo)
          . map_err ( |e| format! (
            "Reading unstaged changed .skg files: {}", e )) ?
          . into_iter ()) {
        if path_is_skgrepo_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    _ => return Err ( format! (
      "Unsupported diff-report endpoints: {:?} to {:?}",
      before_kind, after_kind )), }
  Ok (paths)
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

/// Group sections by pid (sections must arrive in privacy order),
/// normalize owned/non-owned pid collisions, compose each telescope,
/// and record the retained sections' id claims.
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
    let (retained, collision) =
      retain_owned_sections_when_pid_folderlides (sections, config);
    *telescope_sections = retained;
    if let Some (collision) = collision {
      tracing::warn! (
        pid = %pid,
        ignored_skgrepos = ?collision . ignored_skgrepos,
        "diff snapshot ignored non-owned files colliding with an owned telescope" ); }}
  let mut id_claims
    : HashMap<ID, BTreeMap<ID, BTreeSet<SkgrepoName>>> =
    HashMap::new ();
  for sections in sections_by_pid . values () {
    for (skgrepo_name, node_fs) in sections {
      record_section_claims (
        &mut id_claims, node_fs, skgrepo_name ); }}
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |skgid : &ID| -> ID {
    pid_of . get (skgid) . cloned ()
      . unwrap_or_else ( || skgid . clone () ) };
  let mut by_pid : HashMap<ID, Graphnode> =
    HashMap::new ();
  for (pid, telescope_sections) in sections_by_pid {
    let node : Graphnode =
      fold_telescope_tolerating_homelessness (
        config, &pid, telescope_sections, &resolve ) ?;
    by_pid . insert (pid, node); }
  Ok ( GraphSnapshot { nodes: by_pid, id_claims } )
}

/// Compose one telescope, but where init would hard-error on a
/// titleless telescope (no home), a snapshot must not: diff
/// endpoints legitimately pass through ill-formed states (e.g. a
/// home-section deletion staged before its recreation). Retry with
/// a placeholder title on the most public section, so the report
/// can still describe the telescope.
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

/// Record one section's id claims: it claims its pid and every
/// extra id it lists, all attributed to (pid, skgrepo).
fn record_section_claims (
  id_claims : &mut HashMap<ID, BTreeMap<ID, BTreeSet<SkgrepoName>>>,
  node_fs   : &GraphnodeOnDisk,
  skgrepo   : &SkgrepoName,
) {
  for skgid in std::iter::once (&node_fs . pid)
    . chain (node_fs . extra_ids . iter ()) {
    id_claims . entry (skgid . clone ())
      . or_insert_with (BTreeMap::new)
      . entry (node_fs . pid . clone ())
      . or_insert_with (BTreeSet::new)
      . insert (skgrepo . clone ()); }}

fn read_skgrepo_from_head (
  config       : &SkgConfig,
  skgrepo_name : &SkgrepoName,
) -> Result<Vec<(SkgrepoName, GraphnodeOnDisk)>, String> {
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
  let tree : git2::Tree =
    gitrepo . head ()
      . and_then ( |h| h . peel_to_tree () )
      . map_err ( |e| format! (
        "Reading HEAD tree for repo '{}': {}", skgrepo_name, e )) ?;
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> = Vec::new ();
  let mut parse_error : Option<String> = None;
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
    match gitrepo . find_blob (oid)
      . map_err ( |e| e . to_string () )
      . and_then ( |blob| parse_blob_section (
        blob . content (), &rel_path ) ) {
      Ok (node_fs) =>
        sections . push (( skgrepo_name . clone (), node_fs )),
      Err (e) => {
        parse_error = Some (e);
        return TreeWalkResult::Abort; } }
    TreeWalkResult::Ok
  });
  if let Some (error) = parse_error {
    return Err (error); }
  walk_result . map_err ( |e| format! (
    "Walking HEAD tree for repo '{}': {}", skgrepo_name, e )) ?;
  Ok (sections)
}

fn read_skgrepo_from_index (
  config       : &SkgConfig,
  skgrepo_name : &SkgrepoName,
) -> Result<Vec<(SkgrepoName, GraphnodeOnDisk)>, String> {
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
  let index : git2::Index =
    gitrepo . index () . map_err ( |e| format! (
      "Reading index for repo '{}': {}", skgrepo_name, e )) ?;
  let mut sections : Vec<(SkgrepoName, GraphnodeOnDisk)> =
    Vec::new ();
  for entry in index . iter () {
    let rel_path : PathBuf =
      PathBuf::from (String::from_utf8_lossy (&entry . path) . to_string ());
    if ! path_is_skgrepo_skg (&rel_path, &prefix) {
      continue; }
    let blob : git2::Blob =
      gitrepo . find_blob (entry . id) . map_err ( |e| format! (
        "Reading index blob {:?} for repo '{}': {}",
        rel_path, skgrepo_name, e )) ?;
    let node_fs : GraphnodeOnDisk =
      parse_blob_section (blob . content (), &rel_path) ?;
    sections . push (( skgrepo_name . clone (), node_fs )); }
  Ok (sections)
}

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

/// One FILE's contents as a section (pid-checked against the file
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

/// One FILE as a whole node -- the per-blob view the vanished-node
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
