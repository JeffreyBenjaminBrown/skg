use crate::dbs::filesystem::multiple_nodes::{
  read_skg_sections_from_folder};
use crate::telescope::compose::compose_telescope;
use crate::telescope::types::{
  Telescope, retain_owned_sections_when_pid_folderlides,
};
use crate::diff_report::types::{
  ChangedGitSnapshotPair, DiffSelection, GraphSnapshot, GitSnapshotKind, GitSnapshotPair};
use crate::git_ops::misc::path_relative_to_gitrepo;
use crate::git_ops::read_gitrepo::{
  get_staged_changed_skg_files, get_unstaged_changed_skg_files,
  head_is_merge_commit, open_gitrepo};
use crate::types::misc::{
  ID, SkgConfig, SkgRepo, SkgRepoName, members_msv, members_of};
use crate::types::nodes::complete::Graphnode;
use crate::types::nodes::fs::GraphnodeOnDisk;
use crate::types::links::links_from_node;

use git2::{ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::collections::hash_map::DefaultHasher;
use std::fs;
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};
use std::str::from_utf8;
use std::sync::{Mutex, OnceLock};
use std::thread;
use std::time::{Duration, Instant};

static SNAPSHOT_CACHE : OnceLock<Mutex<HashMap<String, GraphSnapshot>>> =
  OnceLock::new ();

pub fn read_git_snapshot_pair (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<GitSnapshotPair, String> {
  let (before_kind, after_kind) : (GitSnapshotKind, GitSnapshotKind) =
    endpoint_kinds (selection) ?;
  validate_skgrepos_for_selection (config, before_kind, after_kind) ?;
  let (before_result, after_result) :
    (Result<GraphSnapshot, String>, Result<GraphSnapshot, String>) =
    thread::scope ( |scope| {
      let before_handle : thread::ScopedJoinHandle<'_, Result<GraphSnapshot, String>> =
        scope . spawn ( || profile_step_result (
          "read before graph snapshot", || {
            read_graph_snapshot_maybe_cached (config, before_kind) }) );
      let after_handle : thread::ScopedJoinHandle<'_, Result<GraphSnapshot, String>> =
        scope . spawn ( || profile_step_result (
          "read after graph snapshot", || {
            read_graph_snapshot_maybe_cached (config, after_kind) }) );
      ( before_handle . join () . unwrap_or_else ( |_| Err (
          "Reading before graph snapshot panicked." . to_string () ) ),
        after_handle . join () . unwrap_or_else ( |_| Err (
          "Reading after graph snapshot panicked." . to_string () ) ) ) });
  let before : GraphSnapshot =
    before_result ?;
  let after : GraphSnapshot =
    after_result ?;
  Ok ( GitSnapshotPair { before, after } )
}

pub fn read_changed_git_snapshot_pair (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<Option<ChangedGitSnapshotPair>, String> {
  let (before_kind, after_kind) : (GitSnapshotKind, GitSnapshotKind) =
    endpoint_kinds (selection) ?;
  validate_skgrepos_for_selection (config, before_kind, after_kind) ?;
  let changed_paths : HashMap<SkgRepoName, BTreeSet<PathBuf>> =
    changed_paths_by_skgrepo (config, before_kind, after_kind) ?;
  if changed_paths . values () . all ( |paths| paths . is_empty () ) {
    return Ok (Some ( ChangedGitSnapshotPair {
      pair: GitSnapshotPair {
        before: GraphSnapshot::default (),
        after: GraphSnapshot::default () },
      affected_pids: BTreeSet::new () } )); }
  let before : GraphSnapshot =
    profile_step_result ("read changed-path before graph snapshot", || {
      read_graph_snapshot_maybe_cached (config, before_kind) }) ?;
  let (after, affected_pids) : (GraphSnapshot, BTreeSet<ID>) =
    profile_step_result ("overlay changed after graph snapshot", || {
      overlay_changed_after_git_snapshot (
        config, after_kind, &before, &changed_paths) }) ?;
  Ok (Some ( ChangedGitSnapshotPair {
    pair: GitSnapshotPair { before, after },
    affected_pids } ))
}

fn endpoint_kinds (
  selection : DiffSelection,
) -> Result<(GitSnapshotKind, GitSnapshotKind), String> {
  match (selection . include_staged, selection . include_unstaged) {
    (true,  true)  => Ok ((GitSnapshotKind::Head,  GitSnapshotKind::Worktree)),
    (true,  false) => Ok ((GitSnapshotKind::Head,  GitSnapshotKind::Index)),
    (false, true)  => Ok ((GitSnapshotKind::Index, GitSnapshotKind::Worktree)),
    (false, false) => Err (
      "Diff report must include staged changes, unstaged changes, or both."
        . to_string () ), } }

fn validate_skgrepos_for_selection (
  config      : &SkgConfig,
  before_kind : GitSnapshotKind,
  after_kind  : GitSnapshotKind,
) -> Result<(), String> {
  let needs_head : bool =
    before_kind == GitSnapshotKind::Head ||
    after_kind  == GitSnapshotKind::Head;
  for (skgrepo_name, skgrepo) in &config . skgrepos {
    let skgrepo_path : &Path =
      Path::new ( &skgrepo . path );
    let gitrepo : Repository =
      open_gitrepo (skgrepo_path) . ok_or_else ( || format! (
        "Cannot compute diff report: Skg repo '{}' is not in a git repository.",
        skgrepo_name )) ?;
    gitrepo . head () . map_err ( |e| format! (
      "Cannot compute diff report: Skg repo '{}' has no HEAD commit: {}",
      skgrepo_name, e )) ?;
    if needs_head && head_is_merge_commit (&gitrepo) . map_err ( |e| format! (
      "Cannot compute diff report: could not inspect HEAD for Skg repo '{}': {}",
      skgrepo_name, e )) ? {
      return Err ( format! (
        "Cannot compute diff report: HEAD is a merge commit in repo '{}'.",
        skgrepo_name )); }} 
  Ok (( )) }

fn read_graph_snapshot (
  config : &SkgConfig,
  kind   : GitSnapshotKind,
) -> Result<GraphSnapshot, String> {
  // Sections arrive in privacy order (ordered_repos) so each
  // telescope composes with its most public section first.
  let mut sections : Vec<(SkgRepoName, GraphnodeOnDisk)> = Vec::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let label : String =
      format! ("read repo '{}' from {:?}", skgrepo_name, kind);
    let mut skgrepo_sections : Vec<(SkgRepoName, GraphnodeOnDisk)> =
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

fn read_graph_snapshot_maybe_cached (
  config : &SkgConfig,
  kind   : GitSnapshotKind,
) -> Result<GraphSnapshot, String> {
  let key : Option<String> =
    git_snapshot_cache_key (config, kind) ?;
  let Some (key) = key else {
    return read_graph_snapshot (config, kind); };
  if let Some (git_snapshot) =
    git_snapshot_cache ()
      . lock ()
      . map_err ( |e| format! (
        "Diff report snapshot cache lock failed: {}", e )) ?
      . get (&key)
      . cloned () {
    profile_log ("snapshot cache hit", Duration::from_millis (0));
    return Ok (git_snapshot); }
  profile_log ("snapshot cache miss", Duration::from_millis (0));
  let git_snapshot : GraphSnapshot =
    read_graph_snapshot (config, kind) ?;
  git_snapshot_cache ()
    . lock ()
    . map_err ( |e| format! (
      "Diff report snapshot cache lock failed: {}", e )) ?
    . insert (key, git_snapshot . clone ());
  Ok (git_snapshot)
}

fn git_snapshot_cache (
) -> &'static Mutex<HashMap<String, GraphSnapshot>> {
  SNAPSHOT_CACHE . get_or_init ( || Mutex::new (HashMap::new ()) )
}

fn git_snapshot_cache_key (
  config : &SkgConfig,
  kind   : GitSnapshotKind,
) -> Result<Option<String>, String> {
  if kind == GitSnapshotKind::Worktree {
    return Ok (None); }
  let mut parts : Vec<String> =
    Vec::new ();
  let mut skgrepo_names : Vec<SkgRepoName> =
    config . skgrepos . keys () . cloned () . collect ();
  skgrepo_names . sort ();
  for skgrepo_name in skgrepo_names {
    let skgrepo : &SkgRepo =
      config . skgrepos . get (&skgrepo_name) . ok_or_else ( || format! (
        "Repo '{}' not found in config", skgrepo_name )) ?;
    let skgrepo_path : &Path =
      Path::new (&skgrepo . path);
    let gitrepo : Repository =
      open_gitrepo (skgrepo_path) . ok_or_else ( || format! (
        "Could not open Git repo for Skg repo '{}'", skgrepo_name )) ?;
    let identity : String =
      match kind {
        GitSnapshotKind::Head =>
          head_cache_identity (&gitrepo, &skgrepo_name) ?,
        GitSnapshotKind::Index =>
          index_cache_identity (&gitrepo, &skgrepo_name) ?,
        GitSnapshotKind::Worktree =>
          unreachable! (), };
    parts . push (format! (
      "{}:{}:{}",
      skgrepo_name,
      skgrepo . path . display (),
      identity )); }
  Ok (Some (format! ("{:?}|{}", kind, parts . join ("|"))))
}

fn head_cache_identity (
  gitrepo        : &Repository,
  skgrepo_name   : &SkgRepoName,
) -> Result<String, String> {
  gitrepo . head ()
    . and_then ( |head| head . peel_to_commit () )
    . map ( |commit| format! ("head:{}", commit . id ()) )
    . map_err ( |e| format! (
      "Reading HEAD identity for repo '{}': {}", skgrepo_name, e ))
}

fn index_cache_identity (
  gitrepo        : &Repository,
  skgrepo_name   : &SkgRepoName,
) -> Result<String, String> {
  let index_path : PathBuf =
    gitrepo . path () . join ("index");
  let bytes : Vec<u8> =
    fs::read (&index_path) . map_err ( |e| format! (
      "Reading index identity for repo '{}' at {:?}: {}",
      skgrepo_name, index_path, e )) ?;
  let mut hasher : DefaultHasher =
    DefaultHasher::new ();
  bytes . hash (&mut hasher);
  Ok (format! ("index:{:016x}", hasher . finish ()))
}

fn changed_paths_by_skgrepo (
  config      : &SkgConfig,
  before_kind : GitSnapshotKind,
  after_kind  : GitSnapshotKind,
) -> Result<HashMap<SkgRepoName, BTreeSet<PathBuf>>, String> {
  let mut result : HashMap<SkgRepoName, BTreeSet<PathBuf>> =
    HashMap::new ();
  for (skgrepo_name, skgrepo) in &config . skgrepos {
    let skgrepo_path : &Path =
      Path::new (&skgrepo . path);
    let gitrepo : Repository =
      open_gitrepo (skgrepo_path) . ok_or_else ( || format! (
        "Could not open Git repo for Skg repo '{}'", skgrepo_name )) ?;
    let prefix : PathBuf =
      repo_prefix_in_gitrepo (&gitrepo, skgrepo_path) ?;
    let skgrepo_paths : BTreeSet<PathBuf> =
      changed_paths_for_skgrepo (&gitrepo, &prefix, before_kind, after_kind) ?;
    result . insert (skgrepo_name . clone (), skgrepo_paths); }
  Ok (result)
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

fn overlay_changed_after_git_snapshot (
  config        : &SkgConfig,
  after_kind    : GitSnapshotKind,
  before        : &GraphSnapshot,
  changed_paths : &HashMap<SkgRepoName, BTreeSet<PathBuf>>,
) -> Result<(GraphSnapshot, BTreeSet<ID>), String> {
  let mut after : GraphSnapshot =
    before . clone ();
  let mut affected_pids : BTreeSet<ID> =
    BTreeSet::new ();
  let changed_pids : BTreeSet<ID> =
    changed_paths . values () . flatten ()
      . filter_map ( |rel_path| rel_path . file_stem () )
      . map ( |stem| ID::new (stem . to_string_lossy () . to_string ()) )
      . collect ();
  // A changed SECTION re-folds its whole telescope, so read every
  // skgrepo's section for each changed pid at the after endpoint.
  let mut sections_by_pid : HashMap<ID, Vec<(SkgRepoName, GraphnodeOnDisk)>> =
    HashMap::new ();
  for pid in &changed_pids {
    sections_by_pid . insert (
      pid . clone (),
      read_telescope_sections_at_endpoint (
        config, after_kind, pid ) ? ); }
  // Anchor resolution needs the whole corpus's extra-id map:
  // unchanged telescopes contribute via their composed nodes, changed
  // ones via their fresh sections.
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, node) in after . nodes . iter () {
      if changed_pids . contains (pid) { continue; }
      for extra in &node . extra_ids {
        m . insert ( extra . clone (), pid . clone () ); }}
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |skgid : &ID| -> ID {
    pid_of . get (skgid) . cloned ()
      . unwrap_or_else ( || skgid . clone () ) };
  for pid in &changed_pids {
    let sections : Vec<(SkgRepoName, GraphnodeOnDisk)> =
      sections_by_pid . remove (pid)
      . expect ("changed_pids tracks sections_by_pid");
    let before_node : Option<&Graphnode> =
      before . nodes . get (pid);
    remove_telescope_claims (&mut after, pid, before_node);
    let after_node : Option<Graphnode> =
      if sections . is_empty () { None }
      else {
        for (skgrepo_name, node_fs) in &sections {
          record_section_claims (
            &mut after . id_claims, node_fs, skgrepo_name ); }
        Some ( fold_telescope_tolerating_homelessness (
          config, pid, sections, &resolve ) ? ) };
    affected_pids . extend (
      affected_pids_for_changed_node (
        before_node, after_node . as_ref () ));
    match after_node {
      Some (node) => { after . nodes . insert (pid . clone (), node); }
      None        => { after . nodes . remove (pid); }} }
  Ok ((after, affected_pids))
}

/// Every skgrepo's section file for this pid at the given endpoint,
/// in privacy order. Missing files simply contribute no section.
fn read_telescope_sections_at_endpoint (
  config : &SkgConfig,
  kind   : GitSnapshotKind,
  pid    : &ID,
) -> Result<Vec<(SkgRepoName, GraphnodeOnDisk)>, String> {
  let mut sections : Vec<(SkgRepoName, GraphnodeOnDisk)> = Vec::new ();
  for skgrepo_name in config . ordered_skgrepos () {
    let skgrepo : &SkgRepo =
      config . skgrepos . get (&skgrepo_name) . ok_or_else ( || format! (
        "Repo '{}' not found in config", skgrepo_name )) ?;
    let skgrepo_path : &Path =
      Path::new (&skgrepo . path);
    let gitrepo : Repository =
      open_gitrepo (skgrepo_path) . ok_or_else ( || format! (
        "Could not open Git repo for Skg repo '{}'", skgrepo_name )) ?;
    let prefix : PathBuf =
      repo_prefix_in_gitrepo (&gitrepo, skgrepo_path) ?;
    let rel_path : PathBuf =
      prefix . join ( format! ("{}.skg", pid) );
    if let Some (node_fs) =
      read_section_at_endpoint (
        kind, &gitrepo, &skgrepo_name, &rel_path ) ? {
      sections . push (( skgrepo_name, node_fs )); }}
  let (sections, collision) =
    retain_owned_sections_when_pid_folderlides (sections, config);
  if let Some (collision) = collision {
    tracing::warn! (
      pid = %pid,
      ignored_skgrepos = ?collision . ignored_skgrepos,
      "diff snapshot ignored non-owned files colliding with an owned telescope" ); }
  Ok (sections)
}

/// Drop the claims a pid's sections contributed at the before
/// endpoint (its claimed ids are exactly the composed node's
/// all_ids). Claims by OTHER pids on the same ids survive.
fn remove_telescope_claims (
  git_snapshot    : &mut GraphSnapshot,
  pid         : &ID,
  before_node : Option<&Graphnode>,
) {
  let Some (node) = before_node else { return; };
  for skgid in node . all_skgids () {
    if let Some (by_pid) = git_snapshot . id_claims . get_mut (skgid) {
      by_pid . remove (pid);
      if by_pid . is_empty () {
        git_snapshot . id_claims . remove (skgid); }} }
}

fn affected_pids_for_changed_node (
  before_node : Option<&Graphnode>,
  after_node  : Option<&Graphnode>,
) -> BTreeSet<ID> {
  let mut pids : BTreeSet<ID> =
    BTreeSet::new ();
  for node in before_node . into_iter () . chain (after_node) {
    pids . insert (node . pid . clone ());
    pids . extend (members_of (&node . contains));
    pids . extend (
      members_msv (&node . subscribes_to) . or_default () . iter () . cloned ());
    pids . extend (
      members_msv (&node . hides_from_its_subscriptions)
        . or_default () . iter () . cloned ());
    pids . extend (
      members_msv (&node . overrides_view_of) . or_default () . iter () . cloned ());
    pids . extend (
      links_from_node (node)
        . into_iter ()
        . map ( |link| link . skgid )); }
  pids
}

fn read_section_at_endpoint (
  kind        : GitSnapshotKind,
  gitrepo        : &Repository,
  skgrepo_name : &SkgRepoName,
  rel_path    : &Path,
) -> Result<Option<GraphnodeOnDisk>, String> {
  match kind {
    GitSnapshotKind::Head =>
      read_section_from_head (gitrepo, skgrepo_name, rel_path),
    GitSnapshotKind::Index =>
      read_section_from_index (gitrepo, skgrepo_name, rel_path),
    GitSnapshotKind::Worktree =>
      read_section_from_worktree (gitrepo, skgrepo_name, rel_path), }
}

fn read_section_from_head (
  gitrepo        : &Repository,
  skgrepo_name : &SkgRepoName,
  rel_path    : &Path,
) -> Result<Option<GraphnodeOnDisk>, String> {
  let tree : git2::Tree =
    gitrepo . head ()
      . and_then ( |h| h . peel_to_tree () )
      . map_err ( |e| format! (
        "Reading HEAD tree for repo '{}': {}", skgrepo_name, e )) ?;
  let entry : git2::TreeEntry =
    match tree . get_path (rel_path) {
      Ok (entry) => entry,
      Err (e) if e . code () == git2::ErrorCode::NotFound =>
        return Ok (None),
      Err (e) => return Err ( format! (
        "Reading HEAD path {:?} for repo '{}': {}",
        rel_path, skgrepo_name, e )), };
  if entry . kind () != Some (ObjectType::Blob) {
    return Ok (None); }
  let blob : git2::Blob =
    gitrepo . find_blob (entry . id ()) . map_err ( |e| format! (
      "Reading HEAD blob {:?} for repo '{}': {}",
      rel_path, skgrepo_name, e )) ?;
  parse_blob_section (blob . content (), rel_path)
    . map (Some)
}

fn read_section_from_index (
  gitrepo        : &Repository,
  skgrepo_name : &SkgRepoName,
  rel_path    : &Path,
) -> Result<Option<GraphnodeOnDisk>, String> {
  let index : git2::Index =
    gitrepo . index () . map_err ( |e| format! (
      "Reading index for repo '{}': {}", skgrepo_name, e )) ?;
  let skgid : git2::Oid =
    match index . get_path (rel_path, 0) {
      Some (entry) => entry . id,
      None => return Ok (None), };
  let blob : git2::Blob =
    gitrepo . find_blob (skgid) . map_err ( |e| format! (
      "Reading index blob {:?} for repo '{}': {}",
      rel_path, skgrepo_name, e )) ?;
  parse_blob_section (blob . content (), rel_path)
    . map (Some)
}

fn read_section_from_worktree (
  gitrepo        : &Repository,
  skgrepo_name : &SkgRepoName,
  rel_path    : &Path,
) -> Result<Option<GraphnodeOnDisk>, String> {
  let workdir : &Path =
    gitrepo . workdir () . ok_or_else ( || format! (
      "Repository for repo '{}' has no workdir", skgrepo_name )) ?;
  let abs_path : PathBuf =
    workdir . join (rel_path);
  if ! abs_path . exists () {
    return Ok (None); }
  let bytes : Vec<u8> =
    fs::read (&abs_path) . map_err ( |e| format! (
      "Reading worktree path {:?} for repo '{}': {}",
      abs_path, skgrepo_name, e )) ?;
  parse_blob_section (&bytes, rel_path)
    . map (Some)
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
  if std::env::var_os ("SKG_PROFILE_DIFF_REPORT") . is_none () {
    return; }
  eprintln! (
    "diff-report profile: {}: {}.{:03}s",
    label,
    duration . as_secs (),
    duration . subsec_millis ()); }

/// Group sections by pid (sections must arrive in privacy order),
/// normalize owned/non-owned pid collisions, compose each telescope,
/// and record the retained sections' id claims.
fn git_snapshot_from_sections (
  config   : &SkgConfig,
  sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
) -> Result<GraphSnapshot, String> {
  let mut sections_by_pid : HashMap<ID, Vec<(SkgRepoName, GraphnodeOnDisk)>> =
    HashMap::new ();
  for (skgrepo_name, node_fs) in sections {
    sections_by_pid . entry (node_fs . pid . clone ())
      . or_insert_with (Vec::new)
      . push (( skgrepo_name, node_fs )); }
  for (pid, telescope_sections) in &mut sections_by_pid {
    let sections : Vec<(SkgRepoName, GraphnodeOnDisk)> =
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
    : HashMap<ID, BTreeMap<ID, BTreeSet<SkgRepoName>>> =
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
  sections : Vec<(SkgRepoName, GraphnodeOnDisk)>,
  resolve  : &dyn Fn (&ID) -> ID,
) -> Result<Graphnode, String> {
  let retry : Vec<(SkgRepoName, GraphnodeOnDisk)> =
    sections . clone ();
  let telescope : Telescope =
    Telescope::try_new ( pid . clone (), sections, config )
    . map_err ( |e| e . to_string () ) ?;
  match compose_telescope ( telescope, resolve )
  { Ok (node) => Ok (node),
    Err (_) => {
      let mut retry : Vec<(SkgRepoName, GraphnodeOnDisk)> = retry;
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
  id_claims : &mut HashMap<ID, BTreeMap<ID, BTreeSet<SkgRepoName>>>,
  node_fs   : &GraphnodeOnDisk,
  skgrepo   : &SkgRepoName,
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
  skgrepo_name : &SkgRepoName,
) -> Result<Vec<(SkgRepoName, GraphnodeOnDisk)>, String> {
  let skgrepo : &SkgRepo =
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
  let mut sections : Vec<(SkgRepoName, GraphnodeOnDisk)> = Vec::new ();
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
  skgrepo_name : &SkgRepoName,
) -> Result<Vec<(SkgRepoName, GraphnodeOnDisk)>, String> {
  let skgrepo : &SkgRepo =
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
  let mut sections : Vec<(SkgRepoName, GraphnodeOnDisk)> =
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

pub(super) fn repo_prefix_in_gitrepo (
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

pub(super) fn path_is_skgrepo_skg (
  rel_path : &Path,
  prefix   : &Path,
) -> bool {
  rel_path . starts_with (prefix) &&
    rel_path . extension () . map_or (
      false, |ext| ext == "skg" )
}

/// One FILE's contents as a section (pid-checked against the file
/// stem). The snapshot composes same-pid sections into one telescope.
pub(super) fn parse_blob_section (
  bytes    : &[u8],
  rel_path : &Path,
) -> Result<GraphnodeOnDisk, String> {
  let yaml : &str =
    from_utf8 (bytes) . map_err ( |e| format! (
      "Blob {:?} is not UTF-8: {}", rel_path, e )) ?;
  let node_fs : GraphnodeOnDisk =
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
  Ok (node_fs)
}

/// One FILE as a whole node -- the per-blob view the vanished-node
/// history search uses (it inspects one historical blob at a time,
/// so there is no telescope to compose).
pub(super) fn parse_blob_node (
  bytes        : &[u8],
  skgrepo_name : &SkgRepoName,
  rel_path     : &Path,
) -> Result<Graphnode, String> {
  parse_blob_section (bytes, rel_path)
    . map ( |node_fs|
            node_fs . into_complete_as_single_section (
              skgrepo_name . clone () ) )
}
