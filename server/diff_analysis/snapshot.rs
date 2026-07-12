use crate::dbs::filesystem::multiple_nodes::read_single_sections_from_folder;
use crate::diff_analysis::types::{
  ChangedSnapshotPair, DiffSelection, GraphSnapshot, SnapshotKind, SnapshotPair};
use crate::git_ops::misc::path_relative_to_repo;
use crate::git_ops::read_repo::{
  get_staged_changed_skg_files, get_unstaged_changed_skg_files,
  head_is_merge_commit, open_repo};
use crate::types::misc::{
  ID, SkgConfig, SkgfileSource, SourceName, members_msv, members_of};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::fs::NodeFS;
use crate::types::textlinks::textlinks_from_node;

use git2::{ObjectType, Repository, TreeWalkMode, TreeWalkResult};
use std::collections::{BTreeSet, HashMap};
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

pub fn read_snapshot_pair (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<SnapshotPair, String> {
  let (before_kind, after_kind) : (SnapshotKind, SnapshotKind) =
    endpoint_kinds (selection) ?;
  validate_sources_for_selection (config, before_kind, after_kind) ?;
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
  Ok ( SnapshotPair { before, after } )
}

pub fn read_changed_snapshot_pair (
  config    : &SkgConfig,
  selection : DiffSelection,
) -> Result<Option<ChangedSnapshotPair>, String> {
  let (before_kind, after_kind) : (SnapshotKind, SnapshotKind) =
    endpoint_kinds (selection) ?;
  validate_sources_for_selection (config, before_kind, after_kind) ?;
  let changed_paths : HashMap<SourceName, BTreeSet<PathBuf>> =
    changed_paths_by_source (config, before_kind, after_kind) ?;
  if changed_paths . values () . all ( |paths| paths . is_empty () ) {
    return Ok (Some ( ChangedSnapshotPair {
      pair: SnapshotPair {
        before: GraphSnapshot::default (),
        after: GraphSnapshot::default () },
      affected_pids: BTreeSet::new () } )); }
  let before : GraphSnapshot =
    profile_step_result ("read changed-path before graph snapshot", || {
      read_graph_snapshot_maybe_cached (config, before_kind) }) ?;
  let (after, affected_pids) : (GraphSnapshot, BTreeSet<ID>) =
    profile_step_result ("overlay changed after graph snapshot", || {
      overlay_changed_after_snapshot (
        config, after_kind, &before, &changed_paths) }) ?;
  Ok (Some ( ChangedSnapshotPair {
    pair: SnapshotPair { before, after },
    affected_pids } ))
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
    let label : String =
      format! ("read source '{}' from {:?}", source_name, kind);
    let mut source_nodes : Vec<NodeComplete> =
      profile_step_result (&label, || match kind {
        SnapshotKind::Head =>
          read_source_from_head (config, source_name),
        SnapshotKind::Index =>
          read_source_from_index (config, source_name),
        SnapshotKind::Worktree =>
          read_single_sections_from_folder (source_name, config)
            . map_err ( |e| format! (
              "Reading worktree source '{}': {}", source_name, e )), }) ?;
    nodes . append (&mut source_nodes); }
  Ok (profile_step ("snapshot_from_nodes", || {
    snapshot_from_nodes (nodes) }))
}

fn read_graph_snapshot_maybe_cached (
  config : &SkgConfig,
  kind   : SnapshotKind,
) -> Result<GraphSnapshot, String> {
  let key : Option<String> =
    snapshot_cache_key (config, kind) ?;
  let Some (key) = key else {
    return read_graph_snapshot (config, kind); };
  if let Some (snapshot) =
    snapshot_cache ()
      . lock ()
      . map_err ( |e| format! (
        "Diff analysis snapshot cache lock failed: {}", e )) ?
      . get (&key)
      . cloned () {
    profile_log ("snapshot cache hit", Duration::from_millis (0));
    return Ok (snapshot); }
  profile_log ("snapshot cache miss", Duration::from_millis (0));
  let snapshot : GraphSnapshot =
    read_graph_snapshot (config, kind) ?;
  snapshot_cache ()
    . lock ()
    . map_err ( |e| format! (
      "Diff analysis snapshot cache lock failed: {}", e )) ?
    . insert (key, snapshot . clone ());
  Ok (snapshot)
}

fn snapshot_cache (
) -> &'static Mutex<HashMap<String, GraphSnapshot>> {
  SNAPSHOT_CACHE . get_or_init ( || Mutex::new (HashMap::new ()) )
}

fn snapshot_cache_key (
  config : &SkgConfig,
  kind   : SnapshotKind,
) -> Result<Option<String>, String> {
  if kind == SnapshotKind::Worktree {
    return Ok (None); }
  let mut parts : Vec<String> =
    Vec::new ();
  let mut source_names : Vec<SourceName> =
    config . sources . keys () . cloned () . collect ();
  source_names . sort ();
  for source_name in source_names {
    let source : &SkgfileSource =
      config . sources . get (&source_name) . ok_or_else ( || format! (
        "Source '{}' not found in config", source_name )) ?;
    let source_path : &Path =
      Path::new (&source . path);
    let repo : Repository =
      open_repo (source_path) . ok_or_else ( || format! (
        "Could not open git repo for source '{}'", source_name )) ?;
    let identity : String =
      match kind {
        SnapshotKind::Head =>
          head_cache_identity (&repo, &source_name) ?,
        SnapshotKind::Index =>
          index_cache_identity (&repo, &source_name) ?,
        SnapshotKind::Worktree =>
          unreachable! (), };
    parts . push (format! (
      "{}:{}:{}",
      source_name,
      source . path . display (),
      identity )); }
  Ok (Some (format! ("{:?}|{}", kind, parts . join ("|"))))
}

fn head_cache_identity (
  repo        : &Repository,
  source_name : &SourceName,
) -> Result<String, String> {
  repo . head ()
    . and_then ( |head| head . peel_to_commit () )
    . map ( |commit| format! ("head:{}", commit . id ()) )
    . map_err ( |e| format! (
      "Reading HEAD identity for source '{}': {}", source_name, e ))
}

fn index_cache_identity (
  repo        : &Repository,
  source_name : &SourceName,
) -> Result<String, String> {
  let index_path : PathBuf =
    repo . path () . join ("index");
  let bytes : Vec<u8> =
    fs::read (&index_path) . map_err ( |e| format! (
      "Reading index identity for source '{}' at {:?}: {}",
      source_name, index_path, e )) ?;
  let mut hasher : DefaultHasher =
    DefaultHasher::new ();
  bytes . hash (&mut hasher);
  Ok (format! ("index:{:016x}", hasher . finish ()))
}

fn changed_paths_by_source (
  config      : &SkgConfig,
  before_kind : SnapshotKind,
  after_kind  : SnapshotKind,
) -> Result<HashMap<SourceName, BTreeSet<PathBuf>>, String> {
  let mut result : HashMap<SourceName, BTreeSet<PathBuf>> =
    HashMap::new ();
  for (source_name, source) in &config . sources {
    let source_path : &Path =
      Path::new (&source . path);
    let repo : Repository =
      open_repo (source_path) . ok_or_else ( || format! (
        "Could not open git repo for source '{}'", source_name )) ?;
    let prefix : PathBuf =
      source_prefix_in_repo (&repo, source_path) ?;
    let source_paths : BTreeSet<PathBuf> =
      changed_paths_for_source (&repo, &prefix, before_kind, after_kind) ?;
    result . insert (source_name . clone (), source_paths); }
  Ok (result)
}

fn changed_paths_for_source (
  repo        : &Repository,
  prefix      : &Path,
  before_kind : SnapshotKind,
  after_kind  : SnapshotKind,
) -> Result<BTreeSet<PathBuf>, String> {
  let mut paths : BTreeSet<PathBuf> =
    BTreeSet::new ();
  match (before_kind, after_kind) {
    (SnapshotKind::Head, SnapshotKind::Index) => {
      for entry in get_staged_changed_skg_files (repo)
        . map_err ( |e| format! (
          "Reading staged changed .skg files: {}", e )) ? {
        if path_is_source_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    (SnapshotKind::Index, SnapshotKind::Worktree) => {
      for entry in get_unstaged_changed_skg_files (repo)
        . map_err ( |e| format! (
          "Reading unstaged changed .skg files: {}", e )) ? {
        if path_is_source_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    (SnapshotKind::Head, SnapshotKind::Worktree) => {
      for entry in get_staged_changed_skg_files (repo)
        . map_err ( |e| format! (
          "Reading staged changed .skg files: {}", e )) ?
        . into_iter ()
        . chain (get_unstaged_changed_skg_files (repo)
          . map_err ( |e| format! (
            "Reading unstaged changed .skg files: {}", e )) ?
          . into_iter ()) {
        if path_is_source_skg (&entry . path, prefix) {
          paths . insert (entry . path); }}}
    _ => return Err ( format! (
      "Unsupported diff-analysis endpoints: {:?} to {:?}",
      before_kind, after_kind )), }
  Ok (paths)
}

fn overlay_changed_after_snapshot (
  config        : &SkgConfig,
  after_kind    : SnapshotKind,
  before        : &GraphSnapshot,
  changed_paths : &HashMap<SourceName, BTreeSet<PathBuf>>,
) -> Result<(GraphSnapshot, BTreeSet<ID>), String> {
  let mut after : GraphSnapshot =
    before . clone ();
  let mut affected_pids : BTreeSet<ID> =
    BTreeSet::new ();
  for (source_name, paths) in changed_paths {
    let source : &SkgfileSource =
      config . sources . get (source_name) . ok_or_else ( || format! (
        "Source '{}' not found in config", source_name )) ?;
    let source_path : &Path =
      Path::new (&source . path);
    let repo : Repository =
      open_repo (source_path) . ok_or_else ( || format! (
        "Could not open git repo for source '{}'", source_name )) ?;
    for rel_path in paths {
      let before_pid : Option<ID> =
        rel_path . file_stem ()
          . map ( |stem| ID::new (stem . to_string_lossy () . to_string ()) );
      let before_node : Option<NodeComplete> =
        before_pid . as_ref ()
          . and_then ( |pid| before . nodes . get (pid) )
          . filter ( |node| &node . source == source_name )
          . cloned ();
      let after_node : Option<NodeComplete> =
        read_node_at_endpoint (
          after_kind, &repo, source_path, source_name, rel_path) ?;
      affected_pids . extend (
        affected_pids_for_changed_node (
          before_node . as_ref (), after_node . as_ref () ));
      if let Some (node) = &before_node {
        remove_node_ids_from_snapshot (&mut after, node); }
      if let Some (pid) = before_pid {
        remove_node_from_snapshot (&mut after, &pid, source_name); }
      if let Some (node) = after_node {
        insert_node_into_snapshot (&mut after, node); }}}
  Ok ((after, affected_pids))
}

fn affected_pids_for_changed_node (
  before_node : Option<&NodeComplete>,
  after_node  : Option<&NodeComplete>,
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
      textlinks_from_node (node)
        . into_iter ()
        . map ( |textlink| textlink . id )); }
  pids
}

fn read_node_at_endpoint (
  kind        : SnapshotKind,
  repo        : &Repository,
  source_path : &Path,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeComplete>, String> {
  match kind {
    SnapshotKind::Head =>
      read_node_from_head (repo, source_name, rel_path),
    SnapshotKind::Index =>
      read_node_from_index (repo, source_name, rel_path),
    SnapshotKind::Worktree =>
      read_node_from_worktree (repo, source_path, source_name, rel_path), }
}

fn read_node_from_head (
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeComplete>, String> {
  let tree : git2::Tree =
    repo . head ()
      . and_then ( |h| h . peel_to_tree () )
      . map_err ( |e| format! (
        "Reading HEAD tree for source '{}': {}", source_name, e )) ?;
  let entry : git2::TreeEntry =
    match tree . get_path (rel_path) {
      Ok (entry) => entry,
      Err (e) if e . code () == git2::ErrorCode::NotFound =>
        return Ok (None),
      Err (e) => return Err ( format! (
        "Reading HEAD path {:?} for source '{}': {}",
        rel_path, source_name, e )), };
  if entry . kind () != Some (ObjectType::Blob) {
    return Ok (None); }
  let blob : git2::Blob =
    repo . find_blob (entry . id ()) . map_err ( |e| format! (
      "Reading HEAD blob {:?} for source '{}': {}",
      rel_path, source_name, e )) ?;
  parse_blob_node (blob . content (), source_name, rel_path)
    . map (Some)
}

fn read_node_from_index (
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeComplete>, String> {
  let index : git2::Index =
    repo . index () . map_err ( |e| format! (
      "Reading index for source '{}': {}", source_name, e )) ?;
  let id : git2::Oid =
    match index . get_path (rel_path, 0) {
      Some (entry) => entry . id,
      None => return Ok (None), };
  let blob : git2::Blob =
    repo . find_blob (id) . map_err ( |e| format! (
      "Reading index blob {:?} for source '{}': {}",
      rel_path, source_name, e )) ?;
  parse_blob_node (blob . content (), source_name, rel_path)
    . map (Some)
}

fn read_node_from_worktree (
  repo        : &Repository,
  _source_path : &Path,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeComplete>, String> {
  let workdir : &Path =
    repo . workdir () . ok_or_else ( || format! (
      "Repository for source '{}' has no workdir", source_name )) ?;
  let abs_path : PathBuf =
    workdir . join (rel_path);
  if ! abs_path . exists () {
    return Ok (None); }
  let bytes : Vec<u8> =
    fs::read (&abs_path) . map_err ( |e| format! (
      "Reading worktree path {:?} for source '{}': {}",
      abs_path, source_name, e )) ?;
  parse_blob_node (&bytes, source_name, rel_path)
    . map (Some)
}

fn remove_node_from_snapshot (
  snapshot : &mut GraphSnapshot,
  pid      : &ID,
  source   : &SourceName,
) {
  let should_remove : bool =
    snapshot . nodes . get (pid)
      . map ( |node| &node . source == source )
      . unwrap_or (false);
  if ! should_remove {
    return; }
  if let Some (node) = snapshot . nodes . remove (pid) {
    for id in node . all_ids () {
      if let Some (sources) = snapshot . id_sources . get_mut (id) {
        sources . remove (&node . source);
        if sources . is_empty () {
          snapshot . id_sources . remove (id); }}}}
}

fn remove_node_ids_from_snapshot (
  snapshot : &mut GraphSnapshot,
  node     : &NodeComplete,
) {
  for id in node . all_ids () {
    if let Some (sources) = snapshot . id_sources . get_mut (id) {
      sources . remove (&node . source);
      if sources . is_empty () {
        snapshot . id_sources . remove (id); }}}}

fn insert_node_into_snapshot (
  snapshot : &mut GraphSnapshot,
  node     : NodeComplete,
) {
  for id in node . all_ids () {
    snapshot . id_sources . entry (id . clone ())
      . or_insert_with (BTreeSet::new)
      . insert (node . source . clone ()); }
  snapshot . nodes . insert (node . pid . clone (), node);
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
  if std::env::var_os ("SKG_PROFILE_DIFF_ANALYSIS") . is_none () {
    return; }
  eprintln! (
    "diff-analysis profile: {}: {}.{:03}s",
    label,
    duration . as_secs (),
    duration . subsec_millis ()); }

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

pub(super) fn source_prefix_in_repo (
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

pub(super) fn path_is_source_skg (
  rel_path : &Path,
  prefix   : &Path,
) -> bool {
  rel_path . starts_with (prefix) &&
    rel_path . extension () . map_or (
      false, |ext| ext == "skg" )
}

pub(super) fn parse_blob_node (
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
  Ok ( node_fs . into_complete_as_single_section (source_name . clone ()) )
}
