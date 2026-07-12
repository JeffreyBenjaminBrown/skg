use crate::dbs::filesystem::multiple_nodes::{
  fold_one_telescope, read_skg_sections_from_folder};
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
  // Sections arrive in privacy order (ordered_sources) so each
  // telescope folds with its most public section first.
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  for source_name in config . ordered_sources () {
    let label : String =
      format! ("read source '{}' from {:?}", source_name, kind);
    let mut source_sections : Vec<(SourceName, NodeFS)> =
      profile_step_result (&label, || match kind {
        SnapshotKind::Head =>
          read_source_from_head (config, &source_name),
        SnapshotKind::Index =>
          read_source_from_index (config, &source_name),
        SnapshotKind::Worktree =>
          read_skg_sections_from_folder (&source_name, config)
            . map_err ( |e| format! (
              "Reading worktree source '{}': {}", source_name, e )), }) ?;
    sections . append (&mut source_sections); }
  profile_step ("snapshot_from_sections", || {
    snapshot_from_sections (sections) })
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
  let changed_pids : BTreeSet<ID> =
    changed_paths . values () . flatten ()
      . filter_map ( |rel_path| rel_path . file_stem () )
      . map ( |stem| ID::new (stem . to_string_lossy () . to_string ()) )
      . collect ();
  // A changed SECTION re-folds its whole telescope, so read every
  // source's section for each changed pid at the after endpoint.
  let mut sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>> =
    HashMap::new ();
  for pid in &changed_pids {
    sections_by_pid . insert (
      pid . clone (),
      read_telescope_sections_at_endpoint (
        config, after_kind, pid ) ? ); }
  // Anchor resolution needs the whole corpus's extra-id map:
  // unchanged telescopes contribute via their folded nodes, changed
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
  let resolve = |id : &ID| -> ID {
    pid_of . get (id) . cloned ()
      . unwrap_or_else ( || id . clone () ) };
  for pid in &changed_pids {
    let sections : Vec<(SourceName, NodeFS)> =
      sections_by_pid . remove (pid)
      . expect ("changed_pids tracks sections_by_pid");
    let before_node : Option<&NodeComplete> =
      before . nodes . get (pid);
    remove_telescope_claims (&mut after, pid, before_node);
    let after_node : Option<NodeComplete> =
      if sections . is_empty () { None }
      else {
        for (source_name, node_fs) in &sections {
          record_section_claims (
            &mut after . id_claims, node_fs, source_name ); }
        Some ( fold_telescope_tolerating_homelessness (
          pid, sections, &resolve ) ? ) };
    affected_pids . extend (
      affected_pids_for_changed_node (
        before_node, after_node . as_ref () ));
    match after_node {
      Some (node) => { after . nodes . insert (pid . clone (), node); }
      None        => { after . nodes . remove (pid); }} }
  Ok ((after, affected_pids))
}

/// Every source's section file for this pid at the given endpoint,
/// in privacy order. Missing files simply contribute no section.
fn read_telescope_sections_at_endpoint (
  config : &SkgConfig,
  kind   : SnapshotKind,
  pid    : &ID,
) -> Result<Vec<(SourceName, NodeFS)>, String> {
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  for source_name in config . ordered_sources () {
    let source : &SkgfileSource =
      config . sources . get (&source_name) . ok_or_else ( || format! (
        "Source '{}' not found in config", source_name )) ?;
    let source_path : &Path =
      Path::new (&source . path);
    let repo : Repository =
      open_repo (source_path) . ok_or_else ( || format! (
        "Could not open git repo for source '{}'", source_name )) ?;
    let prefix : PathBuf =
      source_prefix_in_repo (&repo, source_path) ?;
    let rel_path : PathBuf =
      prefix . join ( format! ("{}.skg", pid) );
    if let Some (node_fs) =
      read_section_at_endpoint (
        kind, &repo, &source_name, &rel_path ) ? {
      sections . push (( source_name, node_fs )); }}
  Ok (sections)
}

/// Drop the claims a pid's sections contributed at the before
/// endpoint (its claimed ids are exactly the folded node's
/// all_ids). Claims by OTHER pids on the same ids survive.
fn remove_telescope_claims (
  snapshot    : &mut GraphSnapshot,
  pid         : &ID,
  before_node : Option<&NodeComplete>,
) {
  let Some (node) = before_node else { return; };
  for id in node . all_ids () {
    if let Some (by_pid) = snapshot . id_claims . get_mut (id) {
      by_pid . remove (pid);
      if by_pid . is_empty () {
        snapshot . id_claims . remove (id); }} }
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

fn read_section_at_endpoint (
  kind        : SnapshotKind,
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeFS>, String> {
  match kind {
    SnapshotKind::Head =>
      read_section_from_head (repo, source_name, rel_path),
    SnapshotKind::Index =>
      read_section_from_index (repo, source_name, rel_path),
    SnapshotKind::Worktree =>
      read_section_from_worktree (repo, source_name, rel_path), }
}

fn read_section_from_head (
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeFS>, String> {
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
  parse_blob_section (blob . content (), rel_path)
    . map (Some)
}

fn read_section_from_index (
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeFS>, String> {
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
  parse_blob_section (blob . content (), rel_path)
    . map (Some)
}

fn read_section_from_worktree (
  repo        : &Repository,
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<Option<NodeFS>, String> {
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
  if std::env::var_os ("SKG_PROFILE_DIFF_ANALYSIS") . is_none () {
    return; }
  eprintln! (
    "diff-analysis profile: {}: {}.{:03}s",
    label,
    duration . as_secs (),
    duration . subsec_millis ()); }

/// Group sections by pid (sections must arrive in privacy order),
/// fold each telescope, and record every section's id claims.
fn snapshot_from_sections (
  sections : Vec<(SourceName, NodeFS)>,
) -> Result<GraphSnapshot, String> {
  let mut sections_by_pid : HashMap<ID, Vec<(SourceName, NodeFS)>> =
    HashMap::new ();
  let mut id_claims
    : HashMap<ID, BTreeMap<ID, BTreeSet<SourceName>>> =
    HashMap::new ();
  for (source_name, node_fs) in sections {
    record_section_claims (
      &mut id_claims, &node_fs, &source_name );
    sections_by_pid . entry (node_fs . pid . clone ())
      . or_insert_with (Vec::new)
      . push (( source_name, node_fs )); }
  let pid_of : HashMap<ID, ID> = {
    let mut m : HashMap<ID, ID> = HashMap::new ();
    for (pid, sections) in sections_by_pid . iter () {
      for (_, node_fs) in sections {
        for extra in &node_fs . extra_ids {
          m . insert ( extra . clone (), pid . clone () ); }} }
    m };
  let resolve = |id : &ID| -> ID {
    pid_of . get (id) . cloned ()
      . unwrap_or_else ( || id . clone () ) };
  let mut by_pid : HashMap<ID, NodeComplete> =
    HashMap::new ();
  for (pid, telescope_sections) in sections_by_pid {
    let node : NodeComplete =
      fold_telescope_tolerating_homelessness (
        &pid, telescope_sections, &resolve ) ?;
    by_pid . insert (pid, node); }
  Ok ( GraphSnapshot { nodes: by_pid, id_claims } )
}

/// Fold one telescope, but where init would hard-error on a
/// titleless telescope (no home), a snapshot must not: diff
/// endpoints legitimately pass through ill-formed states (e.g. a
/// home-section deletion staged before its recreation). Retry with
/// a placeholder title on the most public section, so the report
/// can still describe the telescope.
fn fold_telescope_tolerating_homelessness (
  pid      : &ID,
  sections : Vec<(SourceName, NodeFS)>,
  resolve  : &dyn Fn (&ID) -> ID,
) -> Result<NodeComplete, String> {
  let retry : Vec<(SourceName, NodeFS)> =
    sections . clone ();
  match fold_one_telescope (pid, sections, resolve) {
    Ok (node) => Ok (node),
    Err (_) => {
      let mut retry : Vec<(SourceName, NodeFS)> = retry;
      match retry . first_mut () {
        Some ((_, node_fs)) =>
          node_fs . title =
            Some ("(no titled section)" . to_string ()),
        None => return Err ( format! (
          "Telescope '{}' has no sections to fold.", pid )), }
      fold_one_telescope (pid, retry, resolve)
        . map_err ( |e| e . to_string () ) }}
}

/// Record one section's id claims: it claims its pid and every
/// extra id it lists, all attributed to (pid, source).
fn record_section_claims (
  id_claims : &mut HashMap<ID, BTreeMap<ID, BTreeSet<SourceName>>>,
  node_fs   : &NodeFS,
  source    : &SourceName,
) {
  for id in std::iter::once (&node_fs . pid)
    . chain (node_fs . extra_ids . iter ()) {
    id_claims . entry (id . clone ())
      . or_insert_with (BTreeMap::new)
      . entry (node_fs . pid . clone ())
      . or_insert_with (BTreeSet::new)
      . insert (source . clone ()); }}

fn read_source_from_head (
  config      : &SkgConfig,
  source_name : &SourceName,
) -> Result<Vec<(SourceName, NodeFS)>, String> {
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
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
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
      . and_then ( |blob| parse_blob_section (
        blob . content (), &rel_path ) ) {
      Ok (node_fs) =>
        sections . push (( source_name . clone (), node_fs )),
      Err (e) => {
        parse_error = Some (e);
        return TreeWalkResult::Abort; } }
    TreeWalkResult::Ok
  });
  if let Some (error) = parse_error {
    return Err (error); }
  walk_result . map_err ( |e| format! (
    "Walking HEAD tree for source '{}': {}", source_name, e )) ?;
  Ok (sections)
}

fn read_source_from_index (
  config      : &SkgConfig,
  source_name : &SourceName,
) -> Result<Vec<(SourceName, NodeFS)>, String> {
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
  let mut sections : Vec<(SourceName, NodeFS)> =
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
    let node_fs : NodeFS =
      parse_blob_section (blob . content (), &rel_path) ?;
    sections . push (( source_name . clone (), node_fs )); }
  Ok (sections)
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

/// One FILE's contents as a section (pid-checked against the file
/// stem). The snapshot folds same-pid sections into one telescope.
pub(super) fn parse_blob_section (
  bytes    : &[u8],
  rel_path : &Path,
) -> Result<NodeFS, String> {
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
  Ok (node_fs)
}

/// One FILE as a whole node -- the per-blob view the vanished-node
/// history search uses (it inspects one historical blob at a time,
/// so there is no telescope to fold).
pub(super) fn parse_blob_node (
  bytes       : &[u8],
  source_name : &SourceName,
  rel_path    : &Path,
) -> Result<NodeComplete, String> {
  parse_blob_section (bytes, rel_path)
    . map ( |node_fs|
            node_fs . into_complete_as_single_section (
              source_name . clone () ) )
}
