//! Exact low-level reload transaction for specific telescopes.
//!
//! Product requests enter the maintenance coordinator, which builds an
//! immutable candidate before selecting disk.  These primitives retain the
//! focused store-transaction coverage while that higher-level path owns all
//! request parsing, authorization, presentation, and recovery archival.
//!
//! Reload is READ-ONLY with respect to the filesystem: it never writes
//! `.skg` files, so it must not go through `update_graph_minus_nodeMerges`
//! (whose delete-propagation cleanup rewrites other nodes' files). See
//! TODO/partial-reload-and-magit/.

use crate::dbs::filesystem::one_node::{
  parse_nodefs_bytes,
  serialize_telescope_manifest,
  validate_pid_matches_filename,
};
use crate::dbs::filesystem::multiple_nodes::{
  LoadedCorpus,
  distinct_id_claim_conflicts,
  read_all_skg_files_with_manifest,
};
use crate::dbs::filesystem::source_files::{
  SourceFile,
  selected_direct_source_files,
  select_source_file_candidates_for_pid,
  selected_path_digest_manifest,
};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  apply_definenodes_to_inRustGraph,
  override_invariants::error_unless_override_invariants_hold,
};
use crate::save::{
  StoreUpdateOutcome,
  apply_define_nodes_to_stores,
  nodecomplete_from_noderust,
  nodecompletes_from_graph,
};
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  wait_for_tantivy_generation,
};
use crate::serve::handlers::reload_recovery::{
  IncidentDiskSnapshot,
  RecoveryDraft,
  register_incident,
};
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};
use crate::types::store_state::{PathDigest, SelectedPathManifest};
use crate::telescope::fold::fold_telescope;
use crate::telescope::types::Telescope;

use std::collections::{BTreeMap, HashMap, HashSet};
use std::fs;
use std::io;
use std::path::PathBuf;

/// What a single touched telescope resolves to when re-read from disk.
pub enum TelescopeReloadOutcome {
  /// The telescope still folds; upsert it into the stores.
  Save (NodeComplete),
  /// No section of this telescope remains on disk; delete it from the
  /// stores (its inbound references become legal danglers).
  Delete,
  /// The telescope's on-disk state is unloadable (unparseable YAML,
  /// empty/missing pid, pid != filename, or sections-but-no-home). Keep
  /// last-good in the graph; the String is the reason, for the warning.
  Fatal (String),
}

/// One telescope touched by a reload request, with its classification.
pub struct TouchedTelescope {
  pub pid     : ID,
  /// A source under which a touched section of this telescope lived.
  pub source  : SourceName,
  /// One representative touched path (for messages / repo grouping).
  pub path    : PathBuf,
}

struct CapturedTelescope {
  outcome       : TelescopeReloadOutcome,
  /// Every possible direct section path, including explicit absence and
  /// ignored foreign losers.  The final comparison detects a writer racing
  /// either selection or parsing.
  path_bytes    : Vec<(PathBuf, Option<Vec<u8>>)>,
  selected      : Vec<(PathBuf, PathDigest)>,
  warnings      : Vec<String>, }

#[derive(Debug)]
pub struct ReloadStoreOutcome {
  pub message           : String,
  pub applied           : Vec<DefineNode>,
  pub acknowledged_pids : HashSet<ID>,
  pub rejected          : Vec<(ID, String)>,
  pub warnings          : Vec<String>,
  pub recovery_journaled : bool,
  pub recovery          : Option<RecoveryDraft>, }

/// Apply the survivors of a classification to the three derived stores
/// WITHOUT writing the filesystem. Fatal telescopes keep their last-good
/// graph state. Returns a human-readable summary, or `Err` if the store
/// update itself failed.
pub async fn reload_touched_telescopes (
  env     : &mut SkgEnv,
  touched : Vec<TouchedTelescope>,
) -> Result<ReloadStoreOutcome, String> {
  reload_touched_telescopes_for_incident (env, touched, None) . await
}

async fn reload_touched_telescopes_for_incident (
  env         : &mut SkgEnv,
  touched     : Vec<TouchedTelescope>,
  incident_id : Option<&str>,
) -> Result<ReloadStoreOutcome, String> {
  let touched_pids : Vec<ID> = touched . iter ()
    . map (|telescope| telescope . pid . clone ()) . collect ();
  let mut defs : Vec<DefineNode> = Vec::new ();
  let mut fatals : Vec<(ID, String)> = Vec::new ();
  let mut warnings : Vec<String> = Vec::new ();
  let (mut saves, mut deletes) : (usize, usize) = (0, 0);
  // Classification which depends on disk bytes belongs inside the writer
  // transaction.  The pre-lock pass resolves identifiers only.
  let _write_guard =
    crate::write_lock::acquire_graph_write_lock () . await;
  let selected_before = env . in_rust_graph . load_full ();
  let graph_before = selected_before . graph . clone ();
  let resolve = |id : &ID| -> ID {
    graph_before . pid_of (id)
      . unwrap_or_else ( || id . clone ()) };
  let mut manifest : SelectedPathManifest = selected_before . manifest . clone ();
  let mut captured_for_revalidation : Vec<Vec<(PathBuf, Option<Vec<u8>>)>> =
    Vec::new ();
  let mut full_manifest_for_revalidation : Option<SelectedPathManifest> = None;
  for t in touched {
    let captured = capture_telescope (&env . config, &t . pid, &resolve);
    let captured = match captured {
      Ok (captured) => captured,
      Err (error) => CapturedTelescope {
        outcome: TelescopeReloadOutcome::Fatal (error . to_string ()),
        path_bytes: Vec::new (),
        selected: Vec::new (),
        warnings: Vec::new (), }, };
    warnings . extend (captured . warnings . iter () . cloned ());
    match captured . outcome {
      TelescopeReloadOutcome::Save (nc) => {
        saves += 1;
        replace_telescope_manifest (
          &env . config, &t . pid, &captured . selected, &mut manifest);
        captured_for_revalidation . push (captured . path_bytes);
        defs . push ( DefineNode::Save ( SaveNode (nc) )); }
      TelescopeReloadOutcome::Delete => {
        deletes += 1;
        replace_telescope_manifest (
          &env . config, &t . pid, &[], &mut manifest);
        captured_for_revalidation . push (captured . path_bytes);
        defs . push ( DefineNode::Delete ( DeleteNode {
          id     : t . pid,
          source : t . source, } )); }
      TelescopeReloadOutcome::Fatal (reason) =>
        fatals . push (( t . pid, reason )), } }

  let resolver_changed = defs . iter () . any ( |definition| match definition {
    DefineNode::Save (SaveNode (node)) => graph_before . nodes
      . get (&node . pid)
      . map ( |old| old . extra_ids != node . extra_ids)
      . unwrap_or (!node . extra_ids . is_empty ()),
    DefineNode::Delete (DeleteNode { id, .. }) => graph_before . nodes
      . get (id) . map ( |old| !old . extra_ids . is_empty ())
      . unwrap_or (false), });
  if resolver_changed {
    // Extra-ID changes can alter anchors and relationship resolution in
    // untouched telescopes.  The correctness fallback folds the complete
    // normalized corpus, then emits only graph differences.
    let fatal_pids : HashSet<ID> = fatals . iter ()
      . map (|(pid, _)| pid . clone ()) . collect ();
    let (loaded, revalidation_manifest) : (LoadedCorpus, SelectedPathManifest) =
      if fatal_pids . is_empty () {
        let loaded = read_all_skg_files_with_manifest (&env . config)
          . map_err ( |error| format! (
            "full-corpus fallback after extra-ID change failed: {}", error)) ?;
        let revalidation = loaded . manifest . clone ();
        (loaded, revalidation)
      } else {
        read_corpus_substituting_fatals (
          &env . config, &graph_before, &selected_before . manifest,
          &fatal_pids) . map_err (|error| format! (
            "replacement-aware full-corpus fallback after extra-ID change failed: {}",
            error)) ?
      };
    let conflicts = distinct_id_claim_conflicts (&loaded . nodes);
    if !conflicts . is_empty () {
      return Err (format! (
        "reloading would make IDs name multiple nodes: {:?}; stores unchanged",
        conflicts)); }
    let full_graph = InRustGraph::from_nodecompletes (&loaded . nodes);
    defs = graph_delta (&graph_before, &loaded . nodes);
    manifest = loaded . manifest . clone ();
    full_manifest_for_revalidation = Some (revalidation_manifest);
    // Validate below against the already-folded complete graph rather than a
    // second partial simulation.
    if let Err (error) =
      error_unless_override_invariants_hold (&env . config, &full_graph) {
      return Err (format! (
        "full-corpus reload would violate override invariants ({}); \
         stores unchanged", error)); }}

  // Batch guard: applying these to a clone of the live graph must not
  // break override invariants. If it would, reject the whole batch and
  // keep last-good (coarse attribution; see progress.org).
  let mut legal_graph : InRustGraph = (*graph_before) . clone ();
  { apply_definenodes_to_inRustGraph (&mut legal_graph, &defs);
    let conflicts = distinct_id_claim_conflicts (
      &nodecompletes_from_graph (&legal_graph));
    if !conflicts . is_empty () {
      return Err (format! (
        "reloading would make IDs name multiple nodes: {:?}; \
         stores unchanged", conflicts)); }
    if let Err (e) =
      error_unless_override_invariants_hold (&env . config, &legal_graph) {
      return Err ( format! (
        "reloading would violate override invariants ({}); \
          kept last-good state, stores unchanged", e )); } }

  // A fatal telescope keeps its G0 graph state while unrelated legal
  // definitions form G1. Capture the complete incident-time `.skg` overlay
  // before releasing the writer lock; P/L manifests use the same serializer
  // as an ordinary save.
  let mut recovery = if fatals . is_empty () { None } else {
    Some (RecoveryDraft {
      fatal: fatals . clone (),
      touched_pids: touched_pids . clone (),
      pre_manifest: graph_telescope_manifest (
        &graph_before, &touched_pids, &env . config) ?,
      legal_manifest: graph_telescope_manifest (
        &legal_graph, &touched_pids, &env . config) ?,
      disk_snapshot: IncidentDiskSnapshot::capture (
        &env . config, &touched_pids) ?,
    })
  };

  for snapshot in &captured_for_revalidation {
    revalidate_path_bytes (snapshot) . map_err ( |error| format! (
      "reload input changed during its stability check ({}); stores unchanged",
      error)) ?; }
  if let Some (expected) = &full_manifest_for_revalidation {
    let actual = selected_path_digest_manifest (&env . config)
      . map_err ( |error| format! (
        "full-corpus stability check failed ({}); stores unchanged", error)) ?;
    if &actual != expected {
      return Err (
        "full-corpus bytes changed during reload; stores unchanged" . into ()); }}
  if let Some (recovery) = &recovery {
    recovery . disk_snapshot . revalidate (&env . config, &touched_pids)
      . map_err (|error| format! (
        "incident bytes changed during reload ({}); stores unchanged", error)) ?; }

  if defs . is_empty () {
    let recovery_journaled = persist_recovery_before_release (
      &env . config, incident_id, &mut recovery) ?;
    return Ok (ReloadStoreOutcome {
      message: summarize_reload (0, 0, &fatals),
      applied: Vec::new (),
      acknowledged_pids: HashSet::new (),
      rejected: fatals,
      warnings,
      recovery_journaled,
      recovery, }); }

  // Commit to the three stores, filesystem untouched. Keep a copy of the
  // instructions so the caller can re-render the views they touched.
  let applied : Vec<DefineNode> = defs . clone ();
  let acknowledged_pids : HashSet<ID> = defs . iter ()
    . map (|definition| match definition {
      DefineNode::Save (SaveNode (node)) => node . pid . clone (),
      DefineNode::Delete (DeleteNode { id, .. }) => id . clone (), })
    . collect ();
  let config = env . config . clone ();
  let store_outcome : StoreUpdateOutcome = match apply_define_nodes_to_stores (
    defs, &[], config,
    &env . tantivy_index, &env . driver, &env . in_rust_graph,
    false /* write_fs */, Some (manifest), &HashSet::new () ) . await {
    Ok (outcome) => outcome,
    Err (e) => return Err ( format! (
      "store update failed: {}", e )), };
  // The graph/TypeDB generation is now selected and the lock is still held.
  // Make its recovery evidence durable before either releasing the writer or
  // telling the client; this closes the crash window between store commit and
  // journal creation.
  let recovery_journaled = persist_recovery_before_release (
    &env . config, incident_id, &mut recovery) ?;
  drop (_write_guard);
  let index_reconstruction = match wait_for_tantivy_generation (
    store_outcome . tantivy_generation) {
    TantivyGenerationStatus::Committed => None,
    TantivyGenerationStatus::Reconstructed (reason) => Some (reason),
    TantivyGenerationStatus::Failed (reason) => return Err (format! (
      "graph generation {} and TypeDB committed, but Tantivy generation {} \
       failed: {}",
      store_outcome . graph_generation . get (),
      store_outcome . tantivy_generation . get (), reason)),
    TantivyGenerationStatus::Pending => unreachable! (), };
  let mut message = summarize_reload (saves, deletes, &fatals);
  if let Some (reason) = index_reconstruction {
    message . push_str (&format! (
      " WARNING: Tantivy's incremental update failed ({}); Skg reconstructed the complete search index from the committed graph before acknowledging this reload.",
      reason)); }
  Ok (ReloadStoreOutcome {
    message,
    applied,
    acknowledged_pids,
    rejected: fatals,
    warnings,
    recovery_journaled,
    recovery, }) }

fn persist_recovery_before_release (
  config      : &SkgConfig,
  incident_id : Option<&str>,
  recovery    : &mut Option<RecoveryDraft>,
) -> Result<bool, String> {
  let (Some (incident_id), Some (draft)) = (incident_id, recovery . as_ref ())
  else { return Ok (false); };
  register_incident (config, incident_id, draft . clone ())?;
  // Returning `None` tells the request layer that the process-owned durable
  // registry, rather than connection-local state, now owns this evidence.
  *recovery = None;
  Ok (true)
}

fn graph_telescope_manifest (
  graph  : &InRustGraph,
  pids   : &[ID],
  config : &SkgConfig,
) -> Result<BTreeMap<PathBuf, Option<Vec<u8>>>, String> {
  let owned_paths : HashSet<PathBuf> = config . sources . values ()
    . filter (|source| source . user_owns_it)
    . map (|source| source . path . clone ()) . collect ();
  let mut manifest = BTreeMap::new ();
  for pid in pids {
    if let Some (node) = graph . nodes . get (pid) {
      let complete = nodecomplete_from_noderust (node);
      let serialized = serialize_telescope_manifest (&complete, config)
        . map_err (|error| format! (
          "could not serialize recovery state for {}: {}", pid, error)) ?;
      manifest . extend (serialized . into_iter () . filter (|(path, _)|
        path . parent () . map (|parent| owned_paths . contains (parent))
          . unwrap_or (false)));
    } else {
      for source in config . sources . values ()
        . filter (|source| source . user_owns_it)
      {
        manifest . insert (
          source . path . join (format! ("{}.skg", pid)), None); }
    }
  }
  Ok (manifest)
}

fn summarize_reload (
  saves   : usize,
  deletes : usize,
  fatals  : &[(ID, String)],
) -> String {
  let mut msg : String = format! (
    "Reloaded {} telescope(s), removed {}.", saves, deletes );
  if ! fatals . is_empty () {
    msg . push_str ( &format! (
      " {} telescope(s) could not be reloaded and kept their last-good \
       state:", fatals . len () ));
    for (pid, reason) in fatals {
      msg . push_str ( &format! (
        "\n  {}: {}", pid . as_str (), reason )); } }
  msg }

/// Dedup the request's paths to telescope pids (first-seen order) and
/// identify their pids.  This deliberately does not read file contents;
/// byte-dependent classification happens after the writer lock is held.
pub fn classify_touched_telescopes (
  config : &SkgConfig,
  paths  : &[PathBuf],
) -> Vec<TouchedTelescope> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut unique : Vec<(ID, SourceName, PathBuf)> = Vec::new ();
  for path in paths {
    match config . sources . source_and_pid_for_direct_path (path) {
      Some ((source, pid)) => {
        if seen . insert ( pid . clone () ) {
          unique . push (( pid, source, path . clone () )); } }
      None =>
        tracing::warn! (
          path = %path . display (),
          "reload: path is not a .skg file under any source; skipping" ), } }
  unique . into_iter () . map ( |(pid, source, path)|
    TouchedTelescope { pid, source, path } )
    . collect () }

fn capture_telescope (
  config  : &SkgConfig,
  pid     : &ID,
  resolve : &dyn Fn (&ID) -> ID,
) -> io::Result<CapturedTelescope> {
  let mut path_bytes : Vec<(PathBuf, Option<Vec<u8>>)> = Vec::new ();
  let mut candidates : Vec<SourceFile> = Vec::new ();
  for source_name in config . ordered_sources () {
    let source = config . sources . get (&source_name)
      . expect ("ordered source exists");
    let path = source . path . join (format! ("{}.skg", pid));
    match fs::symlink_metadata (&path) {
      Ok (metadata) if metadata . file_type () . is_file () => {
        let bytes = fs::read (&path) ?;
        path_bytes . push ((path . clone (), Some (bytes)));
        candidates . push (SourceFile { source: source_name, path }); },
      Ok (_) => path_bytes . push ((path, None)),
      Err (error) if error . kind () == io::ErrorKind::NotFound =>
        path_bytes . push ((path, None)),
      Err (error) => return Err (error), }}
  let (selected_files, collision) =
    select_source_file_candidates_for_pid (config, pid, candidates);
  let warnings = collision . into_iter () . map (|collision| format! (
    "WARNING: owned telescope {} was retained; ignored same-ID non-owned file(s): {}",
    collision . pid,
    collision . losers . iter ()
      . map (|file| file . path . display () . to_string ())
      . collect::<Vec<_>> () . join (", ")))
    . collect::<Vec<_>> ();
  if selected_files . is_empty () {
    return Ok (CapturedTelescope {
      outcome: TelescopeReloadOutcome::Delete,
      path_bytes,
      selected: Vec::new (),
      warnings, }); }
  let mut sections : Vec<(SourceName, NodeFS)> = Vec::new ();
  let mut selected : Vec<(PathBuf, PathDigest)> = Vec::new ();
  for file in selected_files {
    let bytes = path_bytes . iter ()
      .find ( |(path, _)| path == &file . path )
      .and_then ( |(_, bytes)| bytes . as_ref ())
      .expect ("selected file was captured as present");
    let node_fs = parse_nodefs_bytes (bytes, &file . path) ?;
    validate_pid_matches_filename (&node_fs, &file . path) ?;
    selected . push ((file . path . clone (), PathDigest::of_bytes (bytes)));
    sections . push ((file . source, node_fs)); }
  let telescope = Telescope::try_new (pid . clone (), sections, config)
    . map_err ( |error| io::Error::new (io::ErrorKind::InvalidData, error)) ?;
  let node = fold_telescope (telescope, resolve) ?;
  Ok (CapturedTelescope {
    outcome: TelescopeReloadOutcome::Save (node),
    path_bytes,
    selected,
    warnings, })
}

fn replace_telescope_manifest (
  config  : &SkgConfig,
  pid     : &ID,
  selected : &[(PathBuf, PathDigest)],
  manifest : &mut SelectedPathManifest,
) {
  for source in config . sources . values () {
    manifest . remove (&source . path . join (format! ("{}.skg", pid))); }
  manifest . extend (selected . iter () . cloned ());
}

fn revalidate_path_bytes (
  expected : &[(PathBuf, Option<Vec<u8>>)],
) -> io::Result<()> {
  for (path, expected_bytes) in expected {
    let actual = match fs::symlink_metadata (path) {
      Ok (metadata) if metadata . file_type () . is_file () =>
        Some (fs::read (path) ?),
      Ok (_) => None,
      Err (error) if error . kind () == io::ErrorKind::NotFound => None,
      Err (error) => return Err (error), };
    if &actual != expected_bytes {
      return Err (io::Error::new (
        io::ErrorKind::WouldBlock,
        format! ("{} changed", path . display ()))); }}
  Ok (( ))
}

/// Full-fold all readable selected telescopes while retaining known-fatal
/// PIDs exactly from G0. This is needed only when an extra-ID change can
/// redirect anchors or relationship members in untouched telescopes.
///
/// The two manifests are intentionally different. `selected_manifest` keeps
/// G0 digests for substituted fatal PIDs because their broken disk bytes did
/// not enter G1. `captured_manifest` records those exact broken bytes so the
/// final stability check still detects any race before commit.
fn read_corpus_substituting_fatals (
  config          : &SkgConfig,
  graph_before    : &InRustGraph,
  manifest_before : &SelectedPathManifest,
  fatal_pids      : &HashSet<ID>,
) -> io::Result<(LoadedCorpus, SelectedPathManifest)> {
  let selected = selected_direct_source_files (config)?;
  let mut sections : HashMap<ID, Vec<(SourceName, NodeFS)>> = HashMap::new ();
  let mut selected_manifest = SelectedPathManifest::new ();
  let mut captured_manifest = SelectedPathManifest::new ();
  for pid in &selected . pid_order {
    for file in selected . by_pid . get (pid) . into_iter () . flatten () {
      let bytes = fs::read (&file . path)?;
      let digest = PathDigest::of_bytes (&bytes);
      captured_manifest . insert (file . path . clone (), digest);
      if fatal_pids . contains (pid) { continue; }
      let node_fs = parse_nodefs_bytes (&bytes, &file . path)?;
      validate_pid_matches_filename (&node_fs, &file . path)?;
      selected_manifest . insert (file . path . clone (), digest);
      sections . entry (pid . clone ()) . or_default ()
        . push ((file . source . clone (), node_fs));
    }
  }
  for (path, digest) in manifest_before {
    if config . sources . source_and_pid_for_direct_path (path)
      . map (|(_, pid)| fatal_pids . contains (&pid)) . unwrap_or (false)
    {
      selected_manifest . insert (path . clone (), *digest); }
  }

  let mut fallback_nodes : Vec<NodeComplete> = fatal_pids . iter ()
    . filter_map (|pid| graph_before . nodes . get (pid))
    . map (nodecomplete_from_noderust) . collect ();
  let mut extra_to_pid : HashMap<ID, ID> = HashMap::new ();
  for (pid, telescope_sections) in &sections {
    for (_, node_fs) in telescope_sections {
      for extra in &node_fs . extra_ids {
        extra_to_pid . insert (extra . clone (), pid . clone ()); }
    }
  }
  for node in &fallback_nodes {
    for extra in &node . extra_ids {
      extra_to_pid . insert (extra . clone (), node . pid . clone ()); }
  }
  let resolve = |id : &ID| extra_to_pid . get (id) . cloned ()
    . unwrap_or_else (|| id . clone ());
  let mut nodes = Vec::new ();
  for pid in selected . pid_order . into_iter ()
    . filter (|pid| !fatal_pids . contains (pid))
  {
    let telescope = Telescope::try_new (
      pid . clone (), sections . remove (&pid) . unwrap_or_default (), config)
      . map_err (|error| io::Error::new (io::ErrorKind::InvalidData, error))?;
    nodes . push (fold_telescope (telescope, &resolve)?);
  }
  nodes . append (&mut fallback_nodes);
  nodes . sort_by (|a, b| a . pid . cmp (&b . pid));
  Ok ((LoadedCorpus {
    nodes, violations: Vec::new (), manifest: selected_manifest,
  }, captured_manifest))
}

fn graph_delta (
  before : &InRustGraph,
  after  : &[NodeComplete],
) -> Vec<DefineNode> {
  let after_pids : HashSet<ID> =
    after . iter () . map ( |node| node . pid . clone ()) . collect ();
  let mut definitions : Vec<DefineNode> = after . iter ()
    . filter ( |node| before . nodes . get (&node . pid)
      != Some (&NodeRust::from (*node)))
    .cloned ()
    . map ( |node| DefineNode::Save (SaveNode (node)))
    . collect ();
  let mut deleted : Vec<ID> = before . nodes . keys ()
    .filter ( |pid| !after_pids . contains (*pid))
    .cloned ()
    .collect ();
  deleted . sort ();
  definitions . extend (deleted . into_iter () . filter_map ( |pid| {
    let source = before . nodes . get (&pid) ? . source . clone ();
    Some (DefineNode::Delete (DeleteNode { id: pid, source })) }));
  definitions
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceCatalog};
  use std::path::Path;
  use tempfile::tempdir;

  fn sources (entries : &[(&str, &str)]) -> SourceCatalog {
    let mut catalog : SourceCatalog = SourceCatalog::default ();
    for (name, dir) in entries {
      catalog . insert (
        SourceName::from (*name),
        SkgfileSource {
          name         : SourceName::from (*name),
          abbreviation : None,
          path         : PathBuf::from (dir),
          user_owns_it : true, }); }
    catalog
  }

  #[test]
  fn resolves_path_in_a_source_to_its_stem_and_source () {
    let sources = sources (&[
      ("public", "/data/public"),
      ("private", "/data/private"), ]);
    assert_eq! (
      sources . source_and_pid_for_direct_path (
        Path::new ("/data/private/abc123.skg") ),
      Some (( SourceName::from ("private"),
              ID ("abc123" . to_string ()) )) ); }

  #[test]
  fn non_skg_files_do_not_resolve () {
    let sources = sources (&[("public", "/data/public")]);
    assert_eq! (
      sources . source_and_pid_for_direct_path (
        Path::new ("/data/public/notes.org") ),
      None ); }

  #[test]
  fn paths_outside_every_source_do_not_resolve () {
    let sources = sources (&[("public", "/data/public")]);
    assert_eq! (
      sources . source_and_pid_for_direct_path (
        Path::new ("/elsewhere/abc.skg") ),
      None ); }

  #[test]
  fn nested_files_and_prefix_siblings_do_not_resolve () {
    let sources = sources (&[("public", "/data/public")]);
    assert_eq! (
      sources . source_and_pid_for_direct_path (
        Path::new ("/data/public/nested/abc.skg") ),
      None );
    assert_eq! (
      sources . source_and_pid_for_direct_path (
        Path::new ("/data/publicity/abc.skg") ),
      None ); }

  #[test]
  fn reload_reports_ignored_foreign_collision_without_parsing_loser () {
    let temp = tempdir () . unwrap ();
    let owned = temp . path () . join ("owned");
    let foreign = temp . path () . join ("foreign");
    fs::create_dir_all (&owned) . unwrap ();
    fs::create_dir_all (&foreign) . unwrap ();
    fs::write (owned . join ("A.skg"), "pid: A\ntitle: retained\n") . unwrap ();
    fs::write (foreign . join ("A.skg"), [0xff, 0x00]) . unwrap ();
    let mut entries = HashMap::new ();
    for (name, path, user_owns_it) in [
      ("owned", owned, true), ("foreign", foreign . clone (), false)]
    {
      entries . insert (SourceName::from (name), SkgfileSource {
        name: SourceName::from (name), abbreviation: None,
        path, user_owns_it,
      });
    }
    let mut config = SkgConfig::dummyFromSources (entries);
    config . sources . set_order (vec![
      SourceName::from ("owned"), SourceName::from ("foreign")]);
    let captured = capture_telescope (
      &config, &ID::from ("A"), &|id| id . clone ()) . unwrap ();
    assert! (matches! (captured . outcome, TelescopeReloadOutcome::Save (_)));
    assert_eq! (captured . warnings . len (), 1);
    assert! (captured . warnings[0] . contains (
      &foreign . join ("A.skg") . display () . to_string ()));
  }

  #[test]
  fn full_fold_substitutes_known_fatal_node_but_validates_its_live_bytes () {
    let temp = tempdir () . unwrap ();
    let source = temp . path () . join ("owned");
    fs::create_dir_all (&source) . unwrap ();
    let bad_path = source . join ("bad.skg");
    let good_path = source . join ("good.skg");
    fs::write (&bad_path, b"not: [valid") . unwrap ();
    fs::write (&good_path, b"pid: good\ntitle: good\nextra_ids:\n- alias\n")
      . unwrap ();
    let name = SourceName::from ("owned");
    let mut entries = HashMap::new ();
    entries . insert (name . clone (), SkgfileSource {
      name: name . clone (), abbreviation: None,
      path: source, user_owns_it: true,
    });
    let mut config = SkgConfig::dummyFromSources (entries);
    config . sources . set_order (vec![name . clone ()]);
    let mut old_bad = crate::types::nodes::complete::empty_node_complete ();
    old_bad . pid = ID::from ("bad");
    old_bad . title = "last good" . into ();
    old_bad . source = name;
    let graph = InRustGraph::from_nodecompletes (&[old_bad]);
    let old_digest = PathDigest::of_bytes (b"pid: bad\ntitle: last good\n");
    let manifest_before = SelectedPathManifest::from ([
      (bad_path . clone (), old_digest),
    ]);

    let (loaded, captured) = read_corpus_substituting_fatals (
      &config, &graph, &manifest_before,
      &[ID::from ("bad")] . into_iter () . collect ()) . unwrap ();
    assert_eq! (loaded . nodes . len (), 2);
    assert_eq! (loaded . nodes . iter () . find (|node| node . pid == ID::from ("bad"))
      . unwrap () . title, "last good");
    assert_eq! (loaded . manifest . get (&bad_path), Some (&old_digest));
    assert_eq! (captured . get (&bad_path),
                Some (&PathDigest::of_bytes (b"not: [valid")));
    assert_eq! (loaded . manifest . get (&good_path),
                captured . get (&good_path));
  }
}
