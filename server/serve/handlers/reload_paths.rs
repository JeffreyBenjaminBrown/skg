//! Partial reload of specific telescopes from disk.
//!
//! When a `.skg` worktree file changes out of band (a magit discard is
//! the motivating case), the client sends the affected paths and the
//! server re-reads those telescopes from disk and updates the three
//! derived stores to match -- a *partial* reload, not a full rebuild.
//!
//! Reload is READ-ONLY with respect to the filesystem: it never writes
//! `.skg` files, so it must not go through `update_graph_minus_nodeMerges`
//! (whose delete-propagation cleanup rewrites other nodes' files). See
//! TODO/partial-reload-and-magit/.

use crate::dbs::filesystem::one_node::{
  parse_nodefs_bytes,
  validate_pid_matches_filename,
};
use crate::dbs::filesystem::multiple_nodes::{
  LoadedCorpus,
  distinct_id_claim_conflicts,
  read_all_skg_files_with_manifest,
};
use crate::dbs::filesystem::source_files::{
  SourceFile,
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
  nodecompletes_from_graph,
};
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  wait_for_tantivy_generation,
};
use crate::serve::ViewsState;
use crate::serve::handlers::reload_batch::reload_batch_active;
use crate::serve::handlers::scalar_release::approved_pids_from_request;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_terminal_sexp_response,
  tag_terminal_text_response,
  value_from_request_sexp,
};
use crate::source_sets::ActiveSourceSet;
use crate::sound::play_harsh_sound_in_background;
use crate::update_buffer::{
  ReloadPresentation,
  ReloadRerenderOutcome,
  ReloadViewImpact,
  rerender_views_after_reload,
};
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SourceCatalog, SourceName};
use crate::types::nodes::complete::NodeComplete;
use crate::types::nodes::fs::NodeFS;
use crate::types::nodes::rust::NodeRust;
use crate::types::save::{DefineNode, DeleteNode, SaveNode};
use crate::types::sexp::extract_string_list_from_sexp;
use crate::types::store_state::{PathDigest, SelectedPathManifest};
use crate::types::views_state::ViewUri;
use crate::telescope::fold::fold_telescope;
use crate::telescope::types::Telescope;

use futures::executor::block_on;
use std::cell::RefCell;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::fs;
use std::io;
use std::net::TcpStream;
use std::path::PathBuf;
use sexp::{Atom, Sexp};

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
  selected      : Vec<(PathBuf, PathDigest)>, }

#[derive(Debug)]
pub struct ReloadStoreOutcome {
  pub message           : String,
  pub applied           : Vec<DefineNode>,
  pub acknowledged_pids : HashSet<ID>,
  pub rejected          : Vec<(ID, String)>, }

#[derive(Clone)]
struct RequestedIdOutcome {
  requested_id : ID,
  pid          : Option<ID>,
  status       : &'static str,
  reason       : Option<String>,
  paths        : Vec<PathBuf>, }

#[derive(Clone)]
pub(crate) struct PendingReloadPresentation {
  applied            : Vec<DefineNode>,
  message            : String,
  requested_outcomes : Vec<RequestedIdOutcome>,
  affected_paths     : Vec<PathBuf>,
  any_rejected       : bool,
}

thread_local! {
  /// Presentation authority is connection-local because each connection
  /// thread owns one independent `ViewsState`.  Keeping it here avoids
  /// burdening that public struct (which integration fixtures construct), and
  /// the thread exit drops every declined or abandoned incident.
  static PENDING_RELOAD_PRESENTATIONS :
    RefCell<HashMap<String, PendingReloadPresentation>> =
      RefCell::new (HashMap::new ());
}

fn take_pending_reload (incident : &str) -> Option<PendingReloadPresentation> {
  PENDING_RELOAD_PRESENTATIONS . with (|pending|
    pending . borrow_mut () . remove (incident))
}

fn retain_pending_reload (
  incident : &str,
  pending  : PendingReloadPresentation,
) {
  PENDING_RELOAD_PRESENTATIONS . with (|registry|
    registry . borrow_mut () . insert (incident . to_string (), pending));
}

pub fn handle_reload_paths_request (
  stream            : &mut TcpStream,
  request           : &str,
  env               : &mut SkgEnv,
  views_state       : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let parsed = match sexp::parse (request) {
    Ok (s) => s,
    Err (e) => {
      send_reload_error (stream, &format! (
        "reload paths: failed to parse request: {}", e ));
      return; } };
  let dirty_uris : HashSet<ViewUri> =
    match optional_string_list (&parsed, "dirty-view-uris") {
      Ok (values) => values . into_iter ()
        . map (ViewUri::from_client_string) . collect (),
      Err (error) => { send_reload_error (stream, &error); return; }};
  let incident_id = value_from_request_sexp ("incident-id", request) . ok ();
  let scalar_approved_pids = approved_pids_from_request (request);
  if let Some (incident) = incident_id . as_ref () {
    if let Some (pending) = take_pending_reload (incident) {
      present_committed_reload (
        stream, env, views_state, active_source_set,
        incident, pending, &dirty_uris, &scalar_approved_pids );
      return; }}
  let path_strings : Vec<String> = match optional_string_list (&parsed, "paths") {
    Ok (values) => values,
    Err (error) => { send_reload_error (stream, &error); return; }};
  let requested_ids : Vec<ID> = match optional_string_list (&parsed, "ids") {
    Ok (values) => values . into_iter () . map (ID::from) . collect (),
    Err (error) => { send_reload_error (stream, &error); return; }};
  let full_sweep = value_from_request_sexp ("full-sweep", request)
    . map (|value| value == "true") . unwrap_or (false);
  if path_strings . is_empty () && requested_ids . is_empty () && !full_sweep {
    send_reload_error (stream, "reload paths: no paths or IDs were supplied");
    return; }
  if reload_batch_active () {
    let payload = Sexp::List (vec![
      Sexp::List (vec![
        Sexp::Atom (Atom::S ("content" . into ())),
        Sexp::Atom (Atom::S (
          "Reload deferred while an external reload batch is active" . into ())),
      ]),
      Sexp::List (vec![
        Sexp::Atom (Atom::S ("deferred" . into ())),
        Sexp::Atom (Atom::S ("true" . into ())),
      ]),
    ]) . to_string ();
    send_response_with_length_prefix (
      stream, &tag_terminal_sexp_response (
        TcpToClient::ReloadPaths, "complete", &payload));
    return; }
  let mut paths : Vec<PathBuf> =
    path_strings . into_iter () . map (PathBuf::from) . collect ();
  if full_sweep {
    match changed_paths_since_selected (env) {
      Ok (changed) => paths . extend (changed),
      Err (error) => {
        send_reload_error (stream, &format! (
          "reload full sweep failed: {}", error));
        return; }} }
  paths . sort ();
  paths . dedup ();
  let mut touched : Vec<TouchedTelescope> =
    classify_touched_telescopes (&env . config, &paths);
  let mut seen : HashSet<ID> = touched . iter ()
    . map (|item| item . pid . clone ()) . collect ();
  let graph = env . in_rust_graph_snapshot ();
  let mut requested : Vec<(ID, Option<ID>)> = Vec::new ();
  for requested_id in requested_ids {
    let pid = graph . pid_of (&requested_id);
    if let Some (pid) = &pid {
      if seen . insert (pid . clone ()) {
        let node = graph . nodes . get (pid)
          . expect ("resolved primary pid exists");
        let source = node . source . clone ();
        let path = env . config . sources . get (&source)
          . expect ("graph source remains configured")
          . path . join (format! ("{}.skg", pid));
        touched . push (TouchedTelescope {
          pid: pid . clone (), source, path }); }}
    requested . push ((requested_id, pid)); }
  let store_outcome : ReloadStoreOutcome =
    match block_on ( reload_touched_telescopes (env, touched) ) {
      Ok (outcome) => outcome,
      Err (e)   => {
        send_reload_error (stream, &format! ("Reload failed: {}", e));
        return; } };
  let requested_outcomes = requested . into_iter () . map (
    |(requested_id, pid)| {
      let paths = pid . as_ref () . map (|pid| possible_paths (
        &env . config, pid)) . unwrap_or_default ();
      match pid {
        None => RequestedIdOutcome {
          requested_id, pid: None, status: "rejected",
          reason: Some ("ID is not present in the selected graph" . into ()),
          paths, },
        Some (pid) => match store_outcome . rejected . iter ()
            . find (|(rejected, _)| rejected == &pid) {
          Some ((_, reason)) => RequestedIdOutcome {
            requested_id, pid: Some (pid), status: "rejected",
            reason: Some (reason . clone ()), paths, },
          None if store_outcome . acknowledged_pids . contains (&pid) =>
            RequestedIdOutcome {
            requested_id, pid: Some (pid), status: "acknowledged",
            reason: None, paths, },
          None => RequestedIdOutcome {
            requested_id, pid: Some (pid), status: "rejected",
            reason: Some ("telescope was not acknowledged" . into ()),
            paths, }, }}})
    . collect::<Vec<_>> ();
  let any_rejected = requested_outcomes . iter ()
    . any (|outcome| outcome . status == "rejected")
    || ! store_outcome . rejected . is_empty ();
  let mut affected_paths = paths;
  for outcome in &requested_outcomes {
    if outcome . pid . is_some () {
      affected_paths . extend (outcome . paths . iter () . cloned ()); }}
  affected_paths . sort ();
  affected_paths . dedup ();
  let pending = PendingReloadPresentation {
    applied: store_outcome . applied,
    message: store_outcome . message,
    requested_outcomes,
    affected_paths,
    any_rejected,
  };
  present_committed_reload (
    stream, env, views_state, active_source_set,
    incident_id . as_deref () . unwrap_or (""), pending,
    &dirty_uris, &scalar_approved_pids ); }

fn present_committed_reload (
  stream               : &mut TcpStream,
  env                  : &SkgEnv,
  views_state          : &mut ViewsState,
  active_source_set    : &ActiveSourceSet,
  incident_id          : &str,
  pending              : PendingReloadPresentation,
  dirty_uris           : &HashSet<ViewUri>,
  scalar_approved_pids : &HashSet<ID>,
) {
  let presentation = if pending . applied . is_empty () {
    ReloadPresentation {
      updated: Vec::new (), conflicted: Vec::new (),
      errors: Vec::new (), warnings: Vec::new (), }
  } else {
    let diff_mode = views_state . diff_mode_enabled;
    match block_on (rerender_views_after_reload (
      stream, &pending . applied, env, diff_mode, views_state,
      Some (active_source_set), dirty_uris, scalar_approved_pids )) {
      Ok (ReloadRerenderOutcome::Presented (presentation)) => presentation,
      Ok (ReloadRerenderOutcome::Challenge (challenge)) => {
        if incident_id . is_empty () {
          send_reload_error (
            stream,
            "reload rerender needs authorization but request has no incident-id" );
        } else {
          retain_pending_reload (incident_id, pending);
          send_response_with_length_prefix (stream, &challenge); }
        return; },
      Err (error) => {
        if !incident_id . is_empty () {
          retain_pending_reload (incident_id, pending); }
        send_reload_error (stream, &format! (
          "reload presentation failed after stores committed: {}", error));
        return; }} };
  if !presentation . conflicted . is_empty () {
    play_harsh_sound_in_background (); }
  let payload = format_reload_response (
    &pending . message, &pending . requested_outcomes,
    &presentation, &pending . affected_paths, &env . config . sources );
  send_response_with_length_prefix (
    stream,
    &tag_terminal_sexp_response (
      TcpToClient::ReloadPaths,
      if pending . any_rejected { "complete-with-rejected-files" }
      else { "complete" },
      &payload)); }

/// Apply the survivors of a classification to the three derived stores
/// WITHOUT writing the filesystem. Fatal telescopes keep their last-good
/// graph state. Returns a human-readable summary, or `Err` if the store
/// update itself failed.
pub async fn reload_touched_telescopes (
  env     : &mut SkgEnv,
  touched : Vec<TouchedTelescope>,
) -> Result<ReloadStoreOutcome, String> {
  let mut defs : Vec<DefineNode> = Vec::new ();
  let mut fatals : Vec<(ID, String)> = Vec::new ();
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
        selected: Vec::new (), }, };
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
    let loaded : LoadedCorpus = read_all_skg_files_with_manifest (&env . config)
      . map_err ( |error| format! (
        "full-corpus fallback after extra-ID change failed: {}", error)) ?;
    let conflicts = distinct_id_claim_conflicts (&loaded . nodes);
    if !conflicts . is_empty () {
      return Err (format! (
        "reloading would make IDs name multiple nodes: {:?}; stores unchanged",
        conflicts)); }
    let full_graph = InRustGraph::from_nodecompletes (&loaded . nodes);
    defs = graph_delta (&graph_before, &loaded . nodes);
    manifest = loaded . manifest . clone ();
    full_manifest_for_revalidation = Some (loaded . manifest);
    // Validate below against the already-folded complete graph rather than a
    // second partial simulation.
    if let Err (error) =
      error_unless_override_invariants_hold (&env . config, &full_graph) {
      return Err (format! (
        "full-corpus reload would violate override invariants ({}); \
         stores unchanged", error)); }}

  if defs . is_empty () {
    return Ok (ReloadStoreOutcome {
      message: summarize_reload (0, 0, &fatals),
      applied: Vec::new (),
      acknowledged_pids: HashSet::new (),
      rejected: fatals, }); }

  // Batch guard: applying these to a clone of the live graph must not
  // break override invariants. If it would, reject the whole batch and
  // keep last-good (coarse attribution; see progress.org).
  { let mut candidate : InRustGraph =
      (* env . in_rust_graph . load_full () . graph) . clone ();
    apply_definenodes_to_inRustGraph (&mut candidate, &defs);
    let conflicts = distinct_id_claim_conflicts (
      &nodecompletes_from_graph (&candidate));
    if !conflicts . is_empty () {
      return Err (format! (
        "reloading would make IDs name multiple nodes: {:?}; \
         stores unchanged", conflicts)); }
    if let Err (e) =
      error_unless_override_invariants_hold (&env . config, &candidate) {
        return Err ( format! (
          "reloading would violate override invariants ({}); \
          kept last-good state, stores unchanged", e )); } }

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
  drop (_write_guard);
  match wait_for_tantivy_generation (store_outcome . tantivy_generation) {
    TantivyGenerationStatus::Committed => {},
    TantivyGenerationStatus::Failed (reason) => return Err (format! (
      "graph generation {} and TypeDB committed, but Tantivy generation {} \
       failed: {}",
      store_outcome . graph_generation . get (),
      store_outcome . tantivy_generation . get (), reason)),
    TantivyGenerationStatus::Pending => unreachable! (), }
  Ok (ReloadStoreOutcome {
    message: summarize_reload (saves, deletes, &fatals),
    applied,
    acknowledged_pids,
    rejected: fatals, }) }

fn optional_string_list (sexp : &Sexp, key : &str) -> Result<Vec<String>, String> {
  let present = match sexp {
    Sexp::List (items) => items . iter () . any (|item| match item {
      Sexp::List (parts) => matches! (parts . first (),
        Some (Sexp::Atom (Atom::S (candidate))) if candidate == key),
      _ => false, }),
    _ => false, };
  if present { extract_string_list_from_sexp (sexp, key) }
  else { Ok (Vec::new ()) }
}

fn changed_paths_since_selected (env : &SkgEnv) -> io::Result<Vec<PathBuf>> {
  let selected = env . in_rust_graph . load_full ();
  let actual = selected_path_digest_manifest (&env . config)?;
  Ok (changed_manifest_paths (&selected . manifest, &actual))
}

fn changed_manifest_paths (
  selected : &SelectedPathManifest,
  actual   : &SelectedPathManifest,
) -> Vec<PathBuf> {
  let paths : BTreeSet<PathBuf> = selected . keys () . cloned ()
    . chain (actual . keys () . cloned ()) . collect ();
  paths . into_iter () . filter (|path|
    selected . get (path) != actual . get (path)) . collect ()
}

fn possible_paths (config : &SkgConfig, pid : &ID) -> Vec<PathBuf> {
  config . ordered_sources () . into_iter () . map (|source| {
    config . sources . get (&source)
      . expect ("ordered source exists")
      . path . join (format! ("{}.skg", pid)) }) . collect ()
}

fn format_reload_response (
  message        : &str,
  requested      : &[RequestedIdOutcome],
  presentation   : &ReloadPresentation,
  affected_paths : &[PathBuf],
  sources        : &SourceCatalog,
) -> String {
  let atom = |value : &str| Sexp::Atom (Atom::S (value . into ()));
  let field = |key : &str, value : Sexp| Sexp::List (vec![atom (key), value]);
  let outcomes = requested . iter () . map (|outcome| Sexp::List (vec![
    field ("requested-id", atom (&outcome . requested_id)),
    field ("pid", outcome . pid . as_ref ()
      . map (|pid| atom (pid)) . unwrap_or_else (|| atom ("nil"))),
    field ("status", atom (outcome . status)),
    field ("reason", outcome . reason . as_ref ()
      . map (|reason| atom (reason)) . unwrap_or_else (|| atom ("nil"))),
    field ("paths", Sexp::List (outcome . paths . iter ()
      . map (|path| atom (&path . to_string_lossy ())) . collect ())),
  ])) . collect ();
  let format_impact = |impact : &ReloadViewImpact| {
    let paths = paths_for_impact (impact, affected_paths, sources);
    let mut fields = vec![
      field ("view-uri", atom (&impact . uri . repr_in_client ())),
      field ("pids", Sexp::List (impact . pids . iter ()
        . map (|pid| atom (pid . as_str ())) . collect ())),
      field ("paths", Sexp::List (paths . iter ()
        . map (|path| atom (&path . to_string_lossy ())) . collect ())),
    ];
    if let Some (incoming) = &impact . incoming {
      fields . push (field ("incoming", atom (incoming))); }
    Sexp::List (fields) };
  Sexp::List (vec![
    field ("content", atom (message)),
    field ("requested-id-outcomes", Sexp::List (outcomes)),
    field ("conflicted-views", Sexp::List (
      presentation . conflicted . iter () . map (format_impact) . collect ())),
    field ("updated-views", Sexp::List (
      presentation . updated . iter () . map (format_impact) . collect ())),
    field ("files-affected", Sexp::List (affected_paths . iter ()
      . map (|path| atom (&path . to_string_lossy ())) . collect ())),
    field ("rerender-errors", Sexp::List (presentation . errors . iter ()
      . map (|error| atom (error)) . collect ())),
    field ("warnings", Sexp::List (presentation . warnings . iter ()
      . map (|warning| atom (warning)) . collect ())),
  ]) . to_string ()
}

fn paths_for_impact (
  impact         : &ReloadViewImpact,
  affected_paths : &[PathBuf],
  sources        : &SourceCatalog,
) -> Vec<PathBuf> {
  let pids : HashSet<&ID> = impact . pids . iter () . collect ();
  let mut paths : Vec<PathBuf> = affected_paths . iter () . filter (|path|
    sources . source_and_pid_for_direct_path (path)
      . map (|(_, pid)| pids . contains (&pid)) . unwrap_or (false))
    . cloned () . collect ();
  paths . sort ();
  paths . dedup ();
  paths
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
  let (selected_files, _collision) =
    select_source_file_candidates_for_pid (config, pid, candidates);
  if selected_files . is_empty () {
    return Ok (CapturedTelescope {
      outcome: TelescopeReloadOutcome::Delete,
      path_bytes,
      selected: Vec::new (), }); }
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
    selected, })
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

fn send_reload_error (
  stream : &mut TcpStream,
  msg    : &str,
) {
  tracing::error! ("{}", msg);
  send_response_with_length_prefix (
    stream,
    &tag_terminal_text_response (TcpToClient::ReloadPaths, "failed", msg)); }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceCatalog};
  use std::path::Path;

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
  fn explicit_id_outcomes_name_pid_paths_and_rejection () {
    let presentation = ReloadPresentation {
      updated: Vec::new (), conflicted: Vec::new (),
      errors: Vec::new (), warnings: Vec::new (), };
    let payload = format_reload_response ("mixed", &[
      RequestedIdOutcome {
        requested_id: ID::from ("alias"),
        pid: Some (ID::from ("primary")),
        status: "acknowledged", reason: None,
        paths: vec![PathBuf::from ("/data/public/primary.skg")], },
      RequestedIdOutcome {
        requested_id: ID::from ("missing"), pid: None,
        status: "rejected", reason: Some ("not found" . into ()),
        paths: Vec::new (), },
    ], &presentation, &[], &sources (&[("public", "/data/public")]));
    assert! (payload . contains ("(requested-id alias)"));
    assert! (payload . contains ("(pid primary)"));
    assert! (payload . contains ("/data/public/primary.skg"));
    assert! (payload . contains ("(status rejected)"));
    assert! (payload . contains ("(reason \"not found\")")); }

  #[test]
  fn conflict_response_carries_paths_and_authorized_incoming_without_applying () {
    let catalog = sources (&[("public", "/data/public")]);
    let presentation = ReloadPresentation {
      updated: Vec::new (),
      conflicted: vec![ReloadViewImpact {
        uri: ViewUri::ContentView ("dirty-uri" . into ()),
        pids: vec![ID::from ("primary")],
        incoming: Some ("* incoming" . into ()),
      }],
      errors: Vec::new (), warnings: Vec::new (), };
    let payload = format_reload_response (
      "done", &[], &presentation,
      &[PathBuf::from ("/data/public/primary.skg")], &catalog );
    assert! (payload . contains ("(conflicted-views"));
    assert! (payload . contains ("(view-uri dirty-uri)"));
    assert! (payload . contains ("/data/public/primary.skg"));
    assert! (payload . contains ("(incoming \"* incoming\")"));
  }

  #[test]
  fn manifest_comparison_uses_digest_and_explicit_absence () {
    let unchanged = PathBuf::from ("/s/unchanged.skg");
    let rewritten = PathBuf::from ("/s/rewritten.skg");
    let deleted = PathBuf::from ("/s/deleted.skg");
    let added = PathBuf::from ("/s/added.skg");
    let old = SelectedPathManifest::from ([
      (unchanged . clone (), PathDigest::of_bytes (b"same")),
      (rewritten . clone (), PathDigest::of_bytes (b"aaaa")),
      (deleted . clone (), PathDigest::of_bytes (b"gone")),
    ]);
    let new = SelectedPathManifest::from ([
      (unchanged, PathDigest::of_bytes (b"same")),
      // Same byte length, different digest: stamp-only scanning misses this.
      (rewritten . clone (), PathDigest::of_bytes (b"bbbb")),
      (added . clone (), PathDigest::of_bytes (b"new")),
    ]);
    assert_eq! (changed_manifest_paths (&old, &new),
                vec![added, deleted, rewritten]); }
}
