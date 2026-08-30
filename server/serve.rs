/// === Concurrency, Mutexes and Atomicity ===
///
/// An atomic bool is a CPU-level integer (typically 1 machine word) that supports read/modify/write operations guaranteed to be indivisible — no other thread can see a half-written value, even without a mutex.
///
/// In this module, Arc<AtomicBool> is used as the search cancellation flag. The connection thread sets it to true when a new search arrives; the background enrichment  thread checks it before writing to the slot. store and load with Ordering::Relaxed (or SeqCst) are the typical operations — no lock, no blocking, just a single instruction.
///
/// The advantage over Arc<Mutex<bool>>: no lock contention, no possibility of deadlock, and much cheaper (a few nanoseconds vs. potentially microseconds for mutex acquire/release). The tradeoff: atomics only work for simple values — you can't atomically update a String or a struct, which is why the enrichment payload itself uses Arc<Mutex<Option<SearchEnrichmentPayload>>>.

pub mod handlers;
pub mod parse_metadata_sexp;
pub mod protocol;
pub mod util;

use crate::consts::SHUTDOWN_DB_DELETE_DELAY_MS;
use crate::dbs::typedb::util::delete_database;
use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use crate::org_to_text::viewforest_to_string;
use crate::serve::handlers::close_view::handle_close_view_request;
use crate::serve::handlers::collateral_scheduler::CollateralScheduler;
use crate::serve::handlers::diff_analysis::handle_diff_analysis_request_with_source_set;
use crate::serve::handlers::edge_source_info::handle_edge_source_info_request;
use crate::serve::handlers::export_to_org::handle_export_to_org_request;
use crate::serve::handlers::get_file_path::handle_get_file_path_request_with_source_set;
use crate::serve::handlers::herald_rules::handle_herald_rules_request;
use crate::serve::handlers::rebuild_dbs::handle_rebuild_dbs_request;
use crate::serve::handlers::recompute_cyclic_roots::handle_recompute_cyclic_roots_request;
use crate::serve::handlers::reload_batch::{
  handle_begin_reload_batch_request,
  handle_end_reload_batch_request,
  release_connection_reload_batches,
};
use crate::serve::handlers::reload_paths::handle_reload_paths_request;
use crate::serve::handlers::rerender_all_views::{ handle_git_diff_toggle_and_rerender, handle_rerender_all_views_request};
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  decide as decide_scalar_release,
  exclude_ugly_nodes_from_viewforest};
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::source_sets::handle_source_set_request;
use crate::serve::handlers::stage_moves::handle_stage_moves_request;
use crate::serve::handlers::strip_body_whitespace::handle_strip_body_whitespace_request;
use crate::serve::handlers::text_search::render_enriched_search_buffer::{insert_containerward_ancestries_into_search_view, insert_override_ancestries_into_search_view};
use crate::serve::handlers::text_search::{ handle_text_search_request, SearchEnrichmentPayload, mk_search_enrichment_sexp};
use crate::serve::handlers::titles_by_ids::handle_titles_by_ids_request_with_source_set;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{ begin_request_context, read_length_prefixed_content, request_context_active, request_type_from_request, send_response_with_length_prefix, tag_text_response, value_from_request_sexp};
use crate::to_org::util::mark_view_roots_parent_absent;
use crate::types::env::SkgEnv;
use crate::types::errors::BufferValidationError;
use crate::source_sets::ActiveSourceSet;
use crate::source_sets::apply_source_set_to_viewforest;
use crate::types::maybe_placed_viewnode::{MpViewnode,maybePlaced_to_placed_tree};
use crate::types::misc::SourceSetName;
use crate::types::misc::SkgConfig;
use crate::telescope::invariants::TelescopeViolation;
use crate::types::store_state::{
  PathIndexState,
  SelectedPathValue,
  SelectedStoreState,
  StoreHealth,
};
use crate::types::viewnode::ViewNode;
use crate::types::views_state::{OpenViews, ViewUri};
use crate::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;
use crate::update_buffer::set_viewnodestats_in_viewforest;

use ego_tree::{NodeId, Tree};
use std::io::{BufRead, BufReader};
use std::collections::HashSet;
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use std::time::Duration;
use sexp::{Atom, Sexp};

/// Per-connection state for the Emacs client. The in-Rust graph
/// handle is in 'SkgEnv', which is also per-connection (cloned at
/// connection acceptance), so it doesn't appear here.
pub struct ViewsState {
  pub diff_mode_enabled : bool,
  pub open_views        : OpenViews,
  // If Emacs crashes or the TCP connection drops without sending close-view messages, OpenViews is still freed, because ViewsState is owned by handle_emacs and dropped when the connection loop exits (n == 0). There's no leak.
}

/// Pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  env            : SkgEnv,
  emacs_listener : TcpListener,
) -> std::io::Result<()> {

  for stream_res in emacs_listener . incoming() { // the loop
    match stream_res {
      Ok (stream) => {
        let stream : TcpStream = stream; // for type sig
        let env_clone : SkgEnv = env . clone (); // Cloning permits the main thread to keep the env. If it were moved here, ownership would transfer into the first spawned thread, making it unavailable for the next connection.
        thread::spawn ( move || {
          handle_emacs (stream, env_clone) } ); }
      Err (e) => {
        tracing::error!(error = %e, "Connection failed"); }} }
  Ok (( )) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_text_search_request
/// API: See /api.md
fn handle_emacs (
  mut stream : TcpStream,
  mut env    : SkgEnv,
) {
  let mut views_state : ViewsState =
    ViewsState {
      diff_mode_enabled : false,
      open_views        : OpenViews::new (), };
  let mut active_source_set : ActiveSourceSet =
    ActiveSourceSet::default_from_config (
      &env . config )
      . unwrap_or_else ( |e| {
        tracing::error! (
          error = %e,
          "failed to initialize active source-set; falling back to all");
        ActiveSourceSet::named (
          &env . config,
          SourceSetName::from ("all"))
        . expect ("reserved source-set all should always resolve") });

  let enrichment_slot // To update search results once the 'enrichment' (containerward paths + graphnodestats) has been computed.
    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );
  let mut snapshot_requested : bool = false;
  let mut owned_reload_batch_tokens : HashSet<String> = HashSet::new ();
  let mut collateral_scheduler = CollateralScheduler::new ();

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  tracing::info!(peer = %peer, "Emacs connected");
  stream . set_read_timeout (
    Some ( Duration::from_millis (100) ))
    . expect ("set_read_timeout failed");
  let mut reader
    : BufReader<TcpStream> // the underlying stream, but buffered
    = BufReader::new (
      stream . try_clone() . unwrap() );
  let mut request_header : String = String::new();
  loop {
    match reader . read_line (&mut request_header) {
      Ok (0) => break, // emacs disconnected
      Ok (_n) => {
        tracing::info! ( request = request_header . trim_end (), "Received request" );
        if let Err (error) = begin_request_context (&request_header) {
          tracing::error! ("{}", error);
          send_response_with_length_prefix (
            &mut stream,
            &tag_text_response (TcpToClient::Error, &error));
          request_header . clear ();
          continue; }
        let request_type = request_type_from_request (&request_header);
        if ! matches! (request_type,
          Ok (RequestType::ApplyCollateral | RequestType::ViewVisited))
        { collateral_scheduler . preempt (); }
        match request_type {
          // For most types of requests, the header is the entire request, and the reader is no longer needed. For saving, though, the reader still contains the buffer content, so it is passed along.
          Ok (RequestType::SingleRootContentView) =>
            handle_single_root_view_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &active_source_set ),
          Ok (RequestType::SaveBuffer) =>
            // PITFALL: Uses the same BufReader that read the request,
            // so that any already-buffered header/payload are visible.
            handle_save_buffer_request (
              &mut reader,
              &mut stream,
              &request_header,
              &mut env,
              &mut views_state,
              &active_source_set,
              &mut collateral_scheduler ),
          Ok (RequestType::CloseView) =>
            handle_close_view_request (
              &mut stream,
              &request_header,
              &mut views_state ),
          Ok (RequestType::SnapshotResponse) => {
            snapshot_requested = false;
            handle_snapshot_response (
              &mut reader,
              &mut stream,
              &request_header,
              &enrichment_slot,
              &env,
              &mut views_state,
              &active_source_set ); }
          Ok (RequestType::TextSearch) => {
            // Cancel any in-flight background search
            search_cancelled . store (true, Ordering::SeqCst);
            snapshot_requested = false;
            handle_text_search_request (
              &mut stream,
              &request_header,
              &env,
              &enrichment_slot,
              &search_cancelled,
              &mut views_state,
              &active_source_set ); }
          Ok (RequestType::VerifyConnection) =>
            handle_verify_connection_request (
              &mut stream, &env ),
          Ok (RequestType::Shutdown) =>
            // Never returns - exits process
            handle_shutdown_request ( &mut stream, &env ),
          Ok (RequestType::GetFilePath) =>
            handle_get_file_path_request_with_source_set ( &mut stream,
                                           &request_header,
                                           &env . config,
                                           &active_source_set ),
          Ok (RequestType::TitlesByIds) =>
            handle_titles_by_ids_request_with_source_set (
              &mut stream, &request_header,
              &env . tantivy_index, &env . config,
              views_state . diff_mode_enabled,
              &active_source_set,
              &env . in_rust_graph_snapshot () ),
          Ok (RequestType::DiffAnalysis) =>
            handle_diff_analysis_request_with_source_set (
              &mut stream, &request_header, &env . config,
              &active_source_set ),
          Ok (RequestType::StageMoves) =>
            handle_stage_moves_request (
              &mut stream, &env . config ),
          Ok (RequestType::EdgeSourceInfo) =>
            handle_edge_source_info_request (
              &mut stream, &request_header, &env ),
          Ok (RequestType::ListSourceSets)
          | Ok (RequestType::ActiveSourceSet)
          | Ok (RequestType::SetActiveSourceSet) =>
            handle_source_set_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &mut active_source_set,
              &enrichment_slot,
              &search_cancelled ),
          Ok (RequestType::HeraldRules) =>
            handle_herald_rules_request ( &mut stream ),
          Ok (RequestType::GitDiffModeToggle) =>
            handle_git_diff_toggle_and_rerender (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &active_source_set ),
          Ok (RequestType::ExportToOrg) =>
            handle_export_to_org_request ( &mut stream,
                                           &env . config,
                                           &request_header ),
          Ok (RequestType::RebuildDbs) =>
            handle_rebuild_dbs_request ( &mut stream,
                                         &mut env,
                                         &mut views_state ),
          Ok (RequestType::StripBodyWhitespace) =>
            handle_strip_body_whitespace_request ( &mut stream,
                                                   &mut env ),
          Ok (RequestType::RerenderAllViews) =>
            handle_rerender_all_views_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state,
              &active_source_set ),
          Ok (RequestType::ReloadPaths) =>
            handle_reload_paths_request (
              &mut stream,
              &request_header,
              &mut env,
              &mut views_state,
              &active_source_set,
              &mut collateral_scheduler ),
          Ok (RequestType::BeginReloadBatch) =>
            handle_begin_reload_batch_request (
              &mut stream, &mut owned_reload_batch_tokens),
          Ok (RequestType::EndReloadBatch) =>
            handle_end_reload_batch_request (
              &mut stream, &request_header,
              &mut owned_reload_batch_tokens),
          Ok (RequestType::RecomputeCyclicRoots) =>
            handle_recompute_cyclic_roots_request (
              &mut stream, &env),
          Ok (RequestType::ApplyCollateral) =>
            collateral_scheduler . handle_apply_ack (
              &mut stream, &request_header, &mut views_state),
          Ok (RequestType::ViewVisited) => {
            let result = value_from_request_sexp ("view-uri", &request_header)
              . and_then (|uri| value_from_request_sexp (
                  "visit-sequence", &request_header)
                . and_then (|sequence| sequence . parse::<u64> ()
                  . map_err (|_| "Invalid visit-sequence" . to_string ())
                  . map (|sequence| (uri, sequence))));
            match result {
              Ok ((uri, sequence)) => {
                collateral_scheduler . note_visit (
                  ViewUri::from_client_string (uri), sequence);
                send_response_with_length_prefix (
                  &mut stream, &tag_text_response (
                    TcpToClient::ViewVisited, "visit recorded")); }
              Err (error) => send_response_with_length_prefix (
                &mut stream, &tag_text_response (
                  TcpToClient::Error, &error)), } },
          Err (err) => {
            tracing::error!(error = %err, "Error determining request type");
            send_response_with_length_prefix (
              &mut stream,
              & tag_text_response (
                TcpToClient::Error,
                & format! (
                  "Error determining request type: {}",
                  err ))); } };
        request_header . clear(); }
      Err (ref e)
        if e . kind () == std::io::ErrorKind::WouldBlock
        || e . kind () == std::io::ErrorKind::TimedOut =>
      { // Idle timeout — if enrichment is ready, ask Emacs
        // for a snapshot of the search buffer so we can integrate
        // ancestry without losing user edits.
        if ! snapshot_requested {
          if let Ok (guard) = enrichment_slot . try_lock () {
            if guard . is_some () {
              // Peek at the terms without taking the payload yet.
              // The payload stays in the slot until the snapshot arrives.
              let terms : String =
                guard . as_ref () . unwrap () . terms . clone ();
              drop (guard); // release the lock
              tracing::debug! ("slot drain: requesting snapshot for '{}'", terms);
              send_response_with_length_prefix (
                &mut stream,
                & tag_text_response (
                  TcpToClient::RequestSnapshot,
                  &terms ));
              snapshot_requested = true; }}}
        if ! request_context_active () {
          collateral_scheduler . pump (&mut stream, &views_state); }
      }
      Err (_) => break, // real error
    }}
  release_connection_reload_batches (&mut owned_reload_batch_tokens);
  tracing::info!(peer = %peer, "Emacs disconnected"); }

/// Handle the snapshot that Emacs sent back.
/// Parses the buffer text, inserts ancestry, sets graphnodestats,
/// and sends the enriched result to Emacs.
/// Emacs to Rust message format:
///   ((request . "snapshot response") (terms . "TERMS"))
///   Content-Length: N\r\n\r\n<buffer text>
fn handle_snapshot_response (
  reader          : &mut BufReader<TcpStream>,
  stream          : &mut TcpStream,
  request         : &str,
  enrichment_slot : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  env             : &SkgEnv,
  views_state      : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
) {
  let terms : String
    = match value_from_request_sexp ("terms", request)
    { Ok (t) => t,
      Err (e) => { tracing::error! ( "snapshot response: bad terms: {}", e);
                   send_response_with_length_prefix (
                     stream, &tag_text_response (
                       TcpToClient::Error,
                       &format! ("Snapshot response has bad terms: {}", e)));
                   return; }};
  let buffer_text : String
    = match read_length_prefixed_content (reader)
    { Ok (text) => text,
      Err (e) => { tracing::error! ( "snapshot response: failed to read content: {}", e);
                   send_response_with_length_prefix (
                     stream, &tag_text_response (
                       TcpToClient::Error,
                       &format! ("Snapshot response content failed: {}", e)));
                   return; }};
  let payload : SearchEnrichmentPayload = {
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      enrichment_slot . lock () . unwrap ();
    match guard . take () {
      Some (p) => p,
      None => { tracing::warn! (
                  "snapshot response: no enrichment payload");
                send_response_with_length_prefix (
                  stream, &tag_text_response (
                    TcpToClient::Error,
                    "Snapshot response has no pending enrichment"));
                return; }} };
  if payload . terms != terms {
    tracing::warn! ("snapshot response: terms mismatch ('{}' vs '{}')",
                    payload . terms, terms);
    send_response_with_length_prefix (
      stream, &tag_text_response (
        TcpToClient::Error,
        &format! ("Snapshot terms mismatch: '{}' vs '{}'",
                  payload . terms, terms)));
    return; }
  let parse_result : Result<(Tree<MpViewnode>,
                             Vec<BufferValidationError>), String>
    = org_to_uninterpreted_nodes (&buffer_text);
  let mut viewforest : Tree<ViewNode> = match parse_result {
    Ok (( maybePlaced_viewforest, _errors )) =>
      match maybePlaced_to_placed_tree (maybePlaced_viewforest) {
        Ok (f) => f,
        Err (e) => {
          tracing::error! ("snapshot response: check failed: {}", e);
          send_response_with_length_prefix (
            stream, &tag_text_response (
              TcpToClient::Error,
              &format! ("Snapshot structure check failed: {}", e)));
          return; }},
    Err (e) => {
      tracing::error! ("snapshot response: parse failed: {}", e);
      send_response_with_length_prefix (
        stream, &tag_text_response (
          TcpToClient::Error,
          &format! ("Snapshot parse failed: {}", e)));
      return; }};
  insert_containerward_ancestries_into_search_view (
    &mut viewforest, &payload . search_results,
    &payload . ancestry_by_id, &env . tantivy_index,
    &env . config, active_source_set );
  insert_override_ancestries_into_search_view (
    &mut viewforest, &payload . search_results,
    active_source_set );
  { let root_treeid : NodeId =
      viewforest . root () . id ();
    set_metadata_relationships_in_node_recursive (
      &mut viewforest, root_treeid,
      &payload . graphnodestats,
      &env . config ); }
  mark_view_roots_parent_absent (
    &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    & payload . graphnodestats . container_to_contents,
    & payload . graphnodestats . content_to_containers,
    & env . config,
    Some (active_source_set) );
  apply_source_set_to_viewforest (
    &mut viewforest,
    active_source_set );
  if ! payload . include_ugly_telescopes {
    exclude_ugly_nodes_from_viewforest (
      &mut viewforest, &env . in_rust_graph_snapshot () ); }
  let rendered_pids : Vec<_> =
    viewforest . root () . descendants ()
    . filter_map ( |node| match &node . value () . kind {
      crate::types::viewnode::ViewNodeKind::Vognode (
        crate::types::viewnode::Vognode::Active (active_node)) =>
          Some (active_node . id . clone ()),
      _ => None, } )
    . collect ();
  let approved : std::collections::HashSet<_> =
    if payload . include_ugly_telescopes {
      rendered_pids . iter () . cloned () . collect ()
    } else { std::collections::HashSet::new () };
  let release = decide_scalar_release (
    "search-enrichment", active_source_set, &rendered_pids,
    &env . in_rust_graph_snapshot (), &approved );
  if matches! (release, ScalarReleaseDecision::Challenge { .. }) {
    // Preflight and the load-bearing payload should make this unreachable.
    // Fail closed rather than serialize if a future change violates either.
    tracing::error! (
      "search enrichment reached the release boundary without approval" );
    send_response_with_length_prefix (
      stream, &tag_text_response (
        TcpToClient::Error,
        "Search enrichment failed its scalar-release check"));
    return; }
  let release_warnings : Vec<String> = match release {
    ScalarReleaseDecision::AllowWithWarning { warning } => vec! [warning],
    _ => Vec::new (), };
  let enriched : String =
    viewforest_to_string ( &viewforest, &env . config )
    . expect ("search viewforest rendering never fails");
  let enriched_sexp : String =
    mk_search_enrichment_sexp (
      &terms, &enriched, &release_warnings );
  { let uri : ViewUri = // update ViewsState with enriched viewforest
      ViewUri::SearchView ( terms . clone () );
    views_state . open_views . update_view ( &uri, viewforest ); }
  tracing::debug! (bytes = enriched_sexp . len (),
                   "snapshot response: sending enrichment");
  send_response_with_length_prefix (
    stream, &enriched_sexp ); }

fn handle_verify_connection_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  send_response_with_length_prefix (
    stream,
    & verify_connection_response (
      &env . config,
      &env . startup_warnings,
      &env . in_rust_graph . load_full ())); }

fn verify_connection_response (
  config   : &SkgConfig,
  warnings : &[(crate::types::misc::ID, TelescopeViolation)],
  selected : &SelectedStoreState,
) -> String {
  let atom = |value : &str| -> Sexp {
    Sexp::Atom (Atom::S (value . to_string ())) };
  let field = |key : &str, value : Sexp| -> Sexp {
    Sexp::List (vec! [atom (key), value]) };
  let source_entries : Vec<Sexp> = config . ordered_sources ()
    . into_iter ()
    . enumerate ()
    . map ( |(position, name)| {
      let source = config . sources . get (&name)
        . expect ("ordered source exists");
      Sexp::List (vec! [
        field ("name", atom (&name)),
        field ("abbreviation", source . abbreviation . as_deref ()
          . map (&atom) . unwrap_or_else ( || atom ("nil") )),
        field ("owned", atom (
          if source . user_owns_it { "true" } else { "nil" })),
        field ("position", Sexp::Atom (Atom::I (position as i64))),
        field ("configured-path", atom (
          &config . sources . configured_path (&name)
            . unwrap_or (&source . path) . to_string_lossy ())),
        field ("directory", atom (&source . path . to_string_lossy ())),
        field ("directory-identity", atom (
          &config . sources . directory_identity (&name)
            . unwrap_or (&source . path) . to_string_lossy ())),
      ]) })
    . collect ();
  let warning_entries : Vec<Sexp> = warnings . iter ()
    . map ( |(pid, warning)| {
      let (kind, winning_paths, ignored_paths) = match warning {
        TelescopeViolation::IgnoredForeignPidCollision {
          winning_paths, ignored_paths, .. } =>
          ("ignored-foreign-pid-collision",
           winning_paths . as_slice (), ignored_paths . as_slice ()),
        _ => ("telescope-warning", &[][..], &[][..]), };
      let path_list = |paths : &[std::path::PathBuf]| -> Sexp {
        Sexp::List (paths . iter () . map ( |path|
          atom (&path . to_string_lossy ()) ) . collect ()) };
      Sexp::List (vec! [
        field ("pid", atom (pid)),
        field ("kind", atom (kind)),
        field ("message", atom (&warning . to_string ())),
        field ("winning-paths", path_list (winning_paths)),
        field ("ignored-paths", path_list (ignored_paths)),
      ]) })
    . collect ();
  let path_entries : Vec<Sexp> = selected . path_outcomes . iter ()
    . map ( |(path, outcome)| {
      let (index_state, tantivy_generation) = match outcome . index_state {
        PathIndexState::Acknowledged { tantivy_generation } =>
          ("acknowledged", tantivy_generation),
        PathIndexState::SelectedAwaitingIndex { tantivy_generation } =>
          ("selected-awaiting-index", Some (tantivy_generation)), };
      Sexp::List (vec! [
        field ("path", atom (&path . to_string_lossy ())),
        field ("value", match outcome . value {
          SelectedPathValue::Present (digest) => atom (&digest . to_hex ()),
          SelectedPathValue::Absent => atom ("absent"), }),
        field ("graph-generation", Sexp::Atom (Atom::I (
          outcome . graph_generation . get () as i64))),
        field ("index-state", atom (index_state)),
        field ("tantivy-generation", tantivy_generation
          . map ( |generation| Sexp::Atom (Atom::I (
            generation . get () as i64)))
          . unwrap_or_else ( || atom ("nil"))),
      ]) })
    . collect ();
  let health = |health : &StoreHealth| -> Sexp { match health {
    StoreHealth::Healthy => atom ("healthy"),
    StoreHealth::Poisoned (reason) => Sexp::List (vec! [
      atom ("poisoned"), atom (reason)]), }};
  Sexp::List (vec! [
    field ("response-type", atom (
      TcpToClient::VerifyConnection . repr_in_client ())),
    field ("content", atom (
      "This is the skg server verifying the connection.")),
    field ("source-inventory", Sexp::List (source_entries)),
    field ("telescope-warnings", Sexp::List (warning_entries)),
    field ("graph-generation", Sexp::Atom (Atom::I (
      selected . graph_generation . get () as i64))),
    field ("path-outcomes", Sexp::List (path_entries)),
    field ("typedb-health", health (&selected . typedb_health)),
    field ("tantivy-health", health (&selected . tantivy_health)),
  ]) . to_string ()
}

fn handle_shutdown_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::Shutdown, "Server shutting down..." ));
  cleanup_and_shutdown (env); }

/// Performs cleanup before server shutdown.
/// Deletes the database if delete_on_quit is configured, then exits.
fn cleanup_and_shutdown (env : &SkgEnv) {
  if env . config . delete_on_quit {
    tracing::info! (
      db_name = %env . config . db_name,
      "Deleting database before shutdown" );

    // Wait briefly to allow any pending operations to complete.
    // This helps ensure the database isn't marked as "in use".
    std::thread::sleep (
      std::time::Duration::from_millis (
        SHUTDOWN_DB_DELETE_DELAY_MS ) );

    futures::executor::block_on ( async {
      if let Err (e) =
        delete_database (
          &env . driver, & env . config . db_name )
        . await {
          tracing::error! ( error = %e, "Failed to delete database" );
        }} ); }
  tracing::info! ("Shutdown complete.");
  std::process::exit (0); }

#[cfg(test)]
mod connection_tests {
  use super::*;
  use crate::types::misc::{SkgfileSource, SourceName};
  use std::collections::HashMap;
  use std::path::PathBuf;

  #[test]
  fn verification_carries_the_ordered_normalized_source_inventory () {
    let mut sources = HashMap::new ();
    for (name, path, owned) in [
      ("second", "/tmp/second", false),
      ("first", "/tmp/first", true),
    ] {
      sources . insert (SourceName::from (name), SkgfileSource {
        name: SourceName::from (name), abbreviation: None,
        path: PathBuf::from (path), user_owns_it: owned,
      }); }
    let mut config : SkgConfig = SkgConfig::dummyFromSources (sources);
    config . sources . set_order (vec! [
      SourceName::from ("first"), SourceName::from ("second") ]);
    let warnings = vec! [(crate::types::misc::ID::from ("X"),
      TelescopeViolation::IgnoredForeignPidCollision {
        winning_sources: vec![SourceName::from ("first")],
        winning_paths: vec![PathBuf::from ("/tmp/first/X.skg")],
        ignored_sources: vec![SourceName::from ("second")],
        ignored_paths: vec![PathBuf::from ("/tmp/second/X.skg")],
      })];
    let selected = SelectedStoreState::initial (
      crate::dbs::in_rust_graph::InRustGraph::new (),
      crate::types::store_state::SelectedPathManifest::from ([
        (PathBuf::from ("/tmp/first/X.skg"),
         crate::types::store_state::PathDigest::of_bytes (b"pid: X\n")),
      ]));
    let response : String = verify_connection_response (
      &config, &warnings, &selected);
    let first : usize = response . find ("(name first)") . unwrap ();
    let second : usize = response . find ("(name second)") . unwrap ();
    assert! (first < second, "{}", response);
    assert! (response . contains ("(position 0)"), "{}", response);
    assert! (response . contains ("(owned true)"), "{}", response);
    assert! (response . contains ("(directory /tmp/first)"), "{}", response);
    assert! (response . contains (
      "(kind ignored-foreign-pid-collision)"), "{}", response);
    assert! (response . contains ("/tmp/second/X.skg"), "{}", response);
    assert! (response . contains ("(graph-generation 1)"), "{}", response);
    assert! (response . contains ("(index-state acknowledged)"), "{}", response);
    assert! (response . contains ("(tantivy-health healthy)"), "{}", response);
  }
}
