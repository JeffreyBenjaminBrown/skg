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
use crate::serve::handlers::client_census::{
  handle_client_census_request,
  handle_client_census_texts_request,
};
use crate::serve::handlers::collateral_scheduler::{
  CollateralScheduler,
  RenderGeneration,
  add_application_offer_to_response,
};
use crate::serve::handlers::diff_analysis::handle_diff_analysis_request_with_source_set;
use crate::serve::handlers::edge_source_info::handle_edge_source_info_request;
use crate::serve::handlers::export_to_org::handle_export_to_org_request;
use crate::serve::handlers::get_file_path::handle_get_file_path_request_with_source_set;
use crate::serve::handlers::herald_rules::handle_herald_rules_request;
use crate::serve::handlers::maintenance_protocol::{
  handle_acknowledge_terminal_maintenance_request,
  handle_approve_maintenance_scalar_release_request,
  handle_approve_undo_waiver_request,
  handle_begin_maintenance_request,
  handle_cancel_maintenance_request,
  handle_complete_maintenance_request,
  handle_maintenance_archive_failed_request,
  handle_maintenance_archive_finalized_request,
  handle_maintenance_archive_ready_request,
  handle_maintenance_evidence_request,
  handle_maintenance_locked_census_request,
  handle_maintenance_status_request,
  handle_maintenance_view_settled_request,
  handle_finish_maintenance_origin_request,
  handle_run_maintenance_origin_request,
};
use crate::serve::handlers::rebuild_dbs::handle_rebuild_dbs_request;
use crate::serve::handlers::recompute_cyclic_roots::handle_recompute_cyclic_roots_request;
use crate::serve::handlers::reload_batch::{
  handle_begin_reload_batch_request,
  handle_end_reload_batch_request,
  reconciliation_generation,
  release_connection_reload_batches,
};
use crate::serve::handlers::reload_paths::handle_reload_paths_request;
use crate::serve::handlers::reload_recovery::{
  handle_reload_recovery_request,
  load_recovery_journals,
  pending_incidents_for_config,
};
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
use crate::serve::handlers::text_search::render_enriched_search_buffer::{
  insert_containerward_ancestries_from_snapshot,
  insert_override_ancestries_from_graph,
};
use crate::serve::handlers::text_search::{ handle_text_search_request, SearchEnrichmentPayload, mk_search_enrichment_sexp};
use crate::serve::handlers::titles_by_ids::handle_titles_by_ids_request_with_source_set;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::runtime::{
  InteractiveConnectionGuard,
  ServerRuntime,
};
use crate::maintenance::{CoordinatorState, PendingReason};
use crate::runtime::interactive_session::{
  AttachedClient,
  ClientCapabilities,
  ClientKind,
  InteractiveSession,
};
use crate::serve::util::{ begin_request_context, ensure_request_has_terminal_response, read_length_prefixed_content, request_context_active, request_type_from_request, send_response_with_length_prefix, tag_server_push_sexp_response, tag_terminal_text_response, tag_text_response, take_send_failure, value_from_request_sexp};
use crate::to_org::util::mark_view_roots_parent_absent;
use crate::types::env::SkgEnv;
use crate::types::errors::BufferValidationError;
use crate::source_sets::ActiveSourceSet;
use crate::source_sets::apply_source_set_to_viewforest;
use crate::types::maybe_placed_viewnode::{MpViewnode,maybePlaced_to_placed_tree};
use crate::types::misc::SkgConfig;
use crate::telescope::invariants::TelescopeViolation;
use crate::types::store_state::{
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

enum ConnectionRole {
  Interactive { _guard : InteractiveConnectionGuard },
  Control,
}

impl ConnectionRole {
  fn interactive (&self) -> bool {
    matches! (self, Self::Interactive { .. }) }

  fn permits (&self, request_type : RequestType) -> bool {
    match self {
      Self::Interactive { .. } => !matches! (
        request_type,
        RequestType::BeginReloadBatch | RequestType::EndReloadBatch),
      Self::Control => matches! (
        request_type,
        RequestType::BeginReloadBatch
        | RequestType::EndReloadBatch
        | RequestType::Shutdown),
    }
  }
}

fn authenticate_connection (
  runtime      : &ServerRuntime,
  request      : &str,
  request_type : RequestType,
) -> Result<ConnectionRole, String> {
  let role = value_from_request_sexp ("role", request)
    . map_err (|_| "the first request must carry role=interactive or role=control"
      . to_string ())?;
  match role . as_str () {
    "interactive" => {
      if request_type != RequestType::VerifyConnection {
        return Err (
          "an interactive connection must begin with verify connection"
            . into ()); }
      runtime . interactive_slot . try_attach ()
        . map (|guard| ConnectionRole::Interactive { _guard: guard })
    }
    "control" => {
      if !matches! (request_type,
        RequestType::BeginReloadBatch
        | RequestType::EndReloadBatch
        | RequestType::Shutdown)
      {
        return Err ("the control role requested a non-control endpoint"
          . into ()); }
      Ok (ConnectionRole::Control)
    }
    _ => Err (format! ("unknown connection role '{}'", role)),
  }
}

/// Pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  env            : SkgEnv,
  emacs_listener : TcpListener,
) -> std::io::Result<()> {

  match load_recovery_journals (&env . config) {
    Ok (count) if count > 0 => tracing::warn! (
      count, "loaded unresolved fatal-reload recovery journals"),
    Ok (_) => {}
    Err (error) => tracing::error! (
      %error, "could not completely load fatal-reload recovery journals"),
  }

  let runtime = Arc::new (ServerRuntime::new (env)
    . map_err (|error| std::io::Error::new (
      std::io::ErrorKind::Other, error))?);
  runtime . start_background_services ()
    . map_err (|error| std::io::Error::new (
      std::io::ErrorKind::Other, error))?;

  for stream_res in emacs_listener . incoming() { // the loop
    match stream_res {
      Ok (stream) => {
        let stream : TcpStream = stream; // for type sig
        let runtime = Arc::clone (&runtime);
        thread::spawn ( move || {
          handle_connection (stream, runtime) } ); }
      Err (e) => {
        tracing::error!(error = %e, "Connection failed"); }} }
  Ok (( )) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_text_search_request
/// API: See /api.md
fn handle_connection (
  mut stream : TcpStream,
  runtime    : Arc<ServerRuntime>,
) {
  let enrichment_slot // To update search results once the 'enrichment' (containerward paths + graphnodestats) has been computed.
    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );
  let mut snapshot_requested : bool = false;
  let mut owned_reload_batch_tokens : HashSet<String> = HashSet::new ();
  let mut role : Option<ConnectionRole> = None;
  let mut seen_reconciliation_generation = reconciliation_generation ();

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  tracing::info!(peer = %peer, "Skg socket connected");
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
          let _ = send_response_with_length_prefix (
            &mut stream,
            &tag_text_response (TcpToClient::Error, &error));
          request_header . clear ();
          continue; }
        let request_type = match request_type_from_request (&request_header) {
          Ok (request_type) => request_type,
          Err (error) => {
            tracing::error! (%error, "Error determining request type");
            let _ = send_response_with_length_prefix (
              &mut stream, &tag_terminal_text_response (
                TcpToClient::Error, "failed", &error));
            request_header . clear ();
            continue; }};
        if role . is_none () {
          match authenticate_connection (
              &runtime, &request_header, request_type)
          {
            Ok (authenticated) => role = Some (authenticated),
            Err (error) => {
              let _ = send_response_with_length_prefix (
                &mut stream, &tag_terminal_text_response (
                  TcpToClient::Error, "failed", &error));
              break; }
          }}
        let authenticated = role . as_ref ()
          . expect ("authentication filled role");
        if !authenticated . permits (request_type) {
          let _ = send_response_with_length_prefix (
            &mut stream, &tag_terminal_text_response (
              TcpToClient::Error, "failed",
              "The control role is not allowed to access that endpoint"));
          request_header . clear ();
          continue; }
        if authenticated . interactive ()
           && !matches! (request_type,
             RequestType::VerifyConnection
             | RequestType::ClientCensus
             | RequestType::ClientCensusTexts
             | RequestType::MaintenanceLockedCensus)
           && !runtime . interactive . lock () . unwrap ()
             . attached_client . as_ref ()
             . map (|client| client . census_complete)
             . unwrap_or (false)
        {
          let _ = send_response_with_length_prefix (
            &mut stream, &tag_terminal_text_response (
              TcpToClient::Error, "failed",
              "client census must complete before ordinary requests"));
          // A payload-bearing unauthorized request cannot safely remain on
          // this byte stream.  Closing also makes the mandatory handshake
          // order unambiguous on retry.
          break; }
        if authenticated . interactive ()
        && !matches! (request_type,
          RequestType::ApplyCollateral
          | RequestType::ViewVisited
          | RequestType::ObservePresentation
          | RequestType::ClientCensus
          | RequestType::ClientCensusTexts)
        {
          runtime . interactive . lock () . unwrap ()
            . collateral_scheduler . preempt (); }
        dispatch_request (
          &runtime,
          &mut reader,
          &mut stream,
          &request_header,
          request_type,
          &enrichment_slot,
          &search_cancelled,
          &mut snapshot_requested,
          &mut owned_reload_batch_tokens);
        if request_type != RequestType::TextSearch {
          let _ = ensure_request_has_terminal_response (
            &mut stream, request_type); }
        if let Some (error) = take_send_failure () {
          tracing::warn! (%error,
            "response transport failed; abandoning connection-owned work");
          break; }
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
              let _ = send_response_with_length_prefix (
                &mut stream,
                & tag_text_response (
                  TcpToClient::RequestSnapshot,
                  &terms ));
              snapshot_requested = true; }}}
        if ! request_context_active () {
          let reconciliation = reconciliation_generation ();
          if role . as_ref () . map (ConnectionRole::interactive)
             . unwrap_or (false)
             && reconciliation > seen_reconciliation_generation
          {
            let payload = Sexp::List (vec![
              Sexp::List (vec![
                Sexp::Atom (Atom::S ("content" . into ())),
                Sexp::Atom (Atom::S (
                  "External reload batch closed; run one exact full manifest sweep"
                    . into ())),
              ]),
              Sexp::List (vec![
                Sexp::Atom (Atom::S ("sweep-generation" . into ())),
                Sexp::Atom (Atom::I (reconciliation as i64)),
              ]),
            ]) . to_string ();
            let _ = send_response_with_length_prefix (
              &mut stream, &tag_server_push_sexp_response (
                TcpToClient::ReconciliationReady,
                &format! ("reconciliation-{}", reconciliation),
                &payload));
            seen_reconciliation_generation = reconciliation;
          }
          if role . as_ref () . map (ConnectionRole::interactive)
             . unwrap_or (false)
          {
            let events : Vec<_> = {
              let mut interactive = runtime . interactive . lock () . unwrap ();
              interactive . queued_server_events . drain (..) . collect ()
            };
            for event in events {
              let response_type = match event . frame_kind . as_str () {
                "maintenance-offer" => TcpToClient::MaintenanceOffer,
                "maintenance-status" => TcpToClient::MaintenanceStatus,
                other => {
                  tracing::error! (frame_kind = other,
                    "discarding unknown queued server event kind");
                  continue; }
              };
              let _ = send_response_with_length_prefix (
                &mut stream, &tag_server_push_sexp_response (
                  response_type, &event . operation_id, &event . payload));
            }
            let mut interactive = runtime . interactive . lock () . unwrap ();
            let InteractiveSession {
              views, collateral_scheduler, ..
            } = &mut *interactive;
            collateral_scheduler . pump (&mut stream, views); }}
        if let Some (error) = take_send_failure () {
          tracing::warn! (%error,
            "server-push transport failed; retaining session work");
          break; }
      }
      Err (_) => break, // real error
    }}
  release_connection_reload_batches (&mut owned_reload_batch_tokens);
  if role . as_ref () . map (ConnectionRole::interactive)
     . unwrap_or (false)
  {
    runtime . maintenance . lock () . unwrap () . disconnected ();
    runtime . persist_maintenance_state ();
    if let Ok (mut interactive) = runtime . interactive . lock () {
      if let Some (client) = &mut interactive . attached_client {
        client . census_complete = false; }} }
  tracing::info!(peer = %peer, "Skg socket disconnected"); }

#[allow(clippy::too_many_arguments)]
fn dispatch_request (
  runtime           : &ServerRuntime,
  reader            : &mut BufReader<TcpStream>,
  stream            : &mut TcpStream,
  request           : &str,
  request_type      : RequestType,
  enrichment_slot   : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled  : &Arc<AtomicBool>,
  snapshot_requested : &mut bool,
  owned_reload_batch_tokens : &mut HashSet<String>,
) {
  match request_type {
    RequestType::SingleRootContentView => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession { views, active_source_set, .. } = interactive;
        handle_single_root_view_request (
          stream, request, env, views, active_source_set); })
      { send_runtime_error (stream, &error); }}
    RequestType::SaveBuffer => {
      // The same BufReader which parsed the request must consume its payload.
      // A policy refusal is therefore implemented in the save handler itself.
      // Retain coordinator admission across the transaction: an observation
      // which finishes concurrently waits, then notices the selected
      // generation change and scans again.
      let maintenance = runtime . maintenance . lock () . unwrap ();
      let save_refusal = skg_save_policy_refusal (&maintenance . state);
      if let Err (error) = runtime . with_store_transition (
          false, |env, interactive| {
            let InteractiveSession {
              views, active_source_set, collateral_scheduler, ..
            } = interactive;
            handle_save_buffer_request (
              reader, stream, request, env, views,
              active_source_set, collateral_scheduler, runtime,
              save_refusal . as_deref ()); })
      { send_runtime_error (stream, &error); }}
    RequestType::CloseView => {
      let mut interactive = runtime . interactive . lock () . unwrap ();
      handle_close_view_request (stream, request, &mut interactive . views); }
    RequestType::SnapshotResponse => {
      *snapshot_requested = false;
      let mut interactive = runtime . interactive . lock () . unwrap ();
      let InteractiveSession {
        views, active_source_set, collateral_scheduler, ..
      } = &mut *interactive;
      handle_snapshot_response (
        reader, stream, request, enrichment_slot, views,
        active_source_set, collateral_scheduler); }
    RequestType::TextSearch => {
      search_cancelled . store (true, Ordering::SeqCst);
      *snapshot_requested = false;
      let lease = match runtime . query_lease () {
        Ok (lease) => lease,
        Err (error) => { send_runtime_error (stream, &error); return; }};
      let mut interactive = runtime . interactive . lock () . unwrap ();
      let presentation_generation = interactive . collateral_scheduler
        . presentation_generation ();
      let InteractiveSession { views, active_source_set, .. } =
        &mut *interactive;
      handle_text_search_request (
        stream, request, lease, presentation_generation,
        enrichment_slot, search_cancelled, views, active_source_set); }
    RequestType::VerifyConnection => {
      let lease = match runtime . query_lease () {
        Ok (lease) => lease,
        Err (error) => { send_runtime_error (stream, &error); return; }};
      let census_required = {
        let mut interactive = runtime . interactive . lock () . unwrap ();
        match install_client_handshake (
            request, &lease . snapshot . env, &mut interactive)
        {
          Ok (required) => required,
          Err (error) => { send_runtime_error (stream, &error); return; }
        }};
      let active_source_set_name = runtime . interactive . lock () . unwrap ()
        . active_source_set . name . 0 . clone ();
      if let Err (error) = runtime . transition_maintenance (|coordinator| {
          coordinator . reconnected ();
          Ok (( ))
        })
      {
        send_runtime_error (stream, &error);
        return;
      }
      handle_verify_connection_request (
        stream, &lease . snapshot . env, &active_source_set_name,
        census_required,
        runtime . maintenance . lock () . unwrap () . clone ()); }
    RequestType::ClientCensus => {
      let snapshot = runtime . selected_snapshot ();
      let writes_allowed = runtime . maintenance . lock () . unwrap ()
        . state . policy () . skg_saves_allowed;
      let mut interactive = runtime . interactive . lock () . unwrap ();
      handle_client_census_request (
        reader, stream, &snapshot . env, &mut interactive, writes_allowed); }
    RequestType::ClientCensusTexts => {
      let snapshot = runtime . selected_snapshot ();
      let writes_allowed = runtime . maintenance . lock () . unwrap ()
        . state . policy () . skg_saves_allowed;
      let mut interactive = runtime . interactive . lock () . unwrap ();
      handle_client_census_texts_request (
        reader, stream, &snapshot . env, &mut interactive, writes_allowed); }
    RequestType::Shutdown => {
      let snapshot = runtime . selected_snapshot ();
      handle_shutdown_request (stream, &snapshot . env); }
    RequestType::GetFilePath => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        handle_get_file_path_request_with_source_set (
          stream, request, &env . config, &interactive . active_source_set); })
      { send_runtime_error (stream, &error); }}
    RequestType::TitlesByIds => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        handle_titles_by_ids_request_with_source_set (
          stream, request, &env . tantivy_index, &env . config,
          interactive . views . diff_mode_enabled,
          &interactive . active_source_set,
          &env . in_rust_graph_snapshot ()); })
      { send_runtime_error (stream, &error); }}
    RequestType::DiffAnalysis => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        handle_diff_analysis_request_with_source_set (
          stream, request, &env . config,
          &interactive . active_source_set); })
      { send_runtime_error (stream, &error); }}
    RequestType::StageMoves => {
      let snapshot = runtime . selected_snapshot ();
      handle_stage_moves_request (stream, &snapshot . env . config); }
    RequestType::EdgeSourceInfo => {
      if let Err (error) = with_query_session (runtime, |env, _| {
        handle_edge_source_info_request (stream, request, env); })
      { send_runtime_error (stream, &error); }}
    RequestType::ListSourceSets
    | RequestType::ActiveSourceSet
    | RequestType::SetActiveSourceSet => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession { views, active_source_set, .. } = interactive;
        handle_source_set_request (
          stream, request, env, views, active_source_set,
          enrichment_slot, search_cancelled); })
      { send_runtime_error (stream, &error); }}
    RequestType::HeraldRules => handle_herald_rules_request (stream),
    RequestType::GitDiffModeToggle => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession { views, active_source_set, .. } = interactive;
        handle_git_diff_toggle_and_rerender (
          stream, request, env, views, active_source_set); })
      { send_runtime_error (stream, &error); }}
    RequestType::ExportToOrg => {
      let snapshot = runtime . selected_snapshot ();
      handle_export_to_org_request (
        stream, &snapshot . env . config, request); }
    RequestType::RebuildDbs => {
      handle_rebuild_dbs_request (stream, runtime); }
    RequestType::StripBodyWhitespace => {
      if let Err (error) = runtime . with_store_transition (
          false, |env, _| {
            handle_strip_body_whitespace_request (stream, env); })
      { send_runtime_error (stream, &error); }}
    RequestType::RerenderAllViews => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession { views, active_source_set, .. } = interactive;
        handle_rerender_all_views_request (
          stream, request, env, views, active_source_set); })
      { send_runtime_error (stream, &error); }}
    RequestType::ReloadPaths => {
      if let Err (error) = runtime . with_store_transition (
          false, |env, interactive| {
            let InteractiveSession {
              views, active_source_set, collateral_scheduler, ..
            } = interactive;
            handle_reload_paths_request (
              stream, request, env, views, active_source_set,
              collateral_scheduler); })
      { send_runtime_error (stream, &error); }}
    RequestType::ReloadRecover => {
      runtime . with_writer_env (|env| {
        handle_reload_recovery_request (stream, request, &env . config); }); }
    RequestType::BeginReloadBatch =>
      handle_begin_reload_batch_request (stream, owned_reload_batch_tokens),
    RequestType::EndReloadBatch =>
      handle_end_reload_batch_request (
        stream, request, owned_reload_batch_tokens),
    RequestType::RecomputeCyclicRoots => {
      if let Err (error) = runtime . with_store_transition (
          false, |env, _| {
            handle_recompute_cyclic_roots_request (stream, env); })
      { send_runtime_error (stream, &error); }}
    RequestType::ApplyCollateral => {
      let mut interactive = runtime . interactive . lock () . unwrap ();
      let InteractiveSession { views, collateral_scheduler, .. } =
        &mut *interactive;
      collateral_scheduler . handle_apply_ack (stream, request, views); }
    RequestType::ViewVisited => {
      let result = value_from_request_sexp ("view-uri", request)
        . and_then (|uri| value_from_request_sexp (
            "visit-sequence", request)
          . and_then (|sequence| sequence . parse::<u64> ()
            . map_err (|_| "Invalid visit-sequence" . to_string ())
            . map (|sequence| (uri, sequence))));
      match result {
        Ok ((uri, sequence)) => {
          runtime . interactive . lock () . unwrap ()
            . collateral_scheduler . note_visit (
              ViewUri::from_client_string (uri), sequence);
          let _ = send_response_with_length_prefix (
            stream, &tag_text_response (
              TcpToClient::ViewVisited, "visit recorded")); }
        Err (error) => send_runtime_error (stream, &error),
      }}
    RequestType::ObservePresentation => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let diff_mode_enabled = interactive . views . diff_mode_enabled;
        let InteractiveSession {
          views, active_source_set, collateral_scheduler, ..
        } = interactive;
        match collateral_scheduler . observe_presentation (
            views, env, active_source_set)
        {
          Ok (changed) => { let _ = send_response_with_length_prefix (
            stream, &tag_text_response (
              TcpToClient::PresentationObserved,
              if changed {
                if diff_mode_enabled {
                  "Git presentation changed; diff-mode views queued"
                } else {
                  "Git presentation changed; diff mode is disabled" }
              } else { "Git presentation is unchanged" })); }
          Err (error) => { send_runtime_error (stream, &format! (
            "Git presentation observation failed: {}", error)); }
        }})
      { send_runtime_error (stream, &error); }}
    RequestType::BeginMaintenance =>
      handle_begin_maintenance_request (stream, request, runtime),
    RequestType::MaintenanceLockedCensus =>
      handle_maintenance_locked_census_request (stream, request, runtime),
    RequestType::RunMaintenanceOrigin =>
      handle_run_maintenance_origin_request (stream, request, runtime),
    RequestType::FinishMaintenanceOrigin =>
      handle_finish_maintenance_origin_request (stream, request, runtime),
    RequestType::MaintenanceArchiveReady =>
      handle_maintenance_archive_ready_request (stream, request, runtime),
    RequestType::MaintenanceArchiveFinalized =>
      handle_maintenance_archive_finalized_request (stream, request, runtime),
    RequestType::MaintenanceArchiveFailed =>
      handle_maintenance_archive_failed_request (stream, request, runtime),
    RequestType::ApproveUndoWaiver =>
      handle_approve_undo_waiver_request (stream, request, runtime),
    RequestType::ApproveMaintenanceScalarRelease =>
      handle_approve_maintenance_scalar_release_request (
        stream, request, runtime),
    RequestType::CancelMaintenance =>
      handle_cancel_maintenance_request (stream, request, runtime),
    RequestType::MaintenanceStatus =>
      handle_maintenance_status_request (stream, runtime),
    RequestType::MaintenanceEvidence =>
      handle_maintenance_evidence_request (stream, request, runtime),
    RequestType::MaintenanceViewSettled =>
      handle_maintenance_view_settled_request (stream, request, runtime),
    RequestType::CompleteMaintenance =>
      handle_complete_maintenance_request (stream, request, runtime),
    RequestType::AcknowledgeTerminalMaintenance =>
      handle_acknowledge_terminal_maintenance_request (
        stream, request, runtime),
  }
}

fn with_query_session (
  runtime  : &ServerRuntime,
  function : impl FnOnce (&SkgEnv, &mut InteractiveSession),
) -> Result<(), String> {
  let lease = runtime . query_lease ()?;
  let mut interactive = runtime . interactive . lock () . unwrap ();
  function (&lease . snapshot . env, &mut interactive);
  Ok (())
}

fn send_runtime_error (stream : &mut TcpStream, error : &str) {
  let _ = send_response_with_length_prefix (
    stream, &tag_terminal_text_response (
      TcpToClient::Error, "failed", error));
}

fn skg_save_policy_refusal (state : &CoordinatorState) -> Option<String> {
  if state . policy () . skg_saves_allowed { return None; }
  let status = match state {
    CoordinatorState::Pending (pending) => match (
      &pending . reason, &pending . candidate)
    {
      (PendingReason::ValidDiskDifference, Some (candidate)) => format! (
        "disk reconciliation is pending for candidate {} (changed primary IDs: {}); run skg-reconcile-pending-changes / :SkgReconcilePendingChanges",
        candidate . id,
        if candidate . changed_primary_ids . is_empty () {
          "none" . into ()
        } else { candidate . changed_primary_ids . join (", ") }),
      (reason, _) => format! (
        "disk reconciliation is pending ({:?}): {}; run skg-reconcile-pending-changes / :SkgReconcilePendingChanges",
        reason,
        if pending . details . is_empty () {
          "see the maintenance status report" . into ()
        } else { pending . details . join ("; ") }),
    },
    CoordinatorState::Active (active) => format! (
      "maintenance incident {} is {:?}; Skg saves remain disabled until its terminal disposition",
      active . incident_id, active . phase),
    CoordinatorState::Terminal (terminal) => format! (
      "maintenance incident {} is terminal ({}) and awaits client acknowledgement",
      terminal . incident_id, terminal . disposition . label ()),
    CoordinatorState::BlockedStoreHealth { reason } => format! (
      "Skg saves are disabled because store health is blocked: {}", reason),
    CoordinatorState::Idle | CoordinatorState::Observing =>
      "Skg save policy is temporarily unavailable" . into (),
  };
  Some (format! ("* NOTHING WAS SAVED\n\n{}\n\nThe rejected save will not be retried automatically.", status))
}

fn install_client_handshake (
  request     : &str,
  env         : &SkgEnv,
  interactive : &mut InteractiveSession,
) -> Result<bool, String> {
  let kind = match value_from_request_sexp ("client-kind", request)? . as_str () {
    "emacs" => ClientKind::Emacs,
    "neovim" => ClientKind::Neovim,
    other => return Err (format! ("unsupported interactive client '{}'", other)),
  };
  let version = value_from_request_sexp ("client-version", request)?;
  let session_id = value_from_request_sexp ("client-session-id", request)?;
  if session_id . is_empty () || session_id . len () > 256 {
    return Err ("client-session-id must contain 1 through 256 bytes" . into ()); }
  let archive_format_version = value_from_request_sexp (
    "archive-format-version", request)? . parse::<u32> ()
    . map_err (|_| "archive-format-version must be an integer" . to_string ())?;
  let native_undo_kind = value_from_request_sexp ("native-undo-kind", request)?;
  let native_undo_version = value_from_request_sexp (
    "native-undo-version", request)?;
  let claimed_source_set = value_from_request_sexp ("source-set", request)?;
  if claimed_source_set != "server-default"
  && claimed_source_set != interactive . active_source_set . name . 0 {
    return Err (format! (
      "client source-set '{}' does not match retained session source-set '{}'",
      claimed_source_set, interactive . active_source_set . name)); }
  // Verification admits the socket but never grants ordinary request/write
  // authority by itself.  Even an empty editor must explicitly close the
  // census phase so reconnect and restart have the same protocol.
  let census_required = true;
  interactive . attached_client = Some (AttachedClient {
    kind,
    version,
    session_id,
    capabilities: ClientCapabilities {
      archive_format_version,
      native_undo_kind,
      native_undo_version,
    },
    census_complete: false,
  });
  if env . config . maintenance_archive_identity . as_os_str () . is_empty () {
    return Err ("server has no validated maintenance archive root" . into ()); }
  Ok (census_required)
}

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
  views_state      : &mut ViewsState,
  active_source_set : &ActiveSourceSet,
  collateral_scheduler : &mut CollateralScheduler,
) {
  let terms : String
    = match value_from_request_sexp ("terms", request)
    { Ok (t) => t,
      Err (e) => { tracing::error! ( "snapshot response: bad terms: {}", e);
                   let _ = send_response_with_length_prefix (
                     stream, &tag_text_response (
                       TcpToClient::Error,
                       &format! ("Snapshot response has bad terms: {}", e)));
                   return; }};
  let buffer_text : String
    = match read_length_prefixed_content (reader)
    { Ok (text) => text,
      Err (e) => { tracing::error! ( "snapshot response: failed to read content: {}", e);
                   let _ = send_response_with_length_prefix (
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
                let _ = send_response_with_length_prefix (
                  stream, &tag_text_response (
                    TcpToClient::Error,
                    "Snapshot response has no pending enrichment"));
                return; }} };
  if payload . terms != terms {
    tracing::warn! ("snapshot response: terms mismatch ('{}' vs '{}')",
                    payload . terms, terms);
    let _ = send_response_with_length_prefix (
      stream, &tag_text_response (
        TcpToClient::Error,
        &format! ("Snapshot terms mismatch: '{}' vs '{}'",
                  payload . terms, terms)));
    return; }
  let client_buffer_id = match value_from_request_sexp (
      "client-buffer-id", request)
  {
    Ok (value) if !value . is_empty () => value,
    Ok (_) => {
      send_runtime_error (stream, "snapshot client-buffer-id is empty");
      return; }
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  let unsigned = |key : &str| -> Result<u64, String> {
    value_from_request_sexp (key, request)? . parse::<u64> ()
      . map_err (|_| format! ("snapshot field '{}' must be unsigned", key))
  };
  let client_graph_generation = match unsigned ("graph-generation") {
    Ok (value) => value,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  let client_presentation_generation = match unsigned (
      "presentation-generation")
  {
    Ok (value) => value,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  let client_server_revision = match unsigned ("server-revision") {
    Ok (value) => value,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  let client_application_token = match unsigned (
      "client-application-token")
  {
    Ok (value) => value,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  if payload . active_source_set . name != active_source_set . name {
    send_runtime_error (
      stream, "search source-set changed before enrichment snapshot");
    return; }
  let uri = ViewUri::SearchView (terms . clone ());
  {
    let Some (state) = views_state . open_views . views . get_mut (&uri)
    else {
      send_runtime_error (stream, "search view closed before enrichment");
      return;
    };
    if state . graph_generation != client_graph_generation
    || state . presentation_generation != client_presentation_generation
    || state . revision != client_server_revision
    || state . client_application_token != client_application_token
    || (state . client_buffer_id . is_some ()
        && state . client_buffer_id . as_deref ()
           != Some (client_buffer_id . as_str ()))
    {
      send_runtime_error (
        stream, "search application authority changed before enrichment");
      return;
    }
    state . client_buffer_id = Some (client_buffer_id . clone ());
  }
  let parse_result : Result<(Tree<MpViewnode>,
                             Vec<BufferValidationError>), String>
    = org_to_uninterpreted_nodes (&buffer_text);
  let mut viewforest : Tree<ViewNode> = match parse_result {
    Ok (( maybePlaced_viewforest, _errors )) =>
      match maybePlaced_to_placed_tree (maybePlaced_viewforest) {
        Ok (f) => f,
        Err (e) => {
          tracing::error! ("snapshot response: check failed: {}", e);
          let _ = send_response_with_length_prefix (
            stream, &tag_text_response (
              TcpToClient::Error,
              &format! ("Snapshot structure check failed: {}", e)));
          return; }},
    Err (e) => {
      tracing::error! ("snapshot response: parse failed: {}", e);
      let _ = send_response_with_length_prefix (
        stream, &tag_text_response (
          TcpToClient::Error,
          &format! ("Snapshot parse failed: {}", e)));
      return; }};
  insert_containerward_ancestries_from_snapshot (
    &mut viewforest, &payload . search_results,
    &payload . ancestry_by_id, &payload . title_and_source_by_id,
    Some (&payload . graph), &payload . config,
    &payload . active_source_set );
  insert_override_ancestries_from_graph (
    &mut viewforest, &payload . search_results,
    &payload . active_source_set, &payload . graph );
  { let root_treeid : NodeId =
      viewforest . root () . id ();
    set_metadata_relationships_in_node_recursive (
      &mut viewforest, root_treeid,
      &payload . graphnodestats,
      &payload . config ); }
  mark_view_roots_parent_absent (
    &mut viewforest );
  set_viewnodestats_in_viewforest (
    &mut viewforest,
    & payload . graphnodestats . container_to_contents,
    & payload . graphnodestats . content_to_containers,
    & payload . config,
    Some (&payload . active_source_set) );
  apply_source_set_to_viewforest (
    &mut viewforest,
    &payload . active_source_set );
  if ! payload . include_ugly_telescopes {
    exclude_ugly_nodes_from_viewforest (
      &mut viewforest, &payload . graph ); }
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
    "search-enrichment", &payload . active_source_set, &rendered_pids,
    &payload . graph, &approved );
  if matches! (release, ScalarReleaseDecision::Challenge { .. }) {
    // Preflight and the load-bearing payload should make this unreachable.
    // Fail closed rather than serialize if a future change violates either.
    tracing::error! (
      "search enrichment reached the release boundary without approval" );
    let _ = send_response_with_length_prefix (
      stream, &tag_text_response (
        TcpToClient::Error,
        "Search enrichment failed its scalar-release check"));
    return; }
  let release_warnings : Vec<String> = match release {
    ScalarReleaseDecision::AllowWithWarning { warning } => vec! [warning],
    _ => Vec::new (), };
  let enriched : String =
    viewforest_to_string ( &viewforest, &payload . config )
    . expect ("search viewforest rendering never fails");
  let offer = match collateral_scheduler . stage_view_application (
      views_state,
      &uri,
      RenderGeneration {
        graph: payload . graph_generation,
        presentation: payload . presentation_generation,
      },
      viewforest)
  {
    Ok (offer) => offer,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  let enriched_sexp = match add_application_offer_to_response (
      &mk_search_enrichment_sexp (
        &terms, &enriched, &release_warnings),
      &offer)
  {
    Ok (response) => response,
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  tracing::debug! (bytes = enriched_sexp . len (),
                   "snapshot response: sending enrichment");
  let _ = send_response_with_length_prefix (
    stream, &enriched_sexp ); }

fn handle_verify_connection_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
  active_source_set_name : &str,
  census_required : bool,
  maintenance : crate::maintenance::MaintenanceCoordinator,
) {
  let _ = send_response_with_length_prefix (
    stream,
    & verify_connection_response (
      &env . config,
      &env . startup_warnings,
      &env . in_rust_graph . load_full (),
      active_source_set_name,
      census_required,
      &maintenance)); }

fn verify_connection_response (
  config   : &SkgConfig,
  warnings : &[(crate::types::misc::ID, TelescopeViolation)],
  selected : &SelectedStoreState,
  active_source_set_name : &str,
  census_required : bool,
  maintenance : &crate::maintenance::MaintenanceCoordinator,
) -> String {
  let atom = |value : &str| -> Sexp {
    Sexp::Atom (Atom::S (value . to_string ())) };
  let field = |key : &str, value : Sexp| -> Sexp {
    Sexp::List (vec! [atom (key), value]) };
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
  let recovery_entries : Vec<Sexp> = pending_incidents_for_config (config)
    . into_iter () . map (|incident| {
      let fatal = incident . draft . fatal . iter () . map (|(pid, reason)|
        Sexp::List (vec![
          field ("pid", atom (pid)),
          field ("reason", atom (reason)),
        ])) . collect ();
      Sexp::List (vec![
        field ("incident-id", atom (&incident . incident_id)),
        field ("fatal", Sexp::List (fatal)),
      ])
    }) . collect ();
  let health = |health : &StoreHealth| -> Sexp { match health {
    StoreHealth::Healthy => atom ("healthy"),
    StoreHealth::Poisoned (reason) => Sexp::List (vec! [
      atom ("poisoned"), atom (reason)]), }};
  Sexp::List (vec! [
    field ("response-type", atom (
      TcpToClient::VerifyConnection . repr_in_client ())),
    field ("content", atom (
      "This is the skg server verifying the connection.")),
    source_inventory_field (config),
    field ("telescope-warnings", Sexp::List (warning_entries)),
    field ("pending-recovery-incidents", Sexp::List (recovery_entries)),
    field ("active-source-set", atom (active_source_set_name)),
    field ("graph-generation", Sexp::Atom (Atom::I (
      selected . graph_generation . get () as i64))),
    field ("manifest-revision", Sexp::Atom (Atom::I (
      selected . manifest_revision . get () as i64))),
    field ("maintenance-epoch", Sexp::Atom (Atom::I (
      maintenance . epoch . get () as i64))),
    field ("maintenance-state", atom (maintenance . state . label ())),
    field ("census-required", atom (
      if census_required { "true" } else { "nil" })),
    field ("maintenance-archive-folder", atom (
      &config . maintenance_archive_folder . to_string_lossy ())),
    field ("maintenance-archive-identity", atom (
      &config . maintenance_archive_identity . to_string_lossy ())),
    field ("typedb-health", health (&selected . typedb_health)),
    field ("tantivy-health", health (&selected . tantivy_health)),
  ]) . to_string ()
}

pub(crate) fn source_inventory_field (config : &SkgConfig) -> Sexp {
  let atom = |value : &str| -> Sexp {
    Sexp::Atom (Atom::S (value . to_string ())) };
  let field = |key : &str, value : Sexp| -> Sexp {
    Sexp::List (vec![atom (key), value]) };
  let entries = config . ordered_sources () . into_iter () . enumerate ()
    . map (|(position, name)| {
      let source = config . sources . get (&name)
        . expect ("ordered source exists");
      Sexp::List (vec![
        field ("name", atom (&name)),
        field ("abbreviation", source . abbreviation . as_deref ()
          . map (&atom) . unwrap_or_else (|| atom ("nil"))),
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
      ])
    }) . collect ();
  field ("source-inventory", Sexp::List (entries))
}

fn handle_shutdown_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  let _ = send_response_with_length_prefix (
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
      &config, &warnings, &selected, "all", false,
      &crate::maintenance::MaintenanceCoordinator::new ());
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
    assert! (response . contains ("(manifest-revision 1)"), "{}", response);
    assert! (! response . contains ("path-outcomes"), "{}", response);
    assert! (response . contains ("(tantivy-health healthy)"), "{}", response);
  }

  #[test]
  fn verification_exposes_only_a_maintenance_state_label () {
    let config = SkgConfig::dummyFromSources (HashMap::new ());
    let selected = SelectedStoreState::initial (
      crate::dbs::in_rust_graph::InRustGraph::new (),
      crate::types::store_state::SelectedPathManifest::default ());
    let mut maintenance = crate::maintenance::MaintenanceCoordinator::new ();
    maintenance . state = CoordinatorState::BlockedStoreHealth {
      reason: "SECRET-MAINTENANCE-PAYLOAD" . into (),
    };
    let response = verify_connection_response (
      &config, &[], &selected, "all", true, &maintenance);
    assert! (response . contains (
      "(maintenance-state blocked-store-health)"), "{}", response);
    assert! (!response . contains ("SECRET-MAINTENANCE-PAYLOAD"),
      "{}", response);
  }
}
