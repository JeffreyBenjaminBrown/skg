/// === Concurrency, Mutexes and Atomicity ===
///
/// An atomic bool is a CPU-level integer (typically 1 machine word) that supports read/modify/write operations guaranteed to be indivisible — no other thread can see a half-written value, even without a mutex.
///
/// In this module, Arc<AtomicBool> is used as the search cancellation flag. The connection thread sets it to true when a new search arrives; the background enrichment  thread checks it before writing to the slot. store and load with Ordering::Relaxed (or SeqCst) are the typical operations — no lock, no blocking, just a single instruction.
///
/// The advantage over Arc<Mutex<bool>>: no lock contention, no possibility of deadlock, and much cheaper (a few nanoseconds vs. potentially microseconds for mutex acquire/release). The tradeoff: atomics only work for simple values — you can't atomically update a String or a struct, which is why the enrichment payload itself uses Arc<Mutex<Option<SearchEnrichmentPayload>>>.

pub mod handlers;
#[cfg(test)]
mod query_wait_tests;
#[cfg(test)]
mod command_responsiveness_tests;
#[cfg(test)]
mod save_responsiveness_tests;
mod maintenance_connection;
pub mod parse_metadata_sexp;
pub mod protocol;
pub(crate) mod response_sink;
pub mod util;

use crate::from_text::buffer_to_viewnodes::uninterpreted::org_to_uninterpreted_nodes;
use crate::org_to_text::viewforest_to_string;
use crate::serve::handlers::close_view::handle_close_view_request;
use crate::serve::handlers::client_census::{
  handle_client_census_request,
  handle_client_census_texts_request,
  handle_verify_connection_request,
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
  handle_maintenance_protocol_request,
};
use crate::serve::handlers::observation_hint::handle_observation_hint_request;
use crate::serve::handlers::rebuild_dbs::handle_rebuild_dbs_request;
use crate::serve::handlers::recompute_cyclic_roots::recompute_cyclic_roots_with_operation;
use crate::serve::handlers::reload_batch::{
  handle_begin_reload_batch_request,
  handle_end_reload_batch_request,
};
use crate::serve::handlers::reload_recovery::{
  handle_reload_recovery_request,
  load_recovery_journals,
};
use crate::serve::handlers::rerender_all_views::{ handle_git_diff_toggle_and_rerender, handle_rerender_all_views_request};
use crate::serve::handlers::scalar_release::{
  ScalarReleaseDecision,
  decide as decide_scalar_release,
  exclude_ugly_nodes_from_viewforest};
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::source_sets::handle_source_set_request;
use crate::serve::handlers::stage_moves::handle_stage_moves_request;
use crate::serve::handlers::strip_body_whitespace::strip_body_whitespace_with_operation;
use crate::serve::handlers::text_search::render_enriched_search_buffer::{
  insert_containerward_ancestries_from_snapshot,
  insert_override_ancestries_from_graph,
};
use crate::serve::handlers::text_search::{ handle_text_search_request, SearchEnrichmentPayload, mk_search_enrichment_sexp};
use crate::serve::handlers::titles_by_ids::handle_titles_by_ids_request_with_source_set;
use crate::serve::maintenance_connection::{
  current_reconciliation_generation,
  finish_connection_maintenance,
  handle_idle_maintenance_events,
  request_allowed_before_census,
  request_requires_collateral_preemption,
};
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::runtime::{
  InteractiveConnectionGuard,
  ServerRuntime,
};
use crate::runtime::interactive_session::InteractiveSession;
use crate::serve::util::{ begin_request_context, ensure_request_has_terminal_response, read_length_prefixed_content, request_type_from_request, send_response_with_length_prefix, tag_sexp_response, tag_terminal_text_response, tag_text_response, take_send_failure, value_from_request_sexp};
use crate::to_org::util::mark_view_roots_parent_absent;
use crate::types::env::SkgEnv;
use crate::types::errors::BufferValidationError;
use crate::source_sets::ActiveSourceSet;
use crate::source_sets::apply_source_set_to_viewforest;
use crate::types::maybe_placed_viewnode::{MpViewnode,maybePlaced_to_placed_tree};
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
use std::sync::mpsc::{self, Receiver, Sender};
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
      crate::serve::handlers::client_census::validate_protocol_version (request)?;
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

/// Complete process-owned setup before the startup busy-signal is retired.
/// Keeping this separate from `serve` makes "Server ready" mean that a queued
/// connection can actually reach the request loop.
pub fn prepare_runtime (env : SkgEnv) -> std::io::Result<Arc<ServerRuntime>> {
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
  Ok (runtime)
}

/// Pipe TCP input from the sole interactive client and narrow control peers.
pub fn serve (
  runtime        : Arc<ServerRuntime>,
  emacs_listener : TcpListener,
) -> std::io::Result<()> {
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
  crate::serve::util::set_connection_server_session (runtime . server_session_id ());
  let enrichment_slot // To update search results once the 'enrichment' (containerward paths + graphnodestats) has been computed.
    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );
  let mut snapshot_requested : bool = false;
  let mut owned_reload_batch_tokens : HashSet<String> = HashSet::new ();
  let mut query_deliveries : handlers::query_wait::QueryDeliveries = Default::default ();
  let (command_sender, command_receiver) : (Sender<String>, Receiver<String>) = mpsc::channel ();
  let mut role : Option<ConnectionRole> = None;
  let mut seen_reconciliation_generation =
    current_reconciliation_generation ();

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
        tracing::info! ("Received request");
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
        if authenticated . interactive () && !matches! (request_type,
          RequestType::VerifyConnection | RequestType::SaveBuffer
          | RequestType::SaveOperationStatus | RequestType::AcknowledgeSaveResult)
        {
          if let Err (reason) = runtime . validate_session_authority (&request_header) {
            let _ = send_response_with_length_prefix (
              &mut stream, &tag_terminal_text_response (TcpToClient::Error, "failed", &reason));
            // Closing retires any unread payload from an invalid session.
            break;
          }
        }
        if authenticated . interactive ()
           && !request_allowed_before_census (request_type)
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
        && request_requires_collateral_preemption (request_type)
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
          &mut owned_reload_batch_tokens,
          &mut query_deliveries,
          &command_sender);
        if request_type != RequestType::TextSearch {
          let _ = ensure_request_has_terminal_response (
            &mut stream, request_type); }
        drain_command_completions (&mut stream, &command_receiver);
        if let Some (error) = take_send_failure () {
          tracing::warn! (%error,
            "response transport failed; abandoning connection-owned work");
          break; }
        request_header . clear(); }
      Err (ref e)
        if e . kind () == std::io::ErrorKind::WouldBlock
        || e . kind () == std::io::ErrorKind::TimedOut =>
      {
        drain_command_completions (&mut stream, &command_receiver);
        // Idle timeout — if enrichment is ready, ask Emacs
        // for a snapshot of the search buffer so we can integrate
        // ancestry without losing user edits.
        if ! snapshot_requested {
          if let Ok (guard) = enrichment_slot . try_lock () {
            if guard . is_some () {
              // Peek at the terms without taking the payload yet.
              // The payload stays in the slot until the snapshot arrives.
              let payload = guard . as_ref () . unwrap ();
              let terms : String = payload . terms . clone ();
              let view_uri : String = payload . view_uri . repr_in_client ();
              drop (guard); // release the lock
              tracing::debug! ("slot drain: requesting snapshot for '{}'", terms);
              let _ = send_response_with_length_prefix (
                &mut stream,
                &tag_sexp_response (
                  TcpToClient::RequestSnapshot,
                  &Sexp::List (vec![
                    Sexp::List (vec![
                      Sexp::Atom (Atom::S ("content" . into ())),
                      Sexp::Atom (Atom::S (terms)),
                    ]),
                    Sexp::List (vec![
                      Sexp::Atom (Atom::S ("view-uri" . into ())),
                      Sexp::Atom (Atom::S (view_uri)),
                    ]),
                  ]) . to_string () ));
              snapshot_requested = true; }}}
        handle_idle_maintenance_events (
          &mut stream,
          &runtime,
          role . as_ref () . map (ConnectionRole::interactive)
            . unwrap_or (false),
          snapshot_requested,
          &mut seen_reconciliation_generation);
        if role . as_ref () . is_some_and (ConnectionRole::interactive)
        && runtime . interactive . lock () . unwrap () . attached_client . as_ref ()
          . is_some_and (|client| client . census_complete) {
          handlers::query_wait::drain (&runtime, &mut stream, &mut query_deliveries);
        }
        if let Some (error) = take_send_failure () {
          tracing::warn! (%error,
            "server-push transport failed; retaining session work");
          break; }
      }
      Err (_) => break, // real error
    }}
  finish_connection_maintenance (
    &runtime,
    role . as_ref () . map (ConnectionRole::interactive)
      . unwrap_or (false),
    &mut owned_reload_batch_tokens);
  tracing::info!(peer = %peer, "Skg socket disconnected"); }

fn drain_command_completions (
  stream : &mut TcpStream,
  completions : &Receiver<String>,
) {
  // Search continuations retain their foreground context between requests.
  if util::request_context_active () { return; }
  while let Ok (response) = completions . try_recv () {
    if send_response_with_length_prefix (stream, &response) . is_err () { break; }
  }
}

#[allow(clippy::too_many_arguments)]
fn dispatch_request (
  runtime           : &Arc<ServerRuntime>,
  reader            : &mut BufReader<TcpStream>,
  stream            : &mut TcpStream,
  request           : &str,
  request_type      : RequestType,
  enrichment_slot   : &Arc<Mutex<Option<SearchEnrichmentPayload>>>,
  search_cancelled  : &Arc<AtomicBool>,
  snapshot_requested : &mut bool,
  owned_reload_batch_tokens : &mut HashSet<String>,
  query_deliveries : &mut handlers::query_wait::QueryDeliveries,
  command_sender : &Sender<String>,
) {
  match request_type {
    RequestType::QueryWait | RequestType::QueryWaitStatus
    | RequestType::QueryWaitCancel | RequestType::QueryWaitApplied =>
      handlers::query_wait::handle_request (runtime, stream, request, request_type, query_deliveries),
    RequestType::SingleRootContentView => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession { views, active_source_set, .. } = interactive;
        handle_single_root_view_request (
          stream, request, env, views, active_source_set, runtime); })
      { send_runtime_error (stream, &error); }}
    RequestType::SaveBuffer => {
      // Consume framing before admission, so even a refused request leaves
      // the socket ready for its next command.
      let content = match crate::serve::util::read_length_prefixed_content (reader) {
        Ok (content) => content,
        Err (error) => {
          let response = crate::serve::handlers::save_buffer::save_refusal_response (
            &error . to_string (), request);
          let _ = send_response_with_length_prefix (stream, &response);
          return;
        }
      };
      handlers::save_buffer::worker::dispatch_save_request (
        stream, request, &content, runtime, command_sender);
    }
    RequestType::SaveOperationStatus | RequestType::AcknowledgeSaveResult => {
      crate::runtime::save_operations::handle_save_operation_request (
        stream, request, runtime, request_type == RequestType::AcknowledgeSaveResult);
    }
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
        enrichment_slot, search_cancelled, views, active_source_set, runtime); }
    RequestType::VerifyConnection => {
      handle_verify_connection_request (stream, request, runtime); }
    RequestType::ClientCensus => {
      let snapshot = runtime . selected_snapshot ();
      let writes_allowed = runtime . maintenance_snapshot ()
        . state . policy () . skg_saves_allowed;
      let mut interactive = runtime . interactive . lock () . unwrap ();
      handle_client_census_request (
        reader, stream, request, &snapshot . env, &mut interactive,
        writes_allowed, runtime); }
    RequestType::ClientCensusTexts => {
      let snapshot = runtime . selected_snapshot ();
      let writes_allowed = runtime . maintenance_snapshot ()
        . state . policy () . skg_saves_allowed;
      let mut interactive = runtime . interactive . lock () . unwrap ();
      handle_client_census_texts_request (
        reader, stream, request, &snapshot . env, &mut interactive,
        writes_allowed, runtime); }
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
          stream, request, &env . config,
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
        let InteractiveSession {
          views, active_source_set, collateral_scheduler, ..
        } = interactive;
        handle_source_set_request (
          stream, request, env, views, active_source_set,
          collateral_scheduler,
          enrichment_slot, search_cancelled); })
      { send_runtime_error (stream, &error); }}
    RequestType::HeraldRules => handle_herald_rules_request (stream),
    RequestType::GitDiffModeToggle => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession {
          views, active_source_set, collateral_scheduler, ..
        } = interactive;
        handle_git_diff_toggle_and_rerender (
          stream, request, env, views, active_source_set,
          collateral_scheduler); })
      { send_runtime_error (stream, &error); }}
    RequestType::ExportToOrg => {
      let snapshot = runtime . selected_snapshot ();
      handle_export_to_org_request (
        stream, &snapshot . env . config, &snapshot . selected . graph, request); }
    RequestType::RebuildDbs => {
      handle_rebuild_dbs_request (stream, runtime); }
    RequestType::StripBodyWhitespace =>
      handlers::durable_command::dispatch_command_request (
        stream, request, runtime, TcpToClient::StripBodyWhitespace,
        strip_body_whitespace_with_operation, command_sender),
    RequestType::RerenderAllViews => {
      if let Err (error) = with_query_session (runtime, |env, interactive| {
        let InteractiveSession {
          views, active_source_set, collateral_scheduler, ..
        } = interactive;
        handle_rerender_all_views_request (
          stream, request, env, views, active_source_set,
          collateral_scheduler); })
      { send_runtime_error (stream, &error); }}
    RequestType::ReloadPaths => {
      handle_observation_hint_request (stream, request, runtime); }
    RequestType::ReloadRecover => {
      runtime . with_writer_env (|env| {
        handle_reload_recovery_request (stream, request, &env . config); }); }
    RequestType::BeginReloadBatch =>
      handle_begin_reload_batch_request (stream, owned_reload_batch_tokens),
    RequestType::EndReloadBatch =>
      handle_end_reload_batch_request (
        stream, request, runtime, owned_reload_batch_tokens),
    RequestType::RecomputeCyclicRoots =>
      handlers::durable_command::dispatch_command_request (
        stream, request, runtime, TcpToClient::RecomputeCyclicRoots,
        recompute_cyclic_roots_with_operation, command_sender),
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
      match runtime . observe_git_presentation () {
        Ok ((changed, diff_mode_enabled)) => {
          let _ = send_response_with_length_prefix (
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
      }}
    RequestType::BeginMaintenance
    | RequestType::MaintenanceLockedCensus
    | RequestType::RunMaintenanceOrigin
    | RequestType::FinishMaintenanceOrigin
    | RequestType::MaintenanceArchiveReady
    | RequestType::MaintenanceArchiveFinalized
    | RequestType::MaintenanceArchiveFailed
    | RequestType::ApproveUndoWaiver
    | RequestType::ApproveMaintenanceScalarRelease
    | RequestType::CancelMaintenance
    | RequestType::MaintenanceStatus
    | RequestType::RetryMaintenance
    | RequestType::MaintenanceEvidence
    | RequestType::MaintenanceViewSettled
    | RequestType::CompleteMaintenance
    | RequestType::AcknowledgeTerminalMaintenance =>
      handle_maintenance_protocol_request (
        stream, request, runtime, request_type),
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
  let snapshot_view_uri = match value_from_request_sexp (
      "view-uri", request)
  {
    Ok (value) => ViewUri::from_client_string (value),
    Err (error) => { send_runtime_error (stream, &error); return; }
  };
  if snapshot_view_uri != payload . view_uri {
    send_runtime_error (stream, "snapshot search view URI changed");
    return;
  }
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
  let uri = payload . view_uri . clone ();
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
      &payload . graph, &mut viewforest, root_treeid,
      &payload . graphnodestats,
      &payload . config ); }
  mark_view_roots_parent_absent (
    &mut viewforest );
  set_viewnodestats_in_viewforest (
    &payload . graph, &mut viewforest,
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
    payload . save_base, views_state,
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

fn handle_shutdown_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  let _ = send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::Shutdown, "Server shutting down..." ));
  cleanup_and_shutdown (env); }

/// Pending source effects remain recoverable from the durable save journal.
fn cleanup_and_shutdown (_env : &SkgEnv) {
  tracing::info! ("Shutdown complete.");
  std::process::exit (0); }
