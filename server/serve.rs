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
use crate::serve::handlers::get_file_path::handle_get_file_path_request;
use crate::serve::handlers::rebuild_dbs::handle_rebuild_dbs_request;
use crate::serve::handlers::rerender_all_views::{ handle_git_diff_toggle_and_rerender, handle_rerender_all_views_request};
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::text_search::render_enriched_search_buffer::insert_containerward_ancestries_into_search_view;
use crate::serve::handlers::text_search::{ handle_text_search_request, SearchEnrichmentPayload, mk_search_enrichment_sexp};
use crate::serve::handlers::titles_by_ids::handle_titles_by_ids_request;
use crate::serve::protocol::{RequestType, TcpToClient};
use crate::serve::util::{ read_length_prefixed_content, request_type_from_request, send_response_with_length_prefix, tag_text_response, value_from_request_sexp};
use crate::to_org::util::set_view_root_births_from_graphstats;
use crate::types::env::SkgEnv;
use crate::types::errors::BufferValidationError;
use crate::types::maybe_placed_viewnode::{MaybePlacedViewnode,maybePlaced_to_placed_tree};
use crate::types::viewnode::ViewNode;
use crate::types::views_state::{OpenViews, ViewUri};
use crate::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;

use ego_tree::{NodeId, Tree};
use std::io::{BufRead, BufReader};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, MutexGuard};
use std::thread;
use std::time::Duration;

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

  let enrichment_slot // To update search results once the 'enrichment' (containerward paths + graphnodestats) has been computed.
    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );
  let mut snapshot_requested : bool = false;

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
        match request_type_from_request (&request_header) {
          // For most types of requests, the header is the entire request, and the reader is no longer needed. For saving, though, the reader still contains the buffer content, so it is passed along.
          Ok (RequestType::SingleRootContentView) =>
            handle_single_root_view_request (
              &mut stream,
              &request_header,
              &env,
              &mut views_state ),
          Ok (RequestType::SaveBuffer) =>
            // PITFALL: Uses the same BufReader that read the request,
            // so that any already-buffered header/payload are visible.
            handle_save_buffer_request (
              &mut reader,
              &mut stream,
              &request_header,
              &mut env,
              &mut views_state ),
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
              &mut views_state ); }
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
              &mut views_state ); }
          Ok (RequestType::VerifyConnection) =>
            handle_verify_connection_request (
              &mut stream ),
          Ok (RequestType::Shutdown) =>
            // Never returns - exits process
            handle_shutdown_request ( &mut stream, &env ),
          Ok (RequestType::GetFilePath) =>
            handle_get_file_path_request ( &mut stream,
                                           &request_header,
                                           &env . config ),
          Ok (RequestType::TitlesByIds) =>
            handle_titles_by_ids_request (
              &mut stream, &request_header,
              &env . tantivy_index, &env . config,
              views_state . diff_mode_enabled ),
          Ok (RequestType::GitDiffModeToggle) =>
            handle_git_diff_toggle_and_rerender (
              &mut stream,
              &env,
              &mut views_state ),
          Ok (RequestType::RebuildDbs) =>
            handle_rebuild_dbs_request ( &mut stream,
                                         &mut env,
                                         &mut views_state ),
          Ok (RequestType::RerenderAllViews) =>
            handle_rerender_all_views_request (
              &mut stream,
              &env,
              &mut views_state ),
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
              snapshot_requested = true; }} }}
      Err (_) => break, // real error
    }}
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
) {
  let terms : String
    = match value_from_request_sexp ("terms", request)
    { Ok (t) => t,
      Err (e) => { tracing::error! ( "snapshot response: bad terms: {}", e);
                   return; }};
  let buffer_text : String
    = match read_length_prefixed_content (reader)
    { Ok (text) => text,
      Err (e) => { tracing::error! ( "snapshot response: failed to read content: {}", e);
                   return; }};
  let payload : SearchEnrichmentPayload = {
    let mut guard : MutexGuard<Option<SearchEnrichmentPayload>> =
      enrichment_slot . lock () . unwrap ();
    match guard . take () {
      Some (p) => p,
      None => { tracing::warn! (
                  "snapshot response: no enrichment payload");
                return; }} };
  if payload . terms != terms {
    tracing::warn! ("snapshot response: terms mismatch ('{}' vs '{}')",
                    payload . terms, terms);
    return; }
  let parse_result : Result<(Tree<MaybePlacedViewnode>,
                             Vec<BufferValidationError>), String>
    = org_to_uninterpreted_nodes (&buffer_text);
  let mut viewforest : Tree<ViewNode> = match parse_result {
    Ok (( maybePlaced_viewforest, _errors )) =>
      match maybePlaced_to_placed_tree (maybePlaced_viewforest) {
        Ok (f) => f,
        Err (e) => {
          tracing::error! ("snapshot response: check failed: {}", e);
          return; }},
    Err (e) => {
      tracing::error! ("snapshot response: parse failed: {}", e);
      return; }};
  insert_containerward_ancestries_into_search_view (
    &mut viewforest, &payload . search_results,
    &payload . ancestry_by_id, &env . tantivy_index,
    &env . config );
  { let root_treeid : NodeId =
      viewforest . root () . id ();
    set_metadata_relationships_in_node_recursive (
      &mut viewforest, root_treeid,
      &payload . graphnodestats,
      &env . config ); }
  set_view_root_births_from_graphstats (
    &mut viewforest );
  let enriched : String =
    viewforest_to_string ( &viewforest, &env . config )
    . expect ("search viewforest rendering never fails");
  let enriched_sexp : String =
    mk_search_enrichment_sexp ( &terms, &enriched );
  { let uri : ViewUri = // update ViewsState with enriched viewforest
      ViewUri::SearchView ( terms . clone () );
    views_state . open_views . update_view ( &uri, viewforest ); }
  tracing::debug! (bytes = enriched_sexp . len (),
                   "snapshot response: sending enrichment");
  send_response_with_length_prefix (
    stream, &enriched_sexp ); }

fn handle_verify_connection_request (
  stream: &mut std::net::TcpStream) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::VerifyConnection,
      "This is the skg server verifying the connection." )); }

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
