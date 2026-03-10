/// === Concurrency, Mutexes and Atomicity ===
///
/// An atomic bool is a CPU-level integer (typically 1 machine word) that supports read/modify/write operations guaranteed to be indivisible — no other thread can see a half-written value, even without a mutex.
///
/// In this module, Arc<AtomicBool> is used as the search cancellation flag. The connection thread sets it to true when a new search arrives; the background enrichment  thread checks it before writing to the slot. store and load with Ordering::Relaxed (or SeqCst) are the typical operations — no lock, no blocking, just a single instruction.
///
/// The advantage over Arc<Mutex<bool>>: no lock contention, no possibility of deadlock, and much cheaper (a few nanoseconds vs. potentially microseconds for mutex acquire/release). The tradeoff: atomics only work for simple values — you can't atomically update a String or a struct, which is why the enrichment payload itself uses Arc<Mutex<Option<SearchEnrichmentPayload>>>.

pub mod handlers;
pub mod parse_metadata_sexp;
pub mod util;

use crate::dbs::typedb::util::delete_database;
use crate::serve::handlers::close_view::handle_close_view_request;
use crate::serve::handlers::get_file_path::handle_get_file_path_request;
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::title_matches::{
  handle_title_matches_request,
  SearchEnrichmentPayload,
  mk_search_enrichment_sexp};
use crate::update_buffer::graphnodestats::set_metadata_relationships_in_node_recursive;
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  tag_text_response};
use crate::org_to_text::viewnode_forest_to_string;
use crate::serve::handlers::title_matches::render_enriched_search_buffer::insert_containerward_paths_into_search_view;
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::memory::ViewUri;
use crate::types::memory::SkgnodesInMemory;

use std::io::{BufRead, BufReader};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};
use std::time::Duration;
use std::thread;
use typedb_driver::TypeDBDriver;

/// Per-connection ("global") state.
pub struct ConnectionState {
  pub diff_mode_enabled : bool,
  pub memory            : SkgnodesInMemory,
  // PITFALL: If Emacs crashes or the TCP connection drops without sending close-view messages, SkgnodesInMemory is still freed, because ConnectionState is owned by handle_emacs and dropped when the connection loop exits (n == 0). There's no leak. HOWEVER, the pool may briefly hold stale entries for views that were conceptually "closed" by the crash. This is harmless: the entries are freed moments later when ConnectionState drops.
}

/// Pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  config        : SkgConfig,
  typedb_driver : Arc<TypeDBDriver>,
  tantivy_index : TantivyIndex,
  emacs_listener : TcpListener,
) -> std::io::Result<()> {

  for stream_res in emacs_listener . incoming() { // the loop
    match stream_res {
      Ok (stream) => {
        let stream : TcpStream = stream; // for type sig
        let typedb_driver_clone : Arc<TypeDBDriver> =
          Arc::clone (&typedb_driver); // Cloning permits the main thread to keep the driver and index. If they were passed here instead of cloned, their ownership would be moved into the first spawned thread, making them unavailable for the next connection.
        let tantivy_index_clone : TantivyIndex =
          tantivy_index . clone ();
        let config_clone : SkgConfig =
          config        . clone ();
        thread::spawn ( move || {
          handle_emacs (
            stream,
            typedb_driver_clone,
            tantivy_index_clone,
            & config_clone, ) } ); }
      Err (e) => {
        tracing::error!(error = %e, "Connection failed"); }} }
  Ok (( )) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_title_matches_request
/// API: See /api.md
fn handle_emacs (
  mut stream        : TcpStream,
  typedb_driver     : Arc<TypeDBDriver>,
  mut tantivy_index : TantivyIndex,
  config            : &SkgConfig,
) {
  let mut conn_state : ConnectionState =
    ConnectionState {
      diff_mode_enabled : false,
      memory            : SkgnodesInMemory::new () };

  let enrichment_slot // To update search results once the 'enrichment' (containerward paths + graphnodestats) has been computed.
    : Arc<Mutex<Option<SearchEnrichmentPayload>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );

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
          Ok (request_type) => {
            if request_type == "single root content view" {
              handle_single_root_view_request (
                &mut stream,
                &request_header,
                &typedb_driver,
                config,
                &mut conn_state );
            } else if request_type == "save buffer" {
              // PITFALL: Uses the same BufReader that read the request,
              // so that any already-buffered header/payload are visible.
              handle_save_buffer_request (
                &mut reader,
                &mut stream,
                &request_header,
                &typedb_driver,
                config,
                &mut tantivy_index,
                &mut conn_state );
            } else if request_type == "close view" {
              handle_close_view_request (
                &mut stream,
                &request_header,
                &mut conn_state );
            } else if request_type == "title matches" {
              // Cancel any in-flight background search
              search_cancelled . store (true, Ordering::SeqCst);
              handle_title_matches_request (
                &mut stream,
                &request_header,
                &tantivy_index,
                &typedb_driver,
                config,
                &enrichment_slot,
                &search_cancelled,
                &mut conn_state );
            } else if request_type == "verify connection" {
              handle_verify_connection_request(
                &mut stream);
            } else if request_type == "shutdown" {
              handle_shutdown_request( &mut stream,
                                       &typedb_driver,
                                       config);
              // Never returns - exits process
            } else if request_type == "get file path" {
              handle_get_file_path_request( &mut stream,
                                            &request_header,
                                            config);
            } else if request_type == "git diff mode toggle" {
              handle_git_diff_mode_request( &mut stream,
                                            &mut conn_state );
            } else {
              let error_msg : String =
                format! ( "Unsupported request type: {}",
                           request_type );
              tracing::warn!(msg = %error_msg, "Unsupported request type");
              send_response_with_length_prefix (
                &mut stream,
                & tag_text_response (
                  "error", &error_msg )); }}
          Err (err) => {
            tracing::error!(error = %err, "Error determining request type");
            send_response_with_length_prefix (
              &mut stream,
              & tag_text_response (
                "error",
                & format! (
                  "Error determining request type: {}",
                  err ))); } };
        request_header . clear(); }
      Err (ref e)
        if e . kind () == std::io::ErrorKind::WouldBlock
        || e . kind () == std::io::ErrorKind::TimedOut =>
      { // Idle timeout — drain enrichment slot if populated.
        // RACE NOTE: A stale enrichment payload from a cancelled search
        // could theoretically land here if the old background thread
        // passed its cancellation check just before the new search
        // cleared the slot. This is harmless: same terms + unchanged
        // index = identical results, and a fresh enrichment will
        // overwrite shortly after.
        if let Ok (mut guard) = enrichment_slot . try_lock () {
          if let Some (payload) = guard . take () {
            let uri : ViewUri =
              ViewUri::SearchView ( payload . terms . clone () );
            // Remove the view temporarily so we can mutably borrow
            // both its forest and the pool without conflicting borrows
            // on conn_state.memory.
            if let Some (mut vs) = conn_state . memory . views . remove (&uri) {
              insert_containerward_paths_into_search_view (
                &mut vs . forest, &payload . result_ids,
                &payload . paths_by_id, &tantivy_index,
                &mut conn_state . memory . pool, config );
              { let root_treeid : ego_tree::NodeId =
                  vs . forest . root () . id ();
                set_metadata_relationships_in_node_recursive (
                  &mut vs . forest, root_treeid,
                  &payload . graphnodestats, // these graphnodestats were pre-fetched by the enrichment thread.
                  &mut conn_state . memory . pool, config ); }
              let enriched : String =
                viewnode_forest_to_string ( &vs . forest )
                . expect ("search forest rendering never fails");
              let enriched_sexp : String =
                mk_search_enrichment_sexp (
                  &payload . terms, &enriched );
              conn_state . memory . views . insert ( uri, vs );
              tracing::debug! (bytes = enriched_sexp . len (),
                               "slot drain: sending enrichment");
              send_response_with_length_prefix (
                &mut stream, &enriched_sexp ); }} }}
      Err (_) => break, // real error
    } }
  tracing::info!(peer = %peer, "Emacs disconnected"); }

/// Handle git diff mode toggle request.
/// Request format: ((request . "git diff mode toggle"))
fn handle_git_diff_mode_request (
  stream     : &mut TcpStream,
  conn_state : &mut ConnectionState,
) {
  conn_state . diff_mode_enabled = ! conn_state . diff_mode_enabled;
  let msg = if conn_state . diff_mode_enabled
    { "Git diff mode enabled" } else
    { "Git diff mode disabled" };
  tracing::info! ( msg = msg, "Git diff mode toggled" );
  send_response_with_length_prefix (
    stream,
    & tag_text_response ( "git-diff-mode", msg )); }

fn handle_verify_connection_request (
  stream: &mut std::net::TcpStream) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      "verify-connection",
      "This is the skg server verifying the connection." )); }

fn handle_shutdown_request (
  stream        : &mut std::net::TcpStream,
  typedb_driver : &Arc<TypeDBDriver>,
  config        : &SkgConfig,
) {
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      "shutdown", "Server shutting down..." ));
  cleanup_and_shutdown (
    typedb_driver, config ); }

/// Performs cleanup before server shutdown.
/// Deletes the database if delete_on_quit is configured, then exits.
fn cleanup_and_shutdown (
  typedb_driver : &Arc<TypeDBDriver>,
  config        : &SkgConfig,
) {
  if config . delete_on_quit {
    tracing::info! (
      db_name = %config . db_name,
      "Deleting database before shutdown" );

    // Wait briefly to allow any pending operations to complete.
    // This helps ensure the database isn't marked as "in use".
    std::thread::sleep (
      std::time::Duration::from_millis (
        crate::consts::SHUTDOWN_DB_DELETE_DELAY_MS ) );

    futures::executor::block_on ( async {
      if let Err (e) =
        delete_database (
          typedb_driver, & config . db_name )
        . await {
          tracing::error! ( error = %e, "Failed to delete database" );
        }} ); }
  tracing::info! ("Shutdown complete.");
  std::process::exit (0); }
