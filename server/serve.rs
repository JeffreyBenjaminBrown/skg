/// === Concurrency, Mutexes and Atomicity ===
///
/// An atomic bool is a CPU-level integer (typically 1 machine word) that supports read/modify/write operations guaranteed to be indivisible — no other thread can see a half-written value, even without a mutex.
///
/// In this module, Arc<AtomicBool> is used as the search cancellation flag. The connection thread sets it to true when a new search arrives; the background enrichment  thread checks it before writing to the slot. store and load with Ordering::Relaxed (or SeqCst) are the typical operations — no lock, no blocking, just a single instruction.
///
/// The advantage over Arc<Mutex<bool>>: no lock contention, no possibility of deadlock, and much cheaper (a few nanoseconds vs. potentially microseconds for mutex acquire/release). The tradeoff: atomics only work for simple values — you can't atomically update a String or a struct, which is why the enrichment payload itself uses Arc<Mutex<Option<String>>>.

pub mod handlers;
pub mod parse_metadata_sexp;
pub mod timing_log;
pub mod util;

use crate::dbs::typedb::util::delete_database;
use crate::serve::handlers::close_view::handle_close_view_request;
use crate::serve::handlers::get_file_path::handle_get_file_path_request;
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::title_matches::handle_title_matches_request;
use crate::serve::util::{
  request_type_from_request,
  send_response_with_length_prefix,
  tag_text_response};
use crate::types::misc::{SkgConfig, TantivyIndex};
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

  { // Set up signal handler for Ctrl+C and SIGTERM,
    // for graceful shutdown with database cleanup.
    let driver_for_signal : Arc<TypeDBDriver> = Arc::clone (&typedb_driver);
    let config_for_signal : SkgConfig = config . clone ();
    ctrlc::set_handler ( move || {
      println! ("\nReceived shutdown signal...");
      cleanup_and_shutdown (
        &driver_for_signal,
        &config_for_signal );
    } ) . expect ("Error setting Ctrl+C handler"); }

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
        eprintln!("Connection failed: {e}"); }} }
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

  let enrichment_slot // To update search results once containerward paths (the 'enrichment') have been computed.
    : Arc<Mutex<Option<String>>> =
    Arc::new ( Mutex::new (None) );
  let search_cancelled : Arc<AtomicBool> =
    Arc::new ( AtomicBool::new (false) );

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  println!("Emacs connected: {peer}");
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
        println! ( "Received request: {}", request_header . trim_end () );
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
                &search_cancelled );
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
              println!("{}", error_msg);
              send_response_with_length_prefix (
                &mut stream,
                & tag_text_response (
                  "error", &error_msg )); }}
          Err (err) => {
            println!("Error determining request type: {}",
                     err);
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
        if let Ok (mut guard) = enrichment_slot . try_lock () {
          if let Some (enrichment) = guard . take () {
            println! ("slot drain: sending enrichment ({} bytes)",
                      enrichment . len ());
            send_response_with_length_prefix (
              &mut stream, &enrichment ); } } }
      Err (_) => break, // real error
    } }
  println!("Emacs disconnected: {peer}"); }

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
  println! ( "{}", msg );
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
    println! (
      "Deleting database '{}' before shutdown...",
      config . db_name );

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
          eprintln! ( "Failed to delete database: {}", e );
        }} ); }
  println! ("Shutdown complete.");
  std::process::exit (0); }
