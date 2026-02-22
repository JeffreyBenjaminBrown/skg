pub mod handlers;
pub mod parse_metadata_sexp;
pub mod timing_log;
pub mod util;

use crate::dbs::neo4j::util::delete_database;
use crate::serve::handlers::get_file_path::handle_get_file_path_request;
use crate::serve::handlers::save_buffer::handle_save_buffer_request;
use crate::serve::handlers::single_root_view::handle_single_root_view_request;
use crate::serve::handlers::title_matches::handle_title_matches_request;
use crate::serve::util::{request_type_from_request, send_response};
use crate::types::misc::{SkgConfig, TantivyIndex};

use std::io::{BufRead, BufReader};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::Arc;
use std::thread;
use neo4rs::Graph;
use tokio::runtime::Handle;

/// Per-connection state for managing diff mode,
/// and (probably, later) other session-specific settings.
pub struct ConnectionState {
  pub diff_mode_enabled: bool,
}

/// Pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  config        : SkgConfig,
  graph         : Arc<Graph>,
  tantivy_index : TantivyIndex,
  handle        : Handle,
) -> std::io::Result<()> {

  { // Set up signal handler for Ctrl+C and SIGTERM,
    // for graceful shutdown with database cleanup.
    let graph_for_signal : Arc<Graph> = Arc::clone ( &graph );
    let config_for_signal : SkgConfig = config . clone ();
    let handle_for_signal : Handle = handle.clone();
    ctrlc::set_handler ( move || {
      println! ( "\nReceived shutdown signal..." );
      cleanup_and_shutdown (
        &graph_for_signal,
        &config_for_signal,
        &handle_for_signal );
    } ) . expect ( "Error setting Ctrl+C handler" ); }

  // Bind to TCP port for Rust-Emacs API communication.
  let emacs_listener : TcpListener =
    TcpListener::bind (
      & format!("0.0.0.0:{}", config.port) )?;
  println!("Listening on port {} for Emacs connections...",
           config.port);

  for stream_res in emacs_listener.incoming() { // the loop
    match stream_res {
      Ok(stream) => {
        let stream : TcpStream = stream; // for type sig
        let graph_clone : Arc<Graph> =
          Arc::clone( &graph ); // Cloning permits the main thread to keep the graph and index. If they were passed here instead of cloned, their ownership would be moved into the first spawned thread, making them unavailable for the next connection.
        let tantivy_index_clone : TantivyIndex =
          tantivy_index . clone ();
        let config_clone : SkgConfig =
          config        . clone ();
        let handle_clone : Handle = handle.clone();
        thread::spawn ( move || {
          handle_emacs (
            stream,
            graph_clone,
            tantivy_index_clone,
            & config_clone,
            handle_clone, ) } ); }
      Err(e) => {
        eprintln!("Connection failed: {e}"); }} }
  Ok (( )) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_title_matches_request
/// API: See /api.md
fn handle_emacs (
  mut stream    : TcpStream,
  graph         : Arc<Graph>,
  tantivy_index : TantivyIndex,
  config        : &SkgConfig,
  handle        : Handle,
) {
  let mut conn_state : ConnectionState =
    ConnectionState { diff_mode_enabled: false, };

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  println!("Emacs connected: {peer}");
  let mut reader
    : BufReader<TcpStream> // the underlying stream, but buffered
    = BufReader::new (
      stream . try_clone() . unwrap() );
  let mut request : String = String::new();
  while let Ok(n) =
    reader.read_line( &mut request ) { // reads until a newline
      if n == 0 { break; } // emacs disconnected
      println! ( "Received request: {}", request.trim_end () );
      match request_type_from_request( &request ) {
        Ok(request_type) => {
          if request_type == "single root content view" {
            handle_single_root_view_request (
              &mut stream,
              &request,
              &graph,
              config,
              conn_state . diff_mode_enabled,
              &handle );
          } else if request_type == "save buffer" {
            // PITFALL: Uses the same BufReader that read the request,
            // so that any already-buffered header/payload are visible.
            handle_save_buffer_request (
              &mut reader,
              &mut stream,
              &graph,
              config,
              &tantivy_index,
              conn_state . diff_mode_enabled,
              &handle );
          } else if request_type == "title matches" {
            handle_title_matches_request( &mut stream,
                                          &request,
                                          &tantivy_index);
          } else if request_type == "verify connection" {
            handle_verify_connection_request(
              &mut stream);
          } else if request_type == "shutdown" {
            handle_shutdown_request( &mut stream,
                                     &graph,
                                     config,
                                     &handle);
            // Never returns - exits process
          } else if request_type == "get file path" {
            handle_get_file_path_request( &mut stream,
                                          &request,
                                          config);
          } else if request_type == "git diff mode toggle" {
            handle_git_diff_mode_request( &mut stream,
                                          &mut conn_state );
          } else {
            let error_msg : String =
              format! ( "Unsupported request type: {}",
                         request_type );
            println!("{}", error_msg);
            send_response ( &mut stream, &error_msg ); }}
        Err(err) => {
          println!("Error determining request type: {}",
                   err);
          send_response(
            &mut stream, &format!(
              "Error determining request type: {}",
              err)); } };
      request.clear(); }
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
  send_response ( stream, msg ); }

fn handle_verify_connection_request (
  stream: &mut std::net::TcpStream) {
  send_response (
    stream,
    "This is the skg server verifying the connection."); }

fn handle_shutdown_request (
  stream : &mut std::net::TcpStream,
  graph  : &Arc<Graph>,
  config : &SkgConfig,
  handle : &Handle,
) {
  send_response ( stream,
                  "Server shutting down..." );
  cleanup_and_shutdown (
    graph, config, handle ); }

/// Performs cleanup before server shutdown.
/// Deletes the database if delete_on_quit is configured, then exits.
fn cleanup_and_shutdown (
  graph  : &Arc<Graph>,
  config : &SkgConfig,
  handle : &Handle,
) {
  if config . delete_on_quit {
    println! (
      "Deleting database '{}' before shutdown...",
      config . db_name );

    // Wait briefly to allow any pending operations to complete.
    // This helps ensure the database isn't marked as "in use".
    std::thread::sleep (
      std::time::Duration::from_millis ( 100 ) );

    handle.block_on ( async {
      if let Err ( e ) =
        delete_database ( graph )
        . await {
          eprintln! ( "Failed to delete database: {}", e );
        }} ); }
  println! ( "Shutdown complete." );
  std::process::exit (0); }
