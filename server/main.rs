/// USAGE:
/// There is an optional command-line argument: the config file path.
/// See api-and-formats.md § skgconfig.toml,
/// or the example at data/skgconfig.toml.
///
/// Subcommand: import-org-roam <org-dir> <skg-output-dir> <source-nickname>
/// Converts org-roam .org files to .skg files.

use skg::context::{compute_and_store_context_types, MapToContent, MapToContainers};
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::{InitData, initialize_dbs};
use skg::import_org_roam::import_org_roam_directory;
use skg::serve::serve;
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};

use std::collections::HashSet;
use std::error::Error;
use std::env;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;
use typedb_driver::TypeDBDriver;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Vec<String> = env::args() . collect();

  if args . len() > 1 && args[1] == "import-org-roam" {
    return run_import (&args); }

  let config: SkgConfig = load_config (
    & { let config_path: String =
          if args . len() > 1 { // config from command line, if given
            args[1] . clone()
          } else { // default config
            "data/skgconfig.toml" . to_string() };
        config_path } ) ?;

  init_tracing (&config);

  let listener: TcpListener = // precedes initialize_dbs so Emacs can connect during init
    TcpListener::bind (
      & format! ("0.0.0.0:{}", config . port) ) ?;
  tracing::info! (port = config . port,
                  "Listening for Emacs connections");
  listener . set_nonblocking (true) ?;

  // The "busy signal". See definition of 'busysignal_accept_loop'.
  let init_done: Arc<AtomicBool> =
    Arc::new (AtomicBool::new (false));
  let init_done_clone: Arc<AtomicBool> =
    Arc::clone (&init_done);
  let busysignal_listener: TcpListener =
    listener . try_clone () ?;
  let logs_dir_for_busysignal : String =
    config . logs_dir () . display () . to_string ();
  let busysignal_handle: std::thread::JoinHandle<()> =
    std::thread::spawn ( move || {
      busysignal_accept_loop (
        busysignal_listener,
        init_done_clone,
        &logs_dir_for_busysignal ); } );

  let init : InitData =
    { let _span : tracing::span::EnteredSpan = tracing::info_span! (
        "initialize_dbs") . entered ();
      initialize_dbs (&config) };
  let typedb_driver : Arc<TypeDBDriver> = init . driver;
  let tantivy_index : TantivyIndex = init . tantivy_index;

  // Compute context origin types for search ranking.
  // Fully in-memory: all data is pre-computed from SkgNodes at init.
  { let had_id_set   : HashSet<ID> = init . had_id_set;
    let all_node_ids : HashSet<ID> = init . all_node_ids;
    let link_targets : HashSet<ID> = init . link_targets;
    let map_to_content    : MapToContent    = init . map_to_content;
    let map_to_containers : MapToContainers = init . map_to_containers;
    { let _span : tracing::span::EnteredSpan = tracing::info_span! (
          "context_computation") . entered ();
      match compute_and_store_context_types (
        &tantivy_index,
        &had_id_set,
        &all_node_ids,
        &link_targets,
        &map_to_content,
        &map_to_containers )
      { Ok (_) => {}
        Err (e) => { tracing::warn! (
          error = %e,
          "context computation failed, search results will not have context-based ranking"
        ); }} }}
  // had_id_set, all_node_ids, link_targets, map_to_content, map_to_containers
  // are dropped here, freeing memory before the serve loop.

  init_done . store (true, Ordering::Release);
  busysignal_handle . join ()
    . expect ("busysignal thread panicked");
  listener . set_nonblocking (false) ?;
  tracing::info! ("Server ready.");

  serve (config, typedb_driver, tantivy_index, listener)
    . map_err ( |e| Box::new (e)
                 as Box<dyn Error>) ?;
  Ok (( )) }

/// During initialization, accept connections and reply
/// with an "initializing" message to every request line.
fn busysignal_accept_loop (
  listener  : TcpListener,
  init_done : Arc<AtomicBool>,
  logs_dir  : &str,
) {
  let init_msg : String = format! (
    "((busy . \"Server is initializing, please wait. \
     See {}/server-to-user.log for progress.\"))\n",
    logs_dir );
  while ! init_done . load (Ordering::Acquire) {
    match listener . accept () {
      Ok (( stream, addr )) => {
        let mut stream: std::net::TcpStream = stream;
        tracing::debug! ("Busysignal: connection from {}", addr);
        stream . set_nonblocking (false)
          . ok ();
        // Set a read timeout so we don't block forever
        // if init finishes while we're mid-read.
        stream . set_read_timeout (
          Some (std::time::Duration::from_millis (
            skg::consts::BUSYSIGNAL_READ_TIMEOUT_MS )) )
          . ok ();
        let mut reader: BufReader<std::net::TcpStream> =
          BufReader::new (
            stream . try_clone ()
              . expect ("try_clone in busysignal") );
        let mut line: String = String::new ();
        while let Ok (n) =
          reader . read_line (&mut line) {
            if n == 0 { break; }
            let _: Result<(), _> =
              stream . write_all (init_msg . as_bytes ());
            line . clear ();
            if init_done . load (Ordering::Acquire) {
              break; } } }
      Err (ref e)
        if e . kind ()
           == std::io::ErrorKind::WouldBlock => {
          std::thread::sleep (
            std::time::Duration::from_millis (
              skg::consts::BUSYSIGNAL_POLL_INTERVAL_MS ) ); }
      Err (e) => {
        tracing::warn! ("Busysignal accept error: {}", e); } } } }

fn run_import (
  args : &[String],
) -> Result<(), Box<dyn Error>> {
  if args . len() < 5 {
    tracing::error! ("Usage: cargo run -- import-org-roam <org-dir> <skg-output-dir> <source-nickname>");
    std::process::exit (1); }
  let org_dir    : &Path       = Path::new (&args[2]);
  let output_dir : &Path       = Path::new (&args[3]);
  let source     : SourceName  = SourceName::from (&args[4]);
  let stats : skg::import_org_roam::ImportStats =
    import_org_roam_directory (org_dir, output_dir, &source)?;
  tracing::info! ("{}", stats);
  for err in &stats . errors {
    tracing::warn! ("  {}", err); }
  Ok (( )) }

/// Set up where log output goes. Three destinations:
///
/// 1. Stderr (always on): human-readable lines like
///      2026-03-09T14:00:00 INFO Listening for Emacs connections port=1730
///    Verbosity is controlled by the RUST_LOG environment variable.
///    Default is "info". Examples:
///      RUST_LOG=debug                              — everything
///      RUST_LOG=info,skg::update_buffer=debug      — one module louder
///    See https://docs.rs/tracing-subscriber/latest/tracing_subscriber/filter/struct.EnvFilter.html
///
/// 2. Human-readable file (always on):
///    Same format and content as stderr,
///    appended to <data_root>/logs/server-to-user.log.
///    Useful when the server runs in the background and stderr is lost.
///
/// 3. JSON file (when timing_log = true in skgconfig.toml):
///    Appends one JSON object per log event to <data_root>/logs/server.jsonl.
///    Queryable with jq, e.g.:
///      jq 'select(.fields.message | test("rerender"))' data/logs/server.jsonl
fn init_tracing (
  config : &SkgConfig,
) {
  use tracing_subscriber as tsub;
  use tracing_appender as tapp;
  use tsub::fmt::format::FmtSpan;
  use tsub::Layer;
  let logs_dir : std::path::PathBuf =
    config . logs_dir ();
  std::fs::create_dir_all (&logs_dir) . ok ();
  let env_filter : tsub::EnvFilter =
    tsub::EnvFilter::try_from_default_env ()
    . unwrap_or_else ( |_| tsub::EnvFilter::new ("info") );
  let stderr_is_tty : bool =
    std::io::IsTerminal::is_terminal (&std::io::stderr ());
  let stderr_layer : Box<dyn Layer<_> + Send + Sync> =
    Box::new (
      tsub::fmt::layer ()
      . with_writer (std::io::stderr)
      . with_target (false)
      . with_ansi (stderr_is_tty)
      . with_span_events (FmtSpan::CLOSE) );
  let user_log_appender : tapp::rolling::RollingFileAppender =
    tapp::rolling::never (&logs_dir, "server-to-user.log");
  let user_log_layer : Box<dyn Layer<_> + Send + Sync> =
    Box::new (
      tsub::fmt::layer ()
      . with_writer (user_log_appender)
      . with_target (false)
      . with_ansi (false) // omit the terminal color escape codes stderr receives
      . with_span_events (FmtSpan::CLOSE) );
  let json_layer : Option<Box<dyn Layer<_> + Send + Sync>> =
    if config . timing_log {
      let file_appender : tapp::rolling::RollingFileAppender =
        tapp::rolling::never (&logs_dir, "server.jsonl");
      Some ( Box::new (
        tsub::fmt::layer ()
        . json ()
        . with_writer (file_appender)
        . with_span_events (FmtSpan::CLOSE) ))
    } else { None };
  tsub::registry ()
    . with (env_filter)
    . with (stderr_layer)
    . with (user_log_layer)
    . with (json_layer)
    . init (); }
