/// USAGE:
/// There is an optional command-line argument: the config file path.
/// See api-and-formats.md § skgconfig.toml,
/// or the example at data/skgconfig.toml.
///
/// Subcommand: import-org-roam <org-dir> <skg-output-dir> <source-name>
/// Converts org-roam .org files to .skg files.

use skg::consts::{BUSYSIGNAL_POLL_INTERVAL_MS, BUSYSIGNAL_READ_TIMEOUT_MS};
use skg::context::{compute_and_store_context_types, MapToContent, MapToContainers};
use skg::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::export_org::{
  export_candidate_pids, export_to_org, ExportReport};
use skg::source_sets::{ActiveSourceSet, SourceSetName};
use skg::dbs::init::{InitContextHandoff, initialize_dbs};
use skg::dbs::in_rust_graph::InRustGraphHandle;
use skg::types::env::SkgEnv;
use skg::import_org_roam::{ImportStats, import_org_roam_directory};
use skg::serve::{prepare_runtime, serve};
use skg::sound::play_ready_sound_in_background;
use skg::types::misc::{ID, SkgConfig, SourceName, TantivyIndex};
use skg::types::nodes::complete::NodeComplete;

use std::collections::HashSet;
use std::error::Error;
use std::env;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

fn main() -> Result<(), Box<dyn Error>> {
  let args: Vec<String> = env::args() . collect();

  if args . len() > 1 && args[1] == "import-org-roam" {
    tracing_subscriber::fmt()
      . with_env_filter (
        tracing_subscriber::EnvFilter::try_from_default_env ()
        . unwrap_or_else ( |_| tracing_subscriber::EnvFilter::new ("info") ) )
      . init ();
    return run_import (&args); }

  if args . len() > 1 && args[1] == "export-org" {
    tracing_subscriber::fmt()
      . with_env_filter (
        tracing_subscriber::EnvFilter::try_from_default_env ()
        . unwrap_or_else ( |_| tracing_subscriber::EnvFilter::new ("info") ) )
      . init ();
    return run_export_org (&args); }

  if args . len() > 1 && args[1] == "check-config" {
    // Pre-flight used by bash/start-servers.sh: run the same
    // 'load_config' the server runs at startup (TOML parse +
    // source-set/path validation) and exit 0/1, reporting a bad config
    // exactly the way the real startup does (via 'die_bad_config'). No
    // tracing or file walk.
    let config_path: String =
      if args . len() > 2 { args[2] . clone() }
      else { "data/skgconfig.toml" . to_string() };
    return match load_config (&config_path) {
      Ok (_)  => Ok (( )),
      Err (e) => die_bad_config (&config_path, e) }; }

  // The real server start. load_config runs on EVERY launch -- via
  // start-servers.sh, a cargo-watch auto-restart, or a manual
  // 'cargo run' -- so routing its failure through 'die_bad_config'
  // guarantees a bad skgconfig.toml is reported clearly (in red on a
  // terminal) no matter how skg was (re)started, rather than the
  // default '?' that dumps an unreadable Debug blob of the whole file.
  let config_path: String =
    if args . len() > 1 { args[1] . clone() } // config from command line, if given
    else { "data/skgconfig.toml" . to_string() }; // default config
  let config: SkgConfig = match load_config (&config_path) {
    Ok (c)  => c,
    Err (e) => die_bad_config (&config_path, e) };

  init_tracing (&config);

  { // Log which binary this is and how old it is, so a stale
    // long-running server (one predating a code change) is obvious
    // from the logs.
    let exe : std::path::PathBuf =
      std::env::current_exe ()
      . unwrap_or_else ( |_| "unknown" . into () );
    let age : String =
      exe . metadata ()
      . and_then ( |m| m . modified () )
      . ok () . and_then ( |t| t . elapsed () . ok () )
      . map ( |d| { let s : u64 = d . as_secs ();
                    format! ( "{}h {}m {}s",
                              s / 3600, (s % 3600) / 60, s % 60 ) } )
      . unwrap_or_else ( || "unknown" . to_string () );
    tracing::info! ( exe = %exe . display (),
                     binary_age = %age,
                     "Server binary" ); }

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

  install_shutdown_signal_handler ();
  let save_recovery = skg::runtime::recover_source_effects_before_startup (&config)?;

  let ( env,
        InitContextHandoff { had_id_set,
                             all_node_ids,
                             link_dests,
                             map_to_content,
                             map_to_containers },
        nodes )
      : (SkgEnv, InitContextHandoff, Vec<NodeComplete>) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span! (
        "initialize_dbs") . entered ();
      initialize_dbs (&config) };
  drop (nodes); // 'initialize_dbs' checked and used them; nothing here needs them.

  compute_context_rankings (
    &env . tantivy_index, &env . in_rust_graph,
    had_id_set, all_node_ids,
    link_dests, map_to_content, map_to_containers );

  skg::runtime::commit_recovered_source_effects (
    save_recovery, env . in_rust_graph . load_full () . graph_generation . get ())?;

  // Runtime construction includes exact Git-presentation seeding, recovery
  // journal loading, watcher installation and worker startup.  Connections
  // must continue receiving the initializing response until all of it is
  // complete; otherwise TCP can queue a request behind work performed after a
  // misleading "Server ready" message.
  let runtime = prepare_runtime (env)
    . map_err (|error| Box::new (error) as Box<dyn Error>)?;

  init_done . store (true, Ordering::Release);
  busysignal_handle . join ()
    . expect ("busysignal thread panicked");
  listener . set_nonblocking (false) ?;
  tracing::info! ("Server ready.");
  if config . beep_when_server_becomes_available {
    play_ready_sound_in_background (); }

  serve (runtime, listener)
    . map_err ( |e| Box::new (e)
                 as Box<dyn Error>) ?;
  Ok (( )) }

/// Report a config-load failure and exit(1). Bold red when stderr is a
/// terminal; plain text otherwise (e.g. cargo-watch.log), so the
/// message reads cleanly wherever skg's stderr points. Routing every
/// 'load_config' failure here -- the real startup and the
/// 'check-config' pre-flight alike -- means a bad skgconfig.toml is
/// reported identically no matter how skg was launched.
fn die_bad_config (
  config_path : &str,
  err         : Box<dyn Error>,
) -> ! {
  let msg : String = format! (
    "Invalid skg config '{}': {}", config_path, err);
  if std::io::IsTerminal::is_terminal (&std::io::stderr ()) {
    eprintln! ("\x1b[1;31m{}\x1b[0m", msg); // bold red on a terminal
  } else {
    eprintln! ("{}", msg); }
  std::process::exit (1); }

/// During initialization, accept connections and reply
/// with an "initializing" message to every request line.
fn busysignal_accept_loop (
  listener  : TcpListener,
  init_done : Arc<AtomicBool>,
  logs_dir  : &str,
) {
  let init_msg : String = format! (
    "((busy-initializing . \"Server is initializing, please wait. \
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
            BUSYSIGNAL_READ_TIMEOUT_MS )) )
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
              BUSYSIGNAL_POLL_INTERVAL_MS ) ); }
      Err (e) => {
        tracing::warn! ("Busysignal accept error: {}", e); } } } }

/// Source effects are recovered from durable intent on the next startup.
fn install_shutdown_signal_handler () {
  ctrlc::set_handler (|| {
    tracing::info! ("Received shutdown signal.");
    std::process::exit (0);
  }) . expect ("Error setting shutdown signal handler"); }

/// Compute context origin types for search ranking.
/// Fully in-Rust-graph: all data is pre-computed from NodeCompletes at init.
/// Consumes (and frees) the large lookup maps after use.
fn compute_context_rankings (
  tantivy_index     : &TantivyIndex,
  graph             : &InRustGraphHandle,
  had_id_set        : HashSet<ID>,
  all_node_ids      : HashSet<ID>,
  link_dests      : HashSet<ID>,
  map_to_content    : MapToContent,
  map_to_containers : MapToContainers,
) {
  let _span : tracing::span::EnteredSpan = tracing::info_span! (
    "context_computation") . entered ();
  match compute_and_store_context_types (
    tantivy_index,
    &had_id_set,
    &all_node_ids,
    &link_dests,
    &map_to_content,
    &map_to_containers )
  { Ok (computation) => {
      let old = graph . load_full ();
      graph . store (Arc::new (
        old . with_cyclic_roots (computation . cyclic_roots))); }
    Err (e) => { tracing::warn! (
      error = %e,
      "context computation failed, \
       search results will not have context-based ranking"
    ); }} }

fn run_import (
  args : &[String],
) -> Result<(), Box<dyn Error>> {
  if args . len() < 5 {
    tracing::error! ("Usage: cargo run -- import-org-roam <org-dir> <skg-output-dir> <source-name>");
    std::process::exit (1); }
  let org_dir    : &Path       = Path::new (&args[2]);
  let output_dir : &Path       = Path::new (&args[3]);
  let source     : SourceName  = SourceName::from (&args[4]);
  let stats : ImportStats =
    import_org_roam_directory (org_dir, output_dir, &source)?;
  tracing::info! ("{}", stats);
  for err in &stats . errors {
    tracing::warn! ("  {}", err); }
  println! ( // reminder printed in yellow
    "\x1b[1;33mRemember to commit the imported .skg files in {:?} to git, \
     so that git-diff mode can detect changes.\x1b[0m",
    output_dir );
  Ok (( )) }

/// Secondary, TypeDB-free entry point for the org export. The
/// primary, documented path is the Emacs command
/// 'skg-export-some-to-org' (the "export to org" TCP endpoint); this
/// subcommand runs the same core for scripting and testing.
///
/// USAGE: cargo run --bin skg -- export-org [config-path] [source-set] [output-dir] [--include-ugly-telescopes]
/// (source-set defaults to "all", output-dir to "org-exports").
/// output-dir is resolved against the current working directory; an
/// absolute path is used as-is.
fn run_export_org (
  args : &[String],
) -> Result<(), Box<dyn Error>> {
  let include_ugly_telescopes : bool =
    args . iter () . any ( |arg| arg == "--include-ugly-telescopes" );
  let positional : Vec<&String> = args . iter () . skip (2)
    . filter ( |arg| arg . as_str () != "--include-ugly-telescopes" )
    . collect ();
  let config_path : String =
    if ! positional . is_empty () { positional[0] . clone () }
    else { "data/skgconfig.toml" . to_string() };
  let config : SkgConfig =
    load_config (&config_path) ?;
  let set_name : SourceSetName =
    if positional . len () > 1 {
      SourceSetName::from (positional[1] . as_str()) }
    else { SourceSetName::from ("all") };
  let output_dir : String =
    if positional . len () > 2 { positional[2] . clone () }
    else { "org-exports" . to_string() };
  let active : ActiveSourceSet =
    ActiveSourceSet::named (&config, set_name) ?;
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (&config) ?;
  let candidates : HashSet<ID> =
    export_candidate_pids (&active, &nodes) . into_iter () . collect ();
  let mut ugly_pids : Vec<ID> = nodes . iter ()
    . filter ( |node| node . ugly_telescope
      && candidates . contains (&node . pid) )
    . map ( |node| node . pid . clone () )
    . collect ();
  ugly_pids . sort ();
  if ! ugly_pids . is_empty () {
    let pids : String = ugly_pids . iter ()
      . map ( |pid| pid . as_str () )
      . collect::<Vec<&str>> () . join (", ");
    if ! active . is_all () && ! include_ugly_telescopes {
      return Err (format! (
        "export-org would release title or body selected below home for PIDs {} under source-set {}; rerun with --include-ugly-telescopes to approve",
        pids, active . name ) . into ()); }
    eprintln! (
      "Warning: export-org includes title or body selected below home for PIDs {}.",
      pids ); }
  let output_base : PathBuf =
    std::env::current_dir () ? . join (output_dir);
  let report : ExportReport =
    export_to_org (&active, &nodes, &output_base) ?;
  print! ("{}", report . summary ());
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
