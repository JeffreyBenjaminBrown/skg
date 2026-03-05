/// USAGE:
/// There is an optional command-line argument: the config file path.
/// See the SkgConfig type at server/types/misc.rs,
/// or the example at data/skgconfig.toml.
///
/// Subcommand: import-org-roam <org-dir> <skg-output-dir> <source-nickname>
/// Converts org-roam .org files to .skg files.

use skg::context::compute_and_store_context_types;
use skg::dbs::filesystem::not_nodes::load_config;
use skg::dbs::init::{InitData, initialize_dbs};
use skg::import_org_roam::import_org_roam_directory;
use skg::serve::serve;
use skg::serve::timing_log::{clear_timing_log, timed};
use skg::types::misc::{SkgConfig, SourceName, TantivyIndex};

use std::error::Error;
use std::env;
use std::io::{BufRead, BufReader, Write};
use std::net::TcpListener;
use std::path::Path;
use std::sync::Arc;
use std::sync::atomic::{AtomicBool, Ordering};
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

  let listener: TcpListener = // precedes initialize_dbs so Emacs can connect during init
    TcpListener::bind (
      & format! ("0.0.0.0:{}", config . port) ) ?;
  println! ("Listening on port {} for Emacs connections...",
            config . port);
  listener . set_nonblocking (true) ?;

  // The "busy signal". See definition of 'busysignal_accept_loop'.
  let init_done: Arc<AtomicBool> =
    Arc::new (AtomicBool::new (false));
  let init_done_clone: Arc<AtomicBool> =
    Arc::clone (&init_done);
  let busysignal_listener: TcpListener =
    listener . try_clone () ?;
  let busysignal_handle: std::thread::JoinHandle<()> =
    std::thread::spawn ( move || {
      busysignal_accept_loop (
        busysignal_listener,
        init_done_clone ); } );

  clear_timing_log (&config);
  let init : InitData =
    timed ( &config, "initialize_dbs",
            || initialize_dbs (&config));
  let typedb_driver : Arc<TypeDBDriver> = init . driver;
  let tantivy_index : TantivyIndex = init . tantivy_index;

  // Compute context origin types for search ranking.
  // Fully in-memory: all data is pre-computed from SkgNodes at init.
  timed ( &config, "context_computation", || {
    match compute_and_store_context_types (
      &tantivy_index,
      &init . had_id_set,
      &init . all_node_ids,
      &init . link_targets,
      &init . contains_map,
      &init . reverse_map )
    { Ok (_) => {}
      Err (e) => {
        eprintln! (
          "Warning: context computation failed: {}. \
           Search results will not have context-based ranking.", e); } } } );

  init_done . store (true, Ordering::Release);
  busysignal_handle . join ()
    . expect ("busysignal thread panicked");
  listener . set_nonblocking (false) ?;
  println! ("Server ready.");

  serve (config, typedb_driver, tantivy_index, listener)
    . map_err ( |e| Box::new (e)
                 as Box<dyn Error>) ?;
  Ok (( )) }

/// During initialization, accept connections and reply
/// with an "initializing" message to every request line.
fn busysignal_accept_loop (
  listener  : TcpListener,
  init_done : Arc<AtomicBool>,
) {
  let init_msg: &str =
    "((busy . \"Server is initializing, please wait. \
     See logs/skg.log for progress.\"))\n";
  while ! init_done . load (Ordering::Acquire) {
    match listener . accept () {
      Ok (( stream, addr )) => {
        let mut stream: std::net::TcpStream = stream;
        println! ("Busysignal: connection from {}", addr);
        stream . set_nonblocking (false)
          . ok ();
        // Set a read timeout so we don't block forever
        // if init finishes while we're mid-read.
        stream . set_read_timeout (
          Some (std::time::Duration::from_millis (500)) )
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
            std::time::Duration::from_millis (100) ); }
      Err (e) => {
        eprintln! ("Busysignal accept error: {}", e); } } } }

fn run_import (
  args : &[String],
) -> Result<(), Box<dyn Error>> {
  if args . len() < 5 {
    eprintln! ("Usage: cargo run -- import-org-roam <org-dir> <skg-output-dir> <source-nickname>");
    std::process::exit (1); }
  let org_dir    : &Path       = Path::new (&args[2]);
  let output_dir : &Path       = Path::new (&args[3]);
  let source     : SourceName  = SourceName::from (&args[4]);
  let stats : skg::import_org_roam::ImportStats =
    import_org_roam_directory (org_dir, output_dir, &source)?;
  println! ("{}", stats);
  for err in &stats . errors {
    eprintln! ("  {}", err); }
  Ok (( )) }
