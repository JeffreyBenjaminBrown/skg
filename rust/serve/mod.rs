pub mod single_root_view;
pub mod title_matches;
pub mod update;
pub mod util;

use crate::file_io::read_skg_files;
use crate::serve::single_root_view::handle_single_root_view_request;
use crate::serve::title_matches::handle_title_matches_request;
use crate::serve::util::request_type_from_request;
use crate::serve::util::send_response;
use crate::tantivy::initialize_tantivy_from_filenodes;
use crate::typedb::init::initialize_typedb_from_filenodes;
use crate::types::{SkgConfig, TantivyIndex, FileNode};

use std::io::{BufRead, BufReader};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::Arc;
use std::thread;
use typedb_driver::TypeDBDriver;

/// Populates the tantivy db ("the index") and the typedb db
/// (sometimes called "the db", as if there were only one).
/// Then pipes TCP input from Emacs into handle_emacs.
pub fn serve (
  config : SkgConfig
) -> std::io::Result<()> {

  let (typedb_driver, tantivy_index)
    : (Arc<TypeDBDriver>, TantivyIndex)
    = initialize_dbs ( &config );
  let emacs_listener : TcpListener =
    TcpListener::bind("0.0.0.0:1730")?;
  println!("Listening on port 1730...");
  for stream_res in emacs_listener.incoming() {
    match stream_res {
      Ok(stream) => {
        let stream : TcpStream = stream; // The least-ugly way to type-annotate `stream`.
        let typedb_driver_clone : Arc<TypeDBDriver> =
          Arc::clone( &typedb_driver ); // Cloning permits the main thread to keep the driver and index. If they were passed here instead of cloned, their ownership would be moved into the first spawned thread, making them unavailable for the next connection.
        let tantivy_index_clone = tantivy_index . clone ();
        let config_clone        = config        . clone ();
        thread::spawn ( move || {
          handle_emacs (
            stream,
            typedb_driver_clone,
            tantivy_index_clone,
            & config_clone, ) } ); }
      Err(e) => {
        eprintln!("Connection failed: {e}"); } } }
  Ok (()) }

/// Reads all FileNodes from disk, then uses that data
/// to initialize both databases (TypeDB and Tantivy).
fn initialize_dbs (
  config : & SkgConfig,
) -> (Arc<TypeDBDriver>, TantivyIndex) {

  println!("Reading .skg files...");
  let skg_folder_str: &str =
    config . skg_folder
    . to_str () . expect ("Invalid UTF-8 in skg folder path");
  let filenodes: Vec<FileNode> =
    read_skg_files ( skg_folder_str )
    . unwrap_or_else(|e| {
      eprintln!("Failed to read .skg files: {}", e);
      std::process::exit(1);
    });
  println!("{} .skg files were read", filenodes.len());

  let typedb_driver: Arc<TypeDBDriver> =
    initialize_typedb_from_filenodes ( config, &filenodes );
  let tantivy_index: TantivyIndex =
    initialize_tantivy_from_filenodes ( config, &filenodes );

  (typedb_driver, tantivy_index) }

/// This function directs requests from the stream to one of
///   handle_sexp_document_request
///   handle_title_matches_request
/// API: See /api.md
fn handle_emacs (
  mut stream    : TcpStream,
  typedb_driver : Arc<TypeDBDriver>,
  tantivy_index : TantivyIndex,
  config        : &SkgConfig,
) {

  let peer : SocketAddr =
    stream . peer_addr() . unwrap();
  println!("Emacs connected: {peer}");
  let mut reader
    : BufReader<TcpStream> // the underlying stream, but buffered
    = BufReader::new (
      stream . try_clone() . unwrap() );
  let mut request = String::new();
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
              &typedb_driver,
              config, );
          } else if request_type == "title matches" {
            handle_title_matches_request(
              &mut stream,
              &request,
              &tantivy_index);
          } else {
            let error_msg = format!(
              "Unsupported request type: {}",
              request_type);
            println!("{}", error_msg);
            send_response(&mut stream, &error_msg); } },
        Err(err) => {
          println!("Error determining request type: {}",
                   err);
          send_response(
            &mut stream, &format!(
              "Error determining request type: {}",
              err)); } };
      request.clear(); }
  println!("Emacs disconnected: {peer}"); }
