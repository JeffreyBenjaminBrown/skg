use crate::file_io::read_skg_files;
use crate::file_io::write_all_filenodes;
use crate::render::org::single_root_content_view;
use crate::tantivy::initialize_tantivy_from_filenodes;
use crate::tantivy::search_index;
use crate::tantivy::update_index_with_filenodes;
use crate::typedb::init::initialize_typedb_from_filenodes;
use crate::typedb::update::update_nodes_and_relationships;
use crate::types::{ID, SkgConfig, TantivyIndex, FileNode};

use futures::executor::block_on;
use std::error::Error;
use std::io::{BufRead, BufReader, Write};
use std::net::SocketAddr;
use std::net::TcpListener;
use std::net::TcpStream; // handles two-way communication
use std::sync::Arc;
use std::thread;
use tantivy::{Document};
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

fn initialize_dbs (
  config : & SkgConfig,
) -> (Arc<TypeDBDriver>, TantivyIndex) {
  // First reads all FileNodes from disk,
  // then initializes both databases using the same data.

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

/// Updates **everything** from the given `FileNode`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_fs_and_dbs (
  filenodes     : Vec<FileNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;
  println!( "1) Updating TypeDB database '{}' ...", db_name );
  update_nodes_and_relationships (
    db_name,
    driver,
    &filenodes, ). await ?;
  println!( "   TypeDB update complete." );

  let total_input : usize =
    filenodes.len ();
  let target_dir  : &std::path::Path =
    &config.skg_folder;
  println!( "2) Writing {} file(s) to disk at {:?} ...",
            total_input, target_dir );
  let written_count : usize =
    write_all_filenodes (
      filenodes.clone (), config.clone () ) ?;
  println!( "   Wrote {} file(s).", written_count );

  println!( "3) Updating Tantivy index ..." );
  let indexed_count : usize =
    update_index_with_filenodes (
      &filenodes, tantivy_index )?;
  println!( "   Tantivy updated for {} document(s).",
                indexed_count );

  println!( "All updates finished successfully." );
  Ok (( )) }

fn handle_emacs (
  mut stream    : TcpStream,
  typedb_driver : Arc<TypeDBDriver>,
  tantivy_index : TantivyIndex,
  config        : &SkgConfig,
) {
  // This function directs requests from the stream to one of
  //   handle_sexp_document_request
  //   handle_title_matches_request
  // API: See /api.md

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
          if request_type == "org document" {
            handle_org_document_request (
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

fn handle_org_document_request (
  // Gets a node id from the request,
  // generates an Org document from the id,
  // and sends the Org to Emacs (length-prefixed).
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match node_id_from_document_request ( request ) {
    Ok ( node_id ) => {
      send_response_with_length_prefix (
        stream,
        & generate_document (
          &node_id,
          typedb_driver,
          & config,
        )); },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting node ID: {}", err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); } } }

fn handle_title_matches_request (
  stream        : &mut TcpStream,
  request       : &str,
  tantivy_index : &TantivyIndex ) {
  // Extracts search terms from request,
  // finds matching titles,
  // and sends a corresponding document to Emacs.

  match search_terms_from_request ( request ) {
    Ok ( search_terms ) => {
      send_response (
        stream,
        & generate_title_matches_response (
          &search_terms,
          tantivy_index ) ); },
    Err ( err ) => {
      let error_msg = format! (
        "Error extracting search terms: {}", err );
      println! ( "{}", error_msg ) ;
      send_response (
        stream, &error_msg ); } } }

fn extract_quoted_value_from_sexp (
  request    : &str,
  pattern    : &str,
  field_name : &str
) -> Result < String, String > {
  // Returns the string between the first appearance of `pattern`
  // and the next quotation mark.

  // TODO: this is brittle to API changes
  // (athough seems safe as the API stands today).
  // It would be better to do proper s-exp parsing.

  if let Some ( start_pos ) = request.find ( pattern ) {
    let value_start = start_pos + pattern.len ();
    if let Some ( end_pos ) =
      request [ value_start.. ].find("\"")
    { Ok ( request [ value_start ..
                     (value_start + end_pos) ]
           . to_string () )
    } else { Err ( format! (
      "Could not find end quote for {} in request", field_name ) ) }
  } else { Err ( format! (
    "Could not find {} in request", field_name ) ) } }

fn request_type_from_request ( request : &str )
                               -> Result<String, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(request . \"",
                                   "request type" ) }

fn node_id_from_document_request ( request : &str )
                                   -> Result<ID, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(id . \"",
                                   "ID" )
    . map(ID) }

fn search_terms_from_request ( request : &str )
                               -> Result<String, String> {
  extract_quoted_value_from_sexp ( request,
                                   "(terms . \"",
                                   "search terms" ) }

fn generate_document (
  // TODO: This needs a name reflecting that it just wraps
  //   single_root_content_view
  node_id       : &ID,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  // Just runs `single_root_content_view`,
  // but with async and error handling.

  block_on (
    async {
      match single_root_content_view (
        typedb_driver,
        config,
        node_id ) . await
      { Ok  (s) => s,
        Err (e) => format!(
          "Error generating document: {}", e), }} ) }

fn generate_title_matches_response (
  search_terms  : &str,
  tantivy_index : &TantivyIndex)
  -> String {
  // Runs `search_index`.
  // If matches are found, returns a String
  // with a match score and a title on each line.

  match search_index (
    tantivy_index,
    search_terms ) {
    Ok (( best_matches, searcher )) => {
      if best_matches.is_empty () {
        "No matches found.".to_string ()
      } else {
        let mut titles = Vec::new();
        for (score, doc_address) in best_matches {
          match searcher.doc (doc_address) {
            // searcher.doc fetches a Document.
            // (Document is an alias of the TantivyDocument type.)
            // Each Document here is just two fields,
            // "title" and "path" (as of <2025-08-08 Fri>).
            Ok (retrieved_doc) => {
              let retrieved_doc : Document = retrieved_doc;
              if let Some (title_value) = retrieved_doc
                . get_first ( tantivy_index.title_field )
              { if let Some ( title_text ) =
                title_value.as_text ()
                { titles.push ( format! (
                  "{:.2}: {}",
                  score,
                  title_text ) ); } } },
            Err (e) => { eprintln! (
              "Error retrieving document: {}", e ); } } }
        titles.join("\n") } },
    Err(e) => { format!("Error searching index: {}", e) } } }

fn send_response (
  stream   : &mut TcpStream,
  response : &str) {

  writeln! ( // appends a newline
    stream, "{}", response )
    . unwrap ();
  stream . flush () . unwrap () ; }

fn send_response_with_length_prefix (
  // Responds "Content-Length: <bytes>\r\n\r\n" + payload
  stream   : &mut TcpStream,
  response : &str) {

  let payload = response.as_bytes ();
  let header  = format! ( "Content-Length: {}\r\n\r\n",
                           payload.len () );
  use std::io::Write as _;
  stream . write_all ( header.as_bytes () ) . unwrap ();
  stream . write_all ( payload )            . unwrap ();
  stream . flush ()                         . unwrap ();
}
