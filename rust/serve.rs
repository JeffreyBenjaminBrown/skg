use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener,
               TcpStream, // handles two-way communication
               SocketAddr};
use std::path::Path;
use std::sync::Arc;
use std::thread;
use tantivy::{ Document, Index, schema};
use typedb_driver::{TypeDBDriver, Credentials,
                    DriverOptions};
use futures::executor::block_on;

use crate::config::{ SKG_DATA_DIR, TANTIVY_INDEX_DIR};
use crate::typedb::create::{
  overwrite_and_populate_new_db};
use crate::index_titles::{
  get_extant_index_or_create_empty_one,
  search_index, update_index};
use crate::typedb::search::single_document_view;
use crate::types::{ID,TantivyIndex};

pub fn serve () -> std::io::Result<()> {
  // Makes a typedb driver and a tantivy index.
  // Then pipes TCP input from Emacs into handle_emacs.

  let typedb_driver : Arc<TypeDBDriver> =
    initialize_typedb();
  let tantivy_index : TantivyIndex =
    initialize_tantivy();
  let db_name = "skg-test";
  let emacs_listener : TcpListener =
    TcpListener::bind("0.0.0.0:1730")?;
  println!("Listening on port 1730...");
  for stream_res in emacs_listener.incoming() {
    match stream_res {
      Ok(stream) => {
        let stream : TcpStream = stream; // The least-ugly way to type-annotate `stream`.
        let typedb_driver_clone : Arc<TypeDBDriver> =
          Arc::clone( &typedb_driver ); // Cloning permits the main thread to keep the driver and index. If they were passed here instead of cloned, their ownership would be moved into the first spawned thread, making them unavailable for the next connection.
        let tantivy_index_clone =
          tantivy_index.clone();
        thread::spawn(move || {
          handle_emacs (
            stream,
            typedb_driver_clone,
            db_name, // static, so no cloning needed
            tantivy_index_clone ) } ); }
      Err(e) => {
        eprintln!("Connection failed: {e}"); } } }
  Ok (()) }

fn handle_emacs (
  mut stream: TcpStream,
  typedb_driver: Arc<TypeDBDriver>,
  db_name: &str,
  tantivy_index: TantivyIndex
) {
  // This function directs requests from the stream to one of
  //   handle_single_document_request
  //   handle_title_matches_request
  // API: See /api.md

  // TODO: Let Emacs send s-expressions with newlines.
  //   The easiest way seems to be to send two messages: first a length, and then the s-exp of that length.
  //   Waiting for the s-exp to end by matching parentheses would be more natural, but requires parsing while reading in order to determine when to stop.

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
          if request_type == "single document" {
            handle_single_document_request (
              &mut stream,
              &request,
              &typedb_driver,
              db_name );
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

pub fn initialize_typedb (
) -> Arc<TypeDBDriver> {
  // Connects to the TypeDB server,
  // then calls overwrite_and_populate_new_db.

  println!("Initializing TypeDB database...");
  let driver = block_on( async {
    TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None).unwrap() )
      .await
      .unwrap_or_else(|e| {
        eprintln!("Error connecting to TypeDB: {}", e);
        std::process::exit(1);
      } )
  } );

  let db_name = "skg-test";
  block_on ( async {
    if let Err(e) = overwrite_and_populate_new_db (
      SKG_DATA_DIR, db_name, &driver
    ) . await {
      eprintln!("Failed to initialize database: {}", e);
      std::process::exit(1);
    } } );
  println!("TypeDB database initialized successfully.");
  Arc::new( driver ) }

fn initialize_tantivy (
) -> TantivyIndex {
  // Build a schema.
  // Fetch the old or start a new index, using
  //   `get_extant_index_or_create_empty_one`.
  // Update it with update_index.

  println!("Initializing Tantivy index...");

  // Define the schema.
  let mut schema_builder = schema::Schema::builder();
  let path_field = schema_builder.add_text_field(
    "path", schema::STRING | schema::STORED);
  let title_field = schema_builder.add_text_field(
    "title", schema::TEXT | schema::STORED);
  let schema : schema::Schema =
    schema_builder.build();

  // Create or open the index, and wrap it in my `TantivyIndex`.
  let index_path = Path::new( TANTIVY_INDEX_DIR );
  let (index, index_is_new) : (Index, bool) =
    get_extant_index_or_create_empty_one (
      schema,
      index_path )
    . unwrap_or_else(|e| {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit(1);
    } );
  let tantivy_index = TantivyIndex {
    index: Arc::new(index),
    path_field,
    title_field,
  };

  // Update the index with current files
  match update_index ( &tantivy_index,
                        SKG_DATA_DIR,
                        index_path,
                        index_is_new ) {
    Ok(indexed_count) => { println!(
      "Tantivy index initialized successfully. Indexed {} files.",
      indexed_count); }
    Err(e) => {
      eprintln!("Failed to update Tantivy index: {}", e);
      std::process::exit(1); } }

  tantivy_index }

fn handle_single_document_request (
  stream: &mut TcpStream,
  request: &str,
  typedb_driver: &TypeDBDriver,
  db_name: &str ) {
  // Gets a node id from the request,
  // generates an s-expression from the id,
  // and sends the s-expression to Emacs.

  match node_id_from_document_request ( request ) {
    Ok ( node_id ) => {
      send_response (
        stream,
        & generate_document (
          &node_id,
          typedb_driver,
          db_name ) ); },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting node ID: {}", err);
      println! ( "{}", error_msg ) ;
      send_response ( stream,
                      &error_msg ); } } }

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
  node_id       : &ID,
  typedb_driver : &TypeDBDriver,
  db_name       : &str )
  -> String {
  // Just runs
  //   single_document_view
  // with async and error handling.

  let result = block_on ( async {
    match single_document_view (
      db_name, typedb_driver, node_id
    ) . await {
      Ok ( s_expr ) => s_expr,
      Err (e) => format! (
        "Error generating s-expression: {}", e) } } );
  result }

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
