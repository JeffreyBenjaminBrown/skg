use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::path::Path;
use std::sync::Arc;
use std::thread;
use tantivy::schema;
use typedb_driver::{TypeDBDriver, Credentials,
                    DriverOptions};
use futures::executor::block_on;

use crate::config::{ SKG_DATA_DIR, TANTIVY_INDEX_DIR};
use crate::typedb::create::{
  make_db_destroying_earlier_one};
use crate::index_titles::{
  get_extant_index_or_create_empty_one,
  search_index, create_index};
use crate::typedb::search::single_document_view;
use crate::types::{ID,TantivyIndex};

pub fn initialize_typedb(
) -> Arc<TypeDBDriver> {

  println!("Initializing TypeDB database...");
  let driver = block_on(async {
    match TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None).unwrap()
    ).await {
      Ok(d) => d,
      Err(e) => {
        eprintln!("Error connecting to TypeDB: {}", e);
        std::process::exit(1);
      } } } );
  let db_name = "skg-test";
  block_on(async {
    if let Err(e) = make_db_destroying_earlier_one(
      SKG_DATA_DIR, db_name, &driver
    ) . await {
      eprintln!("Failed to initialize database: {}", e);
      std::process::exit(1);
    } } );
  println!("TypeDB database initialized successfully.");
  Arc::new(driver) }

fn initialize_tantivy(
) -> TantivyIndex {
  println!("Initializing Tantivy index...");

  // Define the schema
  let mut schema_builder = schema::Schema::builder();
  let path_field = schema_builder.add_text_field(
    "path", schema::STRING | schema::STORED);
  let title_field = schema_builder.add_text_field(
    "title", schema::TEXT | schema::STORED);
  let schema = schema_builder.build();

  // Create or open the index
  let index_path = Path::new( TANTIVY_INDEX_DIR );
  let index = match get_extant_index_or_create_empty_one(
    schema, index_path) {
    Ok(idx) => idx,
    Err(e) => {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit(1);
    } };

  // Create TantivyIndex wrapper
  let tantivy_index = TantivyIndex {
    index: Arc::new(index),
    path_field,
    title_field,
  };

  // Update the index with current files
  match create_index ( &tantivy_index,
                        SKG_DATA_DIR ) {
    Ok(indexed_count) => {
      println!("Tantivy index initialized successfully. Indexed {} files.", indexed_count); },
    Err(e) => {
      eprintln!("Failed to update Tantivy index: {}", e);
      std::process::exit(1); } }

  tantivy_index }

pub fn serve() -> std::io::Result<()> {
  let typedb_driver : Arc<TypeDBDriver> =
    initialize_typedb();
  let tantivy_index : TantivyIndex =
    initialize_tantivy();
  let db_name = "skg-test";
  let listener = TcpListener::bind("0.0.0.0:1730")?;
  println!("Listening on port 1730...");
  for stream in listener.incoming() {
    match stream {
      Ok(stream) => {
        let typedb_driver_clone = Arc::clone(
          &typedb_driver);
        let tantivy_index_clone =
          tantivy_index.clone();
        thread::spawn(move || {
          handle_emacs ( stream,
                         typedb_driver_clone,
                         db_name,
                         tantivy_index_clone ) } ); }
      Err(e) => {
        eprintln!("Connection failed: {e}"); } } }
  Ok (()) }

fn handle_emacs(
  mut stream: TcpStream,
  typedb_driver: Arc<TypeDBDriver>,
  db_name: &str,
  tantivy_index: TantivyIndex) {

  let peer = stream.peer_addr().unwrap();
  println!("Emacs connected: {peer}");
  let mut reader = BufReader::new(
    stream . try_clone() . unwrap() );
  let mut line = String::new();
  while let Ok(n) =
    reader.read_line(&mut line) { // reads until a newline
      if n == 0 { break; } // emacs disconnected
      println!("Received request: {}", line.trim_end());
      match request_type_from_request( &line ) {
        Ok(request_type) => {
          if request_type == "single document" {
            handle_single_document_request(
              &mut stream,
              &line,
              &typedb_driver,
              db_name,
              &tantivy_index);
          } else if request_type == "title matches" {
            handle_title_matches_request(
              &mut stream,
              &line,
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
      line.clear(); }
  println!("Emacs disconnected: {peer}"); }

fn handle_single_document_request(
  stream: &mut TcpStream,
  request: &str,
  typedb_driver: &TypeDBDriver,
  db_name: &str,
  tantivy_index: &TantivyIndex) {

  match node_id_from_document_request(request) {
    Ok(node_id) => {
      send_response(
        stream,
        & generate_s_expression(
          &node_id,
          typedb_driver,
          db_name,
          tantivy_index) ); },
    Err(err) => {
      let error_msg = format!(
        "Error extracting node ID: {}", err);
      println!("{}", error_msg);
      send_response(stream, &error_msg); } } }

fn handle_title_matches_request(
  stream: &mut TcpStream,
  request: &str,
  tantivy_index: &TantivyIndex) {

  match search_terms_from_request(request) {
    Ok(search_terms) => {
      send_response(
        stream,
        & generate_title_matches_response(
          &search_terms,
          tantivy_index) ); },
    Err(err) => {
      let error_msg = format!(
        "Error extracting search terms: {}", err);
      println!("{}", error_msg);
      send_response(
        stream, &error_msg ); } } }

fn request_type_from_request(
  request: &str)
  -> Result<String, String> {

  let request_pattern = "(request . \"";
  if let Some(req_start) = request.find(request_pattern) {
    let req_start = req_start + request_pattern.len();
    if let Some(req_end) =
      request[req_start..].find("\"") {
        return Ok(request[req_start..
                          (req_start + req_end)]
                  .to_string());
      } else {
        return Err( "Could not find quotation mark ending request type in request."
                     .to_string()); }
  } else {
    return Err( "Could not find request type in request."
                 .to_string() ); } }

fn node_id_from_document_request (
  request: &str)
  -> Result<ID, String> {

  let id_pattern = "(id . \"";
  if let Some(id_start) = request.find(id_pattern) {
    let id_start = id_start + id_pattern.len();
    if let Some(id_end) = request[id_start..].find("\"") {
      return Ok(ID(request[id_start..(id_start + id_end)]
                   .to_string()));
    } else {
      return Err("Could not find end of ID in request"
                 .to_string()); }
  } else {
    return Err("Could not find ID in request"
               .to_string()); } }

fn search_terms_from_request(
  // TODO: This reinvents the wheel. Use the s-exp parsing logic already found in `save::sexp_to_orgnodes.rs`.
  request: &str)
  -> Result<String, String> {

  let terms_pattern = "(terms . \"";
  if let Some(terms_start) = request.find(terms_pattern) {
    let terms_start = terms_start + terms_pattern.len();
    if let Some(terms_end) = request[terms_start..].find("\"") {
      return Ok(request[terms_start..(terms_start + terms_end)]
                .to_string());
    } else {
      return Err("Could not find end of search terms in request"
                 .to_string()); }
  } else {
    return Err("Could not find search terms in request"
               .to_string()); } }

fn generate_s_expression(
  node_id: &ID,
  typedb_driver: &TypeDBDriver,
  db_name: &str,
  _tantivy_index: &TantivyIndex)
  -> String {

  let result = block_on(async {
    match single_document_view(
      db_name, typedb_driver, node_id
    ).await {
      Ok(s_expr) => s_expr,
      Err(e) => format!(
        "Error generating s-expression: {}", e) } } );
  result }

fn generate_title_matches_response(
  search_terms: &str,
  tantivy_index: &TantivyIndex)
  -> String {

  match search_index(
    tantivy_index,
    search_terms) {
    Ok((best_matches, searcher)) => {
      if best_matches.is_empty() {
        "No matches found.".to_string()
      } else {
        let mut titles = Vec::new();
        for (score, doc_address) in best_matches {
          match searcher.doc(doc_address) {
            Ok(retrieved_doc) => {
              if let Some(title_value) = retrieved_doc
                .get_first(tantivy_index.title_field) {
                  if let Some(title_text) =
                    title_value.as_text() {
                  titles.push(format!("{:.2}: {}",
                                      score, title_text));
                } } },
            Err(e) => {
              eprintln!(
                "Error retrieving document: {}", e);
            } } }
        titles.join("\n")
      } },
    Err(e) => {
      format!("Error searching index: {}", e)
    } } }

fn send_response(
  stream: &mut TcpStream,
  response: &str) {

  writeln! ( // appends a newline
    stream, "{}", response ) . unwrap();
  stream . flush() . unwrap() ; }
