// PITFALL | TODO: Deletes and rebuilds the TypeDB data
// at every request.

use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;
use typedb_driver::{TypeDBDriver, Credentials,
                    DriverOptions};
use futures::executor::block_on;

use crate::typedb::create::make_db_destroying_earlier_one;
use crate::typedb::search::single_document_view;
use crate::types::ID;

pub fn serve() -> std::io::Result<()> {
  let listener = TcpListener::bind("0.0.0.0:1730")?;
  println!("Listening on port 1730...");
  for stream in listener.incoming() {
    match stream {
      Ok(stream) => {
        thread::spawn(move || handle_emacs(stream));
      } Err(e) => {
        eprintln!("Connection failed: {e}"); } } }
  Ok (()) }

fn handle_emacs(mut stream: TcpStream) {
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
              &mut stream, &line); }
          else {
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
  request: &str) {
  match node_id_from_document_request(request) {
    Ok(node_id) => {
      send_response(
        stream,
        & generate_s_expression(
          &node_id)); },
    Err(err) => {
      let error_msg = format!(
        "Error extracting node ID: {}", err);
      println!("{}", error_msg);
      send_response(stream, &error_msg); } } }

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

fn generate_s_expression(
  node_id: &ID)
  -> String {

  let result = block_on ( async {
    let driver = match TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None).unwrap()
    ).await {
      Ok(d) => d,
      Err(e) => return format!(
        "Error connecting to TypeDB: {}", e) };
    let db_name = "skg-test";
    if let Err(e) = make_db_destroying_earlier_one (
      "tests/typedb/fixtures", db_name, &driver )
      . await {
        return format!(
          "Failed to initialize database: {}", e); }
    match single_document_view(
      db_name, &driver, node_id).await {
      Ok(s_expr) => s_expr,
      Err(e) => format!(
        "Error generating s-expression: {}", e) } } );
  result }

fn send_response(
  stream: &mut TcpStream,
  response: &str) {

  writeln! ( // appends a newline
    stream, "{}", response ) . unwrap();
  stream . flush() . unwrap() ; }
