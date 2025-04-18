// PITFALL | TODO: This deletes and rebuilds the TypeDB data
// at every request.

use std::io::{BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions};
use futures::executor::block_on;

use skg::typedb::create::make_db_destroying_earlier_one;
use skg::typedb::search::recursive_s_expression_from_node;

fn main() -> std::io::Result<()> {
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
  while let Ok(n) = reader.read_line(&mut line) {
    if n == 0 { break; } // emacs disconnected
    let node_id = line.trim_end().to_string();
    println!("Received request for node: {node_id}");
    let s_expression = generate_s_expression ( &node_id );
    send_response ( &mut stream, &s_expression );
    line.clear(); }
  println!("Emacs disconnected: {peer}"); }

fn generate_s_expression(node_id: &str) -> String {
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
      "tests/content_view/fixtures", db_name, &driver ) . await {
      return format!("Failed to initialize database: {}", e); }
    match recursive_s_expression_from_node(
      db_name, &driver, node_id).await {
      Ok(s_expr) => s_expr,
      Err(e) => format!("Error generating s-expression: {}", e)
    } } );
  result }

fn send_response(stream: &mut TcpStream, response: &str) {
  writeln! ( stream, "{}", response ) . unwrap();
  stream . flush() . unwrap() ; }
