use crate::file_io::write_all_filenodes;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::tantivy::update_index_with_filenodes;
use crate::typedb::update::update_nodes_and_relationships;
use crate::types::{SkgConfig, TantivyIndex, FileNode};

use std::error::Error;
use std::io::{BufRead, BufReader, Read};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Handles save buffer requests from Emacs.
/// Reads the buffer content with length prefix,
/// processes it by prepending a line,
/// and sends it back with length prefix.
///
/// TODO: The "processes" step above is a placeholder.
/// What it should do is run `update_fs_and_dbs`,
/// regenerate the buffer, and send it back to Emacs.
pub fn handle_save_buffer_request (
  stream   : &mut TcpStream,
  _request : &str ) {

  match read_length_prefixed_content (stream) {
    Ok (content) => {
      let processed_content : String =
        process_buffer_content ( &content );
      send_response_with_length_prefix (
        stream, &processed_content ); }
    Err(err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      println! ( "{}", error_msg );
      send_response ( stream, &error_msg );
    }} }

/// Reads length-prefixed content from the stream.
/// Expected format:
///   "Content-Length: N\r\n\r\n" followed by N bytes of content.
fn read_length_prefixed_content (
  stream: &mut TcpStream
) -> Result<String, Box<dyn std::error::Error>> {

  let mut reader = BufReader::new(stream);
  let mut header_lines = Vec::new();
  loop { // Read header lines until reaching the empty line.
    let mut line : String = String::new();
    reader.read_line ( &mut line )?;
    if line == "\r\n" { break; }
    header_lines.push (line); }
  let content_length = header_lines
    .iter()
    .find_map ( |line| {
      if line.starts_with("Content-Length: ")
      { line.strip_prefix("Content-Length: ")
        . and_then ( |s|
                      s.trim() . parse::<usize> () . ok() )
      } else { None }} )
    . ok_or ("Content-Length header not found") ?;
  let mut buffer : Vec<u8> = // Read content_length bytes.
    vec! [0u8; content_length] ;
  reader.read_exact (&mut buffer) ?;
  let content = String::from_utf8 (buffer) ?;
  Ok (content) }

/// Prepends a line.
/// This is where the actual "save" logic will go.
fn process_buffer_content (
  content: &str
) -> String {
  format!("Rust added this line.\n{}", content) }

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
