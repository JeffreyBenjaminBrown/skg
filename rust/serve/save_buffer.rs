use crate::file_io::update_fs_from_saveinstructions;
use crate::save::buffer_to_save_instructions;
use crate::types::SaveError;
use crate::mk_org_text::content_view::{
  render_forest_to_org,
  set_metadata_relationships_in_forest};
use crate::rebuild::completeOrgnodeForest;
use crate::serve::util::send_response;
use crate::tantivy::update_index_from_saveinstructions;
use crate::typedb::update::update_typedb_from_saveinstructions;
use crate::types::{SkgConfig, TantivyIndex, SaveInstruction, OrgNode};
use crate::types::save::format_save_error_as_org;

use ego_tree::Tree;
use futures::executor::block_on;
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use std::path::Path;
use typedb_driver::TypeDBDriver;

/* Handles save buffer requests from Emacs.
.
- Reads the buffer content with length prefix.
- Puts that text through `update_from_and_rerender_buffer`.
- Sends that back to Emacs (with a length prefix). */
pub fn handle_save_buffer_request (
  reader        : &mut BufReader <TcpStream>,
  stream        : &mut TcpStream,
  _request      : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex ) {

  match read_length_prefixed_content (reader) {
    Ok (initial_buffer_content) => {
      match block_on(
        update_from_and_rerender_buffer (
          // Most of the work happens here.
          & initial_buffer_content,
          typedb_driver, config, tantivy_index ))
      { Ok (regenerated_buffer_content) =>
        { // Send success indicator followed by length-prefixed content
          let success_header : &str =
            "save: success\n";
          let header : String =
            format!("Content-Length: {}\r\n\r\n",
                    regenerated_buffer_content.len());
          let full_response : String =
            format!("{}{}{}",
                    success_header, header,
                    regenerated_buffer_content);
          stream . write_all(
            full_response . as_bytes () ). unwrap ();
          stream . flush() . unwrap (); }
        Err (err) => {
          // Check if this is a SaveError that should be formatted for the client
          if let Some(save_error) = err.downcast_ref::<SaveError>() {
            let error_buffer_content : String =
              format_save_error_as_org(save_error);
            // Send failure indicator followed by length-prefixed error content
            let failure_header : &str =
              "save: failure\n";
            let header : String =
              format!("Content-Length: {}\r\n\r\n",
                      error_buffer_content.len());
            let full_response : String =
              format!("{}{}{}",
                      failure_header, header, error_buffer_content);
            stream.write_all(full_response.as_bytes()).unwrap();
            stream.flush().unwrap();
          } else {
            let error_msg : String =
              format!("Error processing buffer content: {}", err);
            println!("{}", error_msg);
            send_response(stream, &error_msg);
          }} }}
    Err(err) => {
      let error_msg : String =
        format! ("Error reading buffer content: {}", err );
      println! ( "{}", error_msg );
      send_response ( stream, &error_msg ); }} }

/// Reads length-prefixed content from the stream.
/// Expected format:
///   "Content-Length: N\r\n\r\n" followed by N bytes of content.
fn read_length_prefixed_content (
  reader : &mut BufReader <TcpStream>
) -> Result<String, Box<dyn Error>> {

  // Consume header lines already in this reader's buffer,
  // then read exactly Content-Length bytes from the same reader.
  let mut header_lines : Vec <String> =
    Vec::new ();
  loop { // Read header lines until reaching the empty line.
    let mut line : String = String::new();
    reader.read_line ( &mut line )?;
    if line == "\r\n" { break; }
    header_lines.push (line); }
  let content_length : usize =
    header_lines
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
  let content : String =
    String::from_utf8 (buffer) ?;
  Ok (content) }

/* Update dbs and filesystem, and generate text for a new org-buffer.
Steps:
- Put the text through `buffer_to_save_instructions` to get orgnode forest and save instructions.
- Extract root IDs from the orgnode forest.
- Runs `update_graph` on the save instructions.
- Returns a multi-root content view. */
async fn update_from_and_rerender_buffer (
  org_buffer_text : &str,
  typedb_driver   : &TypeDBDriver,
  config          : &SkgConfig,
  tantivy_index   : &TantivyIndex
) -> Result<String, Box<dyn Error>> {

  let (mut orgnode_forest, save_instructions)
    : (Vec<Tree<OrgNode>>, Vec<SaveInstruction>)
    = buffer_to_save_instructions (
      org_buffer_text, config, typedb_driver )
    . await . map_err (
      |e| Box::new(e) as Box<dyn Error> ) ?;
  if orgnode_forest.is_empty() { return Err (
    "No valid org nodes found in org_buffer_text" . into()); }
  update_graph (
    save_instructions,
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;
  { // modify the orgnode forest before re-rendering it
    completeOrgnodeForest (
      &mut orgnode_forest,
      config ) ?;
    set_metadata_relationships_in_forest (
      &mut orgnode_forest,
      config,
      typedb_driver ) . await ?; }
  Ok ( render_forest_to_org (
    & orgnode_forest )) }

/// Updates **everything** from the given `SaveInstruction`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_graph (
  instructions  : Vec<SaveInstruction>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;

  { println!( "1) Updating TypeDB database '{}' ...", db_name );
    update_typedb_from_saveinstructions (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB update complete." ); }

  { // filesystem
    let total_input : usize = instructions.len ();
    let target_dir  : &Path = &config.skg_folder;
    println!( "2) Writing {} instruction(s) to disk at {:?} ...",
               total_input, target_dir );
    let (deleted_count, written_count) : (usize, usize) =
      update_fs_from_saveinstructions (
        instructions.clone (), config.clone () ) ?;
    println!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  { // Tantivy
    println!( "3) Updating Tantivy index ..." );
    let indexed_count : usize =
      update_index_from_saveinstructions (
        &instructions, tantivy_index )?;
    println!( "   Tantivy updated for {} document(s).",
                  indexed_count ); }

  println!( "All updates finished successfully." );
  Ok (( )) }
