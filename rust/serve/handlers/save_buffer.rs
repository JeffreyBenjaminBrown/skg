use crate::read_buffer::buffer_to_save_instructions;
use crate::save::update_graph;
use crate::types::errors::SaveError;
use crate::to_org::content_view::{
  render_forest_to_org,
  set_metadata_relationship_viewdata_in_forest};
use crate::merge::merge_nodes_in_graph;
use crate::to_org::complete_contents::completeOrgnodeForest;
use crate::serve::util::{
  send_response,
  format_buffer_response_sexp};
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::{SaveInstruction, MergeInstructionTriple, format_save_error_as_org};
use crate::types::orgnode::OrgNode;

use ego_tree::Tree;
use futures::executor::block_on;
use sexp::{Sexp, Atom};
use std::error::Error;
use std::io::{BufRead, BufReader, Read, Write};
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Rust's response to Emacs for a save operation.
/// Contains the regenerated buffer content and any warnings/errors.
struct SaveResponse {
  buffer_content : String,
  errors         : Vec < String >,
}

impl SaveResponse {
  /// Format the response as an s-expression.
  /// Format: ((content "...") (errors ("error1" "error2" ...)))
  fn to_sexp_string ( &self ) -> String {
    format_buffer_response_sexp (
      & self . buffer_content,
      & self . errors ) }}

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
      { Ok (save_response) =>
        { // S-exp response format: ((content "...") (errors (...)))
          let response_sexp : String =
            save_response . to_sexp_string ();
          let header : String =
            format! ( "Content-Length: {}\r\n\r\n",
                      response_sexp . len () );
          stream . write_all (
            format! ( "{}{}", header, response_sexp )
              . as_bytes () ). unwrap ();
          stream . flush() . unwrap (); }
        Err (err) => {
          // Check if this is a SaveError that should be formatted for the client
          if let Some(save_error) = err.downcast_ref::<SaveError>() {
            let error_buffer_content : String =
              format_save_error_as_org(save_error);
            let response : Sexp =
              empty_response_sexp ( &error_buffer_content );
            let response_sexp : String =
              response . to_string ();
            let header : String =
              format! ( "Content-Length: {}\r\n\r\n",
                        response_sexp . len ( ));
            let full_response : String =
              format! ( "{}{}", header, response_sexp );
            stream.write_all(full_response.as_bytes( )).unwrap();
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
                      s.trim() . parse::<usize> () . ok( ))
      } else { None }} )
    . ok_or ("Content-Length header not found") ?;
  let mut buffer : Vec<u8> = // Read content_length bytes.
    vec! [0u8; content_length] ;
  reader.read_exact (&mut buffer) ?;
  let content : String =
    String::from_utf8 (buffer) ?;
  Ok (content) }

/// Create an s-expression with nil content and an error message.
fn empty_response_sexp (
  error_buffer_content : &str
) -> Sexp {
  Sexp::List ( vec! [
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "content" . to_string () )),
      Sexp::Atom ( Atom::S ( "nil" . to_string () )) ] ),
    Sexp::List ( vec! [
      Sexp::Atom ( Atom::S ( "errors" . to_string () )),
      Sexp::List ( vec! [
        Sexp::Atom ( Atom::S (
          error_buffer_content . to_string () )) ] ) ] ) ] ) }

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
) -> Result<SaveResponse, Box<dyn Error>> {

  let (mut orgnode_forest, save_instructions, mergeInstructions)
    : (Vec<Tree<OrgNode>>, Vec<SaveInstruction>, Vec<MergeInstructionTriple>)
    = buffer_to_save_instructions (
      org_buffer_text, config, typedb_driver )
    . await . map_err (
      |e| Box::new(e) as Box<dyn Error> ) ?;
  if orgnode_forest.is_empty() { return Err (
    "No valid org nodes found in org_buffer_text" . into( )); }
  update_graph (
    save_instructions,
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;

  merge_nodes_in_graph (
    mergeInstructions,
    config.clone(),
    tantivy_index,
    typedb_driver ) . await ?;

  let mut errors : Vec < String > = Vec::new ();

  { // modify the orgnode forest before re-rendering it
    completeOrgnodeForest (
      &mut orgnode_forest,
      config,
      typedb_driver,
      &mut errors ) . await ?;
    set_metadata_relationship_viewdata_in_forest (
      &mut orgnode_forest,
      config,
      typedb_driver ) . await ?; }

  let buffer_content : String =
    render_forest_to_org ( & orgnode_forest );

  Ok ( SaveResponse { buffer_content, errors } ) }
