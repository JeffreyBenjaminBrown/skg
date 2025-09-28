use crate::file_io::write_all_nodes;
use crate::render::single_root_view;
use crate::save::assign_ids::assign_ids_recursive;
use crate::save::none_node_fields_are_noops::clobber_none_fields_with_data_from_disk;
use crate::text_to_orgnodes::parse_skg_org_to_nodes;
use crate::save::orgnode_to_node::orgNodeInterpretation_to_nodes;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::tantivy::update_index_with_nodes;
use crate::typedb::update::update_nodes_and_relationships;
use crate::types::{ID, SkgNode, OrgNodeInterp, SkgConfig, TantivyIndex};

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use std::io::{BufRead, BufReader, Read};
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
    Ok (content) => {
      match update_from_and_rerender_buffer (
        // Most of the work happens here.
        &content, typedb_driver, config, tantivy_index ) {
        Ok (processed_content) => {
          send_response_with_length_prefix (
            stream, &processed_content ); }
        Err (err) => {
          let error_msg : String =
            format! ("Error processing buffer content: {}", err );
          println! ( "{}", error_msg );
          send_response ( stream, &error_msg ); }} }
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
  let content = String::from_utf8 (buffer) ?;
  Ok (content) }

/* Updates dbs and filesystem, and generates text for a new org-buffer.
Steps:
- Put the text through `parse_skg_org_to_nodes` to get some OrgNodeInterps.
- To those OrgNodeInterps:
  - Assigns IDs where needed (`assign_ids_recursive`).
  - Stores as `document_root` the root ID.
- Puts the result through `orgNodeInterpretation_to_nodes` to get Nodes.
- Runs `update_fs_and_dbs` on those Nodes.
- Builds a single root content view from `document_root`. */
fn update_from_and_rerender_buffer (
  content       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &TantivyIndex
) -> Result<String, Box<dyn Error>> {

  let orgnodes : Vec<OrgNodeInterp> =
    parse_skg_org_to_nodes (content);
  if orgnodes.is_empty() {
    return Err ("No valid org nodes found in content".into()); }
  let orgnodes_with_ids : Vec<OrgNodeInterp> =
    orgnodes.into_iter()
    .map ( |node|
            assign_ids_recursive (&node) )
    .collect();
  let document_root : ID =
    match &orgnodes_with_ids[0] {
      OrgNodeInterp::Content(content_node) => {
        content_node.id.clone() . ok_or(
          // earlier 'assign_ids_recursive' => should be 'ok'
          "Root node has no ID")? },
      OrgNodeInterp::Aliases(_) => {
        return Err("Root node cannot be an Aliases node".into()); },
      OrgNodeInterp::Ignored => {
        return Err("Root node cannot be an Ignored node".into());
      }};

  let mut all_nodes : HashSet<SkgNode> =
    HashSet::new ();
  for orgnode in &orgnodes_with_ids {
    let (nodes, _focused_id, _folded_ids) :
      (HashSet<SkgNode>, Option<ID>, HashSet<ID>) =
      orgNodeInterpretation_to_nodes (orgnode);
    all_nodes.extend (nodes); }
  block_on (
    update_fs_and_dbs (
      all_nodes.into_iter().collect::<Vec<SkgNode>>(),
      config.clone(),
      tantivy_index,
      typedb_driver )) ?;
  let regenerated_document : String =
    block_on (
      single_root_view(
        typedb_driver,
        config,
        &document_root ))?;
  Ok (regenerated_document) }

/// Updates **everything** from the given `SkgNode`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_fs_and_dbs (
  nodes         : Vec<SkgNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;
  println!( "1) Updating TypeDB database '{}' ...", db_name );

  let nodes: Vec<SkgNode> =
    nodes . into_iter ()
    . map ( |node|
      clobber_none_fields_with_data_from_disk (
        & config, node ))
    . collect :: <Result <_, _>> () ?;

  update_nodes_and_relationships (
    db_name,
    driver,
    &nodes, ). await ?;
  println!( "   TypeDB update complete." );

  let total_input : usize =
    nodes.len ();
  let target_dir  : &Path =
    &config.skg_folder;
  println!( "2) Writing {} file(s) to disk at {:?} ...",
            total_input, target_dir );
  let written_count : usize =
    write_all_nodes (
      nodes.clone (), config.clone () ) ?;
  println!( "   Wrote {} file(s).", written_count );

  println!( "3) Updating Tantivy index ..." );
  let indexed_count : usize =
    update_index_with_nodes (
      &nodes, tantivy_index )?;
  println!( "   Tantivy updated for {} document(s).",
                indexed_count );

  println!( "All updates finished successfully." );
  Ok (( )) }
