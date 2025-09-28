use crate::file_io::fetch_aliases_from_file;
use crate::render::aliases_to_org;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::text_to_orgnodes::uninterpreted::parse_headline_from_sexp;
use crate::types::{ID, SkgConfig, find_id_in_metadata_collection};

use std::net::TcpStream;

/// Extracts parameters from a node aliases request,
/// fetches aliases from file, formats as org-mode,
/// and sends the response with length prefix.
pub fn handle_node_aliases_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {

  match parse_headline_from_sexp ( request ) {
    Ok ( (metadata_items, level, _title) ) => {
      let node_id: Result<ID, &str> =
        find_id_in_metadata_collection(&metadata_items)
        .ok_or("No ID found in headline metadata");
      match node_id {
        Ok(id) => {
          let aliases : Vec<String> =
            fetch_aliases_from_file ( config, id );
          let org_content : String =
            aliases_to_org ( aliases, level );
          send_response_with_length_prefix (
            stream,
            & org_content ); },
        Err(e) => {
          let error_msg = format!(
            "Error extracting ID from metadata: {}",
            e);
          println! ( "{}", error_msg ) ;
          send_response ( stream, &error_msg ); }}
    }, Err ( err ) => {
      let error_msg = format!(
        "Error extracting node aliases parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); }} }
