use crate::file_io::one_node::fetch_aliases_from_file;
use crate::mk_org_text::aliases::aliases_to_org;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::serve::parse_headline_md_sexp::parse_headline_from_sexp;
use crate::types::misc::SkgConfig;

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
    Ok ( (orgnode_md, level, _title) ) => {
      match orgnode_md.id {
        Some ( id ) => {
          let aliases : Vec<String> =
            fetch_aliases_from_file ( config, id );
          let org_content : String =
            aliases_to_org ( aliases, level );
          send_response_with_length_prefix (
            stream,
            & org_content ); },
        None => {
          let error_msg : String =
            "No ID found in headline metadata"
            . to_string ();
          println! ( "{}", error_msg ) ;
          send_response ( stream, &error_msg ); }}
    }, Err ( err ) => {
      let error_msg : String = format!(
        "Error extracting node aliases parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); }} }
