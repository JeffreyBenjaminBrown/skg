use crate::file_io::fetch_aliases_from_file;
use crate::render::aliases_to_org;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::types::{ID, SkgConfig};

use std::net::TcpStream;

/// Extracts node ID and level from a node aliases request,
/// fetches aliases from file, formats as org-mode,
/// and sends the response with length prefix.
pub fn handle_node_aliases_request (
  stream  : &mut TcpStream,
  request : &str,
  config  : &SkgConfig,
) {

  match extract_node_aliases_params ( request ) {
    Ok ( (node_id, level) ) => {
      let aliases : Vec<String> =
        fetch_aliases_from_file ( config, node_id );
      let org_content : String =
        aliases_to_org ( aliases, level );
      send_response_with_length_prefix (
        stream,
        &org_content );
    }, Err ( err ) => {
      let error_msg = format!(
        "Error extracting node aliases parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg );
    }
  }
}

/// Parses a request like
/// '((request . "node aliases") (id . "NODE_ID") (level . "LEVEL"))'
/// and extracts the node ID and level.
fn extract_node_aliases_params (
  request : &str
) -> Result<(ID, usize), String> {

  // Look for the id field
  let id_start : usize =
    request.find ( "(id . \"" )
    .ok_or ( "Missing id field" )?;
  let id_content_start : usize =
    id_start + "(id . \"".len();
  let id_end : usize =
    request [ id_content_start.. ].find ( "\")" )
    .ok_or ( "Malformed id field" ) ?;
  let node_id_str : &str =
    &request [ id_content_start ..
               id_content_start + id_end ];
  let node_id : ID =
    ID::new ( node_id_str );

  // Look for the level field
  let level_start : usize =
    request.find ( "(level . \"" )
    .ok_or ( "Missing level field" ) ?;
  let level_content_start : usize =
    level_start + "(level . \"".len();
  let level_end : usize =
    request [ level_content_start.. ].find ( "\")" )
    .ok_or ( "Malformed level field" ) ?;
  let level_str : &str =
    &request [ level_content_start..level_content_start + level_end ];
  let level : usize =
    level_str.parse::<usize>()
    .map_err ( |e| format! ( "Invalid level value: {}", e )) ?;

  Ok (( node_id, level ))
}
