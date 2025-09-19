use crate::render::containerward_org_view;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Extracts node ID and level from a containerward view request,
/// generates the containerward org view,
/// and sends the response with length prefix.
pub fn handle_containerward_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match extract_containerward_view_params ( request ) {
    Ok ( (node_id, level) ) => {
      send_response_with_length_prefix (
        stream,
        & containerward_view_wrapped (
          &node_id,
          level,
          typedb_driver,
          & config,
        )); },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting containerward view parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg );
    }
  }
}

/// Wrapper for containerward_org_view with async and error handling.
fn containerward_view_wrapped (
  node_id       : &ID,
  level         : usize,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  block_on (
    async {
      match containerward_org_view (
        typedb_driver,
        config,
        node_id,
        level ) . await
      { Ok  (s) => s,
        Err (e) => format!(
          "Error generating containerward view: {}", e), }} ) }

/// Parses a request like
/// '((request . "containerward view") (id . "NODE_ID") (level . "LEVEL"))'
/// and extracts the node ID and level.
fn extract_containerward_view_params (
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
