use crate::render::containerward_org_view;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::text_to_orgnodes::uninterpreted::parse_headline_from_sexp;
use crate::types::{ID, SkgConfig, find_id_in_metadata_collection};

use futures::executor::block_on;
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

/// Extracts parameters from a containerward view request,
/// generates the containerward org view,
/// and sends the response with length prefix.
pub fn handle_containerward_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match parse_headline_from_sexp ( request ) {
    Ok ( (metadata_items, level, _title) ) => {
      // Extract ID from metadata items
      let node_id: Result<ID, &str> =
        find_id_in_metadata_collection (&metadata_items)
        . ok_or ("No ID found in headline metadata");

      match node_id {
        Ok(id) => {
          send_response_with_length_prefix (
            stream,
            & containerward_view_wrapped (
              &id,
              level,
              typedb_driver,
              & config,
            )); },
        Err(e) => {
          let error_msg = format!(
            "Error extracting ID from metadata: {}",
            e);
          println! ( "{}", error_msg ) ;
          send_response ( stream, &error_msg ); }} },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting containerward view parameters: {}",
        err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); }}
}

/// Wrapper for containerward_org_view with async and error handling.
fn containerward_view_wrapped (
  node_id       : &ID,
  level         : usize,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  block_on ( async {
    match containerward_org_view (
      typedb_driver,
      config,
      node_id,
      level ) . await
    { Ok  (s) => s,
      Err (e) => format!(
        "Error generating containerward view: {}", e), }} ) }
