use crate::render::single_root_view;
use crate::serve::util::node_id_from_single_root_view_request;
use crate::serve::util::send_response;
use crate::serve::util::send_response_with_length_prefix;
use crate::types::{ID, SkgConfig};

use futures::executor::block_on;
use std::net::TcpStream; // handles two-way communication
use typedb_driver::TypeDBDriver;

/// Gets a node id from the request,
/// generates an org view of that id's content (recursively),
/// and sends the Org to Emacs (length-prefixed).
pub fn handle_single_root_view_request (
  stream        : &mut TcpStream,
  request       : &str,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) {

  match node_id_from_single_root_view_request ( request ) {
    Ok ( node_id ) => {
      send_response_with_length_prefix (
        stream,
        & single_root_view_wrapped (
          &node_id,
          typedb_driver,
          & config,
        )); },
    Err ( err ) => {
      let error_msg = format!(
        "Error extracting node ID: {}", err);
      println! ( "{}", error_msg ) ;
      send_response ( stream, &error_msg ); } } }

/// It's just `single_root_view`,
/// plus async and error handling.
fn single_root_view_wrapped (
  node_id       : &ID,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
) -> String {
  block_on (
    async {
      match single_root_view (
        typedb_driver,
        config,
        node_id ) . await
      { Ok  (s) => s,
        Err (e) => format!(
          "Error generating document: {}", e), }} ) }
