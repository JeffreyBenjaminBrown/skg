use crate::serve::ConnectionState;
use crate::serve::util::{view_uri_from_request, send_response};

use std::net::TcpStream;

pub fn handle_close_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  conn_state : &mut ConnectionState,
) {
  match view_uri_from_request ( request ) {
    Ok ( uri ) => {
      conn_state . memory . unregister_view ( &uri );
      send_response ( stream, "view closed" ); },
    Err ( _ ) => {
      send_response ( stream, "Error: missing view-uri" ); } } }
