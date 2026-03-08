use crate::serve::ConnectionState;
use crate::serve::util::{
  view_uri_from_request,
  send_response_with_length_prefix,
  tag_text_response};

use std::net::TcpStream;

pub fn handle_close_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  conn_state : &mut ConnectionState,
) {
  match view_uri_from_request (request) {
    Ok (uri) => {
      conn_state . memory . unregister_view (&uri);
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          "close-view", "view closed" )); },
    Err (_) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          "close-view", "Error: missing view-uri" )); } } }
