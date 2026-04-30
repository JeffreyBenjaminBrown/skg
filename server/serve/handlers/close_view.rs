use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  send_response_with_length_prefix,
  tag_text_response};

use std::net::TcpStream;

pub fn handle_close_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  views_state : &mut ViewsState,
) {
  match view_uri_from_request (request) {
    Ok (uri) => {
      views_state . open_views . unregister_view (&uri);
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "view closed" )); },
    Err (_) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "Error: missing view-uri" )); }} }
