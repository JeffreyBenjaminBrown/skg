use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  view_uri_from_request,
  send_response_with_length_prefix,
  tag_text_response};
use crate::types::views_state::ViewUri;

use std::net::TcpStream;

/// Returns the closed view's URI, if the request named one.
pub fn handle_close_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  views_state : &mut ViewsState,
) -> Option<ViewUri> {
  match view_uri_from_request (request) {
    Ok (uri) => {
      views_state . open_views . unregister_view (&uri);
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "view closed" ));
      Some (uri) },
    Err (_) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "Error: missing view-uri" ));
      None }} }
