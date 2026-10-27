use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  viewforest_id_from_request,
  send_response_with_length_prefix,
  tag_text_response};

use std::net::TcpStream;

pub fn handle_close_view_request (
  stream     : &mut TcpStream,
  request    : &str,
  views_state : &mut ViewsState,
) {
  match viewforest_id_from_request (request) {
    Ok (viewforest_id) => {
      views_state . open_views . unregister_view (&viewforest_id);
      if ! views_state . open_views . has_diffView_views () {
        // The graph straddle lives while a diff view is open.
        views_state . graph_straddle = None; }
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "view closed" )); },
    Err (_) => {
      send_response_with_length_prefix (
        stream,
        & tag_text_response (
          TcpToClient::CloseView, "Error: missing view-id" )); }} }
