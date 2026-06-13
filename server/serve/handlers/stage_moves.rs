use crate::git_ops::find_and_stage_moves::stage_moves_script;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_buffer_response_sexp,
  send_response_with_length_prefix,
  tag_sexp_response};
use crate::types::misc::SkgConfig;

use std::net::TcpStream;

/// Build, for every detected single-source-to-single-source node
/// move, a shell script that stages it, and send it to the client as
/// a buffer. Scans all configured sources irrespective of the active
/// source-set: a move can cross source-set boundaries, so restricting
/// would hide moves. Non-git sources simply contribute nothing.
pub fn handle_stage_moves_request (
  stream : &mut TcpStream,
  config : &SkgConfig,
) {
  let (content, errors) : (String, Vec<String>) =
    match stage_moves_script (config) {
      Ok (script) => (script, Vec::new ()),
      Err (e) => (
        format! ("# stage moves failed\n# {}\n", e),
        vec! [e] ), };
  let response : String =
    format_buffer_response_sexp (&content, &errors, &[]);
  send_response_with_length_prefix (
    stream,
    &tag_sexp_response (TcpToClient::StageMoves, &response) ); }
