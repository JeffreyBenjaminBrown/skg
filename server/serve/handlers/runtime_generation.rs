//! PURPOSE: Answer the current runtime generation. A client opening a
//! buffer no view response drew (a new empty view) asks it, and keeps
//! the answer as that buffer's 'rendered-at', so the buffer's save is
//! checked for staleness like any other.
//!
//! Response: ((response-type runtime-generation) (content "N"))

use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::env::SkgEnv;

use std::net::TcpStream;

pub fn handle_runtime_generation_request (
  stream : &mut TcpStream,
  env    : &SkgEnv,
) {
  send_response_with_length_prefix (
    stream,
    & runtime_generation_response (
      env . runtime_snapshot () . generation )); }

pub fn runtime_generation_response (
  generation : u64,
) -> String {
  tag_text_response (
    TcpToClient::RuntimeGeneration,
    & generation . to_string () ) }

#[cfg(test)]
#[path = "../../../tests/unit/runtime_generation_request.rs"]
mod tests;
