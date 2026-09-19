//! Read-only state lookup for client property gestures.  This endpoint is
//! advisory; ownership and mutability are checked again by the save path.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::nodecomplete_from_graph;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, value_from_request_sexp};
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SourceName};
use crate::types::nodes::complete::{
  FileProperty, file_property_is_true};

use std::net::TcpStream;

pub fn handle_boolprop_state_request (
  stream  : &mut TcpStream,
  request : &str,
  env     : &SkgEnv,
) {
  let response : String = match boolprop_state_response_body (request, env) {
    Ok (body) => body,
    Err (message) => format! ("(error {})", quoted (&message)), };
  send_response_with_length_prefix (stream, &format! (
    "((response-type {}) {})",
    TcpToClient::BoolPropState . repr_in_client (), response));
}

fn boolprop_state_response_body (
  request : &str,
  env     : &SkgEnv,
) -> Result<String, String> {
  let runtime = env . runtime_snapshot ();
  let id : ID = ID::from (value_from_request_sexp ("id", request) ?);
  let property_name : String = value_from_request_sexp ("property", request) ?;
  let property : FileProperty = FileProperty::from_wire_name (&property_name)
    . ok_or_else (|| format! ("unknown property '{}'", property_name)) ?;
  let (pid, source, value, owned) = boolprop_state (
    &runtime . graph, &runtime . config, &id, property) ?;
  Ok (format! (
    "(id {}) (property {}) (value {}) (source {}) (user-owned {})",
    quoted (&pid . 0), quoted (property . wire_name ()),
    quoted (if value { "true" } else { "false" }),
    quoted (&source . 0), quoted (if owned { "true" } else { "false" })))
}

pub fn boolprop_state (
  graph    : &InRustGraph,
  config   : &SkgConfig,
  id       : &ID,
  property : FileProperty,
) -> Result<(ID, SourceName, bool, bool), String> {
  let (pid, source) : (ID, SourceName) = graph . pid_and_source (id)
    . ok_or_else (|| format! ("id '{}' is not in the graph", id)) ?;
  let node = nodecomplete_from_graph (graph, &pid)
    . ok_or_else (|| format! ("canonical id '{}' is not in the graph", pid)) ?;
  let value : bool = file_property_is_true (&node . misc, property);
  let user_owned : bool = config . user_owns_source (&source);
  Ok ((pid, source, value, user_owned))
}

fn quoted (s : &str) -> String {
  let mut result : String = String::from ("\"");
  for ch in s . chars () {
    match ch {
      '\\' => result . push_str ("\\\\"),
      '"'  => result . push_str ("\\\""),
      '\n' => result . push_str ("\\n"),
      _    => result . push (ch), }}
  result . push ('"');
  result
}

#[cfg(test)]
#[path = "../../../tests/unit/boolprop_state.rs"]
mod tests;
