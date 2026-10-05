//! Read-only state lookup for client flag gestures.  This endpoint is
//! advisory; ownership and mutability are checked again by the save path.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::node_lookup::graphnode_from_graph;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, value_from_request_sexp};
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, SkgrepoName};
use crate::types::nodes::complete::{
  Flag, flag_is_true};

use std::net::TcpStream;

pub fn handle_flag_state_request (
  stream  : &mut TcpStream,
  request : &str,
  env     : &SkgEnv,
) {
  let response : String = match flag_state_response_body (request, env) {
    Ok (body) => body,
    Err (message) => format! ("(error {})", quoted (&message)), };
  send_response_with_length_prefix (stream, &format! (
    "((response-type {}) {})",
    TcpToClient::FlagState . repr_in_client (), response));
}

fn flag_state_response_body (
  request : &str,
  env     : &SkgEnv,
) -> Result<String, String> {
  let runtime = env . runtime_snapshot ();
  let skgid : ID = ID::from (value_from_request_sexp ("id", request) ?);
  let flag_name : String = value_from_request_sexp ("flag", request) ?;
  let flag : Flag = Flag::from_wire_name (&flag_name)
    . ok_or_else (|| format! ("unknown flag '{}'", flag_name)) ?;
  let (pid, skgrepo, value, owned) = flag_state (
    &runtime . graph, &runtime . config, &skgid, flag) ?;
  Ok (format! (
    "(id {}) (flag {}) (value {}) (repo {}) (owned {})",
    quoted (&pid . 0), quoted (flag . wire_name ()),
    quoted (if value { "true" } else { "false" }),
    quoted (&skgrepo . 0), quoted (if owned { "true" } else { "false" })))
}

pub fn flag_state (
  graph    : &InRustGraph,
  config   : &SkgConfig,
  skgid    : &ID,
  flag : Flag,
) -> Result<(ID, SkgrepoName, bool, bool), String> {
  let (pid, skgrepo) : (ID, SkgrepoName) = graph . pid_and_skgrepo (skgid)
    . ok_or_else (|| format! ("id '{}' is not in the graph", skgid)) ?;
  let node = graphnode_from_graph (graph, &pid)
    . ok_or_else (|| format! ("canonical id '{}' is not in the graph", pid)) ?;
  let value : bool = flag_is_true (&node . flags, flag);
  let owned : bool = config . skgrepo_is_owned (&skgrepo);
  Ok ((pid, skgrepo, value, owned))
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
#[path = "../../../tests/unit/flag_state.rs"]
mod tests;
