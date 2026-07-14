//! 'edge level info' (BUG-and-fix_make-edge-more-public.org): given
//! one relationship edge -- owner id, member id, and the relation
//! between them -- reply with the edge's DEFAULT level (the more
//! private of the two endpoints' homes) and its CURRENT level (when
//! the graph records the edge). The client's
//! 'skg-set-relationship-source' gesture uses this to offer only
//! levels the save's default floor can accept, instead of the whole
//! ladder. The reply is advisory: the save-time floor check in
//! 'apply_sticky_levels' stays load-bearing, since buffers go stale
//! and the '(relSource ...)' atom is plain text anyone can type.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  value_from_request_sexp };
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SourceName};

use std::net::TcpStream;

pub fn handle_edge_level_info_request (
  stream  : &mut TcpStream,
  request : &str,
  env     : &SkgEnv,
) {
  let response : String =
    match edge_level_info_response_body (request, env) {
      Ok  (body) => body,
      Err (msg)  => format! (
        "(error {})", quoted (&msg) ) };
  send_response_with_length_prefix ( stream, & format! (
    "((response-type {}) {})",
    TcpToClient::EdgeLevelInfo . repr_in_client (),
    response )); }

/// The payload fields of a successful reply:
/// '(default "NAME") (current "NAME")', with '(current ...)' absent
/// when the graph records no such edge (e.g. one just typed into a
/// buffer and not yet saved).
fn edge_level_info_response_body (
  request : &str,
  env     : &SkgEnv,
) -> Result<String, String> {
  let owner : ID = ID (
    value_from_request_sexp ("owner", request) ? );
  let member : ID = ID (
    value_from_request_sexp ("member", request) ? );
  let relation : NodeRelation = relation_from_client_string (
    & value_from_request_sexp ("relation", request) ? ) ?;
  let (default, current) : (SourceName, Option<SourceName>) =
    edge_level_info (
      & env . in_rust_graph_snapshot (), & env . config,
      &owner, &member, relation ) ?;
  let mut body : String = format! (
    "(default {})", quoted ( & default . 0 ));
  if let Some (level) = current {
    body . push_str ( & format! (
      " (current {})", quoted ( & level . 0 ))); }
  Ok (body) }

/// One edge's (default level, current level). The default is the
/// more private of the two endpoints' homes; the current level is
/// None when the graph records no such edge (e.g. one just typed
/// into a buffer and not yet saved).
pub fn edge_level_info (
  graph    : &crate::dbs::in_rust_graph::InRustGraph,
  config   : &crate::types::misc::SkgConfig,
  owner    : &ID,
  member   : &ID,
  relation : NodeRelation,
) -> Result<(SourceName, Option<SourceName>), String> {
  let (owner_pid, owner_home) : (ID, SourceName) =
    graph . pid_and_source (owner)
    . ok_or_else ( || format! (
      "owner '{}' is not in the graph", owner )) ?;
  let member_home : SourceName =
    graph . pid_and_source (member)
    . map ( |(_pid, src)| src )
    . ok_or_else ( || format! (
      "member '{}' is not in the graph", member )) ?;
  let default : SourceName = config . more_private_of (
    owner_home, member_home );
  let current : Option<SourceName> =
    graph . edge_level ( &owner_pid, relation, member );
  Ok (( default, current )) }

/// The three relations an explicit '(relSource ...)' atom can name
/// (matching 'ExplicitLevels'). Hides and textlinks have no
/// explicit-level path, so asking about them is an error.
fn relation_from_client_string (
  s : &str,
) -> Result<NodeRelation, String> {
  match s {
    "contains"          => Ok (NodeRelation::Contains),
    "subscribes_to"     => Ok (NodeRelation::Subscribes),
    "overrides_view_of" => Ok (NodeRelation::OverridesViewOf),
    other => Err ( format! (
      "unsupported relation '{}': the explicit-level path covers \
       contains, subscribes_to and overrides_view_of", other )), }}

fn quoted (
  s : &str,
) -> String {
  let mut result : String = String::from ("\"");
  for ch in s . chars () {
    match ch {
      '\\' => result . push_str ("\\\\"),
      '"'  => result . push_str ("\\\""),
      '\n' => result . push_str ("\\n"),
      _    => result . push (ch), }}
  result . push ('"');
  result }

#[cfg(test)]
#[path = "../../../tests/unit/edge_level_info.rs"]
mod tests;
