//! `relRepo info` (BUG-and-fix_make-edge-more-public.org): given
//! one relationship edge -- owner id, member id, and the relation
//! between them -- reply with the edge's default relRepo and its
//! current relRepo (when the graph records the edge). The client's
//! 'skg-set-relRepo' gesture uses this to offer only
//! repos the save's default floor can accept, instead of the whole
//! ladder. The reply is advisory: the save-time floor check in
//! 'apply_sticky_relRepos' stays load-bearing, since buffers go stale
//! and the '(editRequest (relRepo ...))' request is plain text anyone can type.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  value_from_request_sexp };
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, RepoName};

use std::net::TcpStream;

pub fn handle_relRepo_info_request (
  stream  : &mut TcpStream,
  request : &str,
  env     : &SkgEnv,
) {
  let response : String =
    match relRepo_info_response_body (request, env) {
      Ok  (body) => body,
      Err (msg)  => format! (
        "(error {})", quoted (&msg) ) };
  send_response_with_length_prefix ( stream, & format! (
    "((response-type {}) {})",
    TcpToClient::RelRepoInfo . repr_in_client (),
    response )); }

/// The payload fields of a successful reply:
/// '(default "NAME") (current "NAME")', with '(current ...)' absent
/// when the graph records no such edge (e.g. one just typed into a
/// buffer and not yet saved).
fn relRepo_info_response_body (
  request : &str,
  env     : &SkgEnv,
) -> Result<String, String> {
  let runtime = env . runtime_snapshot ();
  let owner : ID = ID (
    value_from_request_sexp ("owner", request) ? );
  let member : ID = ID (
    value_from_request_sexp ("member", request) ? );
  let relation : NodeRelation = relation_from_client_string (
    & value_from_request_sexp ("relation", request) ? ) ?;
  let (default, current) : (RepoName, Option<RepoName>) =
    relRepo_info (
      &runtime . graph, &runtime . config,
      &owner, &member, relation ) ?;
  let mut body : String = format! (
    "(default {})", quoted ( & default . 0 ));
  if let Some (repo) = current {
    body . push_str ( & format! (
      " (current {})", quoted ( & repo . 0 ))); }
  Ok (body) }

/// One edge's (default relRepo, current relRepo). Between owned nodes,
/// the default is the more private endpoint home. From an owned
/// owner to a foreign or unresolved member, it is the owner's home.
/// The current relRepo is None when the graph records no such exact raw
/// member.  This lets an Unknown placeholder edit a stored dangling edge.
pub fn relRepo_info (
  graph    : &crate::dbs::in_rust_graph::InRustGraph,
  config   : &crate::types::misc::SkgConfig,
  owner    : &ID,
  member   : &ID,
  relation : NodeRelation,
) -> Result<(RepoName, Option<RepoName>), String> {
  let (owner_pid, owner_home) : (ID, RepoName) =
    graph . pid_and_repo (owner)
    . ok_or_else ( || format! (
      "owner '{}' is not in the graph", owner )) ?;
  let member_home : RepoName = graph . pid_and_repo (member)
    . map ( |(_pid, src)| src )
    // An unresolved destination has no home to make this edge more
    // private, so its writable relationship defaults to the owner's home.
    . unwrap_or_else ( || owner_home . clone () );
  let default : RepoName = config . default_relRepo (
    &owner_home, &member_home );
  let current : Option<RepoName> =
    graph . relRepo_for_stored_member ( &owner_pid, relation, member );
  Ok (( default, current )) }

/// The three relations an explicit '(editRequest (relRepo ...))'
/// request can name
/// (matching 'RequestedRelRepos'). Hides and links have no
/// explicit-relRepo path, so asking about them is an error.
fn relation_from_client_string (
  s : &str,
) -> Result<NodeRelation, String> {
  match s {
    "contains"          => Ok (NodeRelation::Contains),
    "subscribes_to"     => Ok (NodeRelation::SubscribesTo),
    "overrides_view_of" => Ok (NodeRelation::OverridesViewOf),
    other => Err ( format! (
      "unsupported relation '{}': the explicit-relSource path covers \
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
#[path = "../../../tests/unit/relRepo_info.rs"]
mod tests;
