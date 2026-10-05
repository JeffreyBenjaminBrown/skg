//! `relRepo info` (BUG-and-fix_make-edge-more-public.org): given
//! one relationship -- recorder id, member id, and the relation
//! between them -- reply with the relationship's default relRepo and its
//! current relRepo (when the graph records the relationship). The client's
//! 'skg-set-relRepo' gesture uses this to offer only
//! Skgrepos the save's default floor can accept, instead of the whole
//! ladder. The reply is advisory: the save-time floor check in
//! 'apply_sticky_relRepos' stays load-bearing, since buffers go stale
//! and the '(editRequest (relRepo ...))' request is plain text anyone can type.

use crate::dbs::in_rust_graph::relation_accessors::NodeRelation;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  value_from_request_sexp };
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgrepoName};

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
/// when the graph records no such relationship (e.g. one just typed into a
/// buffer and not yet saved).
fn relRepo_info_response_body (
  request : &str,
  env     : &SkgEnv,
) -> Result<String, String> {
  let runtime = env . runtime_snapshot ();
  let recorder : ID = ID (
    value_from_request_sexp ("owner", request) ? );
  let member : ID = ID (
    value_from_request_sexp ("member", request) ? );
  let relation : NodeRelation = relation_from_client_string (
    & value_from_request_sexp ("relation", request) ? ) ?;
  let (default, current) : (SkgrepoName, Option<SkgrepoName>) =
    relRepo_info (
      &runtime . graph, &runtime . config,
      &recorder, &member, relation ) ?;
  let mut body : String = format! (
    "(default {})", quoted ( & default . 0 ));
  if let Some (skgrepo) = current {
    body . push_str ( & format! (
      " (current {})", quoted ( & skgrepo . 0 ))); }
  Ok (body) }

/// One relationship's (default relRepo, current relRepo). Between owned nodes,
/// the default is the more private endpoint home. From an owned
/// recorder to a foreign or unresolved member, it is the recorder's home.
/// The current relRepo is None when the graph records no such exact raw
/// member.  This lets an Unknown phantom edit a stored dangling relationship.
pub fn relRepo_info (
  graph    : &crate::dbs::in_rust_graph::InRustGraph,
  config   : &crate::types::misc::SkgConfig,
  recorder : &ID,
  member   : &ID,
  relation : NodeRelation,
) -> Result<(SkgrepoName, Option<SkgrepoName>), String> {
  let (recorder_pid, recorder_home) : (ID, SkgrepoName) =
    graph . pid_and_skgrepo (recorder)
    . ok_or_else ( || format! (
      "recorder '{}' is not in the graph", recorder )) ?;
  let member_home : SkgrepoName = graph . pid_and_skgrepo (member)
    . map ( |(_pid, src)| src )
    // An unresolved destination has no home to make this relationship more
    // private, so its writable relationship defaults to the recorder's home.
    . unwrap_or_else ( || recorder_home . clone () );
  let default : SkgrepoName = config . default_relRepo (
    &recorder_home, &member_home );
  let current : Option<SkgrepoName> =
    graph . relRepo_for_stored_member ( &recorder_pid, relation, member );
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
    "subscribesTo"      => Ok (NodeRelation::SubscribesTo),
    "overrides"         => Ok (NodeRelation::Overrides),
    other => Err ( format! (
      "unsupported relation '{}': the explicit-relRepo path covers \
       contains, subscribesTo and overrides", other )), }}

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
