use crate::git_ops::vanished::{
  investigate_vanished_skgids, vanished_node_report_as_org};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  format_buffer_response_sexp,
  send_response_with_length_prefix,
  tag_sexp_response,
  value_from_request_sexp};
use crate::skgrepo_sets::SkgrepoRestriction;
use crate::types::env::RuntimeGeneration;
use crate::types::misc::ID;
use crate::types::nodes::rust::GraphnodeInRust;

use std::collections::BTreeSet;
use std::net::TcpStream;

/// Handle "investigate unknown node": say what the requested ID is.
/// If it resolves to a node of the live graph, the answer names that
/// node; otherwise it is what git history says the ID used to be
/// ('server/git_ops/vanished.rs'). Refused under a skgrepo
/// restriction other than 'all', since history can hold the text of
/// any skgrepo.
pub fn handle_investigate_unknown_node_request (
  stream      : &mut TcpStream,
  request     : &str,
  runtime     : &RuntimeGeneration,
  restriction : &SkgrepoRestriction,
) {
  let (content, errors) : (String, Vec<String>) =
    match investigation_as_org (request, runtime, restriction) {
      Ok (org) => (org, Vec::new ()),
      Err (e) => (
        format! ("* investigation failed\n** {}\n", e),
        vec! [e] ), };
  let response : String =
    format_buffer_response_sexp (&content, &errors, &[]);
  send_response_with_length_prefix (
    stream,
    &tag_sexp_response (TcpToClient::InvestigateUnknownNode, &response) ); }

fn investigation_as_org (
  request     : &str,
  runtime     : &RuntimeGeneration,
  restriction : &SkgrepoRestriction,
) -> Result<String, String> {
  if ! restriction . is_all () {
    return Err ( format! (
      "Investigating an unknown node requires no skgrepo restriction (the skgrepo-set all); current skgrepo restriction is {}",
      restriction . name )); }
  let skgid : ID = ID::from ( value_from_request_sexp ("id", request) ? );
  if let Some (pid) = runtime . graph . pid_of (&skgid) {
    let node : &GraphnodeInRust = runtime . graph . get (&pid)
      . ok_or_else ( || format! ( "{} resolves to {}, which has no node", skgid, pid )) ?;
    return Ok ( format! ( "{} resolves to node {} ({}) in {}\n",
                          skgid, node . title, pid, node . home_skgrepo )); }
  let reports = investigate_vanished_skgids (
    &runtime . config, &BTreeSet::from ([ skgid ]) );
  Ok ( reports . iter () . map (vanished_node_report_as_org) . collect () ) }
