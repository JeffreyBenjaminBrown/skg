//! Batch existence and home-source lookup from the published graph snapshot.
//! It exposes no title or inactive node source information.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, value_from_request_sexp};
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::{ID, SkgConfig};
use crate::types::sexp::extract_string_list_from_sexp;
use sexp::Sexp;
use std::net::TcpStream;

#[derive(Debug, Eq, PartialEq)]
pub enum LinkStatus {
  Resolved { pid : ID, source_label : String },
  Inactive,
  Missing,
}

pub fn classify_link_ids (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveSourceSet,
  ids    : &[ID],
) -> Vec<(ID, LinkStatus)> {
  ids . iter () . map (|id| {
    let status : LinkStatus = match graph . pid_and_source (id) {
      None => LinkStatus::Missing,
      Some ((_pid, source)) if ! active . is_all ()
        && ! active . contains_source (&source) => LinkStatus::Inactive,
      Some ((pid, source)) => {
        let source_label : String = config . sources . get (&source)
          .map (|s| s . herald_label () . to_string ())
          .unwrap_or_else (|| source . 0 . clone ());
        LinkStatus::Resolved { pid, source_label } } };
    (id . clone (), status) }) . collect () }

pub fn handle_link_statuses_request (
  stream  : &mut TcpStream,
  request : &str,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active  : &ActiveSourceSet,
) {
  let response : String = match link_statuses_response (
    request, graph, config, active ) {
    Ok (body) => body,
    Err (message) => format! ("(error {})", quoted (&message)), };
  send_response_with_length_prefix (stream, &format! (
    "((response-type {}) {})",
    TcpToClient::LinkStatuses . repr_in_client (), response)); }

fn link_statuses_response (
  request : &str,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active  : &ActiveSourceSet,
) -> Result<String, String> {
  let request_id : String = value_from_request_sexp (
    "request-id", request) ?;
  let parsed : Sexp = sexp::parse (request)
    .map_err (|e| format! ("invalid link-status request: {}", e)) ?;
  let ids : Vec<ID> = extract_string_list_from_sexp (&parsed, "ids")
    .map_err (|e| e . to_string ()) ?
    . into_iter () . map (ID::from) . collect ();
  let rows : Vec<String> = classify_link_ids (
    graph, config, active, &ids)
    .into_iter ()
    .map (|(id, status)| match status {
      LinkStatus::Missing => format! ("({} missing)", quoted (&id . 0)),
      LinkStatus::Inactive => format! ("({} inactive)", quoted (&id . 0)),
      LinkStatus::Resolved { pid, source_label } => format! (
        "({} resolved {} {})",
        quoted (&id . 0), quoted (&pid . 0), quoted (&source_label)), })
    .collect ();
  Ok (format! ("(request-id {}) (results ({}))",
    quoted (&request_id), rows . join (" "))) }

fn quoted (s : &str) -> String {
  let mut result : String = String::from ("\"");
  for ch in s . chars () {
    match ch {
      '\\' => result . push_str ("\\\\"),
      '"' => result . push_str ("\\\""),
      '\n' => result . push_str ("\\n"),
      '\r' => result . push_str ("\\r"),
      '\t' => result . push_str ("\\t"),
      _ => result . push (ch), }}
  result . push ('"');
  result }

#[cfg(test)]
mod tests {
  use super::*;
  use crate::dbs::filesystem::not_nodes::load_config;
  use crate::source_sets::SourceSetName;
  use crate::types::misc::SourceName;
  use crate::types::nodes::complete::{NodeComplete, empty_node_complete};

  #[test]
  fn statuses_use_the_published_graph_and_hide_inactive_sources () {
    let mut config : SkgConfig = load_config (
      "tests/source_sets/fixtures/skgconfig.toml") . unwrap ();
    config . sources . get_mut (&SourceName::from ("public"))
      .unwrap () . abbreviation = Some ("pub" . to_string ());
    let active : ActiveSourceSet = ActiveSourceSet::named (
      &config, SourceSetName::from ("public")) . unwrap ();
    let visible : NodeComplete = NodeComplete {
      pid : ID::from ("visible"),
      extra_ids : vec![ID::from ("old-visible")],
      source : SourceName::from ("public"),
      title : "A title that must stay out of lookup results" .to_string (),
      .. empty_node_complete () };
    let private : NodeComplete = NodeComplete {
      pid : ID::from ("private"),
      source : SourceName::from ("private"),
      title : "Private title" .to_string (),
      .. empty_node_complete () };
    let graph : InRustGraph = InRustGraph::from_nodecompletes (
      &[visible, private]);
    let ids : Vec<ID> = ["old-visible", "private", "unknown"]
      .into_iter () . map (ID::from) .collect ();
    assert_eq! (classify_link_ids (&graph, &config, &active, &ids), vec![
      (ID::from ("old-visible"), LinkStatus::Resolved {
        pid : ID::from ("visible"), source_label : "pub" .to_string () }),
      (ID::from ("private"), LinkStatus::Inactive),
      (ID::from ("unknown"), LinkStatus::Missing) ]);
    let response : String = link_statuses_response (
      "((request . \"link statuses\") (request-id . \"buffer:9\") (ids \"old-visible\" \"private\" \"unknown\"))",
      &graph, &config, &active ) . unwrap ();
    assert! (response . contains ("(request-id \"buffer:9\")"));
    assert! (response . contains ("(\"old-visible\" resolved \"visible\" \"pub\")"));
    assert! (response . contains ("(\"private\" inactive)"));
    assert! (response . contains ("(\"unknown\" missing)"));
    assert! (! response . contains ("Private title"));
  }
}
