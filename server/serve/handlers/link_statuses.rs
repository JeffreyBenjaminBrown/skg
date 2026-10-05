//! Batch existence and home-repo lookup from the published graph snapshot.
//! It exposes no title or inactive node skgrepo information.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, value_from_request_sexp};
use crate::skgrepo_sets::ActiveSkgRepoSet;
use crate::types::misc::{ID, SkgConfig};
use crate::types::sexp::extract_string_list_from_sexp;
use sexp::Sexp;
use std::net::TcpStream;

#[derive(Debug, Eq, PartialEq)]
pub enum LinkStatus {
  Resolved { pid : ID, skgrepo_label : String },
  Inactive,
  Missing,
}

pub fn classify_link_skgids (
  graph  : &InRustGraph,
  config : &SkgConfig,
  active : &ActiveSkgRepoSet,
  skgids : &[ID],
) -> Vec<(ID, LinkStatus)> {
  skgids . iter () . map (|skgid| {
    let status : LinkStatus = match graph . pid_and_skgrepo (skgid) {
      None => LinkStatus::Missing,
      Some ((_pid, skgrepo)) if ! active . is_all ()
        && ! active . contains_skgrepo (&skgrepo) => LinkStatus::Inactive,
      Some ((pid, skgrepo)) => {
        let skgrepo_label : String = config . skgrepos . get (&skgrepo)
          .map (|s| s . herald_label () . to_string ())
          .unwrap_or_else (|| skgrepo . 0 . clone ());
        LinkStatus::Resolved { pid, skgrepo_label } } };
    (skgid . clone (), status) }) . collect () }

pub fn handle_link_statuses_request (
  stream  : &mut TcpStream,
  request : &str,
  graph   : &InRustGraph,
  config  : &SkgConfig,
  active  : &ActiveSkgRepoSet,
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
  active  : &ActiveSkgRepoSet,
) -> Result<String, String> {
  let request_id : String = value_from_request_sexp (
    "request-id", request) ?;
  let parsed : Sexp = sexp::parse (request)
    .map_err (|e| format! ("invalid link-status request: {}", e)) ?;
  let skgids : Vec<ID> = extract_string_list_from_sexp (&parsed, "ids")
    .map_err (|e| e . to_string ()) ?
    . into_iter () . map (ID::from) . collect ();
  let rows : Vec<String> = classify_link_skgids (
    graph, config, active, &skgids)
    .into_iter ()
    .map (|(skgid, status)| match status {
      LinkStatus::Missing => format! ("({} missing)", quoted (&skgid . 0)),
      LinkStatus::Inactive => format! ("({} inactive)", quoted (&skgid . 0)),
      LinkStatus::Resolved { pid, skgrepo_label } => format! (
        "({} resolved {} {})",
        quoted (&skgid . 0), quoted (&pid . 0), quoted (&skgrepo_label)), })
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
  use crate::skgrepo_sets::SkgRepoSetName;
  use crate::types::misc::SkgRepoName;
  use crate::types::nodes::complete::{Graphnode, empty_graphnode};
  use crate::dbs::in_rust_graph::InRustGraphHandle;
  use arc_swap::ArcSwap;
  use std::sync::Arc;

  #[test]
  fn statuses_use_the_published_graph_and_hide_inactive_skgrepos () {
    let mut config : SkgConfig = load_config (
      "tests/repo_sets/fixtures/skgconfig.toml") . unwrap ();
    config . skgrepos . get_mut (&SkgRepoName::from ("public"))
      .unwrap () . abbreviation = Some ("pub" . to_string ());
    let active : ActiveSkgRepoSet = ActiveSkgRepoSet::named (
      &config, SkgRepoSetName::from ("public")) . unwrap ();
    let visible : Graphnode = Graphnode {
      pid : ID::from ("visible"),
      extra_ids : vec![ID::from ("old-visible")],
      home_skgrepo : SkgRepoName::from ("public"),
      title : "A title that must stay out of lookup results" .to_string (),
      .. empty_graphnode () };
    let private : Graphnode = Graphnode {
      pid : ID::from ("private"),
      home_skgrepo : SkgRepoName::from ("private"),
      title : "Private title" .to_string (),
      .. empty_graphnode () };
    let graph : InRustGraph = InRustGraph::from_graphnodes (
      &[visible, private]);
    let skgids : Vec<ID> = ["old-visible", "private", "unknown"]
      .into_iter () . map (ID::from) .collect ();
    assert_eq! (classify_link_skgids (&graph, &config, &active, &skgids), vec![
      (ID::from ("old-visible"), LinkStatus::Resolved {
        pid : ID::from ("visible"), skgrepo_label : "pub" .to_string () }),
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

  #[test]
  fn a_newly_published_graph_answers_before_any_title_index_update () {
    let config : SkgConfig = load_config (
      "tests/repo_sets/fixtures/skgconfig.toml") . unwrap ();
    let active : ActiveSkgRepoSet = ActiveSkgRepoSet::named (
      &config, SkgRepoSetName::from ("public")) . unwrap ();
    let handle : InRustGraphHandle = Arc::new (ArcSwap::from_pointee (
      InRustGraph::new ()));
    let requested : Vec<ID> = vec![ID::from ("old-new")];
    assert_eq! (classify_link_skgids (
      &handle . load_full (), &config, &active, &requested),
      vec![(ID::from ("old-new"), LinkStatus::Missing)]);

    let published : InRustGraph = InRustGraph::from_graphnodes (&[
      Graphnode {
        pid : ID::from ("new"),
        extra_ids : requested . clone (),
        home_skgrepo : SkgRepoName::from ("public"),
        .. empty_graphnode () } ]);
    handle . store (Arc::new (published));
    let result = classify_link_skgids (
      &handle . load_full (), &config, &active, &requested);
    assert_eq! (result, vec![(ID::from ("old-new"),
      LinkStatus::Resolved {
        pid : ID::from ("new"),
        skgrepo_label : config . skgrepos
          [&SkgRepoName::from ("public")] . herald_label () . to_string (),
      })]);
  }
}
