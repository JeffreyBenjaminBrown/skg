//! The single release policy for textual data from overPrivateText telescopes.
//!
//! Folding may select a title or body below the node's home source. That is
//! safe to hold internally, but a restricted source-set must not release it
//! without an explicit approval for the affected PID. The reserved 'all'
//! source-set may release it, with a warning.

use crate::dbs::in_rust_graph::InRustGraph;
use crate::serve::protocol::TcpToClient;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;
use crate::types::sexp::extract_string_list_from_sexp;
use crate::types::viewnode::{
  ViewNode, ViewNodeKind, Vognode, mk_inactive_viewnode};

use ego_tree::{NodeId, NodeMut, Tree};
use sexp::{Atom, Sexp};
use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TextReleaseDecision {
  Allow,
  AllowWithWarning {
    warning : String,
  },
  Challenge {
    operation : String,
    pids      : Vec<ID>,
    prompt    : String,
  },
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum SearchOverPrivateTextChoice {
  Include,
  Exclude,
}

/// Parse the optional per-request approval list.
///
/// Its wire shape is '(allow-overPrivateText-telescopes "PID" ...)'. Absence means no
/// approval. A malformed list is also treated as no approval: malformed
/// authority must fail closed, and the resulting challenge is actionable.
pub fn approved_pids_from_request (
  request : &str,
) -> HashSet<ID> {
  sexp::parse (request) . ok ()
    . and_then ( |parsed|
      extract_string_list_from_sexp (
        &parsed, "allow-overPrivateText-telescopes" ) . ok () )
    . unwrap_or_default ()
    . into_iter ()
    . map (ID)
    . collect ()
}

pub fn search_choice_from_request (
  parsed : &Sexp,
) -> Result<Option<SearchOverPrivateTextChoice>, String> {
  use crate::types::sexp::extract_v_from_kv_pair_in_sexp;
  match extract_v_from_kv_pair_in_sexp (parsed, "overPrivateText-telescopes") {
    Ok (choice) => match choice . as_str () {
      "include" => Ok (Some (SearchOverPrivateTextChoice::Include)),
      "exclude" => Ok (Some (SearchOverPrivateTextChoice::Exclude)),
      other => Err (format! (
        "Unknown overPrivateText-telescopes choice: {} (expected include or exclude)",
        other )), },
    Err (_) => Ok (None), }
}

/// A search challenge intentionally has no PIDs: even the identity of a
/// matching overPrivateText telescope is part of what the search has not been allowed
/// to reveal. The retry must choose inclusion or exclusion before querying.
pub fn search_challenge_response () -> String {
  Sexp::List ( vec! [
    pair (
      "response-type",
      TcpToClient::OverPrivateTextTelescopeConfirmation . repr_in_client () ),
    pair ("operation", "text-search"),
    Sexp::List ( vec! [ atom ("pids"), Sexp::List (Vec::new ()) ] ),
    pair (
      "prompt",
      "Some indexed nodes have title or body text selected below home. Include or exclude those telescopes from this search?" ),
  ] ) . to_string ()
}

/// Decide whether a response involving 'candidate_pids' may cross the
/// release boundary. IDs are canonicalized before checking overPrivateTextness so an
/// extra ID cannot evade the telescope-coarse policy.
pub fn decide (
  operation      : &str,
  active         : &ActiveSourceSet,
  candidate_pids : &[ID],
  graph          : &InRustGraph,
  approved_pids  : &HashSet<ID>,
) -> TextReleaseDecision {
  let overPrivateText_pids : Vec<ID> =
    canonical_overPrivateText_pids (candidate_pids, graph);
  decide_for_overPrivateText_pids (
    operation, active, overPrivateText_pids, approved_pids )
}

/// Apply the shared policy when a caller has classified overPrivateTextness from a
/// source other than the live graph, such as deleted-node diff data.
pub fn decide_for_overPrivateText_pids (
  operation     : &str,
  active        : &ActiveSourceSet,
  mut overPrivateText_pids : Vec<ID>,
  approved_pids : &HashSet<ID>,
) -> TextReleaseDecision {
  overPrivateText_pids . sort_by ( |a, b| a . as_str () . cmp (b . as_str ()) );
  overPrivateText_pids . dedup ();
  if overPrivateText_pids . is_empty () {
    return TextReleaseDecision::Allow; }
  let warning : String = warning_for (operation, &overPrivateText_pids);
  if active . is_all ()
     || overPrivateText_pids . iter () . all (
          |pid| approved_pids . contains (pid) ) {
    return TextReleaseDecision::AllowWithWarning { warning }; }
  TextReleaseDecision::Challenge {
    operation : operation . to_string (),
    pids      : overPrivateText_pids . clone (),
    prompt    : format! (
      "{} would reveal title or body text selected below its node's home source for {}. Include that text?",
      operation_label (operation), pid_phrase (&overPrivateText_pids) ),
  }
}

/// Serialize the deliberately text-free challenge response.
pub fn challenge_response (
  decision : &TextReleaseDecision,
) -> Option<String> {
  let TextReleaseDecision::Challenge {
    operation, pids, prompt,
  } = decision else { return None; };
  Some ( Sexp::List ( vec! [
    pair (
      "response-type",
      TcpToClient::OverPrivateTextTelescopeConfirmation . repr_in_client () ),
    pair ("operation", operation),
    Sexp::List ( vec! [
      atom ("pids"),
      Sexp::List (
        pids . iter ()
        . map ( |pid| atom (pid . as_str ()) )
        . collect () ) ] ),
    pair ("prompt", prompt),
  ] ) . to_string () )
}

/// Replace overPrivateText active nodes with text-free inactive placeholders. Search
/// exclusion uses this after enrichment so ancestry and override grafting
/// cannot broaden the choice made before the Tantivy query.
pub fn exclude_overPrivateText_nodes_from_viewforest (
  viewforest : &mut Tree<ViewNode>,
  graph      : &InRustGraph,
) {
  let node_ids : Vec<NodeId> =
    viewforest . root () . descendants ()
    . map ( |node| node . id () )
    . collect ();
  for node_id in node_ids {
    let should_convert : bool =
      viewforest . get (node_id)
      . and_then ( |node| match &node . value () . kind {
        ViewNodeKind::Vognode (Vognode::Active (active_node)) =>
          graph . pid_of (&active_node . id),
        _ => None, } )
      . and_then ( |pid| graph . get (&pid) )
      . map ( |node| node . overPrivateText_telescope )
      . unwrap_or (false);
    if should_convert {
      let mut node : NodeMut<crate::types::viewnode::ViewNode> =
        viewforest . get_mut (node_id) . unwrap ();
      node . value () . kind = mk_inactive_viewnode () . kind; }}
}

fn canonical_overPrivateText_pids (
  candidates : &[ID],
  graph      : &InRustGraph,
) -> Vec<ID> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut overPrivateText : Vec<ID> = Vec::new ();
  for candidate in candidates {
    let Some (pid) = graph . pid_of (candidate) else { continue; };
    let is_overPrivateText : bool =
      graph . get (&pid)
      . map ( |node| node . overPrivateText_telescope )
      . unwrap_or (false);
    if is_overPrivateText && seen . insert (pid . clone ()) {
      overPrivateText . push (pid); }}
  overPrivateText . sort_by ( |a, b| a . as_str () . cmp (b . as_str ()) );
  overPrivateText
}

fn warning_for (
  operation : &str,
  pids      : &[ID],
) -> String {
  format! (
    "{} includes title or body text selected below its node's home source for {}.",
    operation_label (operation), pid_phrase (pids) )
}

fn operation_label (
  operation : &str,
) -> String {
  operation . replace ('-', " ")
}

fn pid_phrase (
  pids : &[ID],
) -> String {
  let rendered : String =
    pids . iter ()
    . map ( |pid| pid . as_str () )
    . collect::<Vec<&str>> ()
    . join (", ");
  if pids . len () == 1 {
    format! ("PID {}", rendered)
  } else {
    format! ("PIDs {}", rendered) }
}

fn atom (
  value : &str,
) -> Sexp {
  Sexp::Atom ( Atom::S ( value . to_string () ) )
}

fn pair (
  key   : &str,
  value : &str,
) -> Sexp {
  Sexp::List ( vec! [ atom (key), atom (value) ] )
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::source_sets::SourceSetName;
  use crate::types::misc::SourceName;
  use crate::types::nodes::complete::{
    NodeComplete, empty_node_complete};

  fn graph_with_overPrivateText_node () -> InRustGraph {
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = ID::from ("overPrivateText-pid");
    node . source = SourceName::from ("home");
    node . title = "SECRET title" . to_string ();
    node . extra_ids = vec! [ID::from ("extra-id")];
    node . overPrivateText_telescope = true;
    InRustGraph::from_nodecompletes (&[node])
  }

  fn restricted () -> ActiveSourceSet {
    ActiveSourceSet {
      name    : SourceSetName::from ("public"),
      sources : [SourceName::from ("home")]
                . into_iter () . collect (),
    }
  }

  #[test]
  fn restricted_challenge_contains_no_text () {
    let graph : InRustGraph = graph_with_overPrivateText_node ();
    let decision : TextReleaseDecision = decide (
      "single-root-view", &restricted (),
      &[ID::from ("extra-id")], &graph, &HashSet::new () );
    let response : String = challenge_response (&decision) . unwrap ();
    assert! ( response . contains ("overPrivateText-telescope-confirmation") );
    assert! ( response . contains ("overPrivateText-pid") );
    assert! ( ! response . contains ("SECRET") );
  }

  #[test]
  fn exact_canonical_pid_approval_allows_with_warning () {
    let graph : InRustGraph = graph_with_overPrivateText_node ();
    let approved : HashSet<ID> =
      [ID::from ("overPrivateText-pid")] . into_iter () . collect ();
    assert! ( matches! (
      decide (
        "single-root-view", &restricted (),
        &[ID::from ("extra-id")], &graph, &approved ),
      TextReleaseDecision::AllowWithWarning { .. } ));
  }

  #[test]
  fn unrelated_pid_approval_fails_closed () {
    let graph : InRustGraph = graph_with_overPrivateText_node ();
    let approved : HashSet<ID> =
      [ID::from ("someone-else")] . into_iter () . collect ();
    assert! ( matches! (
      decide (
        "single-root-view", &restricted (),
        &[ID::from ("overPrivateText-pid")], &graph, &approved ),
      TextReleaseDecision::Challenge { .. } ));
  }
}
