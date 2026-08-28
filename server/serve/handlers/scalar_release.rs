//! The single release policy for textual data from ugly telescopes.
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

use sexp::{Atom, Sexp};
use std::collections::HashSet;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum ScalarReleaseDecision {
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

/// Parse the optional per-request approval list.
///
/// Its wire shape is '(allow-ugly-telescopes "PID" ...)'. Absence means no
/// approval. A malformed list is also treated as no approval: malformed
/// authority must fail closed, and the resulting challenge is actionable.
pub fn approved_pids_from_request (
  request : &str,
) -> HashSet<ID> {
  sexp::parse (request) . ok ()
    . and_then ( |parsed|
      extract_string_list_from_sexp (
        &parsed, "allow-ugly-telescopes" ) . ok () )
    . unwrap_or_default ()
    . into_iter ()
    . map (ID)
    . collect ()
}

/// Decide whether a response involving 'candidate_pids' may cross the
/// release boundary. IDs are canonicalized before checking ugliness so an
/// extra ID cannot evade the telescope-coarse policy.
pub fn decide (
  operation      : &str,
  active         : &ActiveSourceSet,
  candidate_pids : &[ID],
  graph          : &InRustGraph,
  approved_pids  : &HashSet<ID>,
) -> ScalarReleaseDecision {
  let ugly_pids : Vec<ID> =
    canonical_ugly_pids (candidate_pids, graph);
  if ugly_pids . is_empty () {
    return ScalarReleaseDecision::Allow; }
  let warning : String = warning_for (operation, &ugly_pids);
  if active . is_all ()
     || ugly_pids . iter () . all (
          |pid| approved_pids . contains (pid) ) {
    return ScalarReleaseDecision::AllowWithWarning { warning }; }
  ScalarReleaseDecision::Challenge {
    operation : operation . to_string (),
    pids      : ugly_pids . clone (),
    prompt    : format! (
      "{} would reveal title or body text selected below its node's home source for {}. Include that text?",
      operation_label (operation), pid_phrase (&ugly_pids) ),
  }
}

/// Serialize the deliberately text-free challenge response.
pub fn challenge_response (
  decision : &ScalarReleaseDecision,
) -> Option<String> {
  let ScalarReleaseDecision::Challenge {
    operation, pids, prompt,
  } = decision else { return None; };
  Some ( Sexp::List ( vec! [
    pair (
      "response-type",
      TcpToClient::UglyTelescopeConfirmation . repr_in_client () ),
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

fn canonical_ugly_pids (
  candidates : &[ID],
  graph      : &InRustGraph,
) -> Vec<ID> {
  let mut seen : HashSet<ID> = HashSet::new ();
  let mut ugly : Vec<ID> = Vec::new ();
  for candidate in candidates {
    let Some (pid) = graph . pid_of (candidate) else { continue; };
    let is_ugly : bool =
      graph . get (&pid)
      . map ( |node| node . ugly_telescope )
      . unwrap_or (false);
    if is_ugly && seen . insert (pid . clone ()) {
      ugly . push (pid); }}
  ugly . sort_by ( |a, b| a . as_str () . cmp (b . as_str ()) );
  ugly
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

  fn graph_with_ugly_node () -> InRustGraph {
    let mut node : NodeComplete = empty_node_complete ();
    node . pid = ID::from ("ugly-pid");
    node . source = SourceName::from ("home");
    node . title = "SECRET title" . to_string ();
    node . extra_ids = vec! [ID::from ("extra-id")];
    node . ugly_telescope = true;
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
  fn restricted_challenge_contains_no_scalar_text () {
    let graph : InRustGraph = graph_with_ugly_node ();
    let decision : ScalarReleaseDecision = decide (
      "single-root-view", &restricted (),
      &[ID::from ("extra-id")], &graph, &HashSet::new () );
    let response : String = challenge_response (&decision) . unwrap ();
    assert! ( response . contains ("ugly-telescope-confirmation") );
    assert! ( response . contains ("ugly-pid") );
    assert! ( ! response . contains ("SECRET") );
  }

  #[test]
  fn exact_canonical_pid_approval_allows_with_warning () {
    let graph : InRustGraph = graph_with_ugly_node ();
    let approved : HashSet<ID> =
      [ID::from ("ugly-pid")] . into_iter () . collect ();
    assert! ( matches! (
      decide (
        "single-root-view", &restricted (),
        &[ID::from ("extra-id")], &graph, &approved ),
      ScalarReleaseDecision::AllowWithWarning { .. } ));
  }

  #[test]
  fn unrelated_pid_approval_fails_closed () {
    let graph : InRustGraph = graph_with_ugly_node ();
    let approved : HashSet<ID> =
      [ID::from ("someone-else")] . into_iter () . collect ();
    assert! ( matches! (
      decide (
        "single-root-view", &restricted (),
        &[ID::from ("ugly-pid")], &graph, &approved ),
      ScalarReleaseDecision::Challenge { .. } ));
  }
}
