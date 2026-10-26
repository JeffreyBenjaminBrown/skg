use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::ID;
use crate::util::setlike_vector_subtraction;

/// What a subscribee as such shows of its content: its content less
/// what its subscriber hides or contains. The refresh's goal reads it
/// ('server/refresh/goal.rs'), and so do the occurrence-level herald
/// facts. Inputs preserve raw IDs; extra IDs are
/// resolved before comparing membership, without replacing overridden nodes.
pub fn unintegrated_content_skgids (
  graph               : &InRustGraph,
  content_skgids      : &[ID],
  subscriber_hidden   : &[ID],
  subscriber_contains : &[ID],
) -> Vec<ID> {
  let resolve_pids = |skgids : &[ID]| -> Vec<ID> {
    skgids . iter ()
      . map (|skgid| graph . pid_of (skgid) . unwrap_or_else (|| skgid . clone ()))
      . collect () };
  let contents : Vec<ID> = resolve_pids (content_skgids);
  let hidden : Vec<ID> = resolve_pids (subscriber_hidden);
  let contained : Vec<ID> = resolve_pids (subscriber_contains);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained) }
