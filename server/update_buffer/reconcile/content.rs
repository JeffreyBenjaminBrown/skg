use crate::dbs::in_rust_graph::InRustGraph;
use crate::types::misc::ID;
use crate::util::setlike_vector_subtraction;

/// What a subscribee as such shows of its content: its content less
/// what its subscriber hides or contains. The refresh's goal reads it
/// ('server/refresh/goal.rs'), and so do the occurrence-level herald
/// facts. Inputs preserve raw IDs; extra IDs are
/// resolved before comparing membership, without replacing overridden nodes.
pub fn unintegrated_content_gnids (
  graph               : &InRustGraph,
  content_gnids      : &[ID],
  subscriber_hidden   : &[ID],
  subscriber_contains : &[ID],
) -> Vec<ID> {
  let resolve_pids = |gnids : &[ID]| -> Vec<ID> {
    gnids . iter ()
      . map (|gnid| graph . pid_of (gnid) . unwrap_or_else (|| gnid . clone ()))
      . collect () };
  let contents : Vec<ID> = resolve_pids (content_gnids);
  let hidden : Vec<ID> = resolve_pids (subscriber_hidden);
  let contained : Vec<ID> = resolve_pids (subscriber_contains);
  setlike_vector_subtraction (
    setlike_vector_subtraction (contents, &hidden), &contained) }
