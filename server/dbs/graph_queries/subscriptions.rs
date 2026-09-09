use crate::dbs::graph_queries::relations::find_related_nodes;
use crate::dbs::in_rust_graph::InRustGraph;
use crate::dbs::in_rust_graph::relation_accessors::RelationRole;
use crate::source_sets::ActiveSourceSet;
use crate::types::misc::ID;

use std::collections::HashSet;

/// Return the subscribee's visible content and the portion hidden by
/// the subscriber, using only source-visible edges and partners.
pub fn partition_subscribee_content_for_subscriber (
  graph : &InRustGraph,
  subscriber_pid : &ID,
  subscribee_pid : &ID,
  active : Option<&ActiveSourceSet>,
) -> (HashSet<ID>, HashSet<ID>) {
  let hides : HashSet<ID> = what_node_hides (graph, subscriber_pid, active);
  let contains : HashSet<ID> =
    what_nodes_contain (graph, &[subscribee_pid . clone ()], active);
  (contains . difference (&hides) . cloned () . collect (),
   contains . intersection (&hides) . cloned () . collect ()) }

pub fn what_node_hides (
  graph : &InRustGraph,
  subscriber_pid : &ID,
  active : Option<&ActiveSourceSet>,
) -> HashSet<ID> {
  find_related_nodes (graph, &[subscriber_pid . clone ()], RelationRole::HIDER, active) }

pub fn what_nodes_contain (
  graph : &InRustGraph,
  subscribee_pids : &[ID],
  active : Option<&ActiveSourceSet>,
) -> HashSet<ID> {
  find_related_nodes (graph, subscribee_pids, RelationRole::CONTAINER, active) }
