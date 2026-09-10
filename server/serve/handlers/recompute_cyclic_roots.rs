//! Explicit repair of the deliberately stale cyclic-root ranking cache.

use crate::context::{compute_context_types, context_origin_types_for_graph,
  content_maps_from_nodes, had_id_set_from_nodes, link_dests_from_nodes,
  ContextComputation, MapToContent, MapToContainers};
use crate::dbs::tantivy::background_writer::{TantivyGenerationStatus,
  latest_tantivy_generation, wait_for_tantivy_writes_through};
use crate::dbs::tantivy::context_update::update_context_origin_types;
use crate::runtime::ServerRuntime;
use crate::runtime::save_operations::SaveOperation;
use crate::save::nodecompletes_from_graph;
use crate::serve::protocol::TcpToClient;
use crate::types::env::SkgEnv;
use crate::types::misc::ID;
use crate::types::nodes::complete::NodeComplete;
use crate::types::store_state::SelectedStoreState;

use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::net::TcpStream;
use std::sync::Arc;
use tokio::sync::MutexGuard;

pub(crate) fn handle_recompute_cyclic_roots_request (
  stream : &mut TcpStream,
  request : &str,
  runtime : &ServerRuntime,
) {
  super::durable_command::handle_durable_command_request (
    stream, request, runtime, TcpToClient::RecomputeCyclicRoots,
    recompute_cyclic_roots_with_operation);
}

pub(crate) fn recompute_cyclic_roots_with_operation (
  env : &mut SkgEnv,
  operation : &SaveOperation,
) -> Result<String, String> {
  // The shared command wrapper reserves the selected base before computation.
  // Ranking repair changes derived index data, never source-file bytes.
  let selected : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  let nodes : Vec<NodeComplete> = nodecompletes_from_graph (&selected . graph);
  let had_id_set : HashSet<ID> = had_id_set_from_nodes (&nodes);
  let all_node_ids : HashSet<ID> = nodes . iter ()
    . map (|node| node . pid . clone ()) . collect ();
  let link_dests : HashSet<ID> = link_dests_from_nodes (&nodes);
  let (map_to_content, map_to_containers) : (MapToContent, MapToContainers) =
    content_maps_from_nodes (&nodes);
  let computation : ContextComputation = compute_context_types (
    &had_id_set, &all_node_ids, &link_dests, &map_to_content, &map_to_containers);
  let old_labels : HashMap<ID, String> = context_origin_types_for_graph (
    &selected . graph, &selected . cyclic_roots);
  let changed_labels : HashMap<ID, String> = computation . labels . iter ()
    . filter (|(pid, label)| old_labels . get (*pid) != Some (*label))
    . map (|(pid, label)| (pid . clone (), label . clone ())) . collect ();
  if let Some (through) = latest_tantivy_generation () {
    let failed : Vec<String> = wait_for_tantivy_writes_through (through)
      . into_iter () . filter_map (|(generation, status)| match status {
        TantivyGenerationStatus::Failed (reason) => Some (format! (
          "generation {}: {}", generation . get (), reason)),
        TantivyGenerationStatus::Committed | TantivyGenerationStatus::Reconstructed (_) => None,
        TantivyGenerationStatus::Pending => unreachable! (), }) . collect ();
    if !failed . is_empty () {
      return Err (format! (
        "Tantivy has failed writes ({}). Rebuild databases before retrying.", failed . join ("; "))); }
  }
  let _writer : MutexGuard<'static, ()> = block_on (crate::write_lock::acquire_graph_write_lock ());
  let current : Arc<SelectedStoreState> = env . in_rust_graph . load_full ();
  if !Arc::ptr_eq (&current, &selected) {
    return Err ("selected ranking base changed before execution" . into ()); }
  operation . prepare (Vec::new ())?;
  operation . apply_authorized ()?;
  let updated_count : usize = update_context_origin_types (
    &env . tantivy_index, &changed_labels) . map_err (|error| format! (
      "Tantivy rejected cyclic-root repair: {}. Rebuild databases before retrying.", error))?;
  let cyclic_root_count : usize = computation . cyclic_roots . len ();
  env . in_rust_graph . store (Arc::new (
    current . with_cyclic_roots (computation . cyclic_roots)
      . with_searcher (env . tantivy_index . reader . searcher ())));
  Ok (format! (
    "Recomputed {} cyclic roots and updated {} search-index documents.",
    cyclic_root_count, updated_count))
}
