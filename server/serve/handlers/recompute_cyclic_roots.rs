//! Explicit repair of the deliberately stale cyclic-root ranking cache.

use crate::context::{
  compute_context_types,
  context_origin_types_for_graph,
  content_maps_from_nodes,
  had_id_set_from_nodes,
  link_dests_from_nodes,
};
use crate::dbs::tantivy::background_writer::{
  TantivyGenerationStatus,
  latest_tantivy_generation,
  wait_for_tantivy_writes_through,
};
use crate::dbs::tantivy::context_update::update_context_origin_types;
use crate::save::nodecompletes_from_graph;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{
  send_response_with_length_prefix,
  tag_terminal_text_response,
};
use crate::types::env::SkgEnv;
use crate::types::misc::ID;

use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

static RECOMPUTE_RUNNING : AtomicBool = AtomicBool::new (false);

struct RecomputeRunningGuard;

impl Drop for RecomputeRunningGuard {
  fn drop (&mut self) {
    RECOMPUTE_RUNNING . store (false, Ordering::Release); }
}

pub fn handle_recompute_cyclic_roots_request (
  stream : &mut std::net::TcpStream,
  env    : &SkgEnv,
) {
  if RECOMPUTE_RUNNING . swap (true, Ordering::AcqRel) {
    let _ = send_response_with_length_prefix (
      stream, &tag_terminal_text_response (
        TcpToClient::RecomputeCyclicRoots, "complete",
        "Cyclic-root recomputation is already running."));
    return; }
  let _running = RecomputeRunningGuard;
  let result : Result<(usize, usize), String> = (|| loop {
    // Every client connection already runs in its own server thread.  Do the
    // expensive immutable computation here without the graph-writer lock;
    // saves and reloads remain free to advance the graph meanwhile.
    let selected = env . in_rust_graph . load_full ();
    let nodes = nodecompletes_from_graph (&selected . graph);
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_ids : HashSet<ID> = nodes . iter ()
      . map (|node| node . pid . clone ()) . collect ();
    let link_dests = link_dests_from_nodes (&nodes);
    let (map_to_content, map_to_containers) = content_maps_from_nodes (&nodes);
    let computation = compute_context_types (
      &had_id_set, &all_node_ids, &link_dests,
      &map_to_content, &map_to_containers );
    let old_labels = context_origin_types_for_graph (
      &selected . graph, &selected . cyclic_roots);
    let changed_labels : HashMap<ID, String> = computation . labels . iter ()
      . filter ( |(pid, label)| old_labels . get (*pid) != Some (*label) )
      . map ( |(pid, label)| (pid . clone (), label . clone ()) )
      . collect ();

    if let Some (through) = latest_tantivy_generation () {
      let failed : Vec<String> = wait_for_tantivy_writes_through (through)
        . into_iter ()
        . filter_map ( |(generation, status)| match status {
          TantivyGenerationStatus::Failed (reason) => Some (format! (
            "generation {}: {}", generation . get (), reason)),
          TantivyGenerationStatus::Committed => None,
          TantivyGenerationStatus::Reconstructed (_) => None,
          TantivyGenerationStatus::Pending => unreachable! (), })
        . collect ();
      if ! failed . is_empty () {
        return Err (format! (
          "Tantivy has failed writes ({}). Run skg-rebuild-dbs before retrying.",
          failed . join ("; "))); }}

    let _writer = block_on (crate::write_lock::acquire_graph_write_lock ());
    let current = env . in_rust_graph . load_full ();
    if current . graph_generation != selected . graph_generation {
      continue; }
    update_context_origin_types (&env . tantivy_index, &changed_labels)
      . map_err (|error| format! (
        "Tantivy rejected the cyclic-root repair: {}. Run skg-rebuild-dbs before retrying.",
        error)) ?;
    let cyclic_root_count = computation . cyclic_roots . len ();
    let changed_count = changed_labels . len ();
    env . in_rust_graph . store (Arc::new (
      current . with_cyclic_roots (computation . cyclic_roots)));
    break Ok ((cyclic_root_count, changed_count));
  })();
  let (status, message) = match result {
    Ok ((cyclic_root_count, changed_count)) => {
      tracing::info! (
        cyclic_root_count, changed_count,
        "Cyclic-root ranking cache recomputed");
      ("complete", format! (
        "Recomputed {} cyclic roots and updated {} search-index documents.",
        cyclic_root_count, changed_count)) },
    Err (error) => {
      tracing::error! (
        %error,
        "Cyclic-root recomputation failed; the prior cache remains selected");
      ("failed", format! (
        "Cyclic-root recomputation failed; the prior cache is still active. {}",
        error)) }, };
  let _ = send_response_with_length_prefix (
    stream, &tag_terminal_text_response (
      TcpToClient::RecomputeCyclicRoots, status, &message));
}
