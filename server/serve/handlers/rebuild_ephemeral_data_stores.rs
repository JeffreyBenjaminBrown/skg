use crate::context::{
  compute_and_store_context_types,
  content_maps_from_nodes,
  had_id_set_from_nodes,
  link_dests_from_nodes};
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources_collecting_violations;
use crate::dbs::filesystem::not_nodes::load_config;
use crate::dbs::init::rebuild_tantivy_as_generation;
use crate::telescope::invariants::{TelescopeViolation, report_telescope_violations};
use crate::dbs::in_rust_graph::complete_validation::validated_graph;
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;

use futures::executor::block_on;
use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_rebuild_ephemeral_data_stores_request (
  stream     : &mut TcpStream,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
) {
  let result : Result<(), String> =
    rebuild_ephemeral_data_stores_in_place (env, views_state);
  let msg : String = match result {
    Ok (()) => concat! (
      "Ephemeral graph and Tantivy index rebuilt successfully; ",
      ".skg files remain authoritative.") . to_string (),
    Err (e) => {
      tracing::error!("Rebuild failed: {}", e);
      format! ("Rebuild failed: {}", e) } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::RebuildEphemeralDataStores, &msg )); }

/// The rebuild itself, streamless, so other handlers (the
/// telescope migration) can rebuild after rewriting files.
pub fn rebuild_ephemeral_data_stores_in_place (
  env         : &mut SkgEnv,
  views_state : &mut ViewsState,
) -> Result<(), String> {
  tracing::info!("Rebuilding ephemeral data stores from .skg files...");
  // Rebuild is an authoritative publication just like a save.  Clone the Arc
  // before locking so the guard does not borrow `env` while this function
  // replaces its config/index fields below.
  let mutation_gate = env . mutation_gate ();
  let _mutation_guard = block_on ( mutation_gate . lock () );
  let runtime = env . runtime_snapshot ();
  // Let any in-flight background save-index writes finish before we wipe
  // and rebuild the index out from under them.
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let result : Result<(), String> = (|| {
    let config_path : String =
      runtime . config . config_path . display () . to_string ();
    let fresh_config : SkgConfig =
      load_config (&config_path)
      . map_err ( |e| format! (
        "Reloading config from {}: {}", config_path, e) ) ?;
    let (nodes, load_violations)
      : (Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>) =
      read_all_skg_files_from_sources_collecting_violations (&fresh_config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    let (fresh_graph, mut graph_warnings) =
      validated_graph (&fresh_config, &nodes)
      . map_err (|e| format! ("Complete graph validation failed:\n{}", e)) ?;
    graph_warnings . extend (load_violations);
    graph_warnings . sort_by (|(pid_a, a), (pid_b, b)|
      pid_a . cmp (pid_b)
        . then_with (|| a . to_string () . cmp (&b . to_string ())));
    report_telescope_violations (
      &graph_warnings, &fresh_config . data_root )
      . map_err (|e| format! ("Writing telescope warnings: {}", e)) ?;
    let (fresh_config, new_tantivy) : (SkgConfig, TantivyIndex) =
      rebuild_tantivy_as_generation (
        &fresh_config, &nodes, runtime . generation + 1)
      . map_err ( |e| format! ("Tantivy rebuild failed: {}", e) ) ?;
    tracing::info!("Ephemeral graph candidate and Tantivy rebuilt.");
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_ids = nodes . iter ()
      . map ( |n| n . pid . clone () )
      . collect ();
    let link_dests = link_dests_from_nodes (&nodes);
    let (map_to_content, map_to_containers) =
      content_maps_from_nodes (&nodes);
    compute_and_store_context_types (
      &new_tantivy, &had_id_set, &all_node_ids,
      &link_dests, &map_to_content, &map_to_containers )
      . map_err ( |e| format! ("Context computation failed: {}", e) ) ?;
    tracing::info!("Context rankings recomputed.");
    { // Publish config, graph, and replacement index as one generation.
      env . runtime . publish (
        Arc::new (fresh_config), Arc::new (fresh_graph), new_tantivy);
      tracing::info!("In-Rust graph rebuilt."); }
    Ok (())
  })();
  if result . is_ok () {
    views_state . open_views . clear (); }
  result }
