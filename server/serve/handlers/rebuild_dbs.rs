use crate::context::{
  compute_and_store_context_types,
  content_maps_from_nodes,
  had_id_set_from_nodes,
  link_dests_from_nodes};
use crate::dbs::filesystem::multiple_nodes::error_unless_each_id_names_one_node;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources_collecting_violations;
use crate::dbs::filesystem::not_nodes::load_config;
use crate::dbs::init::{rebuild_tantivy_from_nodes, wipe_then_init_typedb_db};
use crate::telescope::invariants::{TelescopeViolation, report_all_telescope_violations};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  override_invariants::error_unless_override_invariants_hold,
};
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;

use futures::executor::block_on;
use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_rebuild_dbs_request (
  stream     : &mut TcpStream,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
) {
  let result : Result<(), String> =
    rebuild_dbs_in_place (env, views_state);
  let msg : String = match result {
    Ok (()) => "Databases rebuilt successfully." . to_string (),
    Err (e) => {
      tracing::error!("Rebuild failed: {}", e);
      format! ("Rebuild failed: {}", e) } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::RebuildDbs, &msg )); }

/// The rebuild itself, streamless, so other handlers (the
/// telescope migration) can rebuild after rewriting files.
pub fn rebuild_dbs_in_place (
  env         : &mut SkgEnv,
  views_state : &mut ViewsState,
) -> Result<(), String> {
  tracing::info!("Rebuilding databases from disk...");
  // Serialize this wholesale rebuild against any concurrent save / reload
  // so no RCU update is lost (last-store-wins on the ArcSwap).
  let _write_guard = block_on (
    crate::write_lock::acquire_graph_write_lock () );
  // Let any in-flight background save-index writes finish before we wipe
  // and rebuild the index out from under them.
  crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_idle ();
  let result : Result<(), String> = (|| {
    let config_path : String =
      env . config . config_path . display () . to_string ();
    let fresh_config : SkgConfig =
      load_config (&config_path)
      . map_err ( |e| format! (
        "Reloading config from {}: {}", config_path, e) ) ?;
    let (nodes, load_violations)
      : (Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>) =
      read_all_skg_files_from_sources_collecting_violations (&fresh_config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    error_unless_each_id_names_one_node (
      &nodes, &fresh_config . data_root)
      . map_err ( |e| format! ("Id-conflict check failed: {}", e) ) ?;
    let fresh_graph : InRustGraph =
      InRustGraph::from_nodecompletes (&nodes);
    error_unless_override_invariants_hold (
        &fresh_config, &fresh_graph )
      . map_err ( |e| format! (
        "Override invariant validation failed: {}", e) ) ?;
    report_all_telescope_violations (
      &fresh_config, &fresh_graph, load_violations . clone () );
    block_on ( wipe_then_init_typedb_db (
      &fresh_config, &env . driver, &nodes) )
      . map_err ( |e| format! ("TypeDB rebuild failed: {}", e) ) ?;
    tracing::info!("TypeDB rebuilt.");
    let new_tantivy : TantivyIndex =
      rebuild_tantivy_from_nodes (&fresh_config, &nodes)
      . map_err ( |e| format! ("Tantivy rebuild failed: {}", e) ) ?;
    env . config = fresh_config;
    env . tantivy_index = new_tantivy;
    env . startup_warnings = Arc::new (load_violations);
    tracing::info!("Tantivy rebuilt.");
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_ids = nodes . iter ()
      . map ( |n| n . pid . clone () )
      . collect ();
    let link_dests = link_dests_from_nodes (&nodes);
    let (map_to_content, map_to_containers) =
      content_maps_from_nodes (&nodes);
    compute_and_store_context_types (
      &env . tantivy_index, &had_id_set, &all_node_ids,
      &link_dests, &map_to_content, &map_to_containers )
      . map_err ( |e| format! ("Context computation failed: {}", e) ) ?;
    tracing::info!("Context rankings recomputed.");
    { // Rebuild the in-Rust graph from disk too, so it stays in sync with the freshly repopulated TypeDB/Tantivy.
      env . in_rust_graph . store (
        Arc::new (fresh_graph) );
      tracing::info!("In-Rust graph rebuilt."); }
    Ok (())
  })();
  if result . is_ok () {
    views_state . open_views . clear (); }
  result }
