use crate::context::{
  compute_and_store_context_types,
  content_maps_from_nodes,
  had_id_set_from_nodes,
  link_targets_from_nodes};
use crate::dbs::filesystem::multiple_nodes::check_for_duplicate_ids_across_sources;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::init::{rebuild_tantivy_from_nodes, wipe_then_init_typedb_db};
use crate::dbs::memory::InRustGraph;
use crate::types::env::SkgEnv;
use crate::serve::ViewsState;
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::misc::TantivyIndex;
use crate::types::nodes::complete::NodeComplete;

use futures::executor::block_on;
use std::net::TcpStream;
use std::sync::Arc;

pub fn handle_rebuild_dbs_request (
  stream     : &mut TcpStream,
  env        : &mut SkgEnv,
  views_state : &mut ViewsState,
) {
  tracing::info!("Rebuilding databases from disk...");
  let result : Result<(), String> = (|| {
    let nodes : Vec<NodeComplete> =
      read_all_skg_files_from_sources (&env . config)
      . map_err ( |e| format! ("Reading .skg files: {}", e) ) ?;
    check_for_duplicate_ids_across_sources (
      &nodes, &env . config . data_root)
      . map_err ( |e| format! ("Duplicate ID check failed: {}", e) ) ?;
    block_on ( wipe_then_init_typedb_db (
      &env . config, &env . driver, &nodes) )
      . map_err ( |e| format! ("TypeDB rebuild failed: {}", e) ) ?;
    tracing::info!("TypeDB rebuilt.");
    let new_tantivy : TantivyIndex =
      rebuild_tantivy_from_nodes (&env . config, &nodes)
      . map_err ( |e| format! ("Tantivy rebuild failed: {}", e) ) ?;
    env . tantivy_index = new_tantivy;
    tracing::info!("Tantivy rebuilt.");
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_ids = nodes . iter ()
      . map ( |n| n . pid . clone () )
      . collect ();
    let link_targets = link_targets_from_nodes (&nodes);
    let (map_to_content, map_to_containers) =
      content_maps_from_nodes (&nodes);
    compute_and_store_context_types (
      &env . tantivy_index, &had_id_set, &all_node_ids,
      &link_targets, &map_to_content, &map_to_containers )
      . map_err ( |e| format! ("Context computation failed: {}", e) ) ?;
    tracing::info!("Context rankings recomputed.");
    { // Rebuild the in-memory graph from disk too, so it stays in sync with the freshly repopulated TypeDB/Tantivy.
      let fresh_graph : InRustGraph =
        InRustGraph::from_nodecompletes (&nodes);
      env . memory . store (
        Arc::new (fresh_graph) );
      tracing::info!("In-memory graph rebuilt."); }
    Ok (())
  })();
  let msg : String = match result {
    Ok (()) => {
      views_state . open_views . clear ();
      "Databases rebuilt successfully." . to_string () },
    Err (e) => {
      tracing::error!("Rebuild failed: {}", e);
      format! ("Rebuild failed: {}", e) } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::RebuildDbs, &msg )); }
