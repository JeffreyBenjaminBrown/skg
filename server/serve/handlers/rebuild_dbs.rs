use crate::context::{
  compute_and_store_context_types,
  contains_maps_from_nodes,
  had_id_set_from_nodes,
  link_targets_from_nodes};
use crate::dbs::init::{rebuild_typedb_from_disk, rebuild_tantivy_from_disk};
use crate::serve::protocol::TcpToClient;
use crate::serve::util::{send_response_with_length_prefix, tag_text_response};
use crate::types::misc::{SkgConfig, TantivyIndex};

use futures::executor::block_on;
use std::net::TcpStream;
use typedb_driver::TypeDBDriver;

pub fn handle_rebuild_dbs_request (
  stream        : &mut TcpStream,
  typedb_driver : &TypeDBDriver,
  config        : &SkgConfig,
  tantivy_index : &mut TantivyIndex,
) {
  tracing::info!("Rebuilding databases from disk...");
  let result : Result<(), String> = (|| {
    block_on ( rebuild_typedb_from_disk (config, typedb_driver) )
      . map_err ( |e| format! ("TypeDB rebuild failed: {}", e) ) ?;
    tracing::info!("TypeDB rebuilt.");
    let new_tantivy : TantivyIndex =
      rebuild_tantivy_from_disk (config)
      . map_err ( |e| format! ("Tantivy rebuild failed: {}", e) ) ?;
    *tantivy_index = new_tantivy;
    tracing::info!("Tantivy rebuilt.");
    let nodes : Vec<crate::types::skgnode::SkgNode> =
      crate::dbs::filesystem::multiple_nodes
        ::read_all_skg_files_from_sources (config)
      . map_err ( |e| format! ("Reading .skg files for context: {}", e) ) ?;
    let had_id_set = had_id_set_from_nodes (&nodes);
    let all_node_ids = nodes . iter ()
      . map ( |n| n . pid . clone () )
      . collect ();
    let link_targets = link_targets_from_nodes (&nodes);
    let (map_to_content, map_to_containers) =
      contains_maps_from_nodes (&nodes);
    compute_and_store_context_types (
      tantivy_index, &had_id_set, &all_node_ids,
      &link_targets, &map_to_content, &map_to_containers )
      . map_err ( |e| format! ("Context computation failed: {}", e) ) ?;
    tracing::info!("Context rankings recomputed.");
    Ok (())
  })();
  let msg : String = match result {
    Ok (()) => "Databases rebuilt successfully." . to_string (),
    Err (e) => {
      tracing::error!("Rebuild failed: {}", e);
      format! ("Rebuild failed: {}", e) } };
  send_response_with_length_prefix (
    stream,
    & tag_text_response (
      TcpToClient::RebuildDbs, &msg )); }
