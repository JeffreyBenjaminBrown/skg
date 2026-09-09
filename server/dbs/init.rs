// PURPOSE: Initialize the selected graph and its Tantivy search index.

use crate::context::{MapToContent, MapToContainers};
use crate::context::{content_maps_from_nodes, had_id_set_from_nodes};
use crate::context::link_dests_from_nodes;
use crate::dbs::filesystem::multiple_nodes::error_unless_each_id_names_one_node;
use crate::dbs::filesystem::multiple_nodes::{
  LoadedCorpus,
  read_all_skg_files_with_manifest,
};
use crate::dbs::tantivy::{mk_tantivy_schema, tantivy_index_from_index};
use crate::dbs::tantivy::write::update_index_with_nodes;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::nodes::complete::NodeComplete;
use crate::telescope::dependencies_manifest::{foreign_manifest_order_warnings, write_dependencies_manifests};
use crate::telescope::invariants::{TelescopeViolation, report_all_telescope_violations};
use crate::types::store_state::SelectedPathManifest;
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  new_handle_with_manifest,
  override_invariants::error_unless_override_invariants_hold,
};

use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::path::Path;
use std::sync::Arc;
use tantivy::Index;

/// One-shot init handoff. Holds derived data needed exactly once
/// after startup: it is fed to 'compute_context_origin_types' and
/// then dropped. Keeping these out of 'SkgEnv' makes it impossible
/// to keep the init-derived sets around past their freshness
/// window.
pub struct InitContextHandoff {
  pub had_id_set        : HashSet<ID>,
  pub all_node_ids      : HashSet<ID>,
  pub link_dests      : HashSet<ID>,
  pub map_to_content    : MapToContent,
  pub map_to_containers : MapToContainers,
}

/// Build graph and search inputs from one validated full filesystem read.
/// Startup recovery must finish before calling this constructor.
pub fn initialize_dbs (
  config : &SkgConfig,
) -> (SkgEnv, InitContextHandoff, Vec<NodeComplete>) {
  let result : (SkgEnv, InitContextHandoff, Vec<NodeComplete>) =
    full_init (config);
  if let Err (error) = write_dependencies_manifests (config) {
    tracing::warn! (error = %error,
      "could not write DEPENDENCIES.toml manifests"); }
  for warning in foreign_manifest_order_warnings (config) {
    tracing::warn! ("{}", warning); }
  result }

fn env_and_handoff_from_nodes (
  config        : &SkgConfig,
  nodes         : &[NodeComplete],
  tantivy_index : TantivyIndex,
  startup_warnings : Vec<(ID, TelescopeViolation)>,
  selected_manifest : SelectedPathManifest,
) -> (SkgEnv, InitContextHandoff) {
  let had_id_set : HashSet<ID> =
    had_id_set_from_nodes (&nodes);
  let all_node_ids : HashSet<ID> =
    nodes . iter ()
    . map ( |n| n . pid . clone () )
    . collect ();
  let link_dests : HashSet<ID> =
    link_dests_from_nodes (&nodes);
  let ( map_to_content, map_to_containers )
    : ( MapToContent, MapToContainers )
    = content_maps_from_nodes (&nodes);
  let in_rust_graph : InRustGraphHandle =
    new_handle_with_manifest (
      InRustGraph::from_nodecompletes (nodes), selected_manifest );
  ( SkgEnv {
      config : config . clone (),
      in_rust_graph,
    searcher: tantivy_index . reader . searcher (),
      tantivy_index,
      startup_warnings : Arc::new (startup_warnings), },
    InitContextHandoff {
      had_id_set,
      all_node_ids,
      link_dests,
      map_to_content,
      map_to_containers } ) }

/// Full rebuild: reads all .skg files and prepares graph/search inputs.
/// Also computes had_id_set and contains maps from the loaded nodes,
/// avoiding a second file read for context computation.
/// RETURNS (long-lived SkgEnv,
///          init handoff,
///          nodes that produced them).
fn full_init (
  config : &SkgConfig,
) -> (SkgEnv, InitContextHandoff, Vec<NodeComplete>) {
  tracing::info! ("Performing full init...");
  let loaded : LoadedCorpus =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "read_all_skg_files" ). entered();
      tracing::info! ("Reading .skg files from all sources...");
      read_all_skg_files_with_manifest (config)
      . unwrap_or_else ( |e| {
        tracing::error! ("Failed to read .skg files: {}", e);
        std::process::exit (1); } ) };
  let nodes : Vec<NodeComplete> = loaded . nodes;
  let load_violations : Vec<(ID, TelescopeViolation)> = loaded . violations;
  error_unless_each_id_names_one_node (
    // Reject ambiguous identity before preparing either derived store.
    &nodes, &config . data_root )
    . unwrap_or_else ( |e| {
      tracing::error! ("Id-conflict check failed: {}", e);
      std::process::exit (1); } );
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
  if let Err (e)
    = error_unless_override_invariants_hold (config, &graph)
    { tracing::error! ("Override invariant validation failed: {}", e);
      std::process::exit (1); }
  report_all_telescope_violations (
    config, &graph, load_violations . clone ());
  tracing::info! (files = nodes . len(),
            sources = config . sources . len(),
            ".skg files read from source(s)");
  let tantivy_index : TantivyIndex =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "initialize_tantivy" ). entered();
      wipe_then_init_tantivy_db_with_logs_and_errors (config, &nodes) };
  let (env, handoff) : (SkgEnv, InitContextHandoff) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "extract_context_data_from_nodes" ). entered();
      env_and_handoff_from_nodes (
        config, &nodes, tantivy_index,
        load_violations, loaded . manifest ) };
  (env, handoff, nodes) }

fn wipe_then_init_tantivy_db_with_logs_and_errors (
  config : & SkgConfig,
  nodes  : & [NodeComplete],
) -> TantivyIndex {
  tracing::info! ("Initializing Tantivy index...");
  let (tantivy_index, indexed_count)
    : ( TantivyIndex, usize ) =
    wipe_then_init_tantivy_db (
      nodes,
      Path::new ( & config . tantivy_folder )
    ) . unwrap_or_else ( |e| {
      tracing::error! ("Failed to create Tantivy index: {}", e);
      std::process::exit (1); } );
  tracing::info! (indexed_count,
    "Tantivy index initialized successfully.");
  tantivy_index }

/// Destroys and rebuilds the Tantivy index from the given nodes.
/// Returns a fresh TantivyIndex.
/// Callers are responsible for reading the .skg files
/// (and, if desired, checking for ids claimed by two nodes) beforehand.
pub fn rebuild_tantivy_from_nodes (
  index  : &TantivyIndex,
  nodes  : &[NodeComplete],
  labels : &HashMap<ID, String>,
) -> Result<TantivyIndex, Box<dyn Error>> {
  crate::dbs::tantivy::write::reconstruct_index_from_nodes (
    nodes, index, labels)?;
  Ok (index . clone ()) }

/// Create an empty TantivyIndex, cleaning up any existing index first.
pub fn create_empty_tantivy_index (
  index_path : &Path,
) -> Result<TantivyIndex, Box<dyn Error>> {
  if index_path . exists() {
    std::fs::remove_dir_all (index_path) ?; }
  std::fs::create_dir_all (index_path)?;
  tantivy_index_from_index (
    Index::create_in_dir ( index_path, mk_tantivy_schema () ) ? ) }

/// Empty in-memory index for graph-only rendering fixtures.
pub fn empty_in_ram_tantivy_index (
) -> Result<TantivyIndex, Box<dyn Error>> {
  tantivy_index_from_index (
    Index::create_in_ram ( mk_tantivy_schema () ) ) }

/// Removes any existing index at given path,
/// creates a new one there,
/// and populates it.
///
/// PITFALL: The index is not the data it indexes.
/// This only deletes the former.
pub fn wipe_then_init_tantivy_db (
  nodes      : &[NodeComplete],
  index_path : &Path,
) -> Result<(TantivyIndex,
             usize), // number of documents indexed
            Box<dyn Error>> {
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (index_path)?;
  let tantivy_nodes : Vec<NodeTantivy> = // Convert to NodeTantivy (narrow) at the boundary.
    nodes . iter () . map (NodeTantivy::from) . collect ();
  let indexed_count: usize =
    update_index_with_nodes ( &tantivy_nodes, & tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }
