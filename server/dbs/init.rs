// PURPOSE: Build the reconstructible in-memory graph and Tantivy index.

use crate::context::{MapToContent, MapToContainers};
use crate::context::{content_maps_from_nodes, had_id_set_from_nodes};
use crate::context::link_dests_from_nodes;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources_collecting_violations;
use crate::dbs::tantivy::{mk_tantivy_schema, tantivy_index_from_index};
use crate::dbs::tantivy::write::update_index_with_nodes;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::nodes::complete::NodeComplete;
use crate::telescope::dependencies_manifest::{foreign_manifest_order_warnings, write_dependencies_manifests};
use crate::telescope::invariants::{TelescopeViolation, report_telescope_violations};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  complete_validation::validated_graph,
};

use std::collections::HashSet;
use std::error::Error;
use std::fs;
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

/// Read the authoritative files once, validate and build the graph, and build
/// Tantivy from that exact node vector. There is intentionally no marker or
/// incremental branch: both derived stores are reconstructed at startup, so
/// files deleted while the server was stopped cannot leave orphaned records.
pub fn initialize_dbs (
  config : &SkgConfig,
) -> (SkgEnv, InitContextHandoff, Vec<NodeComplete>) {
  retire_stale_tantivy_generation_directories (&config . tantivy_folder);
  tracing::info! ("Reading authoritative .skg files from all sources...");
  let (nodes, load_violations)
    : (Vec<NodeComplete>, Vec<(ID, TelescopeViolation)>) =
    read_all_skg_files_from_sources_collecting_violations (config)
    . unwrap_or_else (|e| {
      tracing::error! ("Failed to read .skg files: {}", e);
      std::process::exit (1); });
  let (graph, mut graph_warnings) = validated_graph (config, &nodes)
    . unwrap_or_else (|e| {
      tracing::error! ("Complete graph validation failed:\n{}", e);
      std::process::exit (1); });
  graph_warnings . extend (load_violations);
  graph_warnings . sort_by (|(pid_a, a), (pid_b, b)|
    pid_a . cmp (pid_b)
      . then_with (|| a . to_string () . cmp (&b . to_string ())));
  if let Err (e) = report_telescope_violations (
    &graph_warnings, &config . data_root ) {
    tracing::warn! (error = %e, "could not write the telescope report"); }
  tracing::info! (
    files = nodes . len (), sources = config . sources . len (),
    ".skg files read and graph validated");
  let tantivy_index = wipe_then_init_tantivy_db_with_logs_and_errors (
    config, &nodes);
  let (env, handoff) = env_and_handoff_from_nodes (
    config, &nodes, graph, tantivy_index);
  if let Err (e) = write_dependencies_manifests (config) {
    tracing::warn! (
      error = %e, "could not write DEPENDENCIES.toml manifests" ); }
  for warning in foreign_manifest_order_warnings (config) {
    tracing::warn! ("{}", warning); }
  (env, handoff, nodes)
}

fn env_and_handoff_from_nodes (
  config        : &SkgConfig,
  nodes         : &[NodeComplete],
  graph         : InRustGraph,
  tantivy_index : TantivyIndex,
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
  ( SkgEnv::new (
      config . clone (), Arc::new (graph), tantivy_index ),
    InitContextHandoff {
      had_id_set,
      all_node_ids,
      link_dests,
      map_to_content,
      map_to_containers } ) }

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
  config : &SkgConfig,
  nodes  : &[NodeComplete],
) -> Result<TantivyIndex, Box<dyn Error>> {
  let (tantivy_index, _indexed_count)
    : ( TantivyIndex, usize ) =
    wipe_then_init_tantivy_db (
      nodes,
      Path::new ( & config . tantivy_folder )) ?;
  Ok (tantivy_index) }

/// Build a live-reload candidate in a sibling directory, leaving the index in
/// the currently published generation untouched.  The returned config points
/// at the candidate directory and is published atomically with the index.
pub fn rebuild_tantivy_as_generation (
  config : &SkgConfig,
  nodes : &[NodeComplete],
  generation : u64,
) -> Result<(SkgConfig, TantivyIndex), Box<dyn Error>> {
  let mut generation_config = config . clone ();
  let base = &config . tantivy_folder;
  let parent = base . parent () . unwrap_or_else (|| Path::new ("."));
  let base_name = base . file_name ()
    .and_then (|name| name . to_str ())
    .unwrap_or ("tantivy");
  generation_config . tantivy_folder = parent . join (format! (
    "{}.skg-generation-{}-{}", base_name, std::process::id (), generation));
  let index = rebuild_tantivy_from_nodes (&generation_config, nodes)?;
  Ok ((generation_config, index))
}

/// Prior-process generation directories are never live after restart.  Retire
/// only siblings with our exact generated prefix; the configured base index
/// and unrelated directories are never candidates.
fn retire_stale_tantivy_generation_directories (base : &Path) {
  let parent = base . parent () . unwrap_or_else (|| Path::new ("."));
  let Some (base_name) = base . file_name () . and_then (|name| name . to_str ())
    else { return; };
  let prefix = format! ("{}.skg-generation-", base_name);
  let Ok (entries) = fs::read_dir (parent) else { return; };
  for entry in entries . flatten () {
    let name = entry . file_name ();
    let Some (name) = name . to_str () else { continue; };
    if ! name . starts_with (&prefix) { continue; }
    let path = entry . path ();
    if path . is_dir () {
      if let Err (error) = fs::remove_dir_all (&path) {
        tracing::warn! (
          path = %path . display (), error = %error,
          "could not retire stale Tantivy generation directory"); } } }
}

/// Create an empty TantivyIndex, cleaning up any existing index first.
pub fn create_empty_tantivy_index (
  index_path : &Path,
) -> Result<TantivyIndex, Box<dyn Error>> {
  if index_path . exists() {
    std::fs::remove_dir_all (index_path) ?; }
  std::fs::create_dir_all (index_path)?;
  tantivy_index_from_index (
    Index::create_in_dir ( index_path, mk_tantivy_schema () ) ? ) }

/// An empty in-RAM Tantivy index (no folder IO, nothing wiped). Used to build a
/// SkgEnv for a DE-NOVO render driven through post-save view completion in paths/tests
/// that have no real tantivy on hand: find_source falls back past an empty index
/// to the in-Rust graph / disk, so the index's contents don't matter there.
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
) -> Result<(TantivyIndex, usize), Box<dyn Error>> {
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index (index_path)?;
  let tantivy_nodes : Vec<NodeTantivy> =
    nodes . iter () . map (NodeTantivy::from) . collect ();
  let indexed_count =
    update_index_with_nodes (&tantivy_nodes, &tantivy_index)?;
  Ok ((tantivy_index, indexed_count))
}
