// PURPOSE: Initialize Neo4j and Tantivy databases.

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::tantivy::update_index_with_nodes;
use crate::dbs::neo4j::nodes::create_all_nodes;
use crate::dbs::neo4j::relationships::create_all_relationships;
use crate::dbs::neo4j::schema::apply_schema;
use crate::dbs::neo4j::util::delete_database;
use crate::types::skgnode::SkgNode;
use crate::types::misc::{SkgConfig, TantivyIndex};

use std::error::Error;
use std::path::Path;
use std::sync::Arc;
use tantivy::{schema, Index};
use neo4rs::Graph;
use tokio::runtime::Handle;

/// Reads all SkgNodes from disk, then uses that data
/// to initialize both databases (Neo4j and Tantivy).
pub fn initialize_dbs (
  config : & SkgConfig,
  handle : & Handle,
) -> (Arc<Graph>, TantivyIndex) {

  println!("Reading .skg files from all sources...");
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(config)
    . unwrap_or_else(|e| {
      eprintln!("Failed to read .skg files: {}", e);
      std::process::exit(1); });
  println!("{} .skg files were read from {} source(s)",
           nodes.len(), config.sources.len());

  let graph: Arc<Graph> =
    initialize_neo4j_from_nodes ( config, &nodes, handle );
  let tantivy_index: TantivyIndex =
    initialize_tantivy_from_nodes ( config, &nodes );

  (graph, tantivy_index) }

pub fn initialize_neo4j_from_nodes (
  config : & SkgConfig,
  nodes  : &[SkgNode],
  handle : & Handle,
) -> Arc<Graph> {

  println!("Initializing Neo4j database...");
  let graph: Graph = handle.block_on ( async {
    Graph::new(
      &config.neo4j_uri,
      &config.neo4j_user,
      &config.neo4j_password,
    ) . await
      . unwrap_or_else ( |e| {
        eprintln!("Error connecting to Neo4j: {}", e);
        std::process::exit(1); } ) } );

  handle.block_on ( async {
    // Clear any existing data
    if let Err (e) = delete_database (
      & graph,
    ) . await {
      eprintln! ( "Failed to clear database: {}", e );
      std::process::exit(1); }

    if let Err (e) = apply_schema (
      & graph,
    ) . await {
      eprintln! ( "Failed to apply schema: {}", e );
      std::process::exit(1); }

    if let Err (e) = create_all_nodes (
      & graph,
      nodes,
    ) . await {
      eprintln! ( "Failed to create nodes: {}", e );
      std::process::exit(1); }

    if let Err (e) = create_all_relationships (
      & graph,
      nodes,
    ) . await {
      eprintln! ( "Failed to create relationships: {}", e );
      std::process::exit(1); }} );
  println!("Neo4j database initialized successfully.");
  Arc::new( graph ) }

pub fn initialize_tantivy_from_nodes (
  config : & SkgConfig,
  nodes  : & [SkgNode],
) -> TantivyIndex {
  println!("Initializing Tantivy index...");
  let (tantivy_index, indexed_count)
    : ( TantivyIndex, usize ) =
    in_fs_wipe_index_then_create_it (
      nodes,
      Path::new ( & config . tantivy_folder )
    ). unwrap_or_else(|e| {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit(1); } );
  println!(
    "Tantivy index initialized successfully. Indexed {} files.",
    indexed_count);
  tantivy_index }

/// Create an empty TantivyIndex, cleaning up any existing index first.
pub fn create_empty_tantivy_index (
  index_path : &Path,
) -> Result<TantivyIndex, Box<dyn Error>> {
  if index_path.exists() {
    std::fs::remove_dir_all (index_path) ?; }
  std::fs::create_dir_all ( index_path )?;
  let schema : schema::Schema =
    crate::dbs::tantivy::mk_tantivy_schema();
  let id_field: schema::Field =
    schema.get_field("id")
    .ok_or("Schema missing 'id' field")?;
  let title_or_alias_field: schema::Field =
    schema.get_field("title_or_alias")
    .ok_or("Schema missing 'title_or_alias' field")?;
  let source_field: schema::Field =
    schema.get_field("source")
    .ok_or("Schema missing 'source' field")?;
  Ok ( TantivyIndex {
    index: Arc::new ( { let index : Index =
                          Index::create_in_dir ( index_path, schema )?;
                        index } ),
    id_field,
    title_or_alias_field,
    source_field, }) }

/// Removes any existing index at given path,
/// creates a new one there,
/// and populates it.
///
/// PITFALL: The index is not the data it indexes.
/// This only deletes the former.
pub fn in_fs_wipe_index_then_create_it (
  nodes      : &[SkgNode],
  index_path : &Path,
) -> Result<(TantivyIndex,
             usize), // number of documents indexed
            Box<dyn Error>> {
  let tantivy_index : TantivyIndex =
    create_empty_tantivy_index ( index_path )?;
  let indexed_count: usize =
    update_index_with_nodes ( nodes, & tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }
