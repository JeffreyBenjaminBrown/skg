// PURPOSE: Initialize TypeDB and Tantivy databases.

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::tantivy::update_index_with_nodes;
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::types::skgnode::SkgNode;
use crate::types::misc::{SkgConfig, TantivyIndex};

use futures::executor::block_on;
use std::error::Error;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use tantivy::{schema, Index};
use typedb_driver::{
  Credentials,
  DriverOptions,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

/// Reads all SkgNodes from disk, then uses that data
/// to initialize both databases (TypeDB and Tantivy).
pub fn initialize_dbs (
  config : & SkgConfig,
) -> (Arc<TypeDBDriver>, TantivyIndex) {

  println!("Reading .skg files from all sources...");
  let nodes: Vec<SkgNode> =
    read_all_skg_files_from_sources(config)
    . unwrap_or_else(|e| {
      eprintln!("Failed to read .skg files: {}", e);
      std::process::exit(1); });
  println!("{} .skg files were read from {} source(s)",
           nodes.len(), config.sources.len());

  let typedb_driver: Arc<TypeDBDriver> =
    initialize_typedb_from_nodes ( config, &nodes );
  let tantivy_index: TantivyIndex =
    initialize_tantivy_from_nodes ( config, &nodes );

  (typedb_driver, tantivy_index) }

pub fn initialize_typedb_from_nodes (
  config : & SkgConfig,
  nodes: &[SkgNode],
) -> Arc<TypeDBDriver> {
  // Connects to the TypeDB server,
  // then populates it with the provided SkgNodes.

  println!("Initializing TypeDB database...");
  let driver: TypeDBDriver = block_on ( async {
    TypeDBDriver::new(
      "127.0.0.1:1729",
      Credentials::new("admin", "password"),
      DriverOptions::new(false, None).unwrap() )
      . await
      . unwrap_or_else ( |e| {
        eprintln!("Error connecting to TypeDB: {}", e);
        std::process::exit(1); } ) } );

  block_on ( async {
    // Recreate the database from scratch
    if let Err (e) = overwrite_new_empty_db (
      & config . db_name,
      & driver,
    ) . await {
      eprintln! ( "Failed to create empty database: {}", e );
      std::process::exit(1); }

    if let Err (e) = define_schema (
      & config . db_name,
      & driver,
    ) . await {
      eprintln! ( "Failed to define schema: {}", e );
      std::process::exit(1); }

    if let Err (e) = create_all_nodes (
      & config . db_name,
      & driver,
      nodes,
    ) . await {
      eprintln! ( "Failed to create nodes: {}", e );
      std::process::exit(1); }

    if let Err (e) = create_all_relationships (
      & config . db_name,
      & driver,
      nodes,
    ) . await {
      eprintln! ( "Failed to create relationships: {}", e );
      std::process::exit(1); }} );
  println!("TypeDB database initialized successfully.");
  Arc::new( driver ) }

pub fn initialize_tantivy_from_nodes (
  config : & SkgConfig,
  nodes  : & [SkgNode],
) -> TantivyIndex {
  println!("Initializing Tantivy index...");
  let index_path : &Path =
    Path::new ( & config . tantivy_folder );
  let (tantivy_index, indexed_count)
    : ( TantivyIndex, usize ) =
    in_fs_wipe_index_then_create_it (
      nodes,
      index_path )
    . unwrap_or_else(|e| {
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
  let index : Index =
    Index::create_in_dir ( index_path, schema )?;
  Ok ( TantivyIndex {
    index: Arc::new(index),
    id_field,
    title_or_alias_field,
    source_field, }) }

/// Removes any existing index at given path,
/// creates a new one there,
/// and populates it.
///
/// PITFALL: The index is not the data it indexes.
/// This only deletes the former.
fn in_fs_wipe_index_then_create_it (
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

pub async fn overwrite_new_empty_db (
  // Destroys the db named `db_name` if it exists,
  // then makes a new, empty one.
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {

  // TODO ? Is it hard to give a type signature to 'databases'?
  let databases = driver.databases ();
  if databases.contains (db_name) . await ? {
    println! ( "Deleting existing database '{}'...",
                db_name );
    let database = databases.get (db_name) . await ?;
    database . delete () . await ?; }
  println! ( "Creating empty database '{}'...", db_name );
  databases.create (db_name) . await ?;
  Ok (()) }

pub async fn define_schema (
  db_name : &str,
  driver  : &TypeDBDriver
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver.transaction ( db_name,
                         TransactionType::Schema )
    . await ?;
  let schema : String = fs::read_to_string
    ("schema.tql")
    . expect ( "Failed to read TypeDB schema file" );
  println! ( "Defining schema ..." );
  tx.query (schema) . await ?;
  tx.commit () . await ?;
  Ok (()) }
