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
use std::time::Instant;
use tantivy::{schema, Index};
use typedb_driver::{
  Credentials,
  Database,
  DatabaseManager,
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
    read_all_skg_files_from_sources (config)
    . unwrap_or_else(|e| {
      eprintln!("Failed to read .skg files: {}", e);
      std::process::exit (1); });
  println!("{} .skg files were read from {} source(s)",
           nodes . len(), config . sources . len());

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
      DriverOptions::new(false, None) . unwrap() )
      . await
      . unwrap_or_else ( |e| {
        eprintln!("Error connecting to TypeDB: {}", e);
        std::process::exit (1); } ) } );

  block_on ( async {
    if let Err (e) = populate_typedb_from_nodes (
      config, &driver, nodes
    ) . await {
      eprintln! ( "Failed to populate TypeDB: {}", e );
      std::process::exit (1); }} );
  println!("TypeDB database initialized successfully.");
  Arc::new (driver) }

/// Destroys and rebuilds the TypeDB database from .skg files on disk.
/// Uses an existing driver connection rather than creating a new one.
pub async fn rebuild_typedb_from_disk (
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (config) ?;
  populate_typedb_from_nodes (
    config, driver, &nodes ) . await }

/// Populates a TypeDB database from the given nodes:
/// overwrites with empty db, defines schema,
/// creates all nodes and relationships.
async fn populate_typedb_from_nodes (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  nodes  : &[SkgNode],
) -> Result<(), Box<dyn Error>> {
  overwrite_new_empty_db (
    & config . db_name,
    driver ) . await ?;
  define_schema (
    & config . db_name,
    driver ) . await ?;
  let t0 : Instant = Instant::now();
  create_all_nodes (
    & config . db_name,
    driver,
    nodes ) . await ?;
  println! ("  nodes: {:.1}s", t0 . elapsed() . as_secs_f64());
  let t1 : Instant = Instant::now();
  create_all_relationships (
    & config . db_name,
    driver,
    nodes ) . await ?;
  println! ("  relationships: {:.1}s", t1 . elapsed() . as_secs_f64());
  Ok (( )) }

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
    ) . unwrap_or_else(|e| {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit (1); } );
  println!(
    "Tantivy index initialized successfully. Indexed {} files.",
    indexed_count);
  tantivy_index }

/// Destroys and rebuilds the Tantivy index from .skg files on disk.
/// Returns a fresh TantivyIndex.
pub fn rebuild_tantivy_from_disk (
  config : &SkgConfig,
) -> Result<TantivyIndex, Box<dyn Error>> {
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (config) ?;
  let (tantivy_index, _indexed_count)
    : ( TantivyIndex, usize ) =
    in_fs_wipe_index_then_create_it (
      &nodes,
      Path::new ( & config . tantivy_folder )) ?;
  Ok (tantivy_index) }

/// Create an empty TantivyIndex, cleaning up any existing index first.
pub fn create_empty_tantivy_index (
  index_path : &Path,
) -> Result<TantivyIndex, Box<dyn Error>> {
  if index_path . exists() {
    std::fs::remove_dir_all (index_path) ?; }
  std::fs::create_dir_all (index_path)?;
  let schema : schema::Schema =
    crate::dbs::tantivy::mk_tantivy_schema();
  let id_field: schema::Field =
    schema . get_field ("id")
    . ok_or ("Schema missing 'id' field")?;
  let title_or_alias_field: schema::Field =
    schema . get_field ("title_or_alias")
    . ok_or ("Schema missing 'title_or_alias' field")?;
  let source_field: schema::Field =
    schema . get_field ("source")
    . ok_or ("Schema missing 'source' field")?;
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
    create_empty_tantivy_index (index_path)?;
  let indexed_count: usize =
    update_index_with_nodes ( nodes, & tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }

pub async fn overwrite_new_empty_db (
  // Destroys the db named `db_name` if it exists,
  // then makes a new, empty one.
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {

  let databases : &DatabaseManager = driver . databases ();
  if databases . contains (db_name) . await ? {
    println! ( "Deleting existing database '{}'...",
                db_name );
    { let database : Arc<Database> =
        databases . get (db_name) . await ?;
      database } . delete () . await ?; }
  println! ( "Creating empty database '{}'...", db_name );
  databases . create (db_name) . await ?;
  Ok (()) }

pub async fn define_schema (
  db_name : &str,
  driver  : &TypeDBDriver
)-> Result < (), Box<dyn Error> > {

  let tx : Transaction =
    driver . transaction ( db_name,
                         TransactionType::Schema )
    . await ?;
  println! ("Defining schema ...");
  tx . query ( {
    let schema : String = fs::read_to_string
      ("schema.tql")
      . expect ("Failed to read TypeDB schema file");
    schema } ) . await ?;
  tx . commit () . await ?;
  Ok (()) }
