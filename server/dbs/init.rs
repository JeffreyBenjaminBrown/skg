// PURPOSE: Initialize TypeDB and Tantivy databases.

use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::multiple_nodes::read_modified_skg_files_from_sources;
use crate::dbs::tantivy::open_existing_tantivy_index;
use crate::dbs::tantivy::update_index_with_nodes;
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::relationships::delete_all_outbound_relationships;
use crate::dbs::typedb::util::connect_to_typedb;
use crate::types::skgnode::SkgNode;
use crate::types::misc::{SkgConfig, TantivyIndex};

use futures::executor::block_on;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;
use tantivy::{schema, Index};
use typedb_driver::{
  Database,
  DatabaseManager,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

/// Initializes TypeDB and Tantivy databases.
/// If a marker file exists, the TypeDB database exists,
/// and the schema hasn't changed,
/// it rebuilds incrementally (only re-reading modified .skg files).
/// Otherwise rebuilds completely.
/// Ends by touching the marker file.
pub fn initialize_dbs (
  config : & SkgConfig,
) -> (Arc<TypeDBDriver>, TantivyIndex) {
  let driver : TypeDBDriver = connect_to_typedb();
  let marker_path : PathBuf =
    config . config_dir . join (".skg_init_marker");
  let can_incremental : bool = block_on ( async {
    can_do_incremental_init (
      &driver, &config . db_name, &marker_path
    ) . await } );
  if can_incremental {
    let marker_mtime : std::time::SystemTime =
      fs::metadata (&marker_path)
      . and_then ( |m| m . modified() )
      . unwrap(); // safe: can_do_incremental_init confirmed it exists
    match incremental_init (
      config, &driver, marker_mtime )
      { Ok (( tantivy_index )) => {
          println! ("Incremental init succeeded.");
          touch_init_marker (&marker_path);
          return ( Arc::new (driver), tantivy_index ); }
        Err (e) => {
          println! ("Incremental init failed ({}), \
                     falling back to full rebuild.", e); }} }
  let (tantivy_index) : (TantivyIndex) =
    full_init (config, &driver);
  touch_init_marker (&marker_path);
  ( Arc::new (driver), tantivy_index ) }

/// Checks the three preconditions for incremental init:
/// 1. TypeDB database exists
/// 2. Marker file exists
/// 3. schema.tql mtime <= marker mtime
async fn can_do_incremental_init (
  driver      : &TypeDBDriver,
  db_name     : &str,
  marker_path : &Path,
) -> bool {
  let db_exists : bool =
    match driver . databases() . contains (db_name) . await {
      Ok (b)  => b,
      Err (_) => false, };
  if ! db_exists { return false; }
  let marker_mtime : std::time::SystemTime =
    match fs::metadata (marker_path)
      . and_then ( |m| m . modified() )
    {
      Ok (t)  => t,
      Err (_) => return false, };
  // PITFALL: "schema.tql" is a relative path, resolved from the CWD.
  // If the server is started from an unexpected directory the file
  // won't be found, which triggers a full rebuild -- a safe fallback.
  let schema_mtime : std::time::SystemTime =
    match fs::metadata ("schema.tql")
      . and_then ( |m| m . modified() )
    {
      Ok (t)  => t,
      Err (_) => return false, };
  schema_mtime <= marker_mtime }

/// PITFALL: Deleted .skg files are not detected by this path.
/// Their nodes remain as orphans in TypeDB. This doesn't happen in
/// the normal `cargo watch` workflow (deletions go through the save
/// pipeline while the server is running). For manual deletions,
/// restart with delete_on_quit = true, or delete the TypeDB database
/// manually, to force a clean full rebuild.
fn incremental_init (
  config       : &SkgConfig,
  driver       : &TypeDBDriver,
  marker_mtime : std::time::SystemTime,
) -> Result<TantivyIndex, Box<dyn Error>> {
  let tantivy_index : TantivyIndex =
    open_existing_tantivy_index (
      Path::new ( &config . tantivy_folder )) ?;
  println! ("Reading modified .skg files...");
  let nodes : Vec<SkgNode> =
    read_modified_skg_files_from_sources (
      config, marker_mtime ) ?;
  if nodes . is_empty() {
    println! ("No modified .skg files found.");
    return Ok (tantivy_index); }
  println! ("{} modified .skg file(s) found.", nodes . len());
  block_on ( async {
    let t0 : Instant = Instant::now();
    let created : usize =
      create_only_nodes_with_no_ids_present (
        &config . db_name, driver, &nodes ) . await ?;
    println! ("  new nodes: {} ({:.1}s)",
              created, t0 . elapsed() . as_secs_f64());
    let t1 : Instant = Instant::now();
    let pids : Vec<crate::types::misc::ID> =
      nodes . iter()
      . map ( |n| n . primary_id()
              . map ( |id| id . clone() ) )
      . collect::<Result<Vec<_>, _>>()
      . map_err ( |e| -> Box<dyn Error> { e . into() } ) ?;
    delete_all_outbound_relationships (
      &config . db_name, driver, &pids ) . await ?;
    println! ("  deleted stale relationships ({:.1}s)",
              t1 . elapsed() . as_secs_f64());
    let t2 : Instant = Instant::now();
    create_all_relationships (
      &config . db_name, driver, &nodes ) . await ?;
    println! ("  recreated relationships ({:.1}s)",
              t2 . elapsed() . as_secs_f64());
    Ok::<(), Box<dyn Error>> (( )) } ) ?;
  let t3 : Instant = Instant::now();
  let indexed : usize =
    update_index_with_nodes (&nodes, &tantivy_index) ?;
  println! ("  tantivy: updated {} documents ({:.1}s)",
            indexed, t3 . elapsed() . as_secs_f64());
  Ok (tantivy_index) }

/// Full rebuild: reads all .skg files, populates both databases.
fn full_init (
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> TantivyIndex {
  println! ("Performing full init...");
  println! ("Reading .skg files from all sources...");
  let nodes : Vec<SkgNode> =
    read_all_skg_files_from_sources (config)
    . unwrap_or_else ( |e| {
      eprintln! ("Failed to read .skg files: {}", e);
      std::process::exit (1); } );
  println! ("{} .skg files were read from {} source(s)",
            nodes . len(), config . sources . len());
  block_on ( async {
    if let Err (e) = populate_typedb_from_nodes (
      config, driver, &nodes
    ) . await {
      eprintln! ("Failed to populate TypeDB: {}", e);
      std::process::exit (1); }} );
  println! ("TypeDB database initialized successfully.");
  let tantivy_index : TantivyIndex =
    initialize_tantivy_from_nodes (config, &nodes);
  tantivy_index }

fn touch_init_marker (
  path : &Path,
) {
  if let Err (e) = fs::write (path, b"") {
    eprintln! ("Warning: could not touch init marker {:?}: {}",
               path, e); } }

pub fn initialize_typedb_from_nodes (
  config : & SkgConfig,
  nodes  : &[SkgNode],
) -> Arc<TypeDBDriver> {
  // Connects to the TypeDB server,
  // then populates it with the provided SkgNodes.
  println! ("Initializing TypeDB database...");
  let driver : TypeDBDriver = connect_to_typedb();
  block_on ( async {
    if let Err (e) = populate_typedb_from_nodes (
      config, &driver, nodes
    ) . await {
      eprintln! ( "Failed to populate TypeDB: {}", e );
      std::process::exit (1); }} );
  println! ("TypeDB database initialized successfully.");
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

fn initialize_tantivy_from_nodes (
  config : & SkgConfig,
  nodes  : & [SkgNode],
) -> TantivyIndex {
  println! ("Initializing Tantivy index...");
  let (tantivy_index, indexed_count)
    : ( TantivyIndex, usize ) =
    in_fs_wipe_index_then_create_it (
      nodes,
      Path::new ( & config . tantivy_folder )
    ) . unwrap_or_else ( |e| {
      eprintln! ("Failed to create Tantivy index: {}", e);
      std::process::exit (1); } );
  println! (
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
