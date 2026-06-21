// PURPOSE: Initialize TypeDB and Tantivy databases.

use crate::context::{MapToContent, MapToContainers};
use crate::context::{content_maps_from_nodes, had_id_set_from_nodes};
use crate::context::link_dests_from_nodes;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::multiple_nodes::read_recently_modified_skgfiles_from_sources;
use crate::dbs::tantivy::{mk_tantivy_schema, open_existing_tantivy_index, tantivy_index_from_index};
use crate::dbs::tantivy::write::update_index_with_nodes;
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::nodes::create_only_nodes_with_no_ids_present;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::relationships::delete_all_outbound_relationships_to_nodes;
use crate::dbs::typedb::sources::create_all_sources;
use crate::dbs::typedb::util::connect_to_typedb;
use crate::types::env::SkgEnv;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::tantivy::NodeTantivy;
use crate::types::nodes::typedb::NodeTypedb;
use crate::types::nodes::complete::NodeComplete;
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  new_handle,
  override_invariants::error_unless_override_invariants_hold,
};

use futures::executor::block_on;
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Instant;
use tantivy::Index;
use typedb_driver::{
  Database,
  DatabaseManager,
  Transaction,
  TransactionType,
  TypeDBDriver,
};

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

/// Initializes TypeDB and Tantivy databases.
/// If a marker file exists, the TypeDB database exists,
/// and the schema hasn't changed,
/// it rebuilds incrementally (only re-reading modified .skg files).
/// Otherwise rebuilds completely.
/// Ends by touching the marker file.
/// RETURNS (long-lived SkgEnv,
///          init handoff for context-computation,
///          nodes used to create them).
pub fn initialize_dbs (
  config : & SkgConfig,
) -> (SkgEnv, InitContextHandoff, Vec<NodeComplete>) {
  let driver : TypeDBDriver = connect_to_typedb();
  let marker_path : PathBuf =
    config . data_root . join (".skg_init_marker");
  let can_incremental : bool = block_on ( async {
    can_do_incremental_init_of_dbs (
      &driver, &config . db_name, &marker_path,
      Path::new ( &config . tantivy_folder ),
      &config . config_path,
    ) . await } );
  let result : (SkgEnv, InitContextHandoff, Vec<NodeComplete>) =
    if can_incremental {
      let marker_mtime : std::time::SystemTime =
        fs::metadata (&marker_path)
        . and_then ( |m| m . modified() )
        . unwrap(); // safe: can_do_incremental_init_of_dbs confirmed it exists
      match incremental_init_of_dbs (
        config, &driver, marker_mtime )
      { Ok (( tantivy_index )) => {
          tracing::info! ("Incremental init succeeded.");
          // The incremental step above updated TypeDB and Tantivy from only the modified .skg files, which is all those databases need. The in-Rust graph (env.in_rust_graph) and the InitContextHandoff (contains maps, had_id_set, link_dests) are rebuilt from scratch on every startup, so they need every NodeComplete. The read below is therefore a full file read, but not a full re-initialization of the databases.
          let nodes : Vec<NodeComplete> =
            read_all_skg_files_from_sources (config)
            . unwrap_or_default ();
          let graph : InRustGraph =
            InRustGraph::from_nodecompletes (&nodes);
          if let Err (e)
            = error_unless_override_invariants_hold (config, &graph)
            { tracing::error! (
                "Override invariant validation failed: {}", e);
              std::process::exit (1); }
          let (env, handoff) : (SkgEnv, InitContextHandoff) =
            env_and_handoff_from_nodes (
              config, &nodes, Arc::new (driver), tantivy_index );
          (env, handoff, nodes) }
        Err (e) => {
          tracing::warn! ("Incremental init failed ({}), \
                     falling back to full rebuild.", e);
          full_init (config, driver) }}
    } else {
      full_init (config, driver) };
  touch_init_marker (&marker_path);
  result }

/// DEAD ? See "incremental init" in is-it-dead.org.
///
/// PURPOSE:
/// Checks preconditions for incremental init:
/// 1. TypeDB database exists
/// 2. Marker file exists
/// 3. schema.tql mtime <= marker mtime
/// 4. skgconfig.toml mtime <= marker mtime
/// 5. Tantivy index directory contains files
///    (guards against manual deletion of index contents)
async fn can_do_incremental_init_of_dbs (
  driver        : &TypeDBDriver,
  db_name       : &str,
  marker_path   : &Path,
  tantivy_path  : &Path,
  config_path   : &Path,
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
  if schema_mtime > marker_mtime { return false; }
  if ! config_path . as_os_str () . is_empty () {
    let config_mtime : std::time::SystemTime =
      match fs::metadata (config_path)
        . and_then ( |m| m . modified() )
      { Ok (t)  => t,
        Err (_) => return false, };
    if config_mtime > marker_mtime { return false; }}
  let tantivy_has_files : bool =
    tantivy_path . is_dir ()
    && fs::read_dir (tantivy_path)
       . map ( |mut entries| entries . next () . is_some () )
       . unwrap_or (false);
  if ! tantivy_has_files {
    tracing::warn! (
      "Tantivy index directory is empty or missing; forcing full rebuild.");
    return false; }
  true }

/// PITFALL: Deleted .skg files are not detected by this path.
/// Their nodes remain as orphans in TypeDB. This doesn't happen in
/// the normal `cargo watch` workflow (deletions go through the save
/// pipeline while the server is running). For manual deletions,
/// restart with delete_on_quit = true, or delete the TypeDB database
/// manually, to force a clean full rebuild.
fn incremental_init_of_dbs (
  config       : &SkgConfig,
  driver       : &TypeDBDriver,
  marker_mtime : std::time::SystemTime,
) -> Result<TantivyIndex, Box<dyn Error>> {
  let tantivy_index : TantivyIndex =
    open_existing_tantivy_index (
      Path::new ( &config . tantivy_folder )) ?;
  tracing::info! ("Reading modified .skg files...");
  let nodes : Vec<NodeComplete> =
    read_recently_modified_skgfiles_from_sources (
      config, marker_mtime ) ?;
  if nodes . is_empty() {
    tracing::info! ("No modified .skg files found.");
    return Ok (tantivy_index); }
  tracing::info! (count = nodes . len(),
                  "Modified .skg file(s) found.");
  let typedb_nodes : Vec<NodeTypedb> = // Convert to NodeTypedb (narrow) at the boundary. Parses textlinks from each node's title+body.
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  block_on ( async {
    let t0 : Instant = Instant::now();
    let created : usize =
      create_only_nodes_with_no_ids_present (
        &config . db_name, driver, &typedb_nodes ) . await ?;
    tracing::info! (created, elapsed_s = ?t0 . elapsed(),
              "New nodes created");
    let t1 : Instant = Instant::now();
    let pids : Vec<ID> =
      nodes . iter()
      . map ( |n| n . pid . clone() )
      . collect();
    delete_all_outbound_relationships_to_nodes (
      &config . db_name, driver, &pids ) . await ?;
    tracing::info! (elapsed_s = ?t1 . elapsed(),
              "Deleted stale relationships");
    let t2 : Instant = Instant::now();
    create_all_relationships (
      &config . db_name, driver, &typedb_nodes ) . await ?;
    tracing::info! (elapsed_s = ?t2 . elapsed(),
              "Recreated relationships");
    Ok::<(), Box<dyn Error>> (( )) } ) ?;
  let t3 : Instant = Instant::now();
  let tantivy_nodes : Vec<NodeTantivy> = // Convert to NodeTantivy (narrow) at the boundary.
    nodes . iter () . map (NodeTantivy::from) . collect ();
  let indexed : usize =
    update_index_with_nodes (&tantivy_nodes, &tantivy_index) ?;
  tracing::info! (indexed, elapsed_s = ?t3 . elapsed(),
            "Tantivy: updated documents");
  Ok (tantivy_index) }

fn env_and_handoff_from_nodes (
  config        : &SkgConfig,
  nodes         : &[NodeComplete],
  driver        : Arc<TypeDBDriver>,
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
  let in_rust_graph : InRustGraphHandle =
    new_handle ( InRustGraph::from_nodecompletes (nodes) );
  ( SkgEnv {
      config : config . clone (),
      in_rust_graph,
      tantivy_index,
      driver, },
    InitContextHandoff {
      had_id_set,
      all_node_ids,
      link_dests,
      map_to_content,
      map_to_containers } ) }

/// Full rebuild: reads all .skg files, populates both databases.
/// Also computes had_id_set and contains maps from the loaded nodes,
/// avoiding re-reading files or querying TypeDB for context computation.
/// RETURNS (long-lived SkgEnv,
///          init handoff,
///          nodes that produced them).
fn full_init (
  config : &SkgConfig,
  driver : TypeDBDriver,
) -> (SkgEnv, InitContextHandoff, Vec<NodeComplete>) {
  tracing::info! ("Performing full init...");
  let nodes : Vec<NodeComplete> =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "read_all_skg_files" ). entered();
      tracing::info! ("Reading .skg files from all sources...");
      read_all_skg_files_from_sources (config)
      . unwrap_or_else ( |e| {
        tracing::error! ("Failed to read .skg files: {}", e);
        std::process::exit (1); } ) };
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (&nodes);
  if let Err (e)
    = error_unless_override_invariants_hold (config, &graph)
    { tracing::error! ("Override invariant validation failed: {}", e);
      std::process::exit (1); }
  tracing::info! (files = nodes . len(),
            sources = config . sources . len(),
            ".skg files read from source(s)");
  { let _span : tracing::span::EnteredSpan = tracing::info_span!(
      "populate_typedb" ). entered();
    block_on ( async {
      if let Err (e) = wipe_then_init_typedb_db (
        config, &driver, &nodes
      ) . await {
        tracing::error! ("Failed to populate TypeDB: {}", e);
        std::process::exit (1); }} ) };
  tracing::info! ("TypeDB database initialized successfully.");
  let tantivy_index : TantivyIndex =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "initialize_tantivy" ). entered();
      wipe_then_init_tantivy_db_with_logs_and_errors (config, &nodes) };
  let (env, handoff) : (SkgEnv, InitContextHandoff) =
    { let _span : tracing::span::EnteredSpan = tracing::info_span!(
        "extract_context_data_from_nodes" ). entered();
      env_and_handoff_from_nodes (
        config, &nodes, Arc::new (driver), tantivy_index ) };
  (env, handoff, nodes) }

fn touch_init_marker (
  path : &Path,
) {
  if let Err (e) = fs::write (path, b"") {
    tracing::warn! ("Could not touch init marker {:?}: {}",
               path, e); } }

/// Populates a TypeDB database from the given nodes:
/// overwrites with empty db, defines schema,
/// creates all nodes and relationships.
/// Uses an existing driver connection rather than creating a new one.
/// Callers are responsible for reading the .skg files
/// (and, if desired, checking duplicate IDs) beforehand.
pub async fn wipe_then_init_typedb_db (
  config : &SkgConfig,
  driver : &TypeDBDriver,
  nodes  : &[NodeComplete],
) -> Result<(), Box<dyn Error>> {
  let graph : InRustGraph =
    InRustGraph::from_nodecompletes (nodes);
  error_unless_override_invariants_hold (
    config, &graph ) ?;
  overwrite_new_empty_typedb_db (
    & config . db_name,
    driver ) . await ?;
  read_and_use_schema (
    & config . db_name,
    driver ) . await ?;
  create_all_sources (
    & config . db_name,
    driver,
    config ) . await ?;
  let t0 : Instant = Instant::now();
  let typedb_nodes : Vec<NodeTypedb> = // Convert to NodeTypedb (narrow) at the boundary. Parses textlinks from each node's title+body.
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  create_all_nodes (
    & config . db_name,
    driver,
    &typedb_nodes ) . await ?;
  tracing::info! (elapsed_s = ?t0 . elapsed(), "TypeDB nodes created");
  let t1 : Instant = Instant::now();
  create_all_relationships (
    & config . db_name,
    driver,
    &typedb_nodes ) . await ?;
  tracing::info! (elapsed_s = ?t1 . elapsed(), "TypeDB relationships created");
  Ok (( )) }

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
/// (and, if desired, checking duplicate IDs) beforehand.
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

pub async fn overwrite_new_empty_typedb_db (
  // Destroys the db named `db_name` if it exists,
  // then makes a new, empty one.
  db_name : &str,
  driver  : &TypeDBDriver
) -> Result < (), Box<dyn Error> > {
  let databases : &DatabaseManager = driver . databases ();
  if databases . contains (db_name) . await ? {
    tracing::info! ( db_name, "Deleting existing database" );
    { let database : Arc<Database> =
        databases . get (db_name) . await ?;
      database } . delete () . await ?; }
  tracing::info! ( db_name, "Creating empty database" );
  databases . create (db_name) . await ?;
  Ok (()) }

pub async fn read_and_use_schema (
  db_name : &str,
  driver  : &TypeDBDriver
)-> Result < (), Box<dyn Error> > {
  let tx : Transaction =
    driver . transaction ( db_name,
                         TransactionType::Schema )
    . await ?;
  tracing::info! ("Defining schema ...");
  tx . query ( {
    let schema : String = fs::read_to_string
      ("schema.tql")
      . expect ("Failed to read TypeDB schema file");
    schema } ) . await ?;
  tx . commit () . await ?;
  Ok (()) }
