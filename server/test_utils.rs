mod guard;
pub use guard::TestDbGuard;

use crate::consts::TYPEDB_ADDRESS;
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::filesystem::not_nodes::load_config_with_overrides;
use crate::dbs::init::{overwrite_new_empty_typedb_db, read_and_use_schema, create_empty_tantivy_index};
use crate::dbs::memory::{InRustGraph, InRustGraphHandle, audit::{audit_memory_against_typedb, format_mismatches}, new_handle};
use crate::dbs::tantivy::search::{SearchOptions, search_index};
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::types::env::SkgEnv;
use crate::from_text::buffer_to_viewnodes::uninterpreted::{headline_to_triple, HeadlineInfo};
use crate::serve::ConnectionState;
use crate::serve::handlers::save_buffer::{SaveResponse, update_from_and_rerender_buffer};
use crate::serve::parse_metadata_sexp::ViewnodeMetadata;
use crate::types::memory::ViewUri;
use crate::types::misc::{MSV, SkgConfig, SkgfileSource, ID, TantivyIndex, SourceName};
use crate::types::nodes::typedb::NodeTypedb;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::nodes::complete::NodeComplete;
use crate::types::unchecked_viewnode::{ UncheckedViewNode, UncheckedViewNodeKind };

use ego_tree::{Tree, NodeRef};
use futures::FutureExt;
use futures::StreamExt;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::panic::AssertUnwindSafe;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use typedb_driver::answer::{QueryAnswer, ConceptRow};
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions, Transaction, TransactionType, Database, DatabaseManager};
use crate::dbs::typedb::util::ConceptRowStream;
use std::sync::Arc;
use tantivy::{DocAddress, Searcher, TantivyDocument};
use tantivy::schema::document::Value;


/// Run tests with automatic database setup and cleanup.
///
/// This helper function encapsulates the common pattern of:
/// 1. Copying fixtures to a temp directory (so saves don't corrupt originals)
/// 2. Setting up a test database and Tantivy index
/// 3. Running test functions
/// 4. Cleaning up the database, index, and temp fixtures
///
/// The test_fn closure receives references to SkgConfig, TypeDBDriver, and TantivyIndex
/// and can run multiple test functions sequentially.
///
/// Example:
/// ```
/// #[test]
/// fn my_test() -> Result<(), Box<dyn Error>> {
///   run_with_test_db(
///     "skg-test-my-test",
///     "tests/my_test/fixtures",
///     "/tmp/tantivy-test-my-test",
///     |config, driver, tantivy| Box::pin(async move {
///       test_function_1(config, driver, tantivy).await?;
///       test_function_2(config, driver, tantivy).await?;
///       Ok(())
///     } )) }
/// ```
pub fn run_with_test_db<F>(
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a Arc<TypeDBDriver>, &'a mut TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  let temp_fixtures : PathBuf =
    // Copy fixtures to temp so saves don't corrupt originals
    PathBuf::from(format!("/tmp/{}-fixtures", db_name));
  if temp_fixtures . exists() {
    fs::remove_dir_all (&temp_fixtures)?;
  }
  copy_dir_all(
    &PathBuf::from (fixtures_folder),
    &temp_fixtures)?;
  let result : Result<(), Box<dyn Error>> = block_on(async {
    let (config, driver, mut tantivy)
      : (SkgConfig, TypeDBDriver, TantivyIndex)
      = setup_test_tantivy_and_typedb_dbs(
          db_name,
          temp_fixtures . to_str() . unwrap(),
          tantivy_folder ). await?;
    let driver_arc : Arc<TypeDBDriver> = Arc::new (driver);
    guarded_test_then_cleanup(
      db_name, &config, &driver_arc,
      Some(config . tantivy_folder . clone()),
      test_fn(&config, &driver_arc, &mut tantivy),
    ). await
  } );
  if temp_fixtures . exists() { // more cleanup
    fs::remove_dir_all (&temp_fixtures)?; }
  result
}

/// Like run_with_test_db, but loads config from a TOML file
/// (supporting multi-source setups) and skips Tantivy.
pub fn run_with_test_db_from_config<F>(
  db_name: &str,
  config_path: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a Arc<TypeDBDriver>)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  block_on(async {
    let config: SkgConfig =
      load_config_with_overrides(config_path, Some (db_name), &[])?;
    let driver: TypeDBDriver = TypeDBDriver::new(
        TYPEDB_ADDRESS,
        Credentials::new("admin", "password"),
        DriverOptions::new(false, None)?,
      ). await?;
    let nodes: Vec<NodeComplete> =
      read_all_skg_files_from_sources (&config)?;
    let typedb_nodes : Vec<NodeTypedb> =
      nodes . iter ()
      . map (NodeTypedb::from_complete_parsing_textlinks)
      . collect ();
    overwrite_new_empty_typedb_db(db_name, &driver) . await?;
    read_and_use_schema(db_name, &driver) . await?;
    create_all_nodes(db_name, &driver, &typedb_nodes) . await?;
    create_all_relationships(db_name, &driver, &typedb_nodes) . await?;
    let driver_arc : Arc<TypeDBDriver> = Arc::new (driver);
    guarded_test_then_cleanup(
      db_name, &config, &driver_arc, None,
      test_fn(&config, &driver_arc),
    ) . await
  } )
}

/// Run a test future with a TestDbGuard safety net, then clean up.
/// Catches panics so cleanup runs even if the test fails.
async fn guarded_test_then_cleanup(
  db_name: &str,
  _config: &SkgConfig,
  driver: &TypeDBDriver,
  tantivy_folder: Option<PathBuf>,
  test_future: Pin<Box<dyn Future<Output = Result
                                  <(), Box<dyn Error>>> + '_>>,
) -> Result<(), Box<dyn Error>> {
  let mut guard: TestDbGuard = TestDbGuard::new(
    db_name, tantivy_folder.clone());
  let test_result: Result<Result<(), Box<dyn Error>>, _> =
    AssertUnwindSafe(test_future)
    . catch_unwind() . await;
  let cleanup_result: Result<(), Box<dyn Error>> =
    cleanup_test_tantivy_and_typedb_dbs(
      db_name, driver,
      tantivy_folder . as_deref(),
    ) . await;
  guard . disarm();
  match test_result {
    Ok (inner) => { cleanup_result?; inner },
    Err (panic_payload) => {
      if let Err (e) = cleanup_result {
        tracing::error!("Cleanup error after test panic: {}", e); }
      std::panic::resume_unwind(panic_payload) }, }
}

/// Recursively copy a directory and its contents.
fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all (dst)?;
  for entry in fs::read_dir (src)? {
    let entry : fs::DirEntry = entry?;
    let ty : fs::FileType = entry . file_type()?;
    let src_path : PathBuf = entry . path();
    let dst_path : PathBuf = dst . join(entry . file_name());
    if ty . is_dir() {
      copy_dir_all(&src_path, &dst_path)?;
    } else {
      fs::copy(&src_path, &dst_path)?;
    }
  }
  Ok(())
}

/// Build an in-Rust graph handle preloaded from the test config's
/// fixtures on disk. Use this when a test needs the in-Rust memory
/// to agree with TypeDB for auditing.
pub fn graph_handle_from_config (
  config : &SkgConfig,
) -> Result<InRustGraphHandle, Box<dyn Error>> {
  let nodes : Vec<NodeComplete> =
    read_all_skg_files_from_sources (config) ?;
  Ok ( new_handle ( InRustGraph::from_nodecompletes (&nodes) )) }

/// Bundle a test's existing handles into a 'SkgEnv'.
///
/// Tests should hold their driver as 'Arc<TypeDBDriver>' to match
/// the production shape. '&Arc<TypeDBDriver>' deref-coerces to
/// '&TypeDBDriver' in argument position, so old-style calls
/// continue to work unchanged.
pub fn skg_env_from_parts (
  config        : &SkgConfig,
  driver        : Arc<TypeDBDriver>,
  tantivy_index : &TantivyIndex,
  graph         : &InRustGraphHandle,
) -> SkgEnv {
  SkgEnv {
    config        : config . clone (),
    memory        : graph . clone (),
    tantivy_index : tantivy_index . clone (),
    driver, } }

/// Test shim around 'update_from_and_rerender_buffer' that accepts
/// the four DB handles separately, builds a 'SkgEnv', and calls the
/// underlying function. Lets pre-SkgEnv test code continue to pass
/// '(driver, config, tantivy)' triples without change.
///
/// PITFALL: a SkgEnv field swap inside the call (e.g. a save's
/// rebuild path replacing 'tantivy_index') will not propagate back
/// out to the test's '&mut TantivyIndex' since the env clones the
/// index. Production paths that swap the index are not exercised
/// by these tests, so this is fine in practice.
pub async fn update_from_and_rerender_buffer_test (
  stream                      : &mut std::net::TcpStream,
  org_buffer_text             : &str,
  driver                      : &Arc<TypeDBDriver>,
  config                      : &SkgConfig,
  tantivy_index               : &TantivyIndex,
  diff_mode_enabled           : bool,
  viewuri_from_request_result : &Result<ViewUri, String>,
  conn_state                  : &mut ConnectionState,
) -> Result<SaveResponse, Box<dyn Error>> {
  let mut env : SkgEnv =
    skg_env_from_parts (
      config, Arc::clone (driver),
      tantivy_index, &conn_state . graph );
  update_from_and_rerender_buffer (
    stream,
    org_buffer_text,
    &mut env,
    diff_mode_enabled,
    viewuri_from_request_result,
    conn_state ) . await }

/// Audit the given in-Rust graph handle against TypeDB; panic with a
/// detailed message if they disagree. Intended for per-test-fixture
/// post-mutation verification.
pub async fn audit_memory_or_panic (
  handle  : &InRustGraphHandle,
  db_name : &str,
  driver  : &TypeDBDriver,
) -> Result<(), Box<dyn Error>> {
  let snap : Arc<InRustGraph> = handle . load_full ();
  let mismatches = audit_memory_against_typedb (
    &snap, db_name, driver ) . await ?;
  if ! mismatches . is_empty () {
    panic! ("audit failed:\n{}", format_mismatches (&mismatches)); }
  Ok (( )) }

/// A helper function for tests.
pub async fn populate_test_db_from_fixtures (
  data_folder: &str,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<NodeComplete> = {
    let mut sources: HashMap<SourceName, SkgfileSource> =
      HashMap::new();
    sources . insert(
      SourceName::from ("main"),
      SkgfileSource {
        name: SourceName::from ("main"),
        abbreviation: None,
        path: PathBuf::from (data_folder),
        user_owns_it: true, } );
    read_all_skg_files_from_sources(
      &SkgConfig::dummyFromSources (sources))? };
  overwrite_new_empty_typedb_db (
    db_name, driver ) . await ?;
  read_and_use_schema (
    db_name, driver ) . await?;
  let typedb_nodes : Vec<NodeTypedb> =
    nodes . iter ()
    . map (NodeTypedb::from_complete_parsing_textlinks)
    . collect ();
  create_all_nodes (
    db_name, driver, &typedb_nodes ) . await ?;
  create_all_relationships (
    db_name, driver, &typedb_nodes ) . await ?;
  Ok (( )) }

/* PURPOSE: Set up test dbs (Tantivy and TypeDB)
with fixtures from the given folder.
The test database will be named with the given db_name prefix.
The program calling this should call `cleanup_test_tantivy_and_typedb_dbs`
after the test completes to remove the database.
.
PITFALL: This sets delete_on_quit=false
because tests tear down the db themselves.
Unit tests don't even run the Rust-Emacs server (integration tests do),
so while there's something to delete, there's no server to quit. */
pub async fn setup_test_tantivy_and_typedb_dbs (
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, TypeDBDriver, TantivyIndex), Box<dyn Error>> {
  // PITFALL: Tests control cleanup via cleanup_test_tantivy_and_typedb_dbs,
  // not via delete_on_quit, because there's no server to quit.
  let config: SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new();
    sources . insert (
      SourceName::from ("main"),
      SkgfileSource {
        name         : SourceName::from ("main"),
        abbreviation : None,
        path         : PathBuf::from (fixtures_folder),
        user_owns_it : true, });
    SkgConfig::fromSourcesAndDbName (
      sources, db_name, tantivy_folder ) };
  let driver: TypeDBDriver = TypeDBDriver::new(
    TYPEDB_ADDRESS,
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ) . await ?;
  populate_test_db_from_fixtures(
    fixtures_folder,
    db_name,
    &driver
  ) . await?;
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config . tantivy_folder)?;
  Ok ((config, driver, tantivy_index)) }

/// Clean up test database and tantivy index after a test completes.
///
/// This deletes:
/// - The TypeDB database (if it exists)
/// - The Tantivy index directory (if it exists)
///
/// Does NOT delete the .skg fixture files.
///
/// PITFALL: Retries TypeDB delete on "database is in use" errors.
/// This is a race: when a test returns, Rust Transactions drop and
/// signal close asynchronously, but the TypeDB server may not have
/// finished releasing them before we ask to delete the database.
/// The retries give the server time to catch up.
pub async fn cleanup_test_tantivy_and_typedb_dbs(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  { // Delete TypeDB database (with retry).
    let databases : &DatabaseManager = driver . databases();
    if databases . contains (db_name) . await? {
      let max_attempts : usize = 20; // 20 * 50ms = 1s total
      let mut last_err : Option<Box<dyn Error>> = None;
      for attempt in 0 .. max_attempts {
        let database : Arc<Database> =
          databases . get (db_name) . await?;
        match database . delete() . await {
          Ok (()) => { last_err = None; break; }
          Err (e) => {
            let msg : String = e . to_string();
            if msg . contains ("is in use") && attempt + 1 < max_attempts {
              // block_on is single-threaded; a blocking sleep
              // is safe and doesn't need a tokio/async-std timer.
              std::thread::sleep (
                std::time::Duration::from_millis (50) );
              last_err = Some ( Box::new (e) ); }
            else { last_err = Some ( Box::new (e) ); break; }} } }
      if let Some (e) = last_err { return Err (e); } } }

  // Delete Tantivy index if path provided and exists
  if let Some (tantivy_path) = tantivy_folder {
    if tantivy_path . exists() {
      fs::remove_dir_all (tantivy_path)?; }}

  Ok (( )) }

/// Compare two org-mode headlines ignoring ID differences.
/// Converts each headline to HeadlineInfo and strips ID from metadata.
pub fn compare_headlines_modulo_id(
  headline1: &str,
  headline2: &str
) -> bool {
  let info1: Result<HeadlineInfo, String> =
    headline_to_triple (headline1);
  let info2: Result<HeadlineInfo, String> =
    headline_to_triple (headline2);

  match (info1, info2) {
    (Ok((level1, metadata1, title1)),
     Ok((level2, metadata2, title2))) => {
      let has_id1: bool =
        metadata1 . as_ref() . map_or(false, |m| m . id . is_some());
      let has_id2: bool =
        metadata2 . as_ref() . map_or(false, |m| m . id . is_some());
      if has_id1 != has_id2 {
        // One has an ID and the other doesn't, so they are unequal.
        return false; }
      // Strip IDs from both (no-op if no ID present) and compare
      let stripped_metadata1: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct (metadata1);
      let stripped_metadata2: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct (metadata2);
      (level1, stripped_metadata1, title1) ==
        (level2, stripped_metadata2, title2) },
    (Err (e1), Err (e2)) if (e1 == "__NOT_A_HEADLINE__" &&
                           e2 == "__NOT_A_HEADLINE__")
      => { // Both are not headlines, so compare directly
        headline1 == headline2 },
    _ => false,  // One is headline, other is not, or they have different errors
  }}

/// Compare two UncheckedViewNode trees by DFS.
/// (PITFALL: Naive comparison of trees just compares NodeIds,
/// which are nearly meaningless.)
pub fn compare_viewnode_trees (
  node1 : NodeRef < UncheckedViewNode >,
  node2 : NodeRef < UncheckedViewNode >
) -> bool {
  let n1 : & UncheckedViewNode =
    node1 . value ();
  let n2 : & UncheckedViewNode =
    node2 . value ();
  if n1 != n2 { return false; }
  { // recurse
    let children1 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node1 . children () . collect ();
    let children2 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node2 . children () . collect ();
    children1 . len () == children2 . len () &&
      children1 . iter () . zip ( children2 . iter () )
      . all ( | ( c1, c2 ) |
              compare_viewnode_trees ( *c1, *c2 )) }}

/// Compares ignoring ID value but not ID presence/absence.
pub fn compare_viewnode_trees_modulo_id(
  viewforest1: &Tree<UncheckedViewNode>,
  viewforest2: &Tree<UncheckedViewNode>
) -> bool {
  let root1 : Vec < NodeRef < '_, UncheckedViewNode >> =
    viewforest1 . root() . children() . collect();
  let root2 : Vec < NodeRef < '_, UncheckedViewNode >> =
    viewforest2 . root() . children() . collect();
  if root1 . len() != root2 . len() {
    return false; }
  for (tree1, tree2) in root1 . iter() . zip(root2 . iter()) {
    if !compare_two_viewnode_branches_recursively_modulo_id(
      *tree1, *tree2 )
    { return false; }}
  true }

/// Compare two UncheckedViewNode subtrees, ignoring ID values.
fn compare_two_viewnode_branches_recursively_modulo_id (
  node1: NodeRef<UncheckedViewNode>,
  node2: NodeRef<UncheckedViewNode>
) -> bool {
  let n1 : &UncheckedViewNode = node1 . value();
  let n2 : &UncheckedViewNode = node2 . value();
  match (&n1 . kind, &n2 . kind) {
    ( UncheckedViewNodeKind::True (_),
      UncheckedViewNodeKind::True (t2)) =>
    { // Copy the ID from one to the other, then compare.
      let mut n1_copy : UncheckedViewNode =
        n1 . clone();
      if let UncheckedViewNodeKind::True (t) = &mut n1_copy . kind {
        t . id = t2 . id . clone(); }
      if n1_copy != *n2 { return false; }}
    ( UncheckedViewNodeKind::Scaff (_),
      UncheckedViewNodeKind::Scaff (_)) =>
    { if n1 != n2 { return false; }}
    ( UncheckedViewNodeKind::Deleted (_),
      UncheckedViewNodeKind::Deleted (_)) =>
    { if n1 != n2 { return false; }}
    ( UncheckedViewNodeKind::DeletedScaff (_),
      UncheckedViewNodeKind::DeletedScaff (_)) =>
    { if n1 != n2 { return false; }}
    _ => return false, // mismatched kinds
  }
  { // Recurse on children
    let children1 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node1 . children() . collect();
    let children2 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node2 . children() . collect();
    ( children1 . len() == children2 . len() &&
      children1 . iter() . zip(children2 . iter())
      . all (|(c1, c2)|
             compare_two_viewnode_branches_recursively_modulo_id(
               *c1, *c2)) ) }}

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<ViewnodeMetadata>
) -> Option<ViewnodeMetadata> {
  metadata . map(|mut meta| {
    meta . id = None;
    meta
  } ) }

/// Query all primary node IDs from TypeDB.
/// Returns a HashSet of all IDs belonging to primary nodes (not extra_ids).
pub async fn all_pids_from_typedb(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let tx: Transaction = driver . transaction(
    db_name, TransactionType::Read ) . await ?;
  let query: String =
    "match $node isa node, has id $node_id; select $node_id;"
    . to_string();
  let answer: QueryAnswer = tx . query (query) . await?;
  let mut node_ids: HashSet<ID> = HashSet::new();
  let mut stream : ConceptRowStream = answer . into_rows();
  while let Some (row_result) = stream . next() . await {
    let row : ConceptRow = row_result?;
    if let Some (concept) = row . get ("node_id")? {
      let node_id_str: String =
        extract_payload_from_typedb_string_rep(
          &concept . to_string());
      node_ids . insert(ID (node_id_str)); }}
  Ok (node_ids) }

/// Query all extra_ids for a given primary node ID from TypeDB.
/// Returns a Vec of all extra_ids associated with the node.
pub async fn extra_ids_from_pid(
  db_name: &str,
  driver: &TypeDBDriver,
  skgid: &ID,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let tx: Transaction = driver . transaction(
    db_name, TransactionType::Read ) . await?;
  let query : String = format!(
    r#"match $node isa node, has id "{}";
       $e isa extra_id;
       $rel isa has_extra_id (node: $node, extra_id: $e);
       $e has id $extra_id_value;
       select $extra_id_value;"#,
    skgid . 0 );
  let answer: QueryAnswer = tx . query (query) . await?;
  let mut extra_ids : Vec<ID> = Vec::new();
  let mut stream : ConceptRowStream = answer . into_rows();
  while let Some (row_result) = stream . next() . await {
    let row : ConceptRow = row_result?;
    if let Some (concept) = row . get ("extra_id_value")? {
      let extra_id_str : String =
        extract_payload_from_typedb_string_rep(
          &concept . to_string());
      extra_ids . push(
        ID (extra_id_str)); }}
  Ok (extra_ids) }

/// Check if a specific ID exists in Tantivy search results.
/// Searches for the given query and checks if any result has the exact ID.
pub fn tantivy_contains_id(
  tantivy_index: &TantivyIndex,
  query: &str,
  expected_id: &str,
) -> Result<bool, Box<dyn Error>> {
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, Searcher)
    = search_index ( tantivy_index, query,
                     & SearchOptions::default () )?;
  for (_score, doc_address) in matches {
    let doc: TantivyDocument = searcher . doc (doc_address)?;
    let id_value: Option<String> =
      doc . get_first(tantivy_index . id_field)
      . and_then(|v| v . as_str() . map (String::from));
    if id_value == Some(expected_id . to_string()) {
      return Ok (true); }}
  Ok (false) }

/// Strips comments from an org-buffer string.
///
/// Comments are marked by '#' - everything after the first '#' on each line
/// is removed, along with any trailing whitespace before the '#'.
///
/// # Example
/// ```
/// use skg::test_utils::strip_org_comments;
/// let input = "  * (skg (id 1)) title 1 # here's a comment\n  ** (skg (id 2)) title 2 # here's another";
/// let result = strip_org_comments(input);
/// assert_eq!(result, "  * (skg (id 1)) title 1\n  ** (skg (id 2)) title 2");
/// ```
pub fn strip_org_comments(s: &str) -> String {
  s . lines()
    . map(|line| {
      if let Some (hash_pos) = line . find ('#') {
        // Remove everything from '#' onwards, then trim trailing whitespace
        line[..hash_pos] . trim_end() . to_string()
      } else { line . to_string() }} )
    . collect::<Vec<String>>()
    . join ("\n") }

/// Example NodeComplete for use in tests.
pub fn nodecomplete_example () -> NodeComplete {
  NodeComplete {
    title: "This text gets indexed." . to_string(),
    aliases: MSV::Unspecified,
    source: SourceName::from ("main"),
    pid: ID::new ("example"),
    extra_ids: vec![],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: vec![ ID::new ("1"),
                    ID::new ("2"),
                    ID::new ("3")],
    subscribes_to: MSV::Specified(vec![ID::new ("11"),
                             ID::new ("12"),
                             ID::new ("13")]),
    hides_from_its_subscriptions: MSV::Unspecified,
    overrides_view_of: MSV::Unspecified,
    misc: Vec::new (), }}

/// Extract NodeComplete from Save variant; panics on Delete.
pub fn extract_nodecomplete_if_save_else_error(
  instr: &DefineNode
) -> &NodeComplete {
  match instr {
    DefineNode::Save(SaveNode (node)) => node,
    DefineNode::Delete (_) => panic!("Expected Save, got Delete") }}

/// Read one length-prefixed message from a TCP stream.
/// Returns the body as a String.
/// Format: "Content-Length: N\r\n\r\n<N bytes>"
pub fn read_lp_message (
  reader : &mut std::io::BufReader<std::net::TcpStream>,
) -> Result<String, Box<dyn std::error::Error>> {
  use std::io::{BufRead, Read};
  let mut header_line : String = String::new ();
  reader . read_line (&mut header_line) ?;
  let mut blank : String = String::new ();
  reader . read_line (&mut blank) ?;
  let content_length : usize =
    header_line . trim ()
    . strip_prefix ("Content-Length: ")
    . ok_or ("missing Content-Length header") ?
    . parse () ?;
  let mut body_bytes : Vec<u8> = vec![0u8; content_length];
  reader . read_exact (&mut body_bytes) ?;
  Ok ( String::from_utf8 (body_bytes) ? ) }

/// Read all LP messages from a stream until one contains the
/// given terminal response type (e.g. "save-result").
/// Returns (non_terminal_messages, terminal_message).
pub fn read_lp_messages_until (
  reader        : &mut std::io::BufReader<std::net::TcpStream>,
  terminal_type : &str,
) -> Result<(Vec<String>, String),
            Box<dyn std::error::Error>> {
  let mut collected : Vec<String> = Vec::new ();
  loop {
    let msg : String = read_lp_message (reader) ?;
    if msg . contains (terminal_type) {
      return Ok (( collected, msg )); }
    collected . push (msg); } }

/// Read all LP messages from a stream until EOF.
/// Useful for tests that call pipeline functions directly
/// (without the full handler that sends a terminal message).
pub fn read_all_lp_messages (
  reader : &mut std::io::BufReader<std::net::TcpStream>,
) -> Vec<String> {
  let mut messages : Vec<String> = Vec::new ();
  while let Ok (msg) = read_lp_message (reader) {
    messages . push (msg); }
  messages }

/// Extract a string field from a tagged sexp like
/// ((response-type X) (view-uri "URI") (content "...")).
/// Returns None if the key is not found.
pub fn extract_string_field_from_sexp (
  sexp_str : &str,
  key      : &str,
) -> Option<String> {
  let parsed : sexp::Sexp = sexp::parse (sexp_str) . ok () ?;
  match &parsed {
    sexp::Sexp::List (items) =>
      items . iter () . find_map ( |item| match item {
        sexp::Sexp::List (pair) if pair . len () == 2 =>
          match (&pair[0], &pair[1]) {
            ( sexp::Sexp::Atom (sexp::Atom::S (k)),
              sexp::Sexp::Atom (sexp::Atom::S (v)) )
              if k == key => Some (v . clone ()),
            _ => None },
        _ => None } ),
    _ => None } }
