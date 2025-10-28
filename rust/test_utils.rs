use crate::save::{headline_to_triple, HeadlineInfo};
use crate::types::{OrgNode, OrgnodeMetadata, SkgNode, SkgConfig, ID, TantivyIndex};
use crate::file_io::read_skg_files;
use crate::typedb::init::{overwrite_new_empty_db, define_schema};
use crate::typedb::nodes::create_all_nodes;
use crate::typedb::relationships::create_all_relationships;
use crate::typedb::util::extract_payload_from_typedb_string_rep;
use crate::tantivy::search_index;
use ego_tree::{Tree, NodeRef};
use futures::executor::block_on;
use futures::StreamExt;
use std::collections::HashSet;
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions, Transaction, TransactionType};
use typedb_driver::answer::QueryAnswer;

/// Run tests with automatic database setup and cleanup.
///
/// This helper function encapsulates the common pattern of:
/// 1. Setting up a test database and Tantivy index
/// 2. Running test functions
/// 3. Cleaning up the database and index
///
/// The test_fn closure receives references to SkgConfig and TypeDBDriver
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
///     |config, driver| Box::pin(async move {
///       test_function_1(config, driver).await?;
///       test_function_2(config, driver).await?;
///       Ok(())
///     })
///   )
/// }
/// ```
pub fn run_with_test_db<F>(
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a TypeDBDriver)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  block_on(async {
    let (config, driver): (SkgConfig, TypeDBDriver) =
      setup_test_tantivy_and_typedb_dbs(
        db_name, fixtures_folder, tantivy_folder ). await?;
    let result = test_fn(&config, &driver).await;
    cleanup_test_tantivy_and_typedb_dbs(
      db_name,
      &driver,
      Some(config.tantivy_folder.as_path())
    ).await?;
    result
  } ) }

/// A helper function for tests.
pub async fn populate_test_db_from_fixtures (
  data_folder: &str,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<SkgNode> =
    read_skg_files(data_folder)?;
  overwrite_new_empty_db (
    db_name, driver ). await ?;
  define_schema (
    db_name, driver ). await?;
  create_all_nodes (
    db_name, driver, &nodes ). await ?;
  create_all_relationships (
    db_name, driver, &nodes ). await ?;
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
) -> Result<(SkgConfig, TypeDBDriver), Box<dyn Error>> {
  let config: SkgConfig = SkgConfig {
    db_name: db_name.to_string(),
    skg_folder: PathBuf::from(fixtures_folder),
    tantivy_folder: PathBuf::from(tantivy_folder),
    port: 1730,
    delete_on_quit: false, // PITFALL: Tests control cleanup via cleanup_test_tantivy_and_typedb_dbs, not via delete_on_quit, because there's no server to quit.
  };
  let driver: TypeDBDriver = TypeDBDriver::new(
    "127.0.0.1:1729",
    Credentials::new("admin", "password"),
    DriverOptions::new(false, None)?
  ). await ?;
  populate_test_db_from_fixtures(
    fixtures_folder,
    db_name,
    &driver
  ). await?;
  Ok ((config, driver)) }

/// Clean up test database and tantivy index after a test completes.
///
/// This deletes:
/// - The TypeDB database (if it exists)
/// - The Tantivy index directory (if it exists)
///
/// Does NOT delete the .skg fixture files.
pub async fn cleanup_test_tantivy_and_typedb_dbs(
  db_name: &str,
  driver: &TypeDBDriver,
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  // Delete TypeDB database
  let databases = driver.databases();
  if databases.contains(db_name).await? {
    let database = databases.get(db_name).await?;
    database.delete().await?; }

  // Delete Tantivy index if path provided and exists
  if let Some(tantivy_path) = tantivy_folder {
    if tantivy_path.exists() {
      fs::remove_dir_all(tantivy_path)?; }}

  Ok (( )) }

/// Compare two org-mode headlines ignoring ID differences.
/// Converts each headline to HeadlineInfo and strips ID from metadata.
pub fn compare_headlines_modulo_id(
  headline1: &str,
  headline2: &str
) -> bool {
  let info1: Result<HeadlineInfo, String> =
    headline_to_triple(headline1);
  let info2: Result<HeadlineInfo, String> =
    headline_to_triple(headline2);

  match (info1, info2) {
    (Ok((level1, metadata1, title1)),
     Ok((level2, metadata2, title2))) => {
      let has_id1: bool =
        metadata1.as_ref().map_or(false, |m| m.id.is_some());
      let has_id2: bool =
        metadata2.as_ref().map_or(false, |m| m.id.is_some());
      if has_id1 != has_id2 {
        // One has an ID and the other doesn't, so they are unequal.
        return false; }
      // Strip IDs from both (no-op if no ID present) and compare
      let stripped_metadata1: Option<OrgnodeMetadata> =
        strip_id_from_metadata_struct(metadata1);
      let stripped_metadata2: Option<OrgnodeMetadata> =
        strip_id_from_metadata_struct(metadata2);
      (level1, stripped_metadata1, title1) ==
        (level2, stripped_metadata2, title2) },
    (Err(e1), Err(e2)) if (e1 == "__NOT_A_HEADLINE__" &&
                           e2 == "__NOT_A_HEADLINE__")
      => { // Both are not headlines, so compare directly
        headline1 == headline2 },
    _ => false,  // One is headline, other is not, or they have different errors
  }}

/// Compare two OrgNode trees by traversing them DFS.
/// Compares all structure and all content including IDs.
/// Ignores internal NodeId values.
/// (PITFALL: Naive comparison of trees just compares NodeIds,
/// which are nearly meaningless.)
pub fn compare_orgnode_forests (
  trees1 : & [ Tree < OrgNode > ],
  trees2 : & [ Tree < OrgNode > ]
) -> bool {
  if trees1 . len () != trees2 . len () {
    return false; }
  for ( tree1, tree2 ) in trees1 . iter () . zip ( trees2 . iter () ) {
    if ! compare_orgnode_trees (
      tree1 . root (), tree2 . root () )
    { return false; } }
  true }

/// Compare two nodes and their subtrees recursively.
fn compare_orgnode_trees (
  node1 : NodeRef < OrgNode >,
  node2 : NodeRef < OrgNode >
) -> bool {
  let n1 : & OrgNode = node1 . value ();
  let n2 : & OrgNode = node2 . value ();
  // Compare the node values directly
  if n1 != n2 { return false; }
  // Compare children recursively
  let children1 : Vec < _ > =
    node1 . children () . collect ();
  let children2 : Vec < _ > =
    node2 . children () . collect ();
  children1 . len () == children2 . len () &&
    children1 . iter () . zip ( children2 . iter () )
    . all ( | ( c1, c2 ) |
                compare_orgnode_trees ( *c1, *c2 ) ) }

/// Compare two tree forests modulo ID differences.
/// Trees are considered the same if their structure and content match,
/// ignoring ID values (but not ID presence/absence).
pub fn compare_two_forests_modulo_id(
  trees1: &[Tree<OrgNode>],
  trees2: &[Tree<OrgNode>]
) -> bool {
  if trees1.len() != trees2.len() {
    return false; }
  for (tree1, tree2) in trees1.iter().zip(trees2.iter()) {
    if !compare_two_orgnodes_recursively_modulo_id(
      tree1.root(), tree2.root( ))
    { return false; }}
  true }

/// Compare two nodes and their subtrees modulo ID differences.
fn compare_two_orgnodes_recursively_modulo_id (
  node1: NodeRef<OrgNode>,
  node2: NodeRef<OrgNode>
) -> bool {
  let n1 : &OrgNode = node1.value();
  let n2 : &OrgNode = node2.value();

  // Check if ID presence differs
  if ( n1.metadata.id.is_some() !=
       n2.metadata.id.is_some() )
  { return false; }

  // Clone and strip IDs for comparison
  let mut n1_copy : OrgNode = n1.clone();
  let mut n2_copy : OrgNode = n2.clone();
  n1_copy.metadata.id = None;
  n2_copy.metadata.id = None;

  if n1_copy != n2_copy { return false; }

  // Compare children recursively
  let children1 : Vec<_> =
    node1.children().collect();
  let children2 : Vec<_> =
    node2.children().collect();

  children1.len() == children2.len() &&
    children1 . iter() . zip ( children2.iter() )
    . all ( | ( c1, c2 ) |
                compare_two_orgnodes_recursively_modulo_id ( *c1, *c2 ) ) }

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<OrgnodeMetadata>
) -> Option<OrgnodeMetadata> {
  metadata.map(|mut meta| {
    meta.id = None;
    meta
  } ) }

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_compare_headlines_modulo_id() {
    // Test identical headlines
    assert!(compare_headlines_modulo_id(
      "* Title",
      "* Title"
    ));

    // Test headlines that differ only by ID
    assert!(compare_headlines_modulo_id(
      "* (skg (id abc123)) Title",
      "* (skg (id xyz789)) Title"
    ));

    // Test headlines where one has ID and other doesn't - should be unequal
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc123)) Title",
      "* Title"
    ));

    // Test headlines with same other metadata but different IDs
    assert!(compare_headlines_modulo_id(
      "* (skg (id abc) (code (relToParent content))) Title",
      "* (skg (id xyz) (code (relToParent content))) Title"
    ));

    // Test headlines that differ by title
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc)) Title One",
      "* (skg (id xyz)) Title Two"
    ));

    // Test headlines that differ by level
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc)) Title",
      "** (skg (id xyz)) Title"
    ));

    // Test headlines that differ by other metadata
    assert!(!compare_headlines_modulo_id(
      "* (skg (id abc) (code (relToParent content))) Title",
      "* (skg (id xyz) (code (relToParent alias))) Title"
    ));

    // Test non-headlines
    assert!(compare_headlines_modulo_id(
      "This is body text",
      "This is body text"
    ));

    assert!(!compare_headlines_modulo_id(
      "This is body text",
      "This is different text"
    ));

    // Test mixed (one headline, one not)
    assert!(!compare_headlines_modulo_id(
      "* Title",
      "Body text" )); }}

/// Query all primary node IDs from TypeDB.
/// Returns a HashSet of all IDs belonging to primary nodes (not extra_ids).
pub async fn all_pids_from_typedb(
  db_name: &str,
  driver: &TypeDBDriver,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let tx: Transaction = driver.transaction(
    db_name, TransactionType::Read ). await ?;
  let query: String =
    "match $node isa node, has id $node_id; select $node_id;"
    .to_string();
  let answer: QueryAnswer = tx.query(query).await?;
  let mut node_ids: HashSet<ID> = HashSet::new();
  let mut stream = answer.into_rows();
  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("node_id")? {
      let node_id_str: String =
        extract_payload_from_typedb_string_rep(
          &concept.to_string());
      node_ids.insert(ID(node_id_str)); }}
  Ok (node_ids) }

/// Check if a specific ID exists in Tantivy search results.
/// Searches for the given query and checks if any result has the exact ID.
pub fn tantivy_contains_id(
  tantivy_index: &TantivyIndex,
  query: &str,
  expected_id: &str,
) -> Result<bool, Box<dyn Error>> {
  let (matches, searcher) = search_index(tantivy_index, query)?;
  for (_score, doc_address) in matches {
    let doc: tantivy::Document = searcher.doc(doc_address)?;
    let id_value: Option<String> =
      doc.get_first(tantivy_index.id_field)
      .and_then(|v| v.as_text().map(String::from));
    if id_value == Some(expected_id.to_string()) {
      return Ok(true); }}
  Ok(false) }
