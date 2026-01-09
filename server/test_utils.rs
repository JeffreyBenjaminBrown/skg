use crate::from_text::buffer_to_orgnodes::uninterpreted::{headline_to_triple, HeadlineInfo};
use crate::dbs::init::{overwrite_new_empty_db, define_schema, create_empty_tantivy_index};
use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::tantivy::search_index;
use crate::dbs::typedb::nodes::create_all_nodes;
use crate::dbs::typedb::relationships::create_all_relationships;
use crate::dbs::typedb::util::extract_payload_from_typedb_string_rep;
use crate::to_org::util::forest_root_pair;
use crate::types::misc::{SkgConfig, SkgfileSource, ID, TantivyIndex};
use crate::serve::parse_metadata_sexp::OrgnodeMetadata;
use crate::types::orgnode::{OrgNode, OrgNodeKind};
use crate::types::skgnode::SkgNode;
use crate::types::tree::{PairTree, NodePair};

use ego_tree::{Tree, NodeId, NodeRef};
use futures::StreamExt;
use futures::executor::block_on;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use typedb_driver::answer::QueryAnswer;
use typedb_driver::{TypeDBDriver, Credentials, DriverOptions, Transaction, TransactionType};


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
  FnOnce(&'a SkgConfig, &'a TypeDBDriver, &'a TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  // Copy fixtures to temp so saves don't corrupt originals
  let temp_fixtures = PathBuf::from(format!("/tmp/{}-fixtures", db_name));
  if temp_fixtures.exists() {
    fs::remove_dir_all(&temp_fixtures)?;
  }
  copy_dir_all(
    &PathBuf::from(fixtures_folder),
    &temp_fixtures)?;

  let result = block_on(async {
    let (config, driver, tantivy): (SkgConfig, TypeDBDriver, TantivyIndex) =
      setup_test_tantivy_and_typedb_dbs(
        db_name,
        temp_fixtures.to_str().unwrap(),
        tantivy_folder ). await?;
    let result = test_fn(&config, &driver, &tantivy).await;
    cleanup_test_tantivy_and_typedb_dbs(
      db_name,
      &driver,
      Some(config.tantivy_folder.as_path())
    ).await?;
    result
  });

  // Clean up temp fixtures
  if temp_fixtures.exists() {
    fs::remove_dir_all(&temp_fixtures)?;
  }

  result
}

/// Recursively copy a directory and its contents.
fn copy_dir_all(src: &Path, dst: &Path) -> Result<(), Box<dyn Error>> {
  fs::create_dir_all(dst)?;
  for entry in fs::read_dir(src)? {
    let entry = entry?;
    let ty = entry.file_type()?;
    let src_path = entry.path();
    let dst_path = dst.join(entry.file_name());
    if ty.is_dir() {
      copy_dir_all(&src_path, &dst_path)?;
    } else {
      fs::copy(&src_path, &dst_path)?;
    }
  }
  Ok(())
}

/// A helper function for tests.
pub async fn populate_test_db_from_fixtures (
  data_folder: &str,
  db_name: &str,
  driver: &TypeDBDriver
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<SkgNode> = {
    let mut sources: HashMap<String, SkgfileSource> = HashMap::new();
    sources.insert(
      "main".to_string(),
      SkgfileSource {
        nickname: "main".to_string(),
        path: PathBuf::from(data_folder),
        user_owns_it: true, } );
    read_all_skg_files_from_sources(
      &SkgConfig::dummyFromSources( sources ))? };
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
) -> Result<(SkgConfig, TypeDBDriver, TantivyIndex), Box<dyn Error>> {
  // PITFALL: Tests control cleanup via cleanup_test_tantivy_and_typedb_dbs,
  // not via delete_on_quit, because there's no server to quit.
  let config: SkgConfig = {
    let mut sources : HashMap<String, SkgfileSource> = HashMap::new();
    sources.insert (
      "main".to_string (),
      SkgfileSource {
        nickname     : "main".to_string (),
        path         : PathBuf::from ( fixtures_folder ),
        user_owns_it : true, });
    SkgConfig::fromSourcesAndDbName (
      sources, db_name, tantivy_folder ) };
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
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config.tantivy_folder)?;
  Ok ((config, driver, tantivy_index)) }

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

/// See 'compare_orgnode_trees'.
pub fn compare_orgnode_forests (
  forest1 : & Tree < OrgNode >,
  forest2 : & Tree < OrgNode >
) -> bool {
  compare_orgnode_trees ( forest1.root(), forest2.root() ) }

/// Compare two OrgNode trees (possibly forests, via ForestRoot)
/// by traversing them DFS.
/// Compares all structure and all content including IDs.
/// Ignores internal NodeId values.
/// (PITFALL: Naive comparison of trees just compares NodeIds,
/// which are nearly meaningless.)
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
///
/// Input forests have ForestRoot at root; tree roots are their children.
pub fn compare_two_forests_modulo_id(
  forest1: &Tree<OrgNode>,
  forest2: &Tree<OrgNode>
) -> bool {
  let root1 : Vec<_> = forest1.root().children().collect();
  let root2 : Vec<_> = forest2.root().children().collect();
  if root1.len() != root2.len() {
    return false; }
  for (tree1, tree2) in root1.iter().zip(root2.iter()) {
    if !compare_two_orgnode_branches_recursively_modulo_id( *tree1, *tree2 )
    { return false; }}
  true }

fn compare_orgnode_interpretations(
  k1: &OrgNodeKind,
  k2: &OrgNodeKind
) -> bool {
  match (k1, k2) {
    (OrgNodeKind::Scaff(s1), OrgNodeKind::Scaff(s2)) =>
      // two Scaffolds of the same variant
      s1 == s2,
    (OrgNodeKind::True(t1), OrgNodeKind::True(t2)) =>
      // two TrueNodes of the same Effect_On_Parent
      t1.effect_on_parent == t2.effect_on_parent,
    _ => false, }}

/// Compare two nodes and their subtrees modulo ID differences.
fn compare_two_orgnode_branches_recursively_modulo_id (
  node1: NodeRef<OrgNode>,
  node2: NodeRef<OrgNode>
) -> bool {
  let n1 : &OrgNode = node1.value();
  let n2 : &OrgNode = node2.value();

  // Check if ID presence differs
  if ( n1.id().is_some() !=
       n2.id().is_some() )
  { return false; }

  // Compare all fields except ID
  if n1.title() != n2.title() { return false; }
  if n1.body() != n2.body() { return false; }
  if n1.source() != n2.source() { return false; }
  // Compare kind/effect but skip ID (which is inside TrueNode)
  if !compare_orgnode_interpretations(&n1.kind, &n2.kind) { return false; }
  if n1.is_indefinitive() != n2.is_indefinitive() { return false; }
  if n1.focused != n2.focused { return false; }
  if n1.folded != n2.folded { return false; }
  if n1.edit_request() != n2.edit_request() { return false; }

  // Compare children recursively
  let children1 : Vec<_> =
    node1.children().collect();
  let children2 : Vec<_> =
    node2.children().collect();

  children1.len() == children2.len() &&
    children1 . iter() . zip ( children2.iter() )
    . all ( | ( c1, c2 ) |
                compare_two_orgnode_branches_recursively_modulo_id (
                  *c1, *c2 )) }

/// Compare a PairTree forest (with ForestRoot) against a Vec of OrgNode trees.
/// This compares just the OrgNode portions, ignoring the SkgNode Option.
pub fn compare_orgnode_portions_of_pairforest_and_orgnodeforest (
  forest : &PairTree,
  forest2 : & Tree<OrgNode>,
) -> bool {
  let tree_roots1 : Vec < _ > =
    forest . root () . children () . collect ();
  let tree_roots2 : Vec < _ > =
    forest2 . root () . children () . collect ();
  if tree_roots1 . len () != tree_roots2 . len () {
    return false; }
  fn compare_nodes (
    node1 : NodeRef < NodePair >,
    node2 : NodeRef < OrgNode >
  ) -> bool {
    // Compare the OrgNode values directly
    let n1 : & OrgNode = node1 . value () . orgnode ();
    let n2 : & OrgNode = node2 . value ();
    // Compare the OrgNode values
    if n1 != n2 { return false; }
    // Compare children recursively
    let children1 : Vec < _ > = node1 . children () . collect ();
    let children2 : Vec < _ > = node2 . children () . collect ();
    children1 . len () == children2 . len () &&
      children1 . iter () . zip ( children2 . iter () )
      . all ( | ( c1, c2 ) | compare_nodes ( *c1, *c2 ) ) }
  for ( tree_root1, tree_root2 )
    in tree_roots1 . iter ()
    . zip ( tree_roots2 . iter () ) {
      if ! compare_nodes (
        *tree_root1, *tree_root2 )
      { return false; }}
  true }

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<OrgnodeMetadata>
) -> Option<OrgnodeMetadata> {
  metadata.map(|mut meta| {
    meta.id = None;
    meta
  } ) }

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

/// Query all extra_ids for a given primary node ID from TypeDB.
/// Returns a Vec of all extra_ids associated with the node.
pub async fn extra_ids_from_pid(
  db_name: &str,
  driver: &TypeDBDriver,
  skgid: &ID,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let tx: Transaction = driver.transaction(
    db_name, TransactionType::Read ). await?;
  let query = format!(
    r#"match $node isa node, has id "{}";
       $e isa extra_id;
       $rel isa has_extra_id (node: $node, extra_id: $e);
       $e has id $extra_id_value;
       select $extra_id_value;"#,
    skgid.0 );
  let answer: QueryAnswer = tx.query(query).await?;
  let mut extra_ids = Vec::new();
  let mut stream = answer.into_rows();
  while let Some(row_result) = stream.next().await {
    let row = row_result?;
    if let Some(concept) = row.get("extra_id_value")? {
      let extra_id_str = extract_payload_from_typedb_string_rep(
        &concept.to_string());
      extra_ids.push(
        ID(extra_id_str)); }}
  Ok(extra_ids) }

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
  s.lines()
    .map(|line| {
      if let Some(hash_pos) = line.find('#') {
        // Remove everything from '#' onwards, then trim trailing whitespace
        line[..hash_pos] . trim_end() . to_string()
      } else { line . to_string() }} )
    .collect::<Vec<String>>()
    .join("\n") }

/// Convert an OrgNode "forest" (tree with ForestRoot)
/// to a paired forest, *with None for all SkgNodes*.
/// Used in tests where we don't have save instructions.
pub fn orgnode_forest_to_paired (
  forest : Tree < OrgNode >,
) -> PairTree {
  fn add_orgnode_tree_as_child_of_forest_root (
    paired_forest   : &mut PairTree,
    parent_treeid   : NodeId,
    orgnode_tree    : &Tree < OrgNode >,
    orgnode_treeid  : NodeId,
  ) {
    let orgnode : OrgNode =
      orgnode_tree . get ( orgnode_treeid )
      . unwrap () . value () . clone ();
    let new_treeid : NodeId = {
      let mut parent_mut =
        paired_forest . get_mut ( parent_treeid ) . unwrap ();
      parent_mut
        . append ( NodePair { mskgnode : None,
                              orgnode } )
        . id () };
    let child_treeids : Vec < NodeId > =
      orgnode_tree . get ( orgnode_treeid ) . unwrap ()
      . children () . map ( |c| c . id () ) . collect ();
    for child_treeid in child_treeids {
      add_orgnode_tree_as_child_of_forest_root (
        paired_forest, new_treeid, orgnode_tree, child_treeid ); } }
  let mut result : PairTree = Tree::new ( forest_root_pair () );
  let forest_root_treeid = result . root () . id ();
  // Iterate over tree roots (children of ForestRoot)
  for tree_root in forest . root () . children () {
    add_orgnode_tree_as_child_of_forest_root (
      &mut result, forest_root_treeid, &forest, tree_root . id () ); }
  result }
