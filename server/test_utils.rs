use crate::dbs::filesystem::multiple_nodes::read_all_skg_files_from_sources;
use crate::dbs::init::create_empty_tantivy_index;
use crate::dbs::neo4j::nodes::create_all_nodes;
use crate::dbs::neo4j::relationships::create_all_relationships;
use crate::dbs::neo4j::schema::apply_schema;
use crate::dbs::neo4j::util::delete_database;
use crate::dbs::tantivy::search_index;
use crate::from_text::buffer_to_viewnodes::uninterpreted::{headline_to_triple, HeadlineInfo};
use crate::serve::parse_metadata_sexp::ViewnodeMetadata;
use crate::types::misc::{SkgConfig, SkgfileSource, ID, TantivyIndex, SourceName};
use crate::types::save::{DefineNode, SaveNode};
use crate::types::skgnode::SkgNode;
use crate::types::unchecked_viewnode::{ UncheckedViewNode, UncheckedViewNodeKind };

use ego_tree::{Tree, NodeRef};
use tokio::runtime::Runtime;
use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::fs;
use std::future::Future;
use std::path::{Path, PathBuf};
use std::pin::Pin;
use neo4rs::Graph;
use std::sync::Arc;
use tantivy::{DocAddress, Searcher};


/// Run tests with automatic database setup and cleanup.
///
/// This helper function encapsulates the common pattern of:
/// 1. Copying fixtures to a temp directory (so saves don't corrupt originals)
/// 2. Setting up a test database and Tantivy index
/// 3. Running test functions
/// 4. Cleaning up the database, index, and temp fixtures
///
/// The test_fn closure receives references to SkgConfig, Graph, and TantivyIndex
/// and can run multiple test functions sequentially.
pub fn run_with_test_db<F>(
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
  test_fn: F,
) -> Result<(), Box<dyn Error>>
where
  F: for<'a>
  FnOnce(&'a SkgConfig, &'a Graph, &'a TantivyIndex)
         -> Pin<Box<dyn Future<Output = Result
                               <(), Box<dyn Error>>> + 'a>>,
{
  // Copy fixtures to temp so saves don't corrupt originals
  let temp_fixtures : PathBuf = PathBuf::from(format!("/tmp/{}-fixtures", db_name));
  if temp_fixtures.exists() {
    fs::remove_dir_all(&temp_fixtures)?;
  }
  copy_dir_all(
    &PathBuf::from(fixtures_folder),
    &temp_fixtures)?;

  let rt : Runtime = Runtime::new()?;
  let result : Result<(), Box<dyn Error>> = rt.block_on(async {
    let (config, graph, tantivy): (SkgConfig, Graph, TantivyIndex) =
      setup_test_tantivy_and_neo4j_dbs(
        db_name,
        temp_fixtures.to_str().unwrap(),
        tantivy_folder ). await?;
    let result : Result<(), Box<dyn Error>> = test_fn(&config, &graph, &tantivy).await;
    cleanup_test_tantivy_and_neo4j_dbs(
      &graph,
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
    let entry : fs::DirEntry = entry?;
    let ty : fs::FileType = entry.file_type()?;
    let src_path : PathBuf = entry.path();
    let dst_path : PathBuf = dst.join(entry.file_name());
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
  graph: &Graph
) -> Result<(), Box<dyn Error>> {
  let nodes: Vec<SkgNode> = {
    let mut sources: HashMap<SourceName, SkgfileSource> = HashMap::new();
    sources.insert(
      SourceName::from("main"),
      SkgfileSource {
        nickname: SourceName::from("main"),
        path: PathBuf::from(data_folder),
        user_owns_it: true, } );
    read_all_skg_files_from_sources(
      &SkgConfig::dummyFromSources( sources ))? };
  delete_database ( graph ). await ?;
  apply_schema ( graph ). await?;
  create_all_nodes (
    graph, &nodes ). await ?;
  create_all_relationships (
    graph, &nodes ). await ?;
  Ok (( )) }

/* PURPOSE: Set up test dbs (Tantivy and Neo4j)
with fixtures from the given folder.
The program calling this should call `cleanup_test_tantivy_and_neo4j_dbs`
after the test completes to remove the database.
.
PITFALL: This sets delete_on_quit=false
because tests tear down the db themselves.
Unit tests don't even run the Rust-Emacs server (integration tests do),
so while there's something to delete, there's no server to quit. */
pub async fn setup_test_tantivy_and_neo4j_dbs (
  db_name: &str,
  fixtures_folder: &str,
  tantivy_folder: &str,
) -> Result<(SkgConfig, Graph, TantivyIndex), Box<dyn Error>> {
  let config: SkgConfig = {
    let mut sources : HashMap<SourceName, SkgfileSource> = HashMap::new();
    sources.insert (
      SourceName::from("main"),
      SkgfileSource {
        nickname     : SourceName::from("main"),
        path         : PathBuf::from ( fixtures_folder ),
        user_owns_it : true, });
    SkgConfig::fromSourcesAndDbName (
      sources, db_name, tantivy_folder ) };
  let graph: Graph = Graph::new(
    &config.neo4j_uri,
    &config.neo4j_user,
    &config.neo4j_password,
  ). await ?;
  populate_test_db_from_fixtures(
    fixtures_folder,
    &graph
  ). await?;
  let tantivy_index: TantivyIndex =
    create_empty_tantivy_index(&config.tantivy_folder)?;
  Ok ((config, graph, tantivy_index)) }

/// Clean up test database and tantivy index after a test completes.
///
/// This clears:
/// - All Neo4j data (MATCH (n) DETACH DELETE n)
/// - The Tantivy index directory (if it exists)
///
/// Does NOT delete the .skg fixture files.
pub async fn cleanup_test_tantivy_and_neo4j_dbs(
  graph: &Graph,
  tantivy_folder: Option<&Path>,
) -> Result<(), Box<dyn Error>> {
  // Clear Neo4j data
  delete_database( graph ).await?;

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
      let stripped_metadata1: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct(metadata1);
      let stripped_metadata2: Option<ViewnodeMetadata> =
        strip_id_from_metadata_struct(metadata2);
      (level1, stripped_metadata1, title1) ==
        (level2, stripped_metadata2, title2) },
    (Err(e1), Err(e2)) if (e1 == "__NOT_A_HEADLINE__" &&
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
  forest1: &Tree<UncheckedViewNode>,
  forest2: &Tree<UncheckedViewNode>
) -> bool {
  let root1 : Vec < NodeRef < '_, UncheckedViewNode >> =
    forest1.root().children().collect();
  let root2 : Vec < NodeRef < '_, UncheckedViewNode >> =
    forest2.root().children().collect();
  if root1.len() != root2.len() {
    return false; }
  for (tree1, tree2) in root1.iter().zip(root2.iter()) {
    if !compare_two_viewnode_branches_recursively_modulo_id(
      *tree1, *tree2 )
    { return false; }}
  true }

/// Compare two UncheckedViewNode subtrees, ignoring ID values.
fn compare_two_viewnode_branches_recursively_modulo_id (
  node1: NodeRef<UncheckedViewNode>,
  node2: NodeRef<UncheckedViewNode>
) -> bool {
  let n1 : &UncheckedViewNode = node1.value();
  let n2 : &UncheckedViewNode = node2.value();
  match (&n1.kind, &n2.kind) {
    ( UncheckedViewNodeKind::True(_),
      UncheckedViewNodeKind::True(t2)) =>
    { // Copy the ID from one to the other, then compare.
      let mut n1_copy : UncheckedViewNode =
        n1.clone();
      if let UncheckedViewNodeKind::True(t) = &mut n1_copy.kind {
        t.id_opt = t2.id_opt.clone(); }
      if n1_copy != *n2 { return false; }}
    ( UncheckedViewNodeKind::Scaff(_),
      UncheckedViewNodeKind::Scaff(_)) =>
    { if n1 != n2 { return false; }}
    _ => return false, // mismatched kinds
  }
  { // Recurse on children
    let children1 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node1.children().collect();
    let children2 : Vec < NodeRef < '_, UncheckedViewNode >> =
      node2.children().collect();
    ( children1.len() == children2.len() &&
      children1 . iter() . zip(children2.iter())
      . all (|(c1, c2)|
             compare_two_viewnode_branches_recursively_modulo_id(
               *c1, *c2)) ) }}

/// Remove ID from metadata struct while preserving other metadata
fn strip_id_from_metadata_struct(
  metadata: Option<ViewnodeMetadata>
) -> Option<ViewnodeMetadata> {
  metadata.map(|mut meta| {
    meta.id = None;
    meta
  } ) }

/// Query all primary node IDs from Neo4j.
/// Returns a HashSet of all IDs belonging to primary nodes (not IdAliases).
pub async fn all_pids_from_neo4j(
  graph: &Graph,
) -> Result<HashSet<ID>, Box<dyn Error>> {
  let mut result_stream =
    graph . execute (
      neo4rs::query ( "MATCH (n:Node) RETURN n.id AS node_id" )
    ) . await ?;
  let mut node_ids: HashSet<ID> = HashSet::new();
  while let Some ( row ) = result_stream . next () . await ? {
    let node_id_str: String = row . get ( "node_id" ) ?;
    node_ids.insert(ID(node_id_str)); }
  Ok (node_ids) }

/// Query all extra_ids for a given primary node ID from Neo4j.
/// Returns a Vec of all IdAlias IDs associated with the node.
pub async fn extra_ids_from_pid(
  graph: &Graph,
  skgid: &ID,
) -> Result<Vec<ID>, Box<dyn Error>> {
  let mut result_stream =
    graph . execute (
      neo4rs::query (
        "MATCH (a:IdAlias {primary_id: $pid}) \
         RETURN a.id AS alias_id" )
      . param ( "pid", skgid . as_str () )
    ) . await ?;
  let mut extra_ids : Vec<ID> = Vec::new();
  while let Some ( row ) = result_stream . next () . await ? {
    let alias_id : String = row . get ( "alias_id" ) ?;
    extra_ids.push( ID(alias_id) ); }
  Ok(extra_ids) }

/// Check if a specific ID exists in Tantivy search results.
/// Searches for the given query and checks if any result has the exact ID.
pub fn tantivy_contains_id(
  tantivy_index: &TantivyIndex,
  query: &str,
  expected_id: &str,
) -> Result<bool, Box<dyn Error>> {
  let (matches, searcher)
    : (Vec<(f32, DocAddress)>, Searcher)
    = search_index(tantivy_index, query)?;
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

/// Example SkgNode for use in tests.
pub fn skgnode_example () -> SkgNode {
  SkgNode {
    title: "This text gets indexed.".to_string(),
    aliases: None,
    source: SourceName::from("main"),
    ids: vec![ ID::new("example") ],
    body: Some( r#"This one string could span pages.
It better be okay with newlines."# . to_string() ),
    contains: Some(vec![ ID::new("1"),
                         ID::new("2"),
                         ID::new("3")]),
    subscribes_to: Some(vec![ID::new("11"),
                             ID::new("12"),
                             ID::new("13")]),
    hides_from_its_subscriptions: None,
    overrides_view_of: None, }}

/// Extract SkgNode from Save variant; panics on Delete.
pub fn extract_skgnode_if_save_else_error(
  instr: &DefineNode
) -> &SkgNode {
  match instr {
    DefineNode::Save(SaveNode(node)) => node,
    DefineNode::Delete(_) => panic!("Expected Save, got Delete") }}
