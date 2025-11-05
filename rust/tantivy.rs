// PURPOSE: Creates a Tantivy index from SkgNodes,
// associating each node's primary ID with its processed title and aliases.

// GLOSSARY:
// See the Tantivy section in glossary.md.

use crate::textlinks::replace_each_link_with_its_label;
use crate::types::{ID, SkgNode, SkgConfig, TantivyIndex, SaveInstruction};

use tantivy::{Index, IndexWriter, doc, schema, Term, IndexReader, Searcher, Document};
use tantivy::query::{QueryParser, Query};
use tantivy::collector::TopDocs;
use std::path::Path;
use std::sync::Arc;
use std::error::Error;


/// Returns the top 10 matching Tantivy "Documents"
/// (see glossary, above).
pub fn search_index (
  tantivy_index : &TantivyIndex,
  query_text    : &str
) -> Result <
    ( Vec< ( f32, // relevance score
             tantivy::DocAddress )>, // ID for a tantivy Document. (Not a filepath.)
      Searcher ),
    Box <dyn std::error::Error> > {

  println! (
    "\nFinding files with titles or aliases matching \"{}\".",
    query_text);
  let reader : IndexReader =
    tantivy_index.index.reader () ?;
  let searcher : Searcher =
    reader.searcher();
  let query_parser : QueryParser =
    QueryParser::for_index (
      &tantivy_index.index,
      vec! [ tantivy_index.title_or_alias_field ] );
  let query : Box < dyn Query > =
    query_parser.parse_query ( query_text ) ?;
  let best_matches : Vec < ( f32, tantivy::DocAddress ) > =
    searcher.search (
      &query, &TopDocs::with_limit (10) )?;
  Ok (( best_matches, searcher )) }

/// Build a schema,
/// then run `wipe_fs_then_create_index_there`.
pub fn initialize_tantivy_from_nodes (
  config : & SkgConfig,
  nodes  : & [SkgNode],
) -> TantivyIndex {
  println!("Initializing Tantivy index...");

  // Define the schema.
  let mut schema_builder = schema::Schema::builder();
  let id_field: schema::Field =
    schema_builder.add_text_field(
      "id", schema::STRING | schema::STORED);
  let title_or_alias_field: schema::Field =
    schema_builder.add_text_field(
      "title_or_alias", schema::TEXT | schema::STORED);
  let schema: schema::Schema =
    schema_builder.build();

  let index_path : &Path =
    Path::new ( & config . tantivy_folder );
  let (tantivy_index, indexed_count) : ( TantivyIndex, usize ) =
    wipe_fs_then_create_index_there (
      nodes,
      index_path,
      schema,
      id_field,
      title_or_alias_field )
    . unwrap_or_else(|e| {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit(1);
    } );
  println!(
    "Tantivy index initialized successfully. Indexed {} files.",
    indexed_count);
  tantivy_index }

/// Removes any existing index at given path.
/// creates a new one there,
/// and populates it.
///
/// PITFALL: The index is not the data it indexes.
/// This only deletes the former.
pub fn wipe_fs_then_create_index_there (
  nodes                : &[SkgNode],
  index_path           : &Path,
  schema               : schema::Schema,
  id_field             : schema::Field,
  title_or_alias_field : schema::Field,
) -> Result<(TantivyIndex,
             usize), // number of documents indexed
            Box<dyn Error>> {

  if index_path.exists() {
    std::fs::remove_dir_all (index_path) ?; }
  std::fs::create_dir_all ( index_path )?;
  let index : Index =
    Index::create_in_dir ( index_path, schema )?;
  let tantivy_index : TantivyIndex = TantivyIndex {
    index: Arc::new(index),
    id_field: id_field,
    title_or_alias_field, };
  let indexed_count: usize = // populate it
    empty_then_populate_index (
      nodes, &tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }

/// Creates a fresh index from the provided SkgNodes.
/// Returns the number of documents indexed.
pub fn empty_then_populate_index (
  nodes         : &[SkgNode],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {
  { // Empty the index.
    let mut writer: IndexWriter =
      tantivy_index.index.writer(50_000_000)?;
    writer.delete_all_documents()?;
    writer.commit () ?; }
  // Outside the above block, the writer it created is dropped,
  // which permits this next command to use a different one.
  update_index_with_nodes (
    nodes, tantivy_index ) }

/// Updates the index with the provided SkgNodes.
///   For existing IDs, updates the title.
///   For new IDs, adds new entries.
/// Returns the number of documents processed.
pub fn update_index_with_nodes (
  nodes: &[SkgNode],
  tantivy_index: &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;
  { // Delete those IDs from the index. (They'll come back.)
    for node in nodes {
      if !node.ids.is_empty() {
        let primary_id: &ID = &node.ids[0];
        let term: Term = Term::from_field_text(
          tantivy_index.id_field,
          primary_id.as_str() );
        writer.delete_term(term); } } }
  let processed_count: usize = // Add new associations.
    add_documents_to_writer (
      nodes, &mut writer, tantivy_index)?;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok (processed_count) }

/// Updates the index with the provided SaveInstructions.
/// Deletes IDs from the index for every instruction,
/// but only adds documents for instructions where toDelete is false.
/// Returns the number of documents processed.
pub fn update_index_from_saveinstructions (
  instructions  : &[SaveInstruction],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;
  { // Delete all IDs in the SaveInstructions from the index --
    // be they toDelete or otherwise.
    // (Those that aren't will come back in the next step.)
    for (node, _action) in instructions {
      if !node.ids.is_empty() {
        let primary_id: &ID = &node.ids[0];
        let term: Term = Term::from_field_text(
          tantivy_index.id_field,
          primary_id.as_str() );
        writer.delete_term(term); } } }
  { // Add documents only for non-deleted instructions.
    let mut processed_count: usize = 0;
    for (node, action) in instructions {
      if ! action . toDelete {
        let documents : Vec < Document > =
          create_documents_from_node (
            node, tantivy_index )?;
        for document in documents {
          writer.add_document (document)?;
          processed_count += 1; }} }
    commit_with_status(
      &mut writer, processed_count, "Updated")?;
    Ok (processed_count) }}


/* -------------------- Private helpers -------------------- */

fn add_documents_to_writer (
  nodes         : &[SkgNode],
  writer        : &mut IndexWriter,
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut indexed_count: usize = 0;
  for node in nodes {
    if node.ids.is_empty() {
      return Err ( "SkgNode has no IDs".into () ); }
    let documents: Vec<Document> =
      create_documents_from_node(
        node, tantivy_index )?;
    for document in documents {
      writer.add_document (document)?;
      indexed_count += 1; }}
  Ok (indexed_count) }

fn create_documents_from_node (
  node: &SkgNode,
  tantivy_index: &TantivyIndex,
) -> Result < Vec < Document >,
              Box < dyn Error >> {

  if node.ids.is_empty() {
    return Err("SkgNode has no IDs" . into () ); }
  let primary_id: &ID = &node.ids[0];
  let mut documents_acc: Vec<Document> =
    Vec::new();
  let mut titles_and_aliases: Vec<String> = // what to index
    vec![node.title.clone()];
  if let Some(aliases) = &node.aliases {
    titles_and_aliases . extend(
      aliases.clone () ); }
  for title_or_alias in titles_and_aliases { // index them
    let doc : Document = doc!(
      tantivy_index . id_field =>
        primary_id.as_str (),
      tantivy_index . title_or_alias_field =>
        replace_each_link_with_its_label (
          & title_or_alias ));
    documents_acc.push (doc); }
  Ok ( documents_acc ) }

fn commit_with_status (
  writer: &mut IndexWriter,
  indexed_count: usize,
  operation: &str,
) -> Result<(), Box<dyn Error>> {
  if indexed_count > 0 {
    println!( "{} {} documents. Committing changes...",
              operation, indexed_count );
    writer.commit () ?;
  } else {
    println!("No documents to process found."); }
  Ok (( )) }
