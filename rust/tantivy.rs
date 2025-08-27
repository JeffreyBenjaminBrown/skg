// PURPOSE: Creates a Tantivy index from FileNodes,
// associating each node's primary ID with its processed title.

use crate::hyperlinks::replace_each_link_with_its_label;
use crate::types::{FileNode, ID, SkgConfig, TantivyIndex};

use tantivy::{Index, IndexWriter, doc, schema, Term};
use tantivy::collector::TopDocs;
use std::path::Path;
use std::sync::Arc;
use std::error::Error;


/// Returns the top 10 matching (tantivy) "Documents".
pub fn search_index (
  tantivy_index : &TantivyIndex,
  query_text    : &str
) -> Result <
    ( Vec< ( f32, // relevance score
             tantivy::DocAddress )>, // ID for a tantivy Document. (Not a filepath.)
      tantivy::Searcher ),
    Box <dyn std::error::Error> > {

  println! (
    "\nFinding files with titles matching \"{}\".",
    query_text);
  let reader = tantivy_index.index.reader () ?;
  let searcher = reader.searcher();
  let query_parser : tantivy::query::QueryParser =
    tantivy::query::QueryParser::for_index (
      &tantivy_index.index,
      vec! [ tantivy_index.title_field ] );
  let query = query_parser.parse_query ( query_text ) ?;
  let best_matches = searcher.search (
    &query, &TopDocs::with_limit (10) )?;
  Ok (( best_matches, searcher )) }

pub fn initialize_tantivy_from_filenodes (
  config : & SkgConfig,
  filenodes: &[FileNode],
) -> TantivyIndex {
  // Build a schema and create a fresh index from the provided FileNodes.

  println!("Initializing Tantivy index...");

  // Define the schema.
  let mut schema_builder = schema::Schema::builder();
  let id_field: schema::Field =
    schema_builder.add_text_field(
      "id", schema::STRING | schema::STORED);
  let title_field: schema::Field =
    schema_builder.add_text_field(
      "title", schema::TEXT | schema::STORED);
  let schema: schema::Schema =
    schema_builder.build();

  let index_path: &Path =
    Path::new ( & config . tantivy_folder );

  let (tantivy_index, indexed_count): (TantivyIndex, usize) =
    create_fresh_index (
      filenodes,
      index_path,
      schema,
      id_field,
      title_field )
    . unwrap_or_else(|e| {
      eprintln!("Failed to create Tantivy index: {}", e);
      std::process::exit(1);
    } );

  println!(
    "Tantivy index initialized successfully. Indexed {} files.",
    indexed_count);

  tantivy_index }

/// Creates a fresh index from the provided FileNodes.
/// Returns the number of documents indexed.
pub fn create_index_from_filenodes (
  filenodes     : &[FileNode],
  tantivy_index : &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut writer: IndexWriter =
    tantivy_index.index.writer (50_000_000) ?;
  writer.delete_all_documents () ?;
  let indexed_count: usize =
    add_documents_to_writer (
      filenodes, &mut writer, tantivy_index )?;
  commit_with_status (
    &mut writer, indexed_count, "Indexed" )?;
  Ok (indexed_count) }

/// Updates the index with the provided FileNodes.
///   For existing IDs, updates the title.
///   For new IDs, adds new entries.
/// Returns the number of documents processed.
pub fn update_index_with_filenodes (
  filenodes: &[FileNode],
  tantivy_index: &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {
  let mut writer: IndexWriter =
    tantivy_index.index.writer(50_000_000)?;

  { // Delete those IDs from the index.
    for filenode in filenodes {
      if !filenode.ids.is_empty() {
        let primary_id: &ID = &filenode.ids[0];
        let term: Term = Term::from_field_text(
          tantivy_index.id_field,
          primary_id.as_str() );
        writer.delete_term(term); } } }
  let processed_count: usize = // Add new associations.
    add_documents_to_writer (
      filenodes, &mut writer, tantivy_index)?;
  commit_with_status(
    &mut writer, processed_count, "Updated")?;
  Ok(processed_count) }

/// Creates a new index at the given path,
/// then populates it.
/// Removes any existing index first.
pub fn create_fresh_index (
  filenodes   : &[FileNode],
  index_path  : &Path,
  schema      : tantivy::schema::Schema,
  id_field    : tantivy::schema::Field,
  title_field : tantivy::schema::Field,
) -> Result<(TantivyIndex,
             usize), // number of documents indexed
            Box<dyn Error>> {
  if index_path.exists() {
    std::fs::remove_dir_all(index_path)?; }
  std::fs::create_dir_all ( index_path )?;
  let index: Index =
    Index::create_in_dir ( index_path, schema )?;
  let tantivy_index = TantivyIndex {
    index: Arc::new(index),
    id_field: id_field,
    title_field, };
  let indexed_count: usize = // populate it
    create_index_from_filenodes (
      filenodes, &tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }

/// Helper: Creates a document from a FileNode
fn create_document_from_filenode(
  filenode: &FileNode,
  tantivy_index: &TantivyIndex,
) -> Result<(String, tantivy::Document), Box<dyn Error>> {

  if filenode.ids.is_empty() {
    return Err("FileNode has no IDs".into());
  }

  let primary_id: &ID =
    &filenode.ids[0];
  let processed_title: String =
    replace_each_link_with_its_label(&filenode.title);

  let document: tantivy::Document = doc!(
    tantivy_index.id_field  => primary_id.as_str(),
    tantivy_index.title_field => processed_title
  );

  Ok((primary_id.as_str().to_string(), document))
}

/// Helper: Adds multiple documents to a writer
fn add_documents_to_writer(
  filenodes: &[FileNode],
  writer: &mut IndexWriter,
  tantivy_index: &TantivyIndex,
) -> Result<usize, Box<dyn Error>> {

  let mut indexed_count: usize = 0;

  for filenode in filenodes {
    if filenode.ids.is_empty() {
      // Skip nodes without IDs.
      continue;
    }

    let (_id, document): (String, tantivy::Document) =
      create_document_from_filenode(filenode, tantivy_index)?;

    writer.add_document(document)?;
    indexed_count += 1;
  }

  Ok(indexed_count)
}

/// Helper: Commits changes and prints status
fn commit_with_status(
  writer: &mut IndexWriter,
  indexed_count: usize,
  operation: &str,
) -> Result<(), Box<dyn Error>> {

  if indexed_count > 0 {
    println!("{} {} documents. Committing changes...",
             operation, indexed_count);
    writer.commit()?;
  } else {
    println!("No documents to process found.");
  }

  Ok(())
}
