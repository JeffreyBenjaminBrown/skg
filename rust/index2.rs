// PURPOSE: Creates a Tantivy index from FileNodes,
// associating each node's primary ID with its processed title.

use crate::hyperlinks::replace_each_link_with_its_label;
use crate::types::{FileNode, ID, SkgConfig, TantivyIndex};

use tantivy::{Index, IndexWriter, doc, schema};
use std::path::Path;
use std::sync::Arc;
use std::error::Error;


pub fn initialize_tantivy_from_filenodes (
  config : & SkgConfig,
  filenodes: &[FileNode],
) -> TantivyIndex {
  // Build a schema and create a fresh index from the provided FileNodes.

  println!("Initializing Tantivy index...");

  // Define the schema.
  let mut schema_builder = schema::Schema::builder();
  let path_field: schema::Field =
    schema_builder.add_text_field(
      "path", schema::STRING | schema::STORED);
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
      path_field,
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
    tantivy_index.index.writer ( 50_000_000 )?;
  writer.delete_all_documents () ?;
  let mut indexed_count: usize = 0;
  for filenode in filenodes {
    if filenode.ids.is_empty() {
      // Skip nodes without IDs.
      // TODO: Should this throw an error?
      continue; }
    let primary_id: &ID =
      &filenode.ids [0];
    let processed_title: String =
      replace_each_link_with_its_label (
        &filenode.title );
    writer.add_document(doc!(
      tantivy_index.path_field  => primary_id.as_str(),
      tantivy_index.title_field => processed_title )) ?;
    indexed_count += 1; }
  if indexed_count > 0 {
    println!("Indexed {} nodes. Committing changes...",
             indexed_count);
    writer.commit()?;
  } else {
    println!("No nodes to index found."); }
  Ok (indexed_count) }

/// Creates a new index at the given path,
/// then populates it.
/// Removes any existing index first.
pub fn create_fresh_index (
  filenodes   : &[FileNode],
  index_path  : &Path,
  schema      : tantivy::schema::Schema,
  path_field  : tantivy::schema::Field,
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
    path_field,
    title_field, };
  let indexed_count: usize = // populate it
    create_index_from_filenodes (
      filenodes, &tantivy_index )?;
  Ok (( tantivy_index, indexed_count )) }
