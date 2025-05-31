// This uses Tantivy to create an index
// associating titles to filenames,
// potentially many to one.

use tantivy::collector::TopDocs;
use tantivy::schema as schema;
use tantivy::{Index, doc};
use walkdir::WalkDir;
use std::fs;
use std::path::Path;
use std::time::SystemTime;
use serde_yaml::from_str;

use crate::hyperlinks::strip_org_hyperlinks;

pub fn get_or_create_index(
  schema: schema::Schema,
  index_path: &Path
) -> Result<Index, Box<dyn std::error::Error>> {

  if index_path.exists() {
    println!("Attempting to open existing index at {:?}", index_path);
    match Index::open_in_dir(index_path) {
      Ok(index) => {
        println!("Successfully opened existing index");
        Ok(index) },
      Err(e) => {
        println!("Failed to open existing index: {:?}. Recreating...", e);
        { // Remove the corrupted directory and recreate
          fs::remove_dir_all(index_path)?;
          fs::create_dir_all(index_path)?; };
        println!("Creating new index at {:?}", index_path);
        Ok(Index::create_in_dir(index_path, schema)?)
      } } }
  else {
    println!("Creating new index at {:?}", index_path);
    fs::create_dir_all(index_path)?;
    Ok(Index::create_in_dir(index_path, schema)?) } }

pub fn get_modification_time(
  path: &Path)
  -> Result<SystemTime, Box<dyn std::error::Error>> {
  let metadata = fs::metadata(path)?;
  Ok(metadata.modified()?) }

pub fn needs_indexing( // based on modification time
  path: &Path,
  index_mtime: SystemTime
) -> bool {
  if path.to_string_lossy().contains("index.tantivy") {
    return false; } // Skip files in the index directory
  if !path.extension().map_or( false, |ext| ext == "skg" ) {
    return false; } // Skip non-skg files
  match get_modification_time(path) {
    Ok(file_mtime) => file_mtime > index_mtime,
    Err(_) => true // If its modification time is unknown,
                   // assume it needs indexing.
  } }

pub fn extract_skg_title(
  // Gets the title from the file at path,
  // runs strip_org_hyperlinks on it,
  // and returns it.
  path: &Path)
  -> Option<String> {

  if let Ok(file_content) = fs::read_to_string(path) {
    if let Ok(yaml_value) = ( from_str::<serde_yaml::Value>
                              (&file_content) ) {
      if let Some(title_value) = yaml_value.get("title") {
        if let Some(title_str) = title_value.as_str() {
          return Some(
            strip_org_hyperlinks(
              title_str ) ); } } } }
  None }

pub fn delete_documents_with_path(
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  path_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {
  let path_str = path.to_string_lossy().to_string();
  let term = tantivy::Term::from_field_text(
    path_field, &path_str);
  writer.delete_term(term); // Delete anything with this path
  Ok(()) }

pub fn add_document_and_title_to_index(
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  title: &str,
  path_field: schema::Field,
  title_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {
  let path_str = path.to_string_lossy().to_string();
  writer.add_document(doc!(
    path_field => path_str,
    title_field => title.to_string()
  ))?;
  Ok (()) }

pub fn index_title_at_path (
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  title: &String,
  path_field: schema::Field,
  title_field: schema::Field
) -> Result < (),
              Box < dyn std::error::Error > > {
  delete_documents_with_path(
    writer, path, path_field)?;
  add_document_and_title_to_index(
    writer, path, title, path_field, title_field)?;
  Ok (( )) }

pub fn update_index(
  index: &Index,
  path_field: schema::Field,
  title_field: schema::Field,
  data_dir: &str,
  index_path: &Path
) -> Result<usize, Box<dyn std::error::Error>> {
  println!("Updating index with .skg files from {}.",
           data_dir);
  let mut index_writer = index.writer(50_000_000)?;
  let mut indexed_count = 0;
  let index_mtime = get_modification_time(index_path)
    .unwrap_or(SystemTime::UNIX_EPOCH);
  for entry in WalkDir::new(data_dir)
    .into_iter().filter_map(Result::ok) {
      let path = entry.path();
      if !needs_indexing(path, index_mtime) {
        continue; } // skip this file
      if let Some(title) = extract_skg_title(path) {
        index_title_at_path(
          &mut index_writer,
          path,
          &title,
          path_field,
          title_field)?;
        indexed_count += 1;
      } }

  if indexed_count > 0 {
    println!("Indexed {} files. Committing changes...", indexed_count);
    index_writer.commit()?;
  } else {
    println!("No new or modified files found."); }
  Ok (indexed_count) }

pub fn search_index(
  index: &Index,
  title_field: schema::Field,
  query_text: &str
) -> Result< (Vec<(f32, tantivy::DocAddress)>,
              tantivy::Searcher),
              Box<dyn std::error::Error>> {
  println!(
    "\nFinding files with titles matching \"{}\".",
    query_text);
  let reader = index.reader()?;
  let searcher = reader.searcher();
  let query_parser =
    tantivy::query::QueryParser::for_index(
      &index, vec![title_field]);
  let query = query_parser.parse_query(query_text)?;
  let best_matches = searcher.search(
    &query, &TopDocs::with_limit(10))?;
  Ok((best_matches, searcher)) }
