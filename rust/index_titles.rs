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
use crate::types::TantivyIndex;

pub fn search_index(
  tantivy_index: &TantivyIndex,
  query_text: &str
) -> Result< (Vec<(f32, tantivy::DocAddress)>,
              tantivy::Searcher),
              Box<dyn std::error::Error>> {
  println!(
    "\nFinding files with titles matching \"{}\".",
    query_text);
  let reader = tantivy_index.index.reader()?;
  let searcher = reader.searcher();
  let query_parser =
    tantivy::query::QueryParser::for_index(
      &tantivy_index.index,
      vec![tantivy_index.title_field]);
  let query = query_parser.parse_query(query_text)?;
  let best_matches = searcher.search(
    &query, &TopDocs::with_limit(10))?;
  Ok((best_matches, searcher)) }

pub fn create_index(
  tantivy_index: &TantivyIndex,
  data_dir: &str
) -> Result<usize, Box<dyn std::error::Error>> {
  println!("Creating index with all .skg files from {}.",
           data_dir);
  let mut index_writer =
    tantivy_index.index.writer(50_000_000)?;
  populate_index(
    &mut index_writer,
    data_dir,
    tantivy_index,
    |_path| true) // Accept all files
}

pub fn update_index(
  tantivy_index: &TantivyIndex,
  data_dir: &str,
  index_path: &Path
) -> Result<usize, Box<dyn std::error::Error>> {
  println!("Updating index with .skg files from {}.",
           data_dir);
  let mut index_writer =
    tantivy_index.index.writer(50_000_000)?;
  let index_mtime = get_modification_time(index_path)
    .unwrap_or(SystemTime::UNIX_EPOCH);
  populate_index(
    &mut index_writer,
    data_dir,
    tantivy_index,
    |path| needs_indexing(path, index_mtime))
}

fn populate_index<F>(
  index_writer: &mut tantivy::IndexWriter,
  data_dir: &str,
  tantivy_index: &TantivyIndex,
  should_index: F
) -> Result<usize,
            Box<dyn std::error::Error>>
where
  F: Fn(&Path) -> bool {

  let mut indexed_count = 0;
  for entry in WalkDir::new(data_dir)
    .into_iter().filter_map(Result::ok) {
      let path = entry.path();
      if ( not_a_skg_file_path(path) ||
           !should_index(path) ) {
        continue; }
      if let Some(title) = skg_title_from_file(path) {
        index_title_at_path_after_scrubbing_path(
          index_writer,
          path,
          &title,
          tantivy_index)?;
        indexed_count += 1; } }
  if indexed_count > 0 {
    println!("Indexed {} files. Committing changes...",
             indexed_count);
    index_writer.commit()?; }
  else {
    println!("No files to index found."); }
  Ok (indexed_count) }

pub fn get_extant_index_or_create_empty_one(
  // If it creates an index, the index is empty.
  // But if it fetches an index, the index might not be.
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
        Ok ( Index::create_in_dir (
          index_path, schema )? ) } } }
  else {
    println!("Creating new index at {:?}", index_path);
    fs::create_dir_all(index_path)?;
    Ok(Index::create_in_dir(index_path, schema)?) } }

pub fn skg_title_from_file(
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

pub fn index_title_at_path_after_scrubbing_path (
  // Add one (title,path) pair to the Tantivy index,
  // obliterating preexisting content with the same path.
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  title: &String,
  tantivy_index: &TantivyIndex
) -> Result < (),
              Box < dyn std::error::Error > > {
  delete_documents_with_path_from_index(
    writer, path, tantivy_index.path_field)?;
  add_document_and_title_to_index(
    writer, path, title, tantivy_index)?;
  Ok (( )) }

pub fn delete_documents_with_path_from_index(
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  path_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {
  let path_str = path.to_string_lossy().to_string();
  let term = tantivy::Term::from_field_text(
    path_field, &path_str);
  { // Delete anything with this path
    writer.delete_term(term); }
  Ok(()) }

pub fn add_document_and_title_to_index(
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  title: &str,
  tantivy_index: &TantivyIndex
) -> Result<(), Box<dyn std::error::Error>> {
  let path_str = path.to_string_lossy().to_string();
  writer.add_document(doc!(
    // PITFALL: These `=>` symbols are a Tantivy macro.
    tantivy_index.path_field => path_str,
    tantivy_index.title_field => title.to_string()
  ))?;
  Ok (()) }

pub fn needs_indexing( // based on modification time
  path: &Path,
  index_mtime: SystemTime
) -> bool {
  match get_modification_time(path) {
    Ok(file_mtime) => file_mtime > index_mtime,
    Err(_) => true // If its modification time is unknown,
                   // assume it needs indexing.
  } }

pub fn not_a_skg_file_path (
  path: &Path
) -> bool {

  !path.extension().map_or(
    false, |ext| ext == "skg" ) ||
  ( path.to_string_lossy()
    . contains("index.tantivy") ) }

pub fn get_modification_time(
  path: &Path
) -> Result<SystemTime,
            Box<dyn std::error::Error>> {

  let metadata = fs::metadata(path)?;
  Ok(metadata.modified()?) }
