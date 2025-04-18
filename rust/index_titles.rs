// This uses Tantivy to create an index
// associating titles to filenames,
// potentially many to one.

use tantivy::collector::TopDocs;
use tantivy::schema as schema;
use tantivy::{Index, doc};
use walkdir::WalkDir;
use regex::Regex;
use std::fs;
use std::path::Path;
use std::time::SystemTime;
use serde_yaml::from_str;

pub fn get_or_create_index(
  schema: schema::Schema,
  index_path: &str
) -> Result<Index, Box<dyn std::error::Error>> {
  let path = Path::new(index_path);
  if path.exists() {
    println!("Opening existing index at {:?}", path);
    Ok(Index::open_in_dir(path)?)
  } else {
    println!("Creating new index at {:?}", path);
    fs::create_dir_all(path)?;
    Ok(Index::create_in_dir(path, schema)?) } }

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

pub fn extract_skg_titles(path: &Path) -> Vec<String> {
  let mut titles = Vec::new();
  if let Ok(content) = fs::read_to_string(path) {
    if let Ok(yaml_value) =
      from_str::<serde_yaml::Value>(&content) {
      if let Some(titles_array) =
        yaml_value.get("titles").and_then(|t| t.as_sequence()) {
          for title_value in titles_array {
            if let Some(title_str) = title_value.as_str() {
              titles.push(strip_org_links(title_str)); } } } } }
  titles }

// Titles can include links,
// but can be searched for as if each link
// was equal to its label.
// That is, the ID and brackets of a link in a title are not indexed.
pub fn strip_org_links(text: &str) -> String {
  let link_re = Regex::new(
    r"\[\[.*?\]\[(.*?)\]\]").unwrap();
  let mut result = String::from(text);
  let mut in_offset = 0; // offset in the input string
  for cap in link_re.captures_iter(text) {
    let whole_match = cap.get(0).unwrap();
    let link_label = cap.get(1).unwrap();

    // Define the range to modify
    let start_pos = whole_match.start() - in_offset;
    let end_pos = whole_match.end() - in_offset;

    result.replace_range(
      start_pos .. end_pos,
      link_label.as_str());
    in_offset += whole_match.len() - link_label.len(); }
  result }

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

pub fn index_titles_at_path(
  writer: &mut tantivy::IndexWriter,
  path: &Path,
  titles: &[String],
  path_field: schema::Field,
  title_field: schema::Field
) -> Result<usize, Box<dyn std::error::Error>> {
  if titles.is_empty() {
    return Ok(0); }
  delete_documents_with_path(writer, path, path_field)?;
  let mut count = 0;
  for title in titles {
    add_document_and_title_to_index(
      writer, path, title, path_field, title_field)?;
    count += 1; }
  Ok(count) }

pub fn update_index(
  index: &Index,
  path_field: schema::Field,
  title_field: schema::Field,
  data_dir: &str,
  index_path: &Path
) -> Result<usize, Box<dyn std::error::Error>> {
  println!("Updating index with .skg files in the {} directory...",
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
      let titles = extract_skg_titles(path);
      if !titles.is_empty() {
        println!( "Indexing: {} with {} titles",
                   path.display(),
                   titles.len() );
        for title in &titles {
          println!("  - Title: {}", title); }
        let titles_indexed = index_titles_at_path(
          &mut index_writer,
          path,
          &titles,
          path_field,
          title_field)?;
        indexed_count += titles_indexed;
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

pub fn print_search_results(
  best_matches: Vec<(f32, tantivy::DocAddress)>,
  searcher: &tantivy::Searcher,
  path_field: schema::Field,
  title_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {
  if best_matches.is_empty() {
    println!("No matches found.");
  } else {
    let mut path_to_results: std::collections::HashMap
      <String, Vec<(f32, String)>> =
      std::collections::HashMap::new();
    for (score, doc_address) in best_matches {
      let retrieved_doc = searcher.doc(doc_address)?;
      let path_value =
        retrieved_doc.get_first(path_field)
        .unwrap().as_text().unwrap();
      let title_value =
        retrieved_doc.get_first(title_field)
        .unwrap().as_text().unwrap();
      path_to_results
        .entry(path_value.to_string())
        .or_insert_with(Vec::new)
        .push((score, title_value.to_string())); }

    for (path, titles) in path_to_results {
      println!("File: {}", path);
      for (score, title) in titles {
        println!("  - Score: {:.2} | Title: {}",
                 score, title); }
      println!(); } }
  Ok(()) }
