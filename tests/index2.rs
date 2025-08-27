use std::fs;
use std::path::Path;
use tantivy::schema as schema;

use skg::index2::{ create_fresh_index };
use skg::index_titles::{ search_index };
use skg::file_io::read_skg_files;
use skg::types::TantivyIndex;


#[test]
fn test_index2 (
) -> Result<(), Box<dyn std::error::Error>> {
  let index_dir: &str =
    "tests/index2/generated";
  if Path::new(index_dir).exists() {
    // Remove any existing test artifacts
    fs::remove_dir_all(index_dir)?; }
  fs::create_dir_all(index_dir)?;

  // Define the schema
  let mut schema_builder: schema::SchemaBuilder =
    schema::Schema::builder();
  let path_field: schema::Field =
    schema_builder.add_text_field(
      "path", schema::STRING | schema::STORED);
  let title_field: schema::Field =
    schema_builder.add_text_field(
      "title", schema::TEXT | schema::STORED);
  let schema: schema::Schema =
    schema_builder.build();

  let index_path: &Path =
    Path::new("tests/index2/generated/index.tantivy");
  let filenodes: Vec<skg::types::FileNode> =
    read_skg_files("tests/index_titles/fixtures")?;
  let (tantivy_index, indexed_count): (TantivyIndex, usize) =
    create_fresh_index (
      &filenodes,
      index_path,
      schema,
      path_field,
      title_field )?;
  assert!( indexed_count > 0,
           "Expected to index at least one title" );

  let (best_matches, searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(
      &tantivy_index,
      "test second")?; // the search query
  assert!(!best_matches.is_empty(),
          "Expected to find at least one match");

  print_search_results (
    best_matches.clone(), &searcher, path_field, title_field)?;
  if !best_matches.is_empty() {
    let (_top_score, top_doc_address): (f32, tantivy::DocAddress) =
      best_matches[0];
    let top_doc: tantivy::Document =
      searcher.doc(top_doc_address)?;
    let top_path: &str =
      top_doc.get_first(path_field)
      .unwrap().as_text().unwrap();
    assert!(
      top_path == "5",
      "Expected primary ID '5' to have highest score, but got: {}",
      top_path ); }
  Ok (( )) }

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
      let retrieved_doc: tantivy::Document =
        searcher.doc(doc_address)?;
      let path_value: &str =
        retrieved_doc.get_first(path_field)
        .unwrap().as_text().unwrap();
      let title_value: &str =
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
