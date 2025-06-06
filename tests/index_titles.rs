use std::fs;
use std::path::Path;
use std::sync::Arc;
use tantivy::schema as schema;

use skg::index_titles::{
  get_extant_index_or_create_empty_one, create_index, search_index, };
use skg::types::TantivyIndex;

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

#[test]
fn test_index (
) -> Result<(), Box<dyn std::error::Error>> {
  let index_dir = "tests/index_titles/generated";
  if Path::new(index_dir).exists() {
    // Remove any existing test artifacts
    fs::remove_dir_all(index_dir)?;
  }
  fs::create_dir_all(index_dir)?;

  // Define the schema
  let mut schema_builder = schema::Schema::builder();
  let path_field = schema_builder.add_text_field(
    "path", schema::STRING | schema::STORED);
  let title_field = schema_builder.add_text_field(
    "title", schema::TEXT | schema::STORED);
  let schema = schema_builder.build();

  let index_path = Path::new(
    "tests/index_titles/generated/index.tantivy");
  let index = get_extant_index_or_create_empty_one(
    schema.clone(),
    index_path )?;
  let tantivy_index = TantivyIndex {
    index: Arc::new(index),
    path_field,
    title_field, };
  let indexed_count = create_index(
    &tantivy_index,
    "tests/index_titles/fixtures" )?;

  assert!(indexed_count > 0,
          "Expected to index at least one title");

  let (best_matches, searcher) = search_index(
    &tantivy_index,
    "test second")?; // the search query

  assert!(!best_matches.is_empty(),
          "Expected to find at least one match");

  print_search_results (
    best_matches.clone(), &searcher, path_field, title_field)?;

  if !best_matches.is_empty() {
    let (_top_score, top_doc_address) = best_matches[0];
    let top_doc = searcher.doc(top_doc_address)?;
    let top_path = top_doc.get_first(path_field)
      .unwrap().as_text().unwrap();
    assert!(
      top_path.contains("5.skg"),
      "Expected 5.skg to have highest score, but got: {}",
      top_path ); }
  Ok(()) }
