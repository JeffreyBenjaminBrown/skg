use std::fs;
use std::path::Path;
use tantivy::schema as schema;
use skg::file_io::read_skg_files;
use skg::tantivy::{
  create_fresh_index,
  search_index,
  update_index_with_filenodes};
use skg::types::{TantivyIndex, FileNode, ID};

#[test]
fn test_tantivy (
) -> Result<(), Box<dyn std::error::Error>> {
  let index_dir: &str =
    "tests/tantivy/generated";
  if Path::new(index_dir).exists() {
    // Remove any existing test artifacts
    fs::remove_dir_all(index_dir)?; }
  fs::create_dir_all(index_dir)?;

  // Define the schema
  let mut schema_builder: schema::SchemaBuilder =
    schema::Schema::builder();
  let id_field: schema::Field =
    schema_builder.add_text_field(
      "id", schema::STRING | schema::STORED);
  let title_field: schema::Field =
    schema_builder.add_text_field(
      "title", schema::TEXT | schema::STORED);
  let schema: schema::Schema =
    schema_builder.build();

  let index_path: &Path =
    Path::new("tests/tantivy/generated/index.tantivy");
  let filenodes: Vec<skg::types::FileNode> =
    read_skg_files("tests/tantivy/fixtures")?;

  let (tantivy_index, indexed_count): (TantivyIndex, usize) =
    create_fresh_index (
      &filenodes,
      index_path,
      schema,
      id_field,
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
    best_matches.clone(), &searcher, id_field, title_field)?;

  if !best_matches.is_empty() {
    let (_top_score, top_doc_address): (f32, tantivy::DocAddress) =
      best_matches[0];
    let top_doc: tantivy::Document =
      searcher.doc(top_doc_address)?;
    let top_id: &str =
      top_doc.get_first(id_field)
      .unwrap().as_text().unwrap();
    assert!(
      top_id == "5",
      "Expected primary ID '5' to have highest score, but got: {}",
      top_id ); }

  // Test the new update functionality
  println!("\n--- Testing Update Functionality ---");

  // First search for "This is one big tuna." - should find node 1 as top result
  let (initial_matches, initial_searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(&tantivy_index, "This is one big tuna.")?;

  assert!(!initial_matches.is_empty(),
         "Expected to find at least one match for 'This is one big tuna.'");

  let (_initial_score, initial_top_address): (f32, tantivy::DocAddress) =
    initial_matches[0];
  let initial_top_doc: tantivy::Document =
    initial_searcher.doc(initial_top_address)?;
  let initial_top_id: &str =
    initial_top_doc.get_first(id_field)
    .unwrap().as_text().unwrap();

  assert_eq!(initial_top_id, "1",
            "Expected node 1 to be top result initially, but got: {}",
            initial_top_id);
  println!("✓ Initial search correctly found node 1 as top result");

  // Create a new FileNode with ID 6 and title "This is one big tuna."
  let new_filenode: FileNode = FileNode {
    title: "This is one big tuna.".to_string(),
    ids: vec![ID::new("6")],
    body: None,
    contains: vec![],
    subscribes_to: vec![],
    hides_from_its_subscriptions: vec![],
    overrides_view_of: vec![],
  };

  // Update the index with the new node
  let update_count: usize =
    update_index_with_filenodes(&[new_filenode], &tantivy_index)?;
  assert_eq!(update_count, 1,
            "Expected to update exactly 1 document, but updated: {}",
            update_count);
  println!("✓ Successfully updated index with new node 6");

  // Search again - now node 6 should be first, node 1 should be second
  let (final_matches, final_searcher)
    : (Vec<(f32, tantivy::DocAddress)>, tantivy::Searcher)
    = search_index(&tantivy_index, "This is one big tuna.")?;

  assert!(final_matches.len() >= 2,
         "Expected at least 2 matches after update, but got: {}",
         final_matches.len());

  // Check first result is node 6
  let (_final_score1, final_address1): (f32, tantivy::DocAddress) =
    final_matches[0];
  let final_doc1: tantivy::Document =
    final_searcher.doc(final_address1)?;
  let final_id1: &str =
    final_doc1.get_first(id_field)
    .unwrap().as_text().unwrap();

  assert_eq!(final_id1, "6",
            "Expected node 6 to be first result after update, but got: {}",
            final_id1);
  println!("✓ After update, node 6 is correctly the top result");

  // Check second result is node 1
  let (_final_score2, final_address2): (f32, tantivy::DocAddress) =
    final_matches[1];
  let final_doc2: tantivy::Document =
    final_searcher.doc(final_address2)?;
  let final_id2: &str =
    final_doc2.get_first(id_field)
    .unwrap().as_text().unwrap();

  assert_eq!(final_id2, "1",
            "Expected node 1 to be second result after update, but got: {}",
            final_id2);
  println!("✓ After update, node 1 is correctly the second result");

  println!("All update functionality tests passed!");
  Ok (( )) }

pub fn print_search_results(
  best_matches: Vec<(f32, tantivy::DocAddress)>,
  searcher: &tantivy::Searcher,
  id_field: schema::Field,
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
        retrieved_doc.get_first(id_field)
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
