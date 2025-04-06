use std::fs;
use std::path::Path;
use std::time::SystemTime;
use tantivy::schema as schema;

use skg::index_titles::{
    get_or_create_index, update_index, search_index,
    print_search_results};

#[test]
fn test_index
    () -> Result<(), Box<dyn std::error::Error>>
{ fs::create_dir_all("tests/index/generated")?;

  // Define the schema
  let mut schema_builder = schema::Schema::builder();
  let path_field = schema_builder.add_text_field(
      "path", schema::STRING | schema::STORED);
  let title_field = schema_builder.add_text_field(
      "title", schema::TEXT | schema::STORED);
  let schema = schema_builder.build();

  // Touch all the .skg files that will be indexed,
  // so that the index needs to be completely rebuilt.
  for entry in fs::read_dir("tests/index/fixtures")?
  { let path = entry?.path();
    if path.extension().map_or(false, |ext| ext == "skg")
    { let now = SystemTime::now();
      filetime::set_file_mtime(
	  &path,
	  filetime::FileTime::from_system_time(now))?; } }

  // Build, or find and update, the index
  let index_path = "tests/index/generated/index.tantivy";
  let index = get_or_create_index(schema.clone(), index_path)?;
  let indexed_count = update_index(
      &index,
      path_field,
      title_field,
      "tests/index/fixtures",
      Path::new(index_path))?;

  assert!(indexed_count > 0,
	  "Expected to index at least one title");

  let (best_matches, searcher) = search_index(
      &index,
      title_field,
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
