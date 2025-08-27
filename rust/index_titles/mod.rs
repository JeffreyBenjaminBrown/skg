// PURPOSE: Creates and updates a Tantivy index,
// which associates titles to filenames
// (potentially many to one).

mod update;

pub use update:: { update_index,
                   delete_documents_with_path_from_index,
                   add_document_and_title_to_index,
                   needs_indexing,
                   not_a_skg_file_path,
                   get_modification_time };
use crate::types::TantivyIndex;

use tantivy::{ Index,
               collector::TopDocs,
               schema };
use std::{ fs,
           path::Path, };

pub fn search_index (
  tantivy_index : &TantivyIndex,
  query_text    : &str
) -> Result < ( Vec< ( f32, // relevance score
                       tantivy::DocAddress )>, // ID for a tantivy Document. (Not a filepath.)
                tantivy::Searcher ),
              Box <dyn std::error::Error> > {
  // Returns the top 10 matching (tantivy) "Documents".

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

pub fn get_extant_index_or_create_empty_one (
  // If it creates an index, the index is empty.
  // But if it fetches an index, the index might not be.
  schema: schema::Schema,
  index_folder: &Path, // The index is an entire folder.
) -> Result< (Index,
              bool), // True if the index is new.
              Box<dyn std::error::Error>> {

  if index_folder.exists() {
    println!("Attempting to open existing index at {:?}",
             index_folder);
    match Index::open_in_dir(index_folder) {
      Ok(index) => {
        println!("Successfully opened existing index");
        Ok((index,false)) },
      Err(e) => {
        println!(
          "Failed to open existing index: {:?}. Recreating...", e);
        { fs::remove_dir_all(index_folder)?;
          fs::create_dir_all(index_folder)?; };
        println! ( "Creating new index at {:?}",
                    index_folder );
        Ok ((
          Index::create_in_dir ( index_folder, schema )?,
          true )) }} }
  else {
    println!( "Creating new index at {:?}",
               index_folder);
    fs::create_dir_all ( index_folder )?;
    Ok (( Index::create_in_dir(index_folder, schema)?,
          true )) }}
