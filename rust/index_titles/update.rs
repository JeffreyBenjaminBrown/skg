use crate::hyperlinks::replace_each_link_with_its_label;
use crate::types::TantivyIndex;

use tantivy::{ doc,
               schema };
use walkdir::WalkDir;
use std::{ fs,
           path::Path,
           time::SystemTime };
use serde_yaml::from_str;


pub fn update_index (
  tantivy_index : &TantivyIndex,
  data_dir      : &str,
  index_path    : &Path,
  index_is_new  : bool
) -> Result < usize,
              Box < dyn std::error::Error > > {
  // Defines `index_mtime`, the presumed age of the index.
  // Then calls `populate_index`.

  // TODO: Pull this logic into `populate_index`, maybe separating it into a function that `populate_index` calls.

  println! ( "Updating index with .skg files from {}.",
              data_dir );
  let index_mtime =
    // Using UNIX_EPOCH marks the index as older than any files `populate_index` will consider indexing, so they will all be indexed.
    if index_is_new { SystemTime::UNIX_EPOCH }
    else { get_modification_time ( index_path )
           . unwrap_or ( SystemTime::UNIX_EPOCH ) };
  populate_index (
    &mut tantivy_index.index.writer ( 50_000_000 )?,
    data_dir,
    tantivy_index,
    |path| needs_indexing (
      path, index_mtime) )
}

fn populate_index <F> (
  index_writer  : &mut tantivy::IndexWriter,
  data_dir      : &str,
  tantivy_index : &TantivyIndex,
  should_index  : F
) -> Result < usize,
              Box < dyn std::error::Error > >
where
  F : Fn ( &Path ) -> bool {

  let mut indexed_count = 0;
  for entry in WalkDir::new ( data_dir )
    . into_iter () . filter_map ( Result::ok ) {
      let path : &Path = entry.path ();
      if ( not_a_skg_file_path ( path ) ||
           !should_index ( path ) ) {
        continue; }
      if let Some (title) = title_from_skg_file (path) {
        delete_documents_with_path_from_index(
          index_writer, path, tantivy_index.path_field)?;
        add_document_and_title_to_index(
          index_writer, path, &title, tantivy_index)?;
        indexed_count += 1; } }
  if indexed_count > 0 {
    println! ( "Indexed {} files. Committing changes...",
                indexed_count );
    index_writer.commit () ?; }
  else {
    println! ("No files to index found."); }
  Ok (indexed_count) }

pub fn title_from_skg_file (
  // Gets the title from the file at path,
  // runs replace_each_link_with_its_label on it,
  // and returns it.
  path : &Path )
  -> Option < String > {

  if let Ok (file_content) = fs::read_to_string (path) {
    if let Ok (yaml_value) = ( from_str::<serde_yaml::Value>
                               (&file_content) ) {
      if let Some (title_value) = yaml_value.get ("title") {
        if let Some (title_str) = title_value.as_str () {
          return Some (
            replace_each_link_with_its_label (
              title_str )); }} }}
  None }

pub fn delete_documents_with_path_from_index (
  // PITFALL: Does nothing until `commit` is called,
  // which happens outside this function, to permit batching.
  index_writer: &mut tantivy::IndexWriter,
  path: &Path,
  path_field: schema::Field
) -> Result<(), Box<dyn std::error::Error>> {

  let path_str : String = path
    . to_string_lossy () // "non-UTF-8 sequences are replaced with U+FFFD REPLACEMENT CHARACTER"
    . to_string ();
  let term = schema::Term::from_field_text (
    path_field, &path_str );
  index_writer.delete_term (term); // Deletes anything with this path.
  Ok (()) }

pub fn add_document_and_title_to_index (
  // PITFALL: Does nothing until `commit` is called,
  // which happens outside this function, to permit batching.
  index_writer: &mut tantivy::IndexWriter,
  path: &Path,
  title: &str,
  tantivy_index: &TantivyIndex
) -> Result<(), Box<dyn std::error::Error>> {

  let path_str = path . to_string_lossy () . to_string ();
  index_writer.add_document ( doc! (
    // These `=>` symbols are a Tantivy macro.
    tantivy_index.path_field => path_str,
    tantivy_index.title_field => title.to_string ()
  )) ?;
  Ok (()) }

pub fn needs_indexing ( // based on modification time
  // If the path's mtime is more recent, the path needs indexing.
  path: &Path,
  index_mtime: SystemTime
) -> bool {

  match get_modification_time (path) {
    Ok (file_mtime) => file_mtime > index_mtime,
    Err(_) => true // If its modification time is unknown,
                   // assume it needs indexing.
  } }

pub fn not_a_skg_file_path (
  path: &Path
) -> bool {

  ! path . extension () . map_or (
    false, |ext| ext == "skg" ) ||
  ( path.to_string_lossy ()
    . contains ("index.tantivy") ) }

pub fn get_modification_time (
  path: &Path
) -> Result<SystemTime,
            Box<dyn std::error::Error>> {

  let metadata = fs::metadata (path)?;
  Ok (metadata.modified()?) }
