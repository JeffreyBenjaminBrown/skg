use crate::file_io::write_all_filenodes;
use crate::tantivy::update_index_with_filenodes;
use crate::typedb::update::update_nodes_and_relationships;
use crate::types::{SkgConfig, TantivyIndex, FileNode};

use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Updates **everything** from the given `FileNode`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_fs_and_dbs (
  filenodes     : Vec<FileNode>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;
  println!( "1) Updating TypeDB database '{}' ...", db_name );
  update_nodes_and_relationships (
    db_name,
    driver,
    &filenodes, ). await ?;
  println!( "   TypeDB update complete." );

  let total_input : usize =
    filenodes.len ();
  let target_dir  : &std::path::Path =
    &config.skg_folder;
  println!( "2) Writing {} file(s) to disk at {:?} ...",
            total_input, target_dir );
  let written_count : usize =
    write_all_filenodes (
      filenodes.clone (), config.clone () ) ?;
  println!( "   Wrote {} file(s).", written_count );

  println!( "3) Updating Tantivy index ..." );
  let indexed_count : usize =
    update_index_with_filenodes (
      &filenodes, tantivy_index )?;
  println!( "   Tantivy updated for {} document(s).",
                indexed_count );

  println!( "All updates finished successfully." );
  Ok (( )) }
