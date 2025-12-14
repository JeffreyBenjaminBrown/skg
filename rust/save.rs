// DON'T IMPORT these re-exports. They are for documentation.
// INSTEAD, imports in the codebase should use the original,
// longer definition path. That makes it easier to find definitions.

pub mod fs;
pub mod tantivy;
pub mod typedb;

pub use fs::update_fs_from_saveinstructions;
pub use tantivy::update_index_from_saveinstructions;
pub use typedb::update_typedb_from_saveinstructions;

use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::SaveInstruction;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Updates **everything** from the given `SaveInstruction`s, in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn update_graph_minus_merges (
  instructions  : Vec<SaveInstruction>,
  config        : SkgConfig,
  tantivy_index : &TantivyIndex,
  driver        : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {
  println!( "Updating (1) TypeDB, (2) FS, and (3) Tantivy ..." );

  let db_name : &str = &config.db_name;

  { println!( "1) Updating TypeDB database '{}' ...", db_name );
    update_typedb_from_saveinstructions (
      db_name,
      driver,
      &instructions ). await ?;
    println!( "   TypeDB update complete." ); }

  { // filesystem
    let total_input : usize = instructions.len ();
    // TODO Phase 5: Print per-source write information
    println!( "2) Writing {} instruction(s) to disk ...",
               total_input );
    let (deleted_count, written_count) : (usize, usize) =
      update_fs_from_saveinstructions (
        instructions.clone (), config.clone ()) ?;
    println!( "   Deleted {} file(s), wrote {} file(s).",
              deleted_count, written_count ); }

  { // Tantivy
    println!( "3) Updating Tantivy index ..." );
    let indexed_count : usize =
      update_index_from_saveinstructions (
        &instructions, tantivy_index )?;
    println!( "   Tantivy updated for {} document(s).",
                  indexed_count ); }

  println!( "All updates finished successfully." );
  Ok (( )) }
