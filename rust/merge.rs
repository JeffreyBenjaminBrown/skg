mod typedb;
mod fs;
mod tantivy;
mod merge3saveinstructions;

pub use merge3saveinstructions::instructiontriples_from_the_merges_in_an_orgnode_forest;

use crate::types::{MergeInstructionTriple, SkgConfig};
use std::error::Error;
use ::tantivy::Index;
use typedb_driver::TypeDBDriver;

/// Merges nodes in the graph by applying MergeInstructionTriple.
/// Updates three systems in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes_in_graph (
  merge_instructions : Vec<MergeInstructionTriple>,
  config             : SkgConfig,
  tantivy_index      : &Index,
  driver             : &TypeDBDriver,
) -> Result < (), Box<dyn Error> > {

  println!(
    "Merging nodes in TypeDB, FS, and Tantivy, in that order ..." );
  let db_name : &str = &config.db_name;

  { println!( "1) Merging in TypeDB database '{}' ...", db_name );
    typedb::merge_nodes_in_typedb (
      db_name,
      driver,
      &merge_instructions
    ). await ?;
    println!( "   TypeDB merge complete." ); }
  { println!( "2) Merging in filesystem ..." );
    fs::merge_nodes_in_fs (
      config.clone (),
      &merge_instructions
    ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    tantivy::merge_nodes_in_tantivy (
      &merge_instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }
