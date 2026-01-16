mod typedb;
mod fs;
mod tantivy;
pub mod mergeInstructionTriple;
pub mod validate_merge;

use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::MergeInstructionTriple;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Applies the MergeInstructionTriples to the graph.
/// Updates three systems in order:
///   1) TypeDB
///   2) Filesystem
///   3) Tantivy
/// PITFALL | TODO: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes (
  merge_instructions : Vec<MergeInstructionTriple>,
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
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
      &merge_instructions ) ?;
    println!( "   Filesystem merge complete." ); }
  { println!( "3) Merging in Tantivy ..." );
    tantivy::merge_nodes_in_tantivy (
      &merge_instructions, tantivy_index ) ?;
    println!( "   Tantivy merge complete." ); }
  Ok (( )) }
