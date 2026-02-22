mod neo4j;
mod fs;
mod tantivy;
pub mod mergeInstructionTriple;
pub mod validate_merge;

use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::Merge;
use std::error::Error;
use neo4rs::Graph;

/// Applies the Merges to the graph.
/// Updates three systems in order:
///   1) Neo4j
///   2) Filesystem
///   3) Tantivy
/// PITFALL | TODO: If any but the first step fails,
///   the resulting system state is invalid.
pub async fn merge_nodes (
  merge_instructions : Vec<Merge>,
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  graph              : &Graph,
) -> Result < (), Box<dyn Error> > {
  println!(
    "Merging nodes in Neo4j, FS, and Tantivy, in that order ..." );
  { println!( "1) Merging in Neo4j ..." );
    neo4j::merge_nodes_in_neo4j (
      graph,
      &merge_instructions
    ). await ?;
    println!( "   Neo4j merge complete." ); }
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
