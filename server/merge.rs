mod typedb;
mod fs;
mod tantivy;
pub mod mergeInstructionTriple;
pub mod validate_merge;

use crate::dbs::init::{rebuild_typedb_from_disk, rebuild_tantivy_from_disk};
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::save::Merge;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Applies the Merges to the graph.
/// Updates three systems in order:
///   1) Filesystem (source of truth)
///   2) TypeDB (with recovery: rebuild from disk on failure)
///   3) Tantivy (with recovery: rebuild from disk on failure)
/// Returns `None` when all three stores updated normally.
/// Returns `Some(new_index)` when Tantivy had to be rebuilt.
pub async fn merge_nodes (
  merge_instructions : Vec<Merge>,
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  driver             : &TypeDBDriver,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  tracing::info!(
    "Merging nodes in FS, TypeDB, and Tantivy, in that order ..." );
  let db_name : &str = &config . db_name;

  { tracing::info!("1) Merging in filesystem ...");
    fs::merge_nodes_in_fs (
      config . clone (),
      &merge_instructions ) ?;
    tracing::info!("   Filesystem merge complete."); }

  if let Err (e)
    = typedb::merge_nodes_in_typedb (
      db_name,
      driver,
      &merge_instructions ) . await
    { tracing::error!("   TypeDB merge failed: {}. Rebuilding from disk...", e);
      rebuild_typedb_from_disk (&config, driver) . await
        . map_err ( |e2| -> Box<dyn Error> { format!(
           "TypeDB rebuild also failed: {}. Restart the server.", e2)
           . into () } ) ?;
      tracing::warn!(
        "Merge succeeded, but TypeDB had to be rebuilt from disk.");
    } else {
      tracing::info!("   TypeDB merge complete."); }

  match tantivy::merge_nodes_in_tantivy (
    &merge_instructions, tantivy_index )
    { Ok (()) => { tracing::info!("   Tantivy merge complete.");
                   Ok (None) }
      Err (e) => {
        tracing::error!("Tantivy merge failed: {}. Rebuilding from disk...", e);
        let new_index : TantivyIndex =
          rebuild_tantivy_from_disk (&config)
          . map_err (|e2| -> Box<dyn Error> {
            format!("Tantivy rebuild also failed: {}. Restart the server.", e2)
            . into () }) ?;
        tracing::warn!("Merge succeeded, but Tantivy had to be rebuilt from disk.");
        Ok (Some (new_index)) }} }
