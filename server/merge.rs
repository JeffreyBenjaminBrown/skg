pub mod mergeInstructionTriple;
pub mod validate_merge;

use crate::dbs::filesystem::multiple_nodes::{
  check_for_duplicate_ids_across_sources,
  read_all_skg_files_from_sources};
use crate::dbs::init::{rebuild_tantivy_from_nodes, wipe_then_init_typedb_db};
use crate::dbs::memory::{InRustGraphHandle, apply_definenodes};
use crate::merge::mergeInstructionTriple::neighbor_savenodes_for_merges;
use crate::save::{ update_fs_from_saveinstructions, update_tantivy_from_saveinstructions, update_typedb_from_saveinstructions };
use crate::types::misc::{SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, Merge, SaveNode};
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Applies Merges by fanning a single 'Vec<DefineNode>' through the
/// four ordinary sink functions. Four sinks, in order:
///   1) Filesystem (source of truth)
///   2) In-memory graph
///   3) TypeDB (with recovery: rebuild from disk on failure)
///   4) Tantivy (with recovery: rebuild from disk on failure)
///
/// Returns 'None' when all four stores updated normally.
/// Returns 'Some(new_index)' when Tantivy had to be rebuilt.
///
/// PITFALL: TypeDB receives neighbor SaveNodes in addition to the
/// primary 3N DefineNodes from 'Merge::to_vec()'. Their purpose is to
/// close the temporal gap where TypeDB's cascade-delete of the
/// acquiree destroys inbound edges and nothing re-creates them until
/// neighbors are saved. FS, graph, and Tantivy see only the primary
/// 3N: neighbor .skg files are unchanged (acquiree_id stays in
/// neighbor fields, resolved to acquirer via extra_id at read time);
/// the in-memory graph stores outbound-only references just like disk;
/// Tantivy indexes title+body+aliases, none of which change on a
/// neighbor during a merge.
pub async fn merge_nodes (
  merge_instructions : &[Merge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  driver             : &TypeDBDriver,
  graph              : &InRustGraphHandle,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  if merge_instructions . is_empty () {
    return Ok (None); }
  tracing::info!(
    "Merging nodes in FS, in-memory graph, TypeDB, and Tantivy, in that order ..." );
  let db_name : &str = &config . db_name;

  let primary_definenodes : Vec<DefineNode> =
    merge_instructions . iter ()
    . flat_map ( |m| m . to_vec () )
    . collect ();
  let neighbor_savenodes : Vec<SaveNode> =
    neighbor_savenodes_for_merges (
      merge_instructions, &config, driver ) . await ?;

  { // Filesystem.
    tracing::info!("1) Merging in filesystem ...");
    update_fs_from_saveinstructions (
      &primary_definenodes,
      &[], // No source-moves during a merge.
      config . clone () ) ?;
    tracing::info!("   Filesystem merge complete."); }

  { // In-memory graph.
    apply_definenodes (graph, &primary_definenodes);
    tracing::info!("   In-memory graph merge complete."); }

  { // TypeDB: primary + neighbor SaveNodes.
    let typedb_definenodes : Vec<DefineNode> = {
      let mut v : Vec<DefineNode> =
        primary_definenodes . clone ();
      for sn in & neighbor_savenodes {
        v . push ( DefineNode::Save ( sn . clone () )); }
      v };
    if let Err (e) = update_typedb_from_saveinstructions (
      db_name, driver, &typedb_definenodes, &[] ) . await
    { tracing::error!(
        "   TypeDB merge failed: {}. Rebuilding from disk...", e);
      let nodes : Vec<NodeComplete> =
        read_all_skg_files_from_sources (&config)
        . map_err ( |e2| -> Box<dyn Error> { format!(
           "TypeDB rebuild also failed: {}. Restart the server.", e2)
           . into () } ) ?;
      check_for_duplicate_ids_across_sources (
        &nodes, &config . data_root)
        . map_err ( |e2| -> Box<dyn Error> { format!(
           "TypeDB rebuild also failed: {}. Restart the server.", e2)
           . into () } ) ?;
      wipe_then_init_typedb_db (&config, driver, &nodes) . await
        . map_err ( |e2| -> Box<dyn Error> { format!(
           "TypeDB rebuild also failed: {}. Restart the server.", e2)
           . into () } ) ?;
      tracing::warn!(
        "Merge succeeded, but TypeDB had to be rebuilt from disk.");
    } else {
      tracing::info!("   TypeDB merge complete."); } }

  { // Tantivy.
    match update_tantivy_from_saveinstructions (
      &primary_definenodes, tantivy_index )
    { Ok (_count) => {
        tracing::info!("   Tantivy merge complete.");
        Ok (None) }
      Err (e) => {
        tracing::error!(
          "Tantivy merge failed: {}. Rebuilding from disk...", e);
        let nodes : Vec<NodeComplete> =
          read_all_skg_files_from_sources (&config)
          . map_err (|e2| -> Box<dyn Error> {
            format!("Tantivy rebuild also failed: {}. Restart the server.", e2)
            . into () }) ?;
        check_for_duplicate_ids_across_sources (
          &nodes, &config . data_root)
          . map_err (|e2| -> Box<dyn Error> {
            format!("Tantivy rebuild also failed: {}. Restart the server.", e2)
            . into () }) ?;
        let new_index : TantivyIndex =
          rebuild_tantivy_from_nodes (&config, &nodes)
          . map_err (|e2| -> Box<dyn Error> {
            format!("Tantivy rebuild also failed: {}. Restart the server.", e2)
            . into () }) ?;
        tracing::warn!(
          "Merge succeeded, but Tantivy had to be rebuilt from disk.");
        Ok (Some (new_index)) }}} }
