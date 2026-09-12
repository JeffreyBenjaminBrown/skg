pub mod nodeMergeInstructionTriple;
pub mod validate_nodeMerge;

use crate::dbs::filesystem::multiple_nodes::{
  error_unless_each_id_names_one_node,
  read_all_skg_files_from_sources};
use crate::dbs::init::rebuild_tantivy_from_nodes;
use crate::dbs::in_rust_graph::{InRustGraphHandle, apply_definenodes};
use crate::dbs::in_rust_graph::complete_validation::{
  format_complete_graph_errors, validate_graph_after_definitions,
};
use crate::save::{ update_fs_from_saveinstructions_with_hoist_approval, update_tantivy_from_saveinstructions };
use crate::types::env::MutationGate;
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, NodeMerge};
use std::error::Error;
use std::collections::HashSet;

/// Applies NodeMerges to the three stores, in order:
///   1) Filesystem (source of truth)
///   2) In-Rust graph
///   3) Tantivy (with recovery: rebuild from disk on failure)
///
/// Returns 'None' when all stores updated normally.
/// Returns 'Some(new_index)' when Tantivy had to be rebuilt.
///
pub async fn merge_nodes (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  graph              : &InRustGraphHandle,
  mutation_gate      : &MutationGate,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  let _mutation_guard = mutation_gate . lock () . await;
  merge_nodes_with_hoist_approval (
    nodeMerge_instructions, config, tantivy_index, graph,
    &HashSet::new () )
}

pub(crate) fn merge_nodes_with_hoist_approval (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  graph              : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  if nodeMerge_instructions . is_empty () {
    return Ok (None); }
  // A direct/noninteractive merge gets the empty approval set from the public
  // wrapper above. Refuse if it would copy text out of an overPrivateText acquiree. The
  // interactive save path first inserts and verifies an acquiree Hoist repair,
  // so a fresh reread here finds no remaining candidate.
  let candidates =
    crate::serve::handlers::telescope_hoist::candidates_from_disk (
      &[], nodeMerge_instructions, &config ) ?;
  if crate::serve::handlers::telescope_hoist::needs_confirmation (
      &candidates, hoist_approved_pids ) {
    return Err (format! (
      "Refusing node merge: it would consume title/body selected below home for {} without exact interactive Hoist approval.",
      candidates . iter ()
        . map ( |candidate| candidate . pid . as_str () )
        . collect::<Vec<&str>> () . join (", ") ) . into ()); }
  tracing::info!(
    "Merging nodes in filesystem, in-Rust graph, and Tantivy ..." );

  let primary_definenodes : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |m| m . to_vec () )
    . collect ();
  let graph_snapshot = graph . load_full ();
  let validation = validate_graph_after_definitions (
    &config, &graph_snapshot, &primary_definenodes);
  if ! validation . errors . is_empty () {
    return Err (format_complete_graph_errors (&validation . errors) . into ()); }
  { // Filesystem.
    tracing::info!("1) Merging in filesystem ...");
    update_fs_from_saveinstructions_with_hoist_approval (
      &primary_definenodes,
      &[], // No source-moves during a merge.
      config . clone (),
      hoist_approved_pids ) ?;
    tracing::info!("   Filesystem merge complete."); }

  { // In-Rust graph.
    apply_definenodes (graph, &primary_definenodes);
    tracing::info!("   In-Rust graph merge complete."); }

  { // Tantivy.
    match update_tantivy_from_saveinstructions (
      &primary_definenodes, tantivy_index,
      &std::collections::HashMap::new () ) // merged nodes index with "" context type; refreshed at next rebuild
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
        error_unless_each_id_names_one_node (
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
          "NodeMerge succeeded, but Tantivy had to be rebuilt from disk.");
        Ok (Some (new_index)) }}} }
