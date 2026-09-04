pub mod nodeMergeInstructionTriple;
pub mod validate_nodeMerge;

use crate::dbs::init::{rebuild_tantivy_from_nodes, wipe_then_init_typedb_db};
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  apply_definenodes_to_inRustGraph,
};
use crate::nodeMerge::nodeMergeInstructionTriple::neighbor_savenodes_for_nodeMerges;
use crate::save::{
  nodecompletes_from_graph,
  prepare_fs_update,
  update_tantivy_from_saveinstructions,
  update_typedb_from_saveinstructions,
};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, NodeMerge, SaveNode};
use std::error::Error;
use std::collections::HashSet;
use std::sync::Arc;
use typedb_driver::TypeDBDriver;

/// Applies NodeMerges by fanning a single 'Vec<DefineNode>' through the
/// four ordinary sink functions. Four sinks, in order:
///   1) Filesystem (source of truth)
///   2) In-Rust graph
///   3) TypeDB (with recovery: rebuild from disk on failure)
///   4) Tantivy (with recovery: rebuild from disk on failure)
///
/// Returns 'None' when all four stores updated normally.
/// Returns 'Some(new_index)' when Tantivy had to be rebuilt.
///
/// PITFALL: TypeDB receives neighbor SaveNodes in addition to the
/// primary 3N DefineNodes from 'NodeMerge::to_vec()'. Their purpose is to
/// close the temporal gap where TypeDB's cascade-delete of the
/// acquiree destroys inbound edges and nothing re-creates them until
/// neighbors are saved. FS, graph, and Tantivy see only the primary
/// 3N: neighbor .skg files are unchanged (acquiree_id stays in
/// neighbor fields, resolved to acquirer via extra_id at read time);
/// the in-Rust graph stores outbound-only references just like disk;
/// Tantivy indexes title+body+aliases, none of which change on a
/// neighbor during a merge.
pub async fn merge_nodes (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  driver             : &TypeDBDriver,
  graph              : &InRustGraphHandle,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  if let Some (through) =
    crate::dbs::tantivy::background_writer::latest_tantivy_generation ()
  { let _ = crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_through (
      through); }
  merge_nodes_with_hoist_approval (
    nodeMerge_instructions, config, tantivy_index, driver, graph,
    &HashSet::new () ) . await
}

pub(crate) async fn merge_nodes_with_hoist_approval (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  driver             : &TypeDBDriver,
  graph              : &InRustGraphHandle,
  hoist_approved_pids : &HashSet<ID>,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  if nodeMerge_instructions . is_empty () {
    return Ok (None); }
  // A direct/noninteractive merge gets the empty approval set from the public
  // wrapper above. Refuse if it would copy text out of an ugly acquiree. The
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
    "Merging nodes in FS, in-Rust graph, TypeDB, and Tantivy, in that order ..." );
  let db_name : &str = &config . db_name;

  let primary_definenodes : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |m| m . to_vec () )
    . collect ();
  let neighbor_savenodes : Vec<SaveNode> =
    neighbor_savenodes_for_nodeMerges (
      nodeMerge_instructions, &config, driver ) . await ?;
  let old_selected = graph . load_full ();
  let mut candidate_graph : InRustGraph = (*old_selected . graph) . clone ();
  apply_definenodes_to_inRustGraph (
    &mut candidate_graph, &primary_definenodes);
  let candidate_nodes : Vec<NodeComplete> =
    nodecompletes_from_graph (&candidate_graph);
  let mut selected_manifest = old_selected . manifest . clone ();

  { // Filesystem.
    tracing::info!("1) Merging in filesystem ...");
    let prepared = prepare_fs_update (
      &primary_definenodes,
      &[], // No source-moves during a merge.
      &config,
      hoist_approved_pids ) ?
      . with_selected_fence (&old_selected . manifest);
    prepared . validate_selected_fence ()?;
    prepared . apply (&config) ?;
    prepared . apply_to_manifest (&mut selected_manifest);
    tracing::info!("   Filesystem merge complete."); }

  { // TypeDB: primary + neighbor SaveNodes.
    let typedb_definenodes : Vec<DefineNode> = {
      let mut v : Vec<DefineNode> =
        primary_definenodes . clone ();
      for sn in & neighbor_savenodes {
        v . push ( DefineNode::Save ( sn . clone () )); }
      v };
    if let Err (e) = update_typedb_from_saveinstructions (
      db_name, driver, &typedb_definenodes, &[],
      None ) . await // bulk recreate: pid migration makes a set-diff subtle
    { tracing::error!(
        "   TypeDB merge failed: {}. Reconstructing candidate graph...", e);
      if let Err (rebuild_error) = wipe_then_init_typedb_db (
        &config, driver, &candidate_nodes) . await {
        let reason = format! (
          "TypeDB is poisoned after node merge: candidate reconstruction \
           failed ({})", rebuild_error);
        graph . store (Arc::new (
          old_selected . with_typedb_poisoned (reason . clone ())));
        return Err (reason . into ()); }
      tracing::warn!(
        "NodeMerge succeeded, but TypeDB used complete candidate reconstruction.");
    } else {
      tracing::info!("   TypeDB merge complete."); } }

  let replacement : Option<TantivyIndex> =
    match update_tantivy_from_saveinstructions (
      &primary_definenodes, tantivy_index,
      &std::collections::HashMap::new () ) // merged nodes index with "" context type; refreshed at next rebuild
    { Ok (_count) => {
        tracing::info!("   Tantivy merge complete.");
        None }
      Err (e) => {
        tracing::error!(
          "Tantivy merge failed: {}. Rebuilding from candidate graph...", e);
        let new_index : TantivyIndex = match
          rebuild_tantivy_from_nodes (&config, &candidate_nodes) {
          Ok (index) => index,
          Err (rebuild_error) => {
            let reason = format! (
              "Tantivy is poisoned after node merge: candidate reconstruction \
               failed ({})", rebuild_error);
            let selected = old_selected . with_acknowledged_rebuild (
              candidate_graph, selected_manifest);
            graph . store (Arc::new (
              selected . with_tantivy_poisoned (reason . clone ())));
            return Err (reason . into ()); }};
        tracing::warn!(
          "NodeMerge succeeded, but Tantivy used candidate reconstruction.");
        Some (new_index) }};
  graph . store (Arc::new (
    old_selected . with_acknowledged_rebuild (
      candidate_graph, selected_manifest)));
  tracing::info!("   In-Rust graph and selected manifest published.");
  Ok (replacement) }
