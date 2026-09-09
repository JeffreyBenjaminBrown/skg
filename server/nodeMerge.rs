pub mod nodeMergeInstructionTriple;
pub mod validate_nodeMerge;

use crate::dbs::init::rebuild_tantivy_from_nodes;
use crate::dbs::in_rust_graph::{
  InRustGraph,
  InRustGraphHandle,
  apply_definenodes_to_inRustGraph,
};
use crate::save::{
  nodecompletes_from_graph,
  prepare_fs_update_from_selected,
  update_tantivy_from_saveinstructions,
};
use crate::types::misc::{ID, SkgConfig, TantivyIndex};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, NodeMerge, SaveNode};
use std::error::Error;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

/// Apply merge definitions to files and publish their matching graph/search pair.
/// Returns a replacement index only when index reconstruction was required.
pub async fn merge_nodes (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
  graph              : &InRustGraphHandle,
) -> Result < Option<TantivyIndex>, Box<dyn Error> > {
  let _write_guard = crate::write_lock::acquire_graph_write_lock () . await;
  if let Some (through) =
    crate::dbs::tantivy::background_writer::latest_tantivy_generation ()
  { let _ = crate::dbs::tantivy::background_writer::wait_for_tantivy_writes_through (
      through); }
  merge_nodes_with_hoist_approval (
    nodeMerge_instructions, config, tantivy_index, graph,
    &HashSet::new () ) . await
}

pub(crate) async fn merge_nodes_with_hoist_approval (
  nodeMerge_instructions : &[NodeMerge],
  config             : SkgConfig,
  tantivy_index      : &TantivyIndex,
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
    crate::serve::handlers::telescope_hoist::candidates_from_selected (
      &graph . load_full () . graph, &[], nodeMerge_instructions, &config ) ?;
  if crate::serve::handlers::telescope_hoist::needs_confirmation (
      &candidates, hoist_approved_pids ) {
    return Err (format! (
      "Refusing node merge: it would consume title/body selected below home for {} without exact interactive Hoist approval.",
      candidates . iter ()
        . map ( |candidate| candidate . pid . as_str () )
        . collect::<Vec<&str>> () . join (", ") ) . into ()); }
  tracing::info!(
    "Merging nodes in FS and the graph/search pair, in that order ..." );

  let primary_definenodes : Vec<DefineNode> =
    nodeMerge_instructions . iter ()
    . flat_map ( |m| m . to_vec () )
    . collect ();
  let old_selected = graph . load_full ();
  let mut candidate_graph : InRustGraph = (*old_selected . graph) . clone ();
  apply_definenodes_to_inRustGraph (
    &mut candidate_graph, &primary_definenodes);
  let candidate_nodes : Vec<NodeComplete> =
    nodecompletes_from_graph (&candidate_graph);
  let mut selected_manifest = (*old_selected . manifest) . clone ();

  { // Filesystem.
    tracing::info!("1) Merging in filesystem ...");
    let prepared = prepare_fs_update_from_selected (
      &primary_definenodes,
      &[], // No source-moves during a merge.
      &config,
      hoist_approved_pids, Some (&old_selected))?
      . with_selected_fence (&old_selected . manifest);
    prepared . validate_selected_fence ()?;
    prepared . apply (&config) ?;
    prepared . apply_to_manifest (&mut selected_manifest);
    tracing::info!("   Filesystem merge complete."); }

  let context_labels : HashMap<ID, String> = crate::context::context_origin_types_for_transition (
    &old_selected . graph, &candidate_graph, &primary_definenodes,
    &old_selected . cyclic_roots);
  let mut index_definitions : Vec<DefineNode> = primary_definenodes . clone ();
  let instructed : HashSet<ID> = primary_definenodes . iter ()
    . map (|definition| match definition {
      DefineNode::Save (node) => node . 0 . pid . clone (),
      DefineNode::Delete (node) => node . id . clone (),
    }) . collect ();
  for node in &candidate_nodes {
    if context_labels . contains_key (&node . pid)
       && !instructed . contains (&node . pid) {
      index_definitions . push (DefineNode::Save (SaveNode (node . clone ()))); }}
  let replacement : Option<TantivyIndex> =
    match update_tantivy_from_saveinstructions (
      &index_definitions, tantivy_index,
      &context_labels )
    { Ok (_count) => {
        tracing::info!("   Tantivy merge complete.");
        None }
      Err (e) => {
        tracing::error!(
          "Tantivy merge failed: {}. Rebuilding from candidate graph...", e);
        let new_index : TantivyIndex = match
          rebuild_tantivy_from_nodes (tantivy_index, &candidate_nodes,
            &crate::context::context_origin_types_for_graph (
              &candidate_graph, &old_selected . cyclic_roots)) {
          Ok (index) => index,
          Err (rebuild_error) => {
            let reason = format! (
              "Tantivy is poisoned after node merge: candidate reconstruction \
               failed ({})", rebuild_error);
            graph . store (Arc::new (
              old_selected . with_tantivy_poisoned (reason . clone ())));
            return Err (reason . into ()); }};
        tracing::warn!(
          "NodeMerge succeeded, but Tantivy used candidate reconstruction.");
        Some (new_index) }};
  graph . store (Arc::new (
    old_selected . with_acknowledged_rebuild (
      candidate_graph, selected_manifest)
      . with_searcher (replacement . as_ref () . unwrap_or (tantivy_index)
        . reader . searcher ())));
  tracing::info!("   In-Rust graph and selected manifest published.");
  Ok (replacement) }
