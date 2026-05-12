pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;
pub mod classify;

use crate::dbs::filesystem::one_node::optnodecomplete_from_id;
use crate::types::misc::{ID, SkgConfig};
use crate::types::nodes::complete::NodeComplete;
use crate::types::save::{DefineNode, SaveNode, SourceMove};
use crate::types::viewnode::ViewNode;
use crate::types::views_state::nodecomplete_from_in_rust_graph;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use super::supplement_from_disk::{ canonicalize_ids_from_disk, detect_source_move, supplement_unspecified_fields_from_disk, };
use super::validate::buffernode_differs_from_disknode;
use to_naive_instructions::naive_saveinstructions_from_tree;

use ego_tree::Tree;
use std::error::Error;
use typedb_driver::TypeDBDriver;

/// Converts a viewforest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and supplementing None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// The initial extraction is called "naive" because its output
/// is preliminary: it has not yet gone through same-ID reconciliation,
/// unchanged filtering, or disk supplementation.
pub async fn viewforest_to_nonmerge_save_instructions (
  viewforest : &Tree<ViewNode>, // "viewforest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver,
) -> Result<(Vec<DefineNode>, Vec<SourceMove>), Box<dyn Error>> {
  let naive_instructions : Vec<DefineNode> =
    naive_saveinstructions_from_tree (
      viewforest . clone( )) ?;
  let instructions_without_dups : Vec<DefineNode> =
    reconcile_same_id_instructions (naive_instructions) ?;
  let changed_instructions : Vec<DefineNode> =
    filter_unchanged_save_instructions (
      instructions_without_dups );
  let mut result : Vec<DefineNode> =
    Vec::with_capacity ( changed_instructions . len() );
  let mut source_moves : Vec<SourceMove> = Vec::new();
  for instr in changed_instructions {
    let instr : DefineNode = instr;
    match instr {
      DefineNode::Delete (_) =>
        result . push (instr),
      DefineNode::Save (SaveNode (from_buffer)) => {
        let from_buffer : NodeComplete = from_buffer;
        let pid : ID =
          from_buffer . pid . clone();
        let from_disk : Option<NodeComplete> =
          optnodecomplete_from_id (config, driver, &pid) . await ?;
        match from_disk {
          None => result . push (
            DefineNode::Save (SaveNode (from_buffer))),
          Some (disk_node) => {
            let disk_node : NodeComplete = disk_node;
            let canonicalized : NodeComplete =
              canonicalize_ids_from_disk (from_buffer, &disk_node) ?;
            let maybe_move : Option<SourceMove> =
              detect_source_move ( config,  &pid,
                                   &canonicalized . source,
                                   &disk_node . source) ?;
            let supplemented : NodeComplete =
              supplement_unspecified_fields_from_disk (
                canonicalized, &disk_node);
            result . push (DefineNode::Save (SaveNode (supplemented)));
            if let Some (sm) = maybe_move {
              let sm : SourceMove = sm;
              source_moves . push (sm); }}} }}}
  Ok ((result, source_moves)) }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save in-Rust graph entry
/// (nothing changed). Delete instructions and new nodes (not yet
/// in the in-Rust graph) are kept.
fn filter_unchanged_save_instructions (
  instructions : Vec<DefineNode>,
) -> Vec<DefineNode> {
  let initial_count : usize = instructions . len();
  let filtered : Vec<DefineNode> = instructions
    . into_iter()
    . filter(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => {
        match nodecomplete_from_in_rust_graph (&node . pid) {
          Some (pre_save) =>
            buffernode_differs_from_disknode (node, &pre_save),
          None => true, }}
      DefineNode::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_unchanged_save_instructions: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }
