pub mod to_naive_instructions;
pub mod reconcile_same_id_instructions;

use crate::types::misc::SkgConfig;
use crate::types::save::{DefineNode, SaveNode};
use crate::types::memory::SkgNodeMap;
use crate::types::viewnode::ViewNode;

use to_naive_instructions::naive_saveinstructions_from_tree;
use reconcile_same_id_instructions::reconcile_same_id_instructions;
use super::validate_foreign_nodes::buffernode_differs_from_disknode;
use super::supplement_from_disk::supplement_none_fields_from_disk_if_save;
use ego_tree::Tree;
use typedb_driver::TypeDBDriver;
use std::error::Error;

/// Converts a forest of ViewNodes to DefineNodes,
/// reconciling duplicates via 'reconcile_same_id_instructions'
/// and supplementing None fields with data from disk.
/// ASSUMES indefinitive nodes produced no instructions.
/// (That filtering is done by 'naive_saveinstructions_from_tree'.)
pub async fn viewnode_forest_to_nonmerge_save_instructions (
  forest : &Tree<ViewNode>, // "forest" = tree with BufferRoot
  config : &SkgConfig,
  driver : &TypeDBDriver,
  pool   : &SkgNodeMap
) -> Result<Vec<DefineNode>, Box<dyn Error>> {
  let naive_instructions : Vec<DefineNode> =
    naive_saveinstructions_from_tree (
      forest . clone( )) ?;
  let instructions_without_dups : Vec<DefineNode> =
    reconcile_same_id_instructions (naive_instructions) ?;
  let changed_instructions : Vec<DefineNode> =
    filter_unchanged_save_instructions (
      instructions_without_dups, pool );
  let mut result : Vec<DefineNode> = // Mapping supplement_none_fields_from_disk_if_save over changed_instructions would be simpler, but async functions can't be mapped.
    Vec::with_capacity ( changed_instructions . len() );
  for instr in changed_instructions {
    result . push (
      supplement_none_fields_from_disk_if_save (
        config, driver, pool, instr
      ) . await ? ); }
  Ok (result) }

/// Filters out Save instructions that would be no-ops,
/// because they match the pre-save pool entry (nothing changed).
/// Delete instructions and new nodes (not in pool) are kept.
fn filter_unchanged_save_instructions (
  instructions : Vec<DefineNode>,
  pool         : &SkgNodeMap,
) -> Vec<DefineNode> {
  let initial_count : usize = instructions . len();
  let filtered : Vec<DefineNode> = instructions
    . into_iter()
    . filter(|instr| match instr {
      DefineNode::Save(SaveNode (node)) => {
        match node . primary_id() {
          Ok (id) => match pool . get (id) {
            Some (pool_node) =>
              buffernode_differs_from_disknode (node, pool_node),
            None => true, },
          Err (_) => true, }}
      DefineNode::Delete (_) => true, })
    . collect();
  let removed_count : usize = initial_count - filtered . len();
  tracing::debug!("filter_unchanged_save_instructions: \
             kept {} of {} instructions ({} unchanged filtered out)",
            filtered . len(), initial_count, removed_count);
  filtered }
